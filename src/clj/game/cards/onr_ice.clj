(ns game.cards.onr-ice
  (:require
   [clojure.string :as str]
   [cond-plus.core :refer [cond+]]
   [game.core.access :refer [access-bonus access-card breach-server max-access]]
   [game.core.bad-publicity :refer [gain-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed all-installed-runner-type card->server server-list
                            get-all-cards get-all-installed server->zone]]
   [game.core.card :refer [active? agenda? asset? can-be-advanced? card-index
                           corp? corp-installable-type? faceup?
                           get-card get-counters get-zone
                           hardware? has-subtype? ice? in-discard? in-hand? installed? is-type? operation?
                           program? protecting-a-central? protecting-archives? protecting-hq? protecting-rd?
                           resource? rezzed? runner?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage]]
   [game.core.def-helpers :refer [combine-abilities corp-recur defcard
                                  do-brain-damage do-net-damage offer-jack-out
                                  reorder-choice get-x-fn]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [get-effects register-lingering-effect unregister-static-abilities]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid]]
   [game.core.engine :refer [gather-events pay register-events resolve-ability
                             trigger-event trigger-event-simult unregister-events]]
   [game.core.events :refer [run-events]]
   [game.core.finding :refer [find-cid]]
   [game.core.flags :refer [can-rez? card-flag? prevent-draw prevent-jack-out
                            register-run-flag! register-turn-flag! run-flag? zone-locked?]]
   [game.core.gaining :refer [gain-credits lose-clicks lose-credits]]
   [game.core.hand-size :refer [hand-size]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [add-sub add-sub! any-subs-broken? break-sub get-current-ice get-run-ices ice-strength-bonus
                          remove-sub! remove-subs! resolve-subroutine
                          set-current-ice unbroken-subroutines-choice update-all-ice update-all-icebreakers
                          update-ice-strength]]
   [game.core.identities :refer [disable-card enable-card]]
   [game.core.initializing :refer [card-init]]
   [game.core.installing :refer [corp-install corp-install-list
                                 corp-install-msg]]
   [game.core.memory :refer [available-mu init-mu-cost]]
   [game.core.moving :refer [as-agenda mill move swap-cards swap-cards-async
                             swap-ice swap-installed trash
                             trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay? cost->string build-cost-label]]
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-icon add-prop remove-icon]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez get-rez-cost rez]]
   [game.core.runs :refer [bypass-ice encounter-ends end-run start-next-phase
                           force-ice-encounter get-current-encounter prevent-access
                           redirect-run set-next-phase]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name protecting-same-server?
                              target-server zone->name]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.subtypes :refer [update-all-subtypes]]
   [game.core.tags :refer [gain-tags lose-tags sum-tag-effects]]
   [game.core.threat :refer [threat threat-level]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]
   ;; imported from ice
   [game.cards.ice :refer [end-the-run end-the-run-unless-runner-pays gain-credits-sub give-tags trash-program-sub do-psi reset-variable-subs]]
   ))

(defn onr-trace-ability
  "Run a trace with specified max strength.
  If successful trigger specified ability"
  ([max {:keys [label] :as ability} only-tags]
   {:label (str "Trace " max " - " label)
    :onr-trace {:max-strength max
                :label label
                :only-tags only-tags
                :successful ability}})
  ([max ability un-ability only-tags]
   (let [label (str (:label ability) " / " (:label un-ability))]
     {:label (str "Trace " max " - " label)
      :onr-trace {:max-strength max
                  :label label
                  :only-tags only-tags
                  :successful ability
                  :unsuccessful un-ability}})))

(defn- trace-tag
  ([max] (trace-tag max 1))
  ([max tags]
   (onr-trace-ability max (give-tags tags) true)))

(defn- trace-net
  ([max] (trace-net max 1))
  ([max net]
   (onr-trace-ability max (do-net-damage net) false)))

(defn- bounce-unless-corp-pays
  ([cost]
   (let [cost (if (integer? cost) [:credit cost] cost)]
     {:event :pass-ice
      :req (req (same-card? (:ice context) card))
      :optional {:prompt (msg "Pay " cost "[Credits] to keep " (:title card) " on the field?")
                 :yes-ability {:cost cost
                               :msg (msg "to keep " (card-str state card) " on the field")}
                 :no-ability {:msg (msg "add " (card-str state card) " to HQ")
                              :effect (effect (move :corp card :hand))}}})))

(defn- bounce-and-corp-gains
  [creds]
  {:event :pass-ice
   :req (req (same-card? (:ice context) card))
   :optional {:prompt (msg "Gain " creds "[Credits] and return " (:title card) " on HQ?")
              :yes-ability {:msg (msg "gains " creds "[Credits] and returns " (:title card) " to HQ")
                            :async true
                            :effect (effect (move :corp card :hand)
                                            (gain-credits eid creds))}}})

(defn- used-noisy-discount
  ([state cost]
   (letfn [(find-breaker-in-run [cid state]
             (let [evs (filter #(and (= :breaker-strength-changed (first %)) (= cid (:cid (first (second %))))) (:events (:run @state)))]
               (first (map #(first (second %)) evs))))
           ;; this finds the cids of every icebreaker used to break a subroutine during this run
           (find-breaker-cids [state]
             (let [broken-sub-events (filter #(= :subroutines-broken (first %)) (:events (:run @state)))
                   ;; this should just explicitly be the ices that were broken this run
                   broken-sub-events (map #(:subroutines (first (second %))) broken-sub-events)
                   broken-subs (filter :broken (flatten broken-sub-events))]
               (map :breaker broken-subs)))
           (noisy-breaker-broke-subs-this-run [state]
             (let [breakers (map #(find-breaker-in-run % state) (find-breaker-cids state))]
               (some #(= "Noisy" %) (first (map :subtypes breakers)))))
           (pump-breaker-events [state]
             (map second (filter #(or (= :pump-breaker (first %)) (= :subroutines-broken (first %))) (:events (:run @state)))))
           (noisy-was-pumped [state]
             (some #(has-subtype? % "Noisy") (map first (pump-breaker-events state))))]
     ;; wow! what a nightmare
     (if (or (noisy-breaker-broke-subs-this-run state)
             (noisy-was-pumped state))
       (- cost) 0))))

(def prevent-breaking-next-ice
  {:msg "prevent the Runner from breaking subroutines on the next piece of ice they encounter this run"
   :effect
   (effect (register-events
             card
             [{:event :encounter-ice
               :duration :end-of-run
               :unregister-once-resolved true
               :msg (msg "prevent the runner from breaking subroutines on " (:title (:ice context)))
               :effect (effect (register-lingering-effect
                                 card
                                 (let [encountered-ice (:ice context)]
                                   {:type :cannot-break-subs-on-ice
                                    :duration :end-of-encounter
                                    :req (req (same-card? encountered-ice (:ice context)))
                                    :value true})))}]))})

(defn- change-subtype-on-rez
  [from to cost cdef]
  (merge cdef {:on-rez {:optional {:prompt (msg "Pay " cost " credits to change subtype to " to)
                                   :yes-ability {:cost (if (integer? cost) [:credit cost] cost)
                                                 :msg (msg "lose " from " and gain " to)
                                                 :effect (effect (update! (assoc-in card [:special :change-subtype] {:from from :to to})))}}}
               :static-abilities [{:type :gain-subtype
                                   :req (req (and (same-card? card target)
                                                  (get-in card [:special :change-subtype :to])))
                                   :value (req (get-in card [:special :change-subtype :to]))}
                                  {:type :lose-subtype
                                   :req (req (and (same-card? card target)
                                                  (get-in card [:special :change-subtype :from])))
                                   :value (req (get-in card [:special :change-subtype :from]))}]}))

(defn- purchase-subroutines-on-rez
  [sub cost cdef]
  (let [subcost (if (int? cost) [:credit cost] cost)]
    (letfn [(purchase-sub-abi [qty]
              {:optional
               {:req (req (can-pay? state side
                                    (assoc eid :source card :source-type :ability)
                                    card nil subcost))
                :prompt (msg "Purchase a" (when-not (zero? qty) "nother")
                             " '" (:label sub) "' subroutine?")
                :yes-ability {:cost subcost
                              :async true
                              :msg (msg "purchase a" (when-not (zero? qty) "nother")
                                        " '" (:label sub) "' subroutine")
                              :effect (effect (reset-variable-subs card (inc qty) sub)
                                              (continue-ability
                                                (purchase-sub-abi (inc qty))
                                                card nil))}}})]
      (merge cdef {:on-rez (purchase-sub-abi 0)}))))

(defn- cannot-jack-out
  []
  {:subroutines [{:label "The Runner cannot jack out for the remainder of this run"
                  :msg "prevent the Runner from jacking out"
                  :effect (req (prevent-jack-out state side))}]})

(defn- boop-sub
  ([cost]
   {:prompt "Choose a server to deflect the runner to"
    :label "Deflect the runner to any server"
    :choices (req (cancellable servers))
    :req (req (if-not cost
                true
                (can-pay? state side (assoc eid :source card :source-type :ability) target nil
                          (if (int? cost) [:credit cost] cost))))
    :effect (req (continue-ability
                   state side
                   (boop-sub target cost)
                   card nil))})
  ([server cost]
   {:msg (msg "deflect the runner. The Runner is now running on " server)
    :label (str "Deflect the runner to " server "(encounter)")
    :async true
    :cost (if cost
            (if (int? cost) [:credit cost] cost)
            nil)
    :effect (req (let [dest (server->zone state server)
                       ice (count (get-in corp (conj dest :ices)))
                       phase (if (pos? ice) :encounter-ice :movement)]
                   (redirect-run state side server phase)
                   (start-next-phase state side eid)))}))

(defn- glacier-event [cdef]
  (merge cdef
         {:derezzed-events [{:event :run
                             :interactive (req (not= ((get-autoresolve :auto-fire) state side eid card nil) "No"))
                             :silent (req (= ((get-autoresolve :auto-fire) state side eid card nil) "No"))
                             :optional
                             {:prompt (msg "Reveal " (:title card) " and move it to the outermost position of another server?")
                              :autoresolve (get-autoresolve :auto-fire)
                              :req (req (and
                                          ;;this-server
                                          (can-pay? state side eid card nil [:credit 1])))
                              :yes-ability
                              {:choices (req (cancellable (server-list state)))
                               :prompt "Choose a server"
                               :msg (msg "reveal " (card-str state card {:visible true}) " and move it to the outermost position of" target)
                               :cost [:credit 1]
                               :async true
                               :effect (req (wait-for
                                              (reveal state side (make-eid state eid) card)
                                              (system-msg state side (second (server->zone state target)))
                                              (disable-card state side card)
                                              (let [moved (move state side (get-card state card)
                                                                [:servers (second (server->zone state target)) :ices]
                                                                nil)]
                                                (enable-card state side (get-card state moved))
                                                (register-events state side (get-card state moved) (map #(assoc % :condition :derezzed) (:derezzed-events (card-def moved))))
                                                (when (< run-position (count (get-run-ices state)))
                                                  (swap! state update-in [:run :position] inc))
                                                (set-current-ice state)
                                                (update-all-ice state side)
                                                (update-all-icebreakers state side)
                                                (effect-completed state side eid))
                                              ))}}}]
          :events
          [{:event :run
            :interactive (req (not= ((get-autoresolve :auto-fire) state side eid card nil) "No"))
            :silent (req (= ((get-autoresolve :auto-fire) state side eid card nil) "No"))
            :optional
            {:prompt (msg "move " (:title card) " to the outermost position of another server?")
             :autoresolve (get-autoresolve :auto-fire)
             :req (req (and
                         ;;this-server
                         (can-pay? state side eid card nil [:credit 1])))
             :yes-ability
             {:choices (req (cancellable (server-list state)))
              :prompt "Choose a server"
              :msg (msg "move " (card-str state card {:visible true}) " to the outermost position of" target)
              :cost [:credit 1]
              :async true
              :effect (req (system-msg state side (second (server->zone state target)))
                           (disable-card state side card)
                           (let [moved (move state side (get-card state card)
                                             [:servers (second (server->zone state target)) :ices]
                                             nil)]
                             (enable-card state side (get-card state moved))
                             ;;(register-events state side (get-card state moved) (map #(assoc % :condition :derezzed) (:derezzed-events (card-def moved))))
                             (when (< run-position (count (get-run-ices state)))
                               (swap! state update-in [:run :position] inc))
                             (set-current-ice state)
                             (update-all-ice state side)
                             (update-all-icebreakers state side)
                             (effect-completed state side eid))
                           )}}}]}))

(defn- exterior-ice [state card]
  (if-not (= (first (:zone card)) :servers)
    [] ;; cards not installed protecting a server have no exterior ice
    (let [server-ice (get-in @state (concat [:corp] (:zone card)))]
      ;;first is inside, last is outside
      (letfn [(get-outside [ices card]
                (if (same-card? (first ices) card)
                  (rest ices)
                  (get-outside (rest ices) card)))]
        (get-outside server-ice card)))))

;; card implementations

(defcard "ONR Banpei"
  {:subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Bolter Cluster"
  {:subroutines [(do-net-damage 4)
                 prevent-breaking-next-ice]})

(defcard "ONR Bolter Swarm"
  {:rez-cost-bonus (req (used-noisy-discount state 5))
   :subroutines [(do-net-damage 4)
                 prevent-breaking-next-ice]})

(defcard "ONR Brain Drain"
  {:subroutines [{:label "maybe do 3 brain damage"
                  :effect (req (let [di (inc (rand-int 6))]
                                 (system-msg state side (str "uses " (:title card) " to roll a " di " (1d6)"))
                                 (if (= 1 di)
                                   (continue-ability state side (do-brain-damage 3) card nil)
                                   (effect-completed state side eid))))
                  :async true}]})

(defcard "ONR Brain Wash"
  {:subroutines [(do-brain-damage 1)]})

(defcard "ONR Bug Zapper"
  (let [damage-count (fn [state card]
                 (count (filter rezzed? (exterior-ice state card))))]
    {:subroutines [{:label "Do 2 net damage for each rezzed ice outside this one"
                    :effect (req (continue-ability
                                   state side
                                   (do-net-damage (* 2 (damage-count state card)))
                                   card nil))
                    :async true}
                   end-the-run]}))

(defcard "ONR Canis Major"
  {:subroutines [{:label (str "All further ice is encountered at +2 Strength")
                  :msg "Make all further ice be encountered at +2 strength this run."
                  :effect (effect (register-lingering-effect
                                    card
                                    {:type :ice-strength
                                     :duration :end-of-run
                                     :req (req (and (get-current-encounter state)
                                                    (same-card? current-ice target)))
                                     :value +2}))}]})

(defcard "ONR Canis Minor"
  {:subroutines [{:label (str "All further ice is encountered at +1 Strength")
                  :msg "Make all further ice be encountered at +1 strength this run."
                  :effect (effect (register-lingering-effect
                                    card
                                    {:type :ice-strength
                                     :duration :end-of-run
                                     :req (req (and (get-current-encounter state)
                                                    (same-card? current-ice target)))
                                     :value +1}))}]})

(defcard "ONR Caryatid"
  (change-subtype-on-rez "Wall" "Code Gate" 1 {:subroutines [end-the-run]}))

(defcard "ONR Chihuahua"
  {:on-rez (gain-credits-sub 2)
   :subroutines [(trace-net 1)]})

(defcard "ONR Code Corpse"
  {:subroutines [(do-brain-damage 1)
                 (do-brain-damage 1)
                 end-the-run]})

(defcard "ONR Colonel Failure"
  {:subroutines [trash-program-sub
                 trash-program-sub
                 trash-program-sub
                 end-the-run
                 end-the-run]})

(defcard "ONR Cortical Scanner"
  {:subroutines[end-the-run
                end-the-run
                end-the-run]})

(defcard "ONR Cortical Scrub"
  {:subroutines [(do-brain-damage 1)
                 end-the-run]})

(defcard "ONR Credit Blocks"
  (change-subtype-on-rez "Sentry" "Wall" 1 {:subroutines [end-the-run]}))

(defcard "ONR Crystal Wall"
  {:subroutines[end-the-run]})

(defcard "ONR D'Arc Knight"
  {:subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Datacomb"
  {:subroutines [end-the-run]
   :events [(bounce-unless-corp-pays 1)]})

(defcard "ONR Data Darts"
  {:subroutines [(do-net-damage 3)
                 prevent-breaking-next-ice]})

(defcard "ONR Data Naga"
  {:subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Data Wall"
  {:subroutines [end-the-run]})

(defcard "ONR Data Wall 2.0"
  {:subroutines [end-the-run]})

(defcard "ONR Deadeye"
  {:rez-cost-bonus (req (used-noisy-discount state 5))
   :subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Death Yo-Yo"
  {:events [(bounce-and-corp-gains 1)]
   :subroutines [(do-brain-damage 1)
                 end-the-run]})

(defcard "ONR Dog Pile"
  (let [damage-count (fn [state card]
                 (count (filter rezzed? (exterior-ice state card))))]
    {:static-abilities [(ice-strength-bonus (req true) (req (damage-count state card)))]
     :subroutines [{:label "Do 1 net damage for each rezzed ice outside this one"
                    :effect (req (continue-ability
                                   state side
                                   (do-net-damage (* 1 (damage-count state card)))
                                   card nil))
                    :async true}
                   end-the-run]}))

(defcard "ONR Dumpster"
  {:install-req (req (remove #{"Archives"} targets))
   :subroutines [(boop-sub "Archives" nil)]})

(defcard "ONR Endless Corridor"
  {:subroutines [end-the-run
                 end-the-run]})

(defcard "ONR Entrapment"
  {:subroutines [(boop-sub 2)]})

(defcard "ONR Fetch 4.0.1"
  {:subroutines [(trace-tag 3)]})

(defcard "ONR Filter"
  {:subroutines [end-the-run]})

(defcard "ONR Fire Wall"
  {:subroutines [end-the-run]})

(defcard "ONR Food Fight"
  (purchase-subroutines-on-rez end-the-run 2 {}))

(defcard "ONR Galatea"
  (change-subtype-on-rez "Wall" "Code Gate" 1 {:subroutines [end-the-run]}))

(defcard "ONR Gatekeeper"
  (purchase-subroutines-on-rez end-the-run 2 {}))

(defcard "ONR Glacier"
  (glacier-event {
                 :subroutines [end-the-run
                               end-the-run]
                 :abilities [(set-autoresolve :auto-fire "Glacier swap on run")]}))

(defcard "ONR Hunter"
  {:subroutines [(trace-tag 5)]})

(defcard "ONR Hunting Pack"
  (let [sub-count (fn [state card]
                    (count (filter rezzed? (exterior-ice state card))))
        reset-sub-abi {:label "manually reset subs"
                       :effect (effect (reset-variable-subs card (sub-count state card) (trace-tag 5) {:variable true :front true}))}]
    {:abilities [reset-sub-abi]
     :on-rez reset-sub-abi
     :events [(assoc reset-sub-abi :event :derez)
              (assoc reset-sub-abi :event :rez)
              (assoc reset-sub-abi
                     :event :card-moved
                     :req (req (corp? target)
                               (or (= :servers (first (:zone target)))
                                   (= :servers (first (:previous-zone target))))))]}))

(defcard "ONR Ice Pick Willie"
  {:subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Imperial Guard"
  {:rez-cost-bonus (req (used-noisy-discount state 5))
   :subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Jack Attack"
  {:subroutines [(cannot-jack-out)
                 (trace-tag 5)]})

(defcard "ONR Keeper"
  {:subroutines [end-the-run]})

(defcard "ONR Laser Wire"
  {:subroutines [(do-net-damage 1)
                 end-the-run]})

(defcard "ONR Lesser Arcana"
  (change-subtype-on-rez "Sentry" "Wall" 1 {:subroutines [end-the-run]}))

(defcard "ONR Liche"
  {:subroutines [(do-brain-damage 1)
                 (do-brain-damage 1)
                 (do-brain-damage 1)
                 end-the-run]})

(defcard "ONR Marionette"
  {:events [(bounce-unless-corp-pays 1)]
   :subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Mastermind"
  (let [damage-count (fn [state card]
                 (count (filter rezzed? (exterior-ice state card))))]
    {:static-abilities [(ice-strength-bonus (req true) (req (damage-count state card)))]
     :subroutines [{:label "Do 1 brain damage for each rezzed ice outside this one"
                    :effect (req (continue-ability
                                   state side
                                   (do-brain-damage (* 1 (damage-count state card)))
                                   card nil))
                    :async true}
                   end-the-run]}))

(defcard "ONR Mazer"
  {:subroutines [end-the-run]})

(defcard "ONR Minotaur"
  (let [sub-count (fn [state card]
                    (count (filter #(and (or (has-subtype? % "Code Gate")
                                             (has-subtype? % "Wall"))
                                         (rezzed? %))
                                   (exterior-ice state card))))
        reset-sub-abi {:label "manually reset subs"
                  :effect (effect (reset-variable-subs card (sub-count state card) end-the-run {:variable true :front true}))}]
    {:abilities [reset-sub-abi]
     :on-rez reset-sub-abi
     :events [(assoc reset-sub-abi :event :derez)
              (assoc reset-sub-abi :event :rez)
              (assoc reset-sub-abi
                     :event :card-moved
                     :req (req (corp? target)
                               (or (= :servers (first (:zone target)))
                                   (= :servers (first (:previous-zone target))))))]}))

(defcard "ONR Misleading Access Menus"
  {:on-rez (gain-credits-sub 3)
   :subroutines [(end-the-run-unless-runner-pays [:credit 1])]})

;; I'm gonna be sick...
(defcard "ONR Mobile Barricade"
  {:derezzed-events
   [{:event :run
     :interactive (req (not= ((get-autoresolve :auto-fire) state side eid card nil) "No"))
     :silent (req (= ((get-autoresolve :auto-fire) state side eid card nil) "No"))
     :optional
     {:prompt (msg "Reveal " (:title card) " and swap it with another ice protecting this server?")
      :autoresolve (get-autoresolve :auto-fire)
      :req (req (and
                  this-server
                  (can-pay? state side eid card nil [:credit 1])))
      :yes-ability
      {:choices {:req (req (and
                             (not (same-card? card target))
                             (ice? target)
                             (protecting-same-server? card target)))}
       :msg (msg "reveal " (card-str state card {:visible true}) " and swap it with "
                 (card-str state target))
       :cost [:credit 1]
       :effect (req (wait-for
                      (reveal state side (make-eid state eid) card)
                      (swap-ice state side card target)))}}}]
   :events [{:event :run
             :interactive (req (not= ((get-autoresolve :auto-fire) state side eid card nil) "No"))
             :silent (req (= ((get-autoresolve :auto-fire) state side eid card nil) "No"))
             :optional
             {:prompt (msg "Swap " (:title card) " with another ice protecting this server?")
              :autoresolve (get-autoresolve :auto-fire)
              :req (req (and
                          this-server
                          (can-pay? state side eid card nil [:credit 1])))
              :yes-ability
              {:choices {:req (req (and
                                     (not (same-card? card target))
                                     (ice? target)
                                     (protecting-same-server? card target)))}
               :msg (msg "swap " (card-str state card) " with "
                         (card-str state target))
               :cost [:credit 1]
               :effect (req (wait-for
                              (reveal state side (make-eid state eid) card)
                              (swap-ice state side card target)))}}}]
   :subroutines [(do-net-damage 1)
                 end-the-run]
   :abilities [(set-autoresolve :auto-fire "Mobile Barricade swap on run")]})

(defcard "ONR Nerve Labyrinth"
  {:subroutines [(do-net-damage 2)
                 end-the-run]})

(defcard "ONR Neural Blade"
  {:subroutines [(do-net-damage 1)
                 prevent-breaking-next-ice]})

(defcard "ONR π in the 'Face"
  {:subroutines [end-the-run]})

(defcard "ONR Pocket Virtual Reality"
  {:on-encounter {:msg "place 4 credits on itself"
                  :effect (effect (add-counter card :recurring 4 nil))}
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}
   :events [{:event :end-of-encounter
             :msg "return unused credits to the bank"
             :effect (effect (add-counter card :recurring (- (get-counters card :recurring)) nil))
             :req (req (and (= (:ice context) card)
                            (pos? (get-counters card :recurring))))}]
   :subroutines [(trace-tag 6)
                 (trace-tag 6)]})

(defcard "ONR Razor Wire"
  {:subroutines [(do-net-damage 2)
                 end-the-run]})

(defcard "ONR Reinforced Wall"
  {:subroutines [end-the-run
                 end-the-run]})

(defcard "ONR Rock is Strong"
  {:subroutines [end-the-run]})

(defcard "ONR Sandstorm"
  (purchase-subroutines-on-rez end-the-run 2 {}))

(defcard "ONR Scaffolding"
  {:events [(bounce-and-corp-gains 1)]
   :subroutines [end-the-run]})

(defcard "ONR Scramble"
  {:subroutines [end-the-run]})

(defcard "ONR Sentinels Prime"
  {:subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Shotgun Wire"
  {:subroutines [(do-net-damage 2)
                 end-the-run]})

(defcard "ONR Sleeper"
  {:subroutines [end-the-run]})

(defcard "ONR Snowbank"
  {:on-rez (gain-credits-sub 3)
   :subroutines [(end-the-run-unless-runner-pays [:credit 1])]})

(defcard "ONR Sphinx 2006"
  (change-subtype-on-rez "Code Gate" "Sentry" 4 {:subroutines [end-the-run]}))

(defcard "ONR Sumo 2008"
  (change-subtype-on-rez "Sentry" "Wall" 1 {:subroutines [end-the-run]}))

(defcard "ONR Too Many Doors"
  {:subroutines [(do-psi end-the-run)]})

(defcard "ONR Toughonium [TM] Wall"
  {:subroutines [end-the-run
                 end-the-run
                 end-the-run
                 end-the-run]})

(defcard "ONR Trapdoor"
  {:install-req (req (filter #{"HQ" "R&D"} targets))
   :subroutines [(assoc (boop-sub nil)
                        :choices
                        (req (cancellable (remove #{"HQ" "R&D" "Archives"} servers))))]})

(defcard "ONR Triggerman"
  {:subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Tumblers"
  {:events [(bounce-and-corp-gains 1)]
   :subroutines [end-the-run]})

(defcard "ONR Twisty Passages"
  {:events [(bounce-unless-corp-pays 1)]
   :subroutines [end-the-run]})

(defcard "ONR Quandary"
  {:subroutines [end-the-run]})

(defcard "ONR Vortex"
  {:subroutines [(boop-sub 2)]})

(defcard "ONR Walking Wall"
  {:derezzed-events
   [{:event :run
     :interactive (req (not= ((get-autoresolve :auto-fire) state side eid card nil) "No"))
     :silent (req (= ((get-autoresolve :auto-fire) state side eid card nil) "No"))
     :optional
     {:prompt (msg "Reveal " (:title card) " and swap it with another ice protecting this server?")
      :autoresolve (get-autoresolve :auto-fire)
      :req (req (and
                  this-server
                  (can-pay? state side eid card nil [:credit 1])))
      :yes-ability
      {:choices {:req (req (and
                             (not (same-card? card target))
                             (ice? target)
                             (protecting-same-server? card target)))}
       :msg (msg "reveal " (card-str state card {:visible true}) " and swap it with "
                 (card-str state target))
       :cost [:credit 1]
       :effect (req (wait-for
                      (reveal state side (make-eid state eid) card)
                      (swap-ice state side card target)))}}}]
   :events [{:event :run
             :interactive (req (not= ((get-autoresolve :auto-fire) state side eid card nil) "No"))
             :silent (req (= ((get-autoresolve :auto-fire) state side eid card nil) "No"))
             :optional
             {:prompt (msg "Swap " (:title card) " with another ice protecting this server?")
              :autoresolve (get-autoresolve :auto-fire)
              :req (req (and
                          this-server
                          (can-pay? state side eid card nil [:credit 1])))
              :yes-ability
              {:choices {:req (req (and
                                     (not (same-card? card target))
                                     (ice? target)
                                     (protecting-same-server? card target)))}
               :msg (msg "swap " (card-str state card) " with "
                         (card-str state target))
               :cost [:credit 1]
               :effect (req (wait-for
                              (reveal state side (make-eid state eid) card)
                              (swap-ice state side card target)))}}}]
   :subroutines [end-the-run]
   :abilities [(set-autoresolve :auto-fire "Walking Wall swap on run")]})

(defcard "ONR Wall of Ice"
  {:subroutines [(do-net-damage 2)
                 (do-net-damage 2)
                 end-the-run
                 end-the-run]})

(defcard "ONR Wall of Static"
  {:subroutines [end-the-run]})

(defcard "ONR Washed-Up Solo Construct"
  (let [sub {:label "Trash a program unless Runner pays 1"
             :async true
             :optional {:player :runner
                        :waiting-prompt true
                        :prompt "Pay 1 to prevent the corp trashing a program?"
                        :yes-ability {:cost [:credit 1]
                                      :msg "prevent the Corp from trashing a program"}
                        :no-ability (assoc trash-program-sub :player :corp)}}]
    {:on-rez (gain-credits-sub 3)
     :subroutines [sub]}))

(defcard "ONR Zombie"
  {:subroutines [(do-brain-damage 1)
                 (do-brain-damage 1)
                 end-the-run]})
