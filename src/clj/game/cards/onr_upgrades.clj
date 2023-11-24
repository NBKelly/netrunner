(ns game.cards.onr-upgrades
  (:require
   [clojure.string :as str]
   [cond-plus.core :refer [cond+]]
   [game.core.access :refer [access-bonus set-only-card-to-access
                             installed-access-trigger steal
                             steal-cost-bonus]]
   [game.core.bad-publicity :refer [lose-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed card->server
                            get-remotes server->zone server-list]]
   [game.core.card :refer [agenda? asset? can-be-advanced?
                           corp-installable-type? corp? get-card get-counters get-zone
                           has-subtype? ice? in-discard? in-hand? installed? operation? program? resource? rezzed?
                           runner? upgrade?]]
   [game.core.cost-fns :refer [install-cost rez-cost]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage]]
   [game.core.def-helpers :refer [corp-rez-toast defcard offer-jack-out
                                  reorder-choice get-x-fn]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [effect-completed get-ability-targets is-basic-advance-action? make-eid]]
   [game.core.engine :refer [dissoc-req pay register-default-events
                             register-events resolve-ability unregister-events]]
   [game.core.events :refer [first-event? first-run-event? turn-events]]
   [game.core.expose :refer [expose-prevent]]
   [game.core.finding :refer [find-cid find-latest]]
   [game.core.flags :refer [clear-persistent-flag! is-scored? register-persistent-flag!
                            register-run-flag!]]
   [game.core.gaining :refer [gain-credits lose-clicks lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+]]
   [game.core.ice :refer [all-subs-broken?  insert-extra-sub! remove-sub! reset-sub!
                          get-run-ices pump-ice resolve-subroutine!
                          unbroken-subroutines-choice update-all-ice update-all-icebreakers]]
   [game.core.installing :refer [corp-install corp-install-list]]
   [game.core.moving :refer [mill move remove-from-currently-drawing
                             swap-cards swap-ice trash trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay? cost-value]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-prop set-prop]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [rez derez get-rez-cost]]
   [game.core.runs :refer [end-run force-ice-encounter jack-out redirect-run
                           set-next-phase start-next-phase gain-corp-run-credits]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->zone from-same-server? in-same-server?
                              is-central? protecting-same-server? same-server?
                              target-server unknown->kw zone->name]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.tags :refer [gain-tags]]
   [game.core.threat :refer [threat-level]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]
   [game.core.onr-utils :refer [dice-roll ambush-outside-archives gain-runner-counter
                                register-effect-once]]
   ))



(defn- onr-ambush [impl]
  (merge {:implementation "(classic) Installed ambushes must be rezzed to take effect, unless otherwise noted"} impl))

;; card implementations

(defcard "ONR Aardvark" ;; TODO - this can use some work!
  (letfn [(is-breaker-a-worm
            [state side cid]
            (system-msg state side cid)
            (filter #(and (has-subtype? % "Worm")
                          (= (:cid %) cid)) (all-installed state :runner)))]
    {:implementation "This can interrupt exactly one subroutine-break ability. Doesn't play nice with auto-breaking."
     :static-abilities [{:type :prevent-paid-ability
                         :req (req
                                (let [[break-card break-ability] targets]
                                  (and (has-subtype? break-card "Worm")
                                       run this-server)))
                         :value true}]
   :derezzed-events [{:event :subroutines-broken
                      :req (req (and run this-server))
                      :async true
                      :effect (req (let [ice (first targets)
                                         broken-subs (second targets)
                                         breaker-cid (:breaker (first broken-subs))
                                         breaker (first (is-breaker-a-worm state side breaker-cid))]
                                     (if (seq breaker)
                                       (continue-ability
                                         state side
                                         {:optional
                                          {:prompt (msg "Rez " (:title card) " and trash " (:title breaker) "?")
                                           :yes-ability {:async true
                                                         :msg (msg "trash " (:title breaker))
                                                         :effect (req (wait-for (trash state side breaker {:cause-card card})
                                                                                (wait-for (rez state side (make-eid state eid) card)
                                                                                          ;; unbreak those subroutines!
                                                                                          (doseq [sub broken-subs]
                                                                                            (reset-sub! state (get-card state ice) sub))
                                                                                          (effect-completed state side eid))))}}}
                                         card nil))))}]}))

(defcard "ONR Antiquated Interface Routines"
  {:static-abilities [{:type :ice-strength
                       :req (req (same-server? target card))
                       :value 1}]})

(defcard "ONR Bizarre Encryption Scheme"
  {:implementation "Applies on breach. If the agenda moves zones afterwards, the effect is (by design) broken"
   :on-trash {:req (req (and (= :runner side)
                             (:run @state)))
              :msg (msg "lingering")
              :effect (effect
                        (register-events
                          card
                          (let [oc card]
                            [{:event :access
                              :duration :end-of-run
                              :req (and (agenda? target)
                                        (or (= (get-zone target)
                                               (:previous-zone oc))
                                            (= (central->zone (get-zone target))
                                               (butlast (:previous-zone oc)))))
                              :msg (msg "protect " (:title target) " until the start of the Runners next turn")
                              :effect (req (let [old-card target]
                                             (register-events
                                               state side (:identity corp)
                                               [{:event :runner-turn-begins
                                                 :player runner
                                                 :unregister-once-resolved true
                                                 :persistent true
                                                 :effect (req (steal state :runner eid (get-card state old-card)))
                                                 :req (req (let [new-card (get-card state old-card)]
                                                             (and
                                                               (installed? new-card)
                                                               (= (:zone new-card) (:zone old-card)))))
                                                 :msg (msg "steal " (:title old-card))}])))}]))
                        (register-lingering-effect
                          card;;(:identity corp);;(get-card state card)
                          {:type :cannot-steal
                           :req (req (or (= (get-zone target)
                                            (:previous-zone card))
                                         (= (central->zone (get-zone target))
                                            (butlast (:previous-zone card)))))
                           :value true
                           :duration :end-of-run}))}
   :events [{:event :access
             :req (req (and (agenda? target)
                            (or (in-same-server? card target)
                                (from-same-server? card target))))
             :msg (msg "protect " (:title target) " until the start of the Runners next turn")
             :effect (req (let [old-card target]
                            (register-events
                              state side (:identity runner)
                              [{:event :runner-turn-begins
                                :ability-name "Bizzarely Encrypted Agenda"
                                :unregister-once-resolved true
                                :persistent true
                                :effect (req (steal state :runner eid (get-card state old-card)))
                                :req (req (let [new-card (get-card state old-card)]
                                            (and
                                              (installed? new-card)
                                              (= (:zone new-card) (:zone old-card)))))
                                :msg (msg "steal " (:title old-card))}])))}]
   :static-abilities [{:type :cannot-steal
                       :duration :end-of-run
                       :req (req (or (in-same-server? card target)
                                     (from-same-server? card target)))
                       :value true}]})

(defcard "ONR Chester Mix"
  {:static-abilities [{:type :install-cost
                       :req (req (and (ice? target)
                                      (same-server? card target)))
                       :value -2}]})

(defcard "ONR Chimera"
  (onr-ambush {:on-access {:prompt "choose a daemon to trash"
                           :waiting-prompt true
                           :req (req (and (installed? card)
                                          (rezzed? card)))
                           :msg (msg "trash " (:title target))
                           :choices {:card #(and (installed? %)
                                                 (has-subtype? % "Daemon"))}
                           :async true
                           :effect (effect (trash eid target {:cause :corp-trash}))}}))

(defcard "ONR Crybaby"
  (letfn [(give-crying-counter
            [state side target]
            (gain-runner-counter state side :crying-counter target)
            (register-effect-once
              state :runner target
              {:type :link
               :req (req (pos? (:crying-counter (:runner @state))))
               :ability-name "Crying Counters"
               :value (req (- (* 2 (:crying-counter (:runner @state)))))}))]
    (onr-ambush {:on-access {:msg "give the Runner a Crying counter"
                             :req (req (and (installed? card)
                                            (rezzed? card)))
                             :effect (req (give-crying-counter state side (:identity runner)))}})))

(defcard "ONR Crystal Palace Sation Grid"
  {:static-abilities [{:type :break-sub-additional-cost
                       :req (req (and ;; The card is an icebreaker
                                      ;;(has-subtype? target "Icebreaker")
                                      ; and is using a break ability
                                      (contains? (second targets) :break)
                                      (pos? (count (:broken-subs (second targets))))
                                      ; during a run on this server
                                      this-server))
                       :value [:credit 1]}]})

(defcard "ONR Dedicated Response Team"
  (onr-ambush
    {:on-access {:req (req (and tagged (rezzed? card)))
                 :msg "do 3 meat damage"
                 :async true
                 :effect (effect (damage eid :meat 3 {:card card}))}}))

(defcard "ONR Dieter Esslin"
  (onr-ambush {:on-access {:msg "do 1 net damage"
                           :req (req (rezzed? card))
                           :async true
                           :effect (effect (damage eid :net 1 {:card card}))}}))

(defcard "ONR Dr. Dreff"
  {:implementation "Occurs on approach timing"
   :events [{:event :approach-server
             :interactive (req true)
             :prompt "Force an ICE encounter?"
             :once :per-turn
             :async true
             :choices {:req (req (and (ice? target)
                                      (in-hand? target)
                                      (can-pay? state :corp eid card nil [:credit (quot (:cost target) 2)])))}
             :msg (msg "pay " (quot (:cost target) 2) " [Credits] to force the Runner to encounter " (:title target) " from HQ")
             :effect (req (wait-for
                            (pay state side (make-eid state eid) card :credit (quot (:cost target) 2))
                            (wait-for (force-ice-encounter state side (make-eid state eid) target)
                                      (if (in-hand? (get-card state target))
                                        (do (system-msg state side (str "trashes " (:title target)))
                                            (trash state side eid (assoc target :seen true) {:cause-card card}))
                                        (effect-completed state side eid)))))}]})

(defcard "ONR Herman Revista"
  (letfn [(sun [serv]
            {:prompt "Choose 2 pieces of ice to swap"
             :choices {:card #(and (= [:servers serv :ices] (get-zone %))
                                   (ice? %))
                       :max 2}
             :async true
             :effect (req (if (= (count targets) 2)
                            (do (swap-ice state side (first targets) (second targets))
                                (system-msg state side
                                            (str "uses " (:title card) " to swap "
                                                 (card-str state (first targets))
                                                 " with "
                                                 (card-str state (second targets))))
                                (continue-ability state side (sun serv) card nil))
                            (do (system-msg state side "has finished rearranging ice")
                                (effect-completed state side eid))))})]
    {:implementation "Timing restriction not enforced"
     :abilities [{:cost [:credit 0]
                  :label "Re-organize this server"
                  :msg (msg "rearrange ice protecting " (zone->name (get-zone card)))
                  :async true
                  :effect (req (continue-ability state side (sun (second (:zone card))) card nil))}]}))

(defcard "ONR Jenny Jett"
  {:implementation "Trashing ice pre-install to reduce the cost is legal"
   :events [{:event :approach-server
             :async true
             :interactive (req true)
             :waiting "Corp to make a decision"
             :req (req (and (pos? (count (:hand corp)))
                            this-server))
             :effect (req (if (some ice? (:hand corp))
                            (continue-ability
                              state side
                              {:optional
                               {:prompt "Install a piece of ice?"
                                :once :per-run
                                :yes-ability
                                {:prompt "Choose a piece of ice to install from HQ (paying all costs)"
                                 :once :per-run
                                 :choices {:card #(and (ice? %)
                                                       (in-hand? %))}
                                 :async true
                                 :msg "install a piece of ice at the innermost position of this server. Runner is now approaching that piece of ice"
                                 :effect (req (wait-for (corp-install state side target (zone->name (target-server run))
                                                                      {:front true})
                                                        (swap! state assoc-in [:run :position] 1)
                                                        (set-next-phase state :approach-ice)
                                                        (update-all-ice state side)
                                                        (update-all-icebreakers state side)
                                                        (continue-ability state side
                                                                          (offer-jack-out {:req (req (:approached-ice? (:run @state)))})
                                                                          card nil)))}}}
                              card nil)
                            ;; bogus prompt so Runner cannot infer the Corp has no ice in hand
                            (continue-ability
                              state :corp
                              {:async true
                               :prompt "No ice to install"
                               :choices ["Carry on!"]
                               :prompt-type :bogus
                               :effect (effect (effect-completed eid))}
                              card nil)))}]})

(defcard "ONR Lisa Blight"
  {:abilities [{:async true
                :label "duplicate a subroutine"
                :req (req
                       (and
                         this-server run
                         (can-pay? state :corp (assoc eid :source card :source-type :ability)
                                   [:credit 1 :randomly-trash-from-hand 1] nil)
                         (some #(and (ice? %)
                                        (rezzed? %)
                                        (pos? (count (:subroutines %)))
                                        (same-server? % card))
                                  (all-installed state :corp))))
                :effect (req (continue-ability
                               state side
                               {:prompt "Choose an ice protecting this server"
                                :async true
                                :choices {:card #(and (ice? %)
                                                      (rezzed? %)
                                                      (pos? (count (:subroutines %)))
                                                      (same-server? % card))}
                                :effect (req
                                          (let [target-ice target
                                                from-cid (:cid card)]
                                            (continue-ability
                                              state side
                                              {:cost [:credit 1 :randomly-trash-from-hand 1]
                                               :prompt "Duplicate a subroutine"
                                               :choices (req (concat (map #(make-label (:sub-effect %)) (:subroutines target-ice)) ["Cancel"]))
                                               :msg (msg "repeat '" target "' subroutine on " (card-str state target-ice) " until the end of the run")
                                               :effect (req
                                                         (let [sub (nth (:subroutines target-ice) (:idx (first targets)))]
                                                           (insert-extra-sub! state side target-ice sub from-cid (:index sub) {:printed false :variable true})
                                                           (register-events
                                                             state side card
                                                             [{:event :run-ends
                                                               :unregister-once-resolved true
                                                               :req (req true)
                                                               :duration :end-of-run
                                                               :effect (req
                                                                         (remove-sub! state side target-ice #(= from-cid (:from-cid %))))}])))}
                                              card nil)))}
                               card nil))}]})

(defcard "ONR Marcel DeSoleil"
  {:abilities [{:async true
                :label "duplicate a subroutine"
                :req (req
                       (and
                         this-server run
                         (can-pay? state :corp (assoc eid :source card :source-type :ability)
                                      [:credit 2 :trash-from-deck 2] nil)
                            (some #(and (ice? %)
                                        (rezzed? %)
                                        (pos? (count (:subroutines %)))
                                        (same-server? % card))
                                  (all-installed state :corp))))
                :effect (req (continue-ability
                               state side
                               {:prompt "Choose an ice protecting this server"
                                :async true
                                :choices {:card #(and (ice? %)
                                                      (rezzed? %)
                                                      (pos? (count (:subroutines %)))
                                                      (same-server? % card))}
                                :effect (req
                                          (let [target-ice target
                                                from-cid (:cid card)]
                                            (continue-ability
                                              state side
                                              {:cost [:credit 2 :trash-from-deck 2]
                                               :prompt "Duplicate a subroutine"
                                               :choices (req (concat (map #(make-label (:sub-effect %)) (:subroutines target-ice)) ["Cancel"]))
                                               :msg (msg "repeat '" target "' subroutine on " (card-str state target-ice) " until the end of the run")
                                               :effect (req
                                                         (let [sub (nth (:subroutines target-ice) (:idx (first targets)))]
                                                           (insert-extra-sub! state side target-ice sub from-cid (:index sub) {:printed false :variable true})
                                                           (register-events
                                                             state side card
                                                             [{:event :run-ends
                                                               :unregister-once-resolved true
                                                               :req (req true)
                                                               :duration :end-of-run
                                                               :effect (req
                                                                         (remove-sub! state side target-ice #(= from-cid (:from-cid %))))}])))}
                                              card nil)))}
                               card nil))}]})

(defcard "ONR Olivia Salazar"
  (letfn [(sally-price [ice state]
            (or (int (/ (second (first (get-rez-cost state :corp ice nil))) 2)) 0))]
    {:abilities [{:req (req (and run this-server))
                  :label "Rez an ice for half the rez cost"
                  :async true
                  :effect
                  (effect
                    (continue-ability
                      {:prompt "Choose a card to rez"
                       :async true
                       :once :per-run
                       :choices {:card #(and (same-server? card %)
                                             (ice? %)
                                             (not (rezzed? %))
                                             (can-pay? state :corp
                                                       (assoc eid :source card :source-type :ability)
                                                       card nil [:credit (sally-price % state)]))}
                       :msg (msg "pay " (sally-price target state) "[Credits] to rez " (:title target))
                       :effect (req (wait-for (pay state side
                                                   (make-eid
                                                     state (assoc eid :source card :source-type :ability))
                                                   card :credit (sally-price target state))
                                              (wait-for (rez state side (make-eid state eid) target {:ignore-cost :all-costs :no-msg true})
                                                        (let [c (:card async-result)]
                                                          (register-events
                                                            state side card
                                                            [{:event :run-ends
                                                              :unregister-once-resolved true
                                                              :duration :end-of-run
                                                              :effect (effect (derez c))}])
                                                          (effect-completed state side eid)))))}
                      card nil))}]}))

(defcard "ONR Omni Kismet, Ph. D."
  {:abilities [{:label "Swap an unrezzed ice"
                :req (req (and (some ice? (:hand corp))
                               run this-server
                               (some #(and (not (rezzed? %)) (ice? %)
                                           (protecting-same-server? card %))
                                     (all-installed state :corp))))
                :async true
                :once :per-run
                :prompt "Choose an unrezzed ice"
                :choices {:req (req (and (ice? target)
                                         (not (rezzed? target))
                                         (protecting-same-server? card target)))}
                :waiting-prompt true
                :effect (req (let [from-field target]
                               (continue-ability
                                 state side
                                 {:async true
                                  :prompt "Choose a piece of ice from HQ"
                                  :choices {:card #(and (ice? %)
                                                        (in-hand? %))}
                                  :msg (msg "swap " (card-str state from-field)
                                            " with a piece of ice from HQ")
                                  :effect (req (swap-cards state :corp from-field target)
                                               (effect-completed state side eid))}
                                 card nil)))}]})

(defcard "ONR Raymond Ellison"
  (letfn [(resolve-abi [x state orig-card eid]
            (continue-ability
              state :corp
              {:prompt "remove an advancement counter?"
               :choices {:card #(and (in-same-server? % orig-card)
                                     (not (ice? %))
                                     (pos? (get-counters % :advancement)))}
               :waiting-prompt true
               :async true
               :effect (req
                         (add-prop state :corp target :advance-counter -1 {:placed true})
                         (resolve-abi (inc x) state orig-card eid))
               :cancel-effect (req (continue-ability
                                     state side
                                     {:msg (msg "remove " x " advancement counters from cards in this server and gain " (* 3 x) " credits for this run")
                                      :async true
                                      :effect (req (gain-corp-run-credits state side eid (* x 3)))}
                                     card nil))}
              orig-card nil))]
    {:abilities [{:label "trade advancements for run credits"
                  :req (req (and run this-server))
                  :cost [:trash-can]
                  :async true
                  :effect (req (resolve-abi 0 state card eid))}]}))

(defcard "ONR Red Herrings"
  {:on-trash
   {:req (req (and (= :runner side)
                   (:run @state)))
    :effect (effect (register-lingering-effect
                      card
                      {:type :steal-additional-cost
                       :duration :end-of-run
                       :req (req (or (= (get-zone target) (:previous-zone card))
                                     (= (central->zone (get-zone target))
                                        (butlast (:previous-zone card)))))
                       :value (req [[:credit 5]
                                    {:source card :source-type :ability}])}))}
   :static-abilities [{:type :steal-additional-cost
                       :req (req (or (in-same-server? card target)
                                     (from-same-server? card target)))
                       :value (req [[:credit 5]
                                    {:source card :source-type :ability}])}]})

(defcard "ONR Rio de Janeiro City Grid"
  {:events [{:event :pass-ice
             :req (req (and (rezzed? (:ice context))
                            (same-server? (:ice context) card)))
             :async true
             :effect (req (let [di (dice-roll)]
                            (continue-ability
                              state side
                              {:msg (msg "roll a " di "(1d6)" (when (= 1 di) " - and ends the run!"))
                               :effect (req (if (= 1 di)
                                              (end-run state :corp eid card)
                                              (effect-completed state side eid)))
                               :async true}
                              card nil)))}]})

(defcard "ONR Tokyo-Chiba Infighting"
  {:events [{:event :run-ends
             :req (req (and (= (second (get-zone card)) (target-server context))
                            (:unsuccessful context)))
             :msg (msg "gain 2[Credits]")
             :async true
             :effect (effect (gain-credits eid 2))}]})

(defcard "ONR Twenty-Four-Hour Surviellance"
  ;; TODO - find a way to do this. I know it can be done.
  {:implementation "Unimplemented/Manual Implementation"})

(defcard "ONR Weapons Depot"
  {:static-abilities [{:type :advancement-requirement
                       :req (req (and (in-same-server? card target)
                                      (has-subtype? target "Black Ops")))
                       :value -1}]})
