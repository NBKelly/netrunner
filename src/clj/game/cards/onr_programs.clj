(ns game.cards.onr-programs
  (:require
   [clojure.string :as str]
   [game.core.access :refer [access-bonus max-access]]
   [game.core.bad-publicity :refer [gain-bad-publicity]]
   [game.core.board :refer [all-active all-active-installed all-installed all-installed-runner-type
                            card->server server->zone]]
   [game.core.card :refer [active? agenda? asset? card-index corp? facedown? faceup? upgrade?
                           get-advancement-requirement get-card get-counters
                           get-nested-host get-title get-zone
                           hardware? has-subtype? in-hand? in-discard? ice? installed?
                           is-type? program? resource? rezzed? runner?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.charge :refer [charge-ability]]
   [game.core.cost-fns :refer [all-stealth install-cost min-stealth rez-cost]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage damage-prevent]]
   [game.core.def-helpers :refer [breach-access-bonus defcard offer-jack-out trash-on-empty get-x-fn]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [any-effects gather-effects register-lingering-effect
                              unregister-effects-for-card]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [ability-as-handler dissoc-req not-used-once?  gather-events pay
                             print-msg register-events register-once resolve-ability
                             trigger-event trigger-event-simult unregister-events]]
   [game.core.events :refer [run-events first-event? first-installed-trash? run-events
                             first-successful-run-on-server? turn-events]]
   [game.core.expose :refer [expose]]
   [game.core.finding :refer [find-cid]]
   [game.core.flags :refer [can-host? can-trash? card-flag? lock-zone release-zone zone-locked?]]
   [game.core.gaining :refer [gain-clicks gain-credits lose-credits lose]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [all-subs-broken-by-card? all-subs-broken?
                          any-subs-broken-by-card? auto-icebreaker break-sub
                          break-subroutine! break-subroutines-msg breaker-strength-bonus dont-resolve-subroutine!
                          get-strength ice-strength pump pump-ice set-current-ice strength-pump
                          unbroken-subroutines-choice update-all-icebreakers update-breaker-strength]]
   [game.core.initializing :refer [ability-init card-init]]
   [game.core.installing :refer [install-locked? runner-can-install? runner-can-pay-and-install?
                                 runner-install]]
   [game.core.link :refer [get-link]]
   [game.core.mark :refer [identify-mark-ability]]
   [game.core.memory :refer [available-mu update-mu]]
   [game.core.moving :refer [flip-facedown mill move swap-cards swap-ice trash
                             trash-prevent]]
   [game.core.optional :refer [get-autoresolve set-autoresolve never?]]
   [game.core.payment :refer [build-cost-label can-pay? cost-target cost-value]]
   [game.core.prompts :refer [cancellable]]
   [game.core.props :refer [add-counter add-icon remove-icon]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez get-rez-cost rez]]
   [game.core.runs :refer [active-encounter? bypass-ice continue end-run-prevent end-run
                           get-current-encounter jack-out make-run successful-run-replace-breach
                           update-current-encounter]]
   [game.core.sabotage :refer [sabotage-ability]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name is-central? is-remote? protecting-same-server?
                              remote->name target-server unknown->kw zone->name]]
   [game.core.set-aside :refer [set-aside get-set-aside]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.tags :refer [gain-tags lose-tags]]
   [game.core.to-string :refer [card-str]]
   [game.core.threat :refer [threat threat-level]]
   [game.core.trace :refer [force-base]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [get-virus-counters]]
   [game.core.onr-trace :refer [boost-link set-base-link cancel-successful-trace]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [game.core.onr-utils :refer [base-link-abi boost-link-abi dice-roll
                                deep-merge generic-prevent-damage
                                handle-if-unique register-effect-once]]
   [jinteki.utils :refer :all]))

;; deal with stealth/noisy cards

(defn- count-stealth-creds
  [state]
  (let [inst (all-active-installed state :runner)
        stealth (filter #(has-subtype? % "Stealth") inst)
        creds (map #(+ (or (get-counters % :credit) 0) (or (get-counters % :recurring) 0)) stealth)
        total (reduce + 0 creds)]
    total))

(defn- lose-x-stealth-creds
  [amt]
  {:req (req (pos? amt))
   :prompt (msg "Lose a stealth credit" (when (> amt 1)
                                          (str " (" amt " remaining)")))
   :async true
   :choices {:card #(and
                      (has-subtype? % "Stealth")
                      (or (pos? (get-counters % :credit))
                          (pos? (get-counters % :recurring))))}
   :effect (req (if (pos? (get-counters target :recurring))
                  (add-counter state side target :recurring -1)
                  (add-counter state side target :credit -1))
                (continue-ability
                  state side
                  (lose-x-stealth-creds (dec amt))
                  card nil))})

(defn- lose-from-stealth
  [amt]
  (if (integer? amt)
    {:additional-ability
     {:msg (msg "lose " amt " credits from stealth cards")
      :req (req (pos? (count-stealth-creds state)))
      :async true
      :effect (req (let [max (min amt (count-stealth-creds state))]
                     (continue-ability state side
                                       (lose-x-stealth-creds max)
                                       card nil)))}}
    nil))

(defn- lose-all-stealth
  [state side card eid]
  (let [runner-cards (all-installed state :runner)
        stealthy (filter #(has-subtype? % "Stealth") runner-cards)
        chosen (first (filter #(or (pos? (get-counters % :credit)) (pos? (get-counters % :recurring))) stealthy))
        creds (when chosen (get-counters chosen :credit))
        rec (when chosen (get-counters chosen :recurring))]
    (if-not chosen
      (effect-completed state side eid)
      (continue-ability
        state side
        {:msg (msg "lose "
                   (when creds (str creds " credits " (when rec "and ")))
                   (when rec (str rec " recurring credits "))
                   "from " (card-str state chosen))
         :async true
         :effect (req (when creds (add-counter state side (get-card state chosen) :credit (- creds)))
                      (when rec (add-counter state side (get-card state chosen) :recurring (- rec)))
                      ;;(effect-completed state side eid))}
                      (continue-ability
                        state side
                        {:async true
                         :effect (lose-all-stealth state side card eid)}
                        card nil))}
        card nil))))

(defn- daemon-abilities []
  [{:label "Install and host a program"
    :cost [:click 1]
    :prompt "Choose a program"
    :choices {:req (req (and (program? target)
                             (runner-can-install? state side target false)
                             (in-hand? target)))}
    :msg (msg "install from the grip and host " (:title target))
    :async true
    :effect (effect (runner-install eid target {:host-card card :no-mu true}))}
   {:label "Host an installed program (manual)"
    :prompt "Choose an installed program"
    :choices {:card #(and (program? %)
                          (installed? %))}
    :msg (msg "host " (:title target))
    :effect (effect (host card target)
                    (unregister-effects-for-card target #(= :used-mu (:type %)))
                    (update-mu))}])

(defn- host-with-negative-strength [x]
  {:type :breaker-strength
   :req (req (and (has-subtype? target "Icebreaker")
                  (some #(same-card? % target) (:hosted card))))
   :value (- x)})


;; (defn- register-effect-once [state side card target-side effect]
;;   (let [em (get-effect-maps state target-side (:type effect))
;;         matches (filter #(= (:ability-name %) (:ability-name effect)) em)]
;;     (when (empty? matches)
;;       (register-lingering-effect
;;         state side card
;;         effect))))

;; card implementations

(defcard "ONR Brainiac: Espionage Enjoyer"
  {:interactions
   {:access-ability
    {:label "Trash card"
     :req (req (and (can-trash? state :runner target)
                    ;; 2+ crumble counters and in hq
                    (or
                      (and (<= 2 (get-counters (get-card state (:identity corp)) :garbage))
                           (or (= :rd (second (:zone target)))
                               (= :deck (first (:zone target)))))
                      (and (<= 2 (get-counters (get-card state (:identity corp)) :crumble))
                           (or (= :hq (second (:zone target)))
                               (= :hand (first (:zone target))))))))
     :cost [:credit 0]
     :async true
     :effect (effect
               (system-msg (str "trashes " (:title target) " at no cost"
                                (if (or (=  :hq (second (:zone target)))
                                        (= :hand (first (:zone target))))
                                  " due to Crumble counters (ONR Crumble)"
                                  " due to Garbage counters (ONR Garbage In)")))
               (trash eid (assoc target :seen true)
                            {:accessed true
                             :cause-card card}))}}})

;; I'm implementing braniac here


(defcard "ONR AI Boon"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 1 1)]
                    :events [{:event :run
                              :async true
                              :effect (req (let [str (dice-roll)]
                                             (continue-ability
                                               state side
                                               {:msg (msg "gain " str " strength for the run")
                                                :effect (effect (pump card str :end-of-run))}
                                               card nil)))}]}))

(defcard "ONR Afreet"
  {:implementation "MU Limit not enforced"
   :static-abilities [(host-with-negative-strength 1)]
   :abilities (daemon-abilities)})

(defcard "ONR Armageddon"
  (letfn [(doom-event []
            {:event :corp-install
             :async true
             :unregister-once-resolved true
             :ability-name "Doom Counters"
             :effect (req (let [counters (get-counters (get-card state (:identity corp)) :doom)
                                rolls (take counters (repeatedly #(dice-roll)))
                                sorted (when rolls (sort rolls))
                                hits (when sorted (count (filter #(= 6 %) sorted)))]
                            (when-not (zero? counters)
                              (register-events
                                state side
                                (:identity runner)
                                [(doom-event)]))
                            (if-not (zero? counters)
                              (do (system-msg state side (str "rolls [" (str/join ", " sorted) "] from Doom counters (ONR Armageddon)"))
                                  (if (pos? hits)
                                    (do (system-msg state side (str "forces the corporation to trash " (card-str state (:card context)) ", and remove " (quantify hits " Doom counter")))
                                        (wait-for (trash state :corp (make-eid state eid) (:card context) {:unpreventable true})
                                                  (add-counter state :corp (get-card state (:identity corp)) :doom (- hits))
                                                  (effect-completed state side eid)))
                                    (effect-completed state side eid)))
                              (effect-completed state side eid))))})]
    {:events [(successful-run-replace-breach
                {:target-server :rd
                 :ability {:req (req true)
                           :msg "Give the corp a Doom counter"
                           :effect (req
                                     (handle-if-unique state side (:identity runner) (doom-event))
                                     (add-counter state :corp (get-card state (:identity corp)) :doom 1))}})]}))

(defcard "ONR Baedeker's Net Map"
  {:abilities [(base-link-abi 0 1)
               (boost-link-abi 1 1)]})

(defcard "ONR Bakdoor [TM]"
  {:abilities [(base-link-abi 0 3)
               (boost-link-abi 2 1)]})

(defcard "ONR Bartmoss Memorial Icebreaker"
  (auto-icebreaker {:abilities [(break-sub 1 1 "All")
                                (strength-pump 1 1)]
                    :events [{:event :end-of-encounter
                              :req (req (any-subs-broken-by-card? (:ice context) card))
                              :async true
                              :effect (req (let [di (dice-roll)]
                                             (continue-ability
                                               state side
                                               {:msg (msg "roll " di "(1d6)" (when (= 1 di) " and trash itself"))
                                                :effect (req (if (= 1 di)
                                                               (trash state side eid card {:cause-card card})
                                                               (effect-completed state side eid)))}
                                               card nil)))}]}))

(defcard "ONR Big Frackin' Gun"
  (auto-icebreaker {:abilities [(break-sub 6 5 "Sentry")
                                (strength-pump 1 1)]}))

(defcard "ONR Black Dahlia"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Sentry")
                                (strength-pump 2 1)]}))

(defcard "ONR Black Widow"
  (auto-icebreaker
    {:on-install
     {:prompt "Choose a piece of ice to target for Black Widow"
      :choices {:card ice?}
      :effect (req (let [ice target]
                     (add-icon state side card ice "BW" (faction-label card))
                     (system-msg state side
                                 (str "selects " (card-str state ice)
                                      " for " (:title card) "'s strength gain ability"))
                     (register-events
                       state side card
                       [{:event :encounter-ice
                         :interactive (req true)
                         :req (req (same-card? ice (:ice context)))
                         :once :per-encounter
                         :msg "gain 5 strength for the encounter"
                         :effect (effect (pump (get-card state card) 5))}])))}
     :leave-play (effect (remove-icon card))
     :abilities [(break-sub 1 1 "Sentry")
                 (strength-pump 2 1)]}))

(defcard "ONR Blink"
  (letfn [(attempt
            [state side card sub]
            (let [c (get-card state card)]
              (update!
                state side
                (assoc-in c [:special :attempted-breaks]
                          (concat (get-in c [:special :attempted-breaks]) [sub])))))
          (viable-sub
            [sub card]
            (not (seq (filter #(or (:broken %) (= % sub)) (get-in card [:special :attempted-breaks])))))
          (viable-subs
            [subs card]
            (filter #(viable-sub % card) subs))]
    ;; we're only allowed to attempt each subroutine once!
    {:implementation "always breaks identical subs from the top down (I'll fix this later, send me an @ if it's an issue)"
     :on-install {:silent (req true)
                  :effect (req (update! state :runner (assoc-in (get-card state card) [:special :attempted-breaks] [])))}
     :events [{:event :encounter-ice
               :silent (req true)
               :effect (req (update! state :runner (assoc-in (get-card state card) [:special :attempted-breaks] [])))}]
     :abilities [{:label "Maybe break a subroutine"
                  :req (req (and
                              current-ice
                              (<= (get-strength current-ice) (get-strength card))
                              (seq (viable-subs (remove :broken (:subroutines current-ice)) card))))
                  :cost [:credit 0]
                  :prompt "Choose a subroutine to try to break"
                  :choices (req (cancellable (map #(make-label (:sub-effect %)) (viable-subs (remove :broken (:subroutines current-ice)) card))))
                  :async true
                  :effect (req
                            (let [chosen-sub (first (filter #(and (= target (make-label (:sub-effect %)))
                                                                  (not (:broken %))
                                                                  (viable-sub % card)) (:subroutines current-ice)))
                                  die-result (dice-roll)
                                  success (<= 4 die-result)]
                              (system-msg state side (str "rolls a " die-result " (1d6)"))
                              (if success
                                (do
                                  (system-msg state side (str (break-subroutines-msg current-ice [chosen-sub] card)))
                                  (break-subroutine! state (get-card state current-ice) chosen-sub)
                                  (effect-completed state side eid))
                                (do
                                  (system-msg state side (str "takes " die-result " net damage"))
                                  (damage state side eid :net die-result {:card card})))))}]}))

(defcard "ONR Boardwalk"
  (letfn [(boardwalk-event []
            {:event :runner-turn-begins
             :unregister-once-resolved true
             :ability-name "Boardwalk Counters"
             :async true
             :effect (req (let [counters (get-counters (get-card state (:identity corp)) :boardwalk)
                                to-reveal (int (/ counters 2))
                                can-reveal (count (:hand corp))]
                            ;; dereg these events on 0 counters to save on performance?
                            (when-not (zero? counters)
                              (register-events
                                state side
                                (:identity runner)
                                [(boardwalk-event)]))
                            (if-not (zero? (min to-reveal can-reveal))
                              (let [revealed (take (min to-reveal can-reveal) (shuffle (:hand corp)))]
                                (do (system-msg state side (str "forces the corp to reveal "
                                                                (enumerate-str (map :title (sort-by :title revealed)))
                                                                " from HQ due to boardwalk counters (ONR Boardwalk)"))
                                    (reveal state side eid revealed)))
                              (effect-completed state side eid))))})]
    {:events [{:event :successful-run
               :req (req (= :hq (target-server context)))
               :msg (msg "give the corp a Boardwalk counter")
               :effect (req
                         (handle-if-unique state side (:identity runner) (boardwalk-event))
                         (add-counter state :corp (get-card state (:identity corp)) :boardwalk 1))}]}))

(defcard "ONR Boring Bit"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Wall")
                                (strength-pump 1 1)]}))

(defcard "ONR Bulldozer"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Wall" (lose-from-stealth 2))
                                (strength-pump 2 1)]
                    :events [{:event :subroutines-broken
                              :req (req (and (all-subs-broken-by-card? target card)
                                             (has-subtype? target "Wall")))
                              :once :per-encounter
                              :effect (req (register-events
                                             state side card
                                             [{:event :encounter-ice
                                               :duration :end-of-run
                                               :unregister-once-resolved :true
                                               :async true
                                               :effect (req (if (has-subtype? current-ice "Sentry")
                                                              (continue-ability
                                                                state side
                                                                (break-sub 0 1 "Sentry" {:req (req true) :repeatable false})
                                                                card nil)
                                                              (effect-completed state side eid)))}]))}]}))

(defcard "ONR Cascade"
  (letfn [(cascade-event []
            {:event :corp-turn-begins
             :unregister-once-resolved true
             :ability-name "Cascade Counters"
             :async true
             :effect (req (let [counters (get-counters (get-card state (:identity corp)) :cascade)
                                to-mill (int (/ counters 2))]
                            (when-not (zero? counters)
                               (register-events
                                 state side
                                 (:identity runner)
                                 [(cascade-event)]))
                            (if-not (zero? to-mill)
                              (wait-for (mill state :runner (make-eid state eid) :corp to-mill)
                                        (let [trashed-cards async-result]
                                          ;; make them faceup
                                          (doseq [c trashed-cards]
                                            (update! state side (assoc c :seen true)))
                                          (system-msg state side (str "force the corp to trash "
                                                                      (enumerate-str (map :title (sort-by :title async-result)))
                                                                      " from the top of R&D due to cascade counters (ONR Cascade)"))
                                          (effect-completed state side eid)))
                              (effect-completed state side eid))))})]
    {:events [{:event :successful-run
               :req (req (= :rd (target-server context)))
               :msg (msg "give the corp a Cascade counter")
               :effect (req
                         (handle-if-unique state side (:identity runner) (cascade-event))
                         (add-counter state :corp (get-card state (:identity corp)) :cascade 1))}]}))

(defcard "ONR Butcher Boy"
  (letfn [(butcher-boy-event []
            {:event :runner-turn-begins
             :unregister-once-resolved true
             :ability-name "Butcher Boy Counters"
             :async true
             :effect (req (let [counters (get-counters (get-card state (:identity corp)) :butcher-boy)
                                to-gain (int (/ counters 2))]
                            (when-not (zero? counters)
                               (register-events state side (:identity runner) [(butcher-boy-event)]))
                            (if-not (zero? to-gain)
                              (do (system-msg state side (str "gains " to-gain "[Credits] from butcher boy counters (ONR Butcher Boy)"))
                                  (gain-credits state :runner eid (int (/ (get-counters (get-card state (:identity corp)) :butcher-boy) 2))))
                              (effect-completed state side eid))))})]
    {:events [{:event :successful-run
               :req (req (= :hq (target-server context)))
               :msg (msg "give the corp a Butcher Boy counter")
               :effect (req
                         (handle-if-unique state side (:identity runner) (butcher-boy-event))
                         (add-counter state :corp (get-card state (:identity corp)) :butcher-boy 1))}]}))

(defcard "ONR Cloak"
  {:recurring 3
   :interactions {:pay-credits {:req (req (and run
                                               (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")
                                               (not (has-subtype? target "Noisy"))))
                                :type :recurring}}})

(defcard "ONR Clown"
  {:static-abilities [{:type :ice-strength
                       :req (req (and (get-current-encounter state)
                                      (same-card? current-ice target)))
                       :value -1}]})

(defcard "ONR Cockroach"
  {:implementation "This only effects end-of-turn discard for now. Discards are not paying costs. Send me an @ with other places it needs to hit"
   :events [{:event :successful-run
             :req (req (= :hq (target-server context)))
             :msg (msg "give the corp a Cockroach counter")
             :effect (req
                       (add-counter state :corp (get-card state (:identity corp)) :cockroach 1)
                       (register-effect-once
                         state side card
                         {:type :player-randomly-discards
                          :ability-name "Cockroach Counters"
                          :req (req (and (= :corp side)
                                         (>= (get-counters (get-card state (:identity corp)) :cockroach) 2)))
                          :value true}))}]})

(defcard "ONR Codecracker"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Code Gate")
                                (strength-pump 1 1)]}))

(defcard "ONR Codeslinger"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")]}))

(defcard "ONR Corrosion"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Wall")
                                (strength-pump 1 1)]}))

(defcard "ONR Crumble"
  {:implementation "Depends on you running the ONR Runner ID"
   :events [{:event :run-ends
             :req (req (and (= :hq (target-server context))
                                       (:successful context)))
             :msg (msg "give the corp a Crumble counter")
             :effect (req
                       (add-counter state :corp (get-card state (:identity corp)) :crumble 1))}]})

(defcard "ONR Cyfermaster"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Code Gate")
                                (strength-pump 1 1)]}))

(defcard "ONR Deep Thought"
  (letfn [(thought-event []
            {:event :runner-turn-begins
             :unregister-once-resolved true
             :ability-name "Thought Counters"
             :async true
             :effect (req (let [counters (get-counters (get-card state (:identity corp)) :thought)
                                to-peek (<= 3 counters)]
                            (when-not (zero? counters)
                               (register-events
                                 state side
                                 (:identity runner)
                                 [(thought-event)]))
                            (if to-peek
                              (continue-ability
                                state side
                                {:prompt (req (->> corp :deck first :title (str "The top card of R&D is ")))
                                 :effect (req (system-msg state side "looks at the top cards of R&D due to Thought counters (ONR Deep Thought)"))
                                 :choices ["OK"]}
                                card nil)
                              (effect-completed state side eid))))})]
    {:events [{:event :successful-run
               :req (req (= :rd (target-server context)))
               :msg (msg "give the corp a Thought counter")
               :effect (req
                         (handle-if-unique state side (:identity runner) (thought-event))
                         (add-counter state :corp (get-card state (:identity corp)) :thought 1))}]}))

(defcard "ONR Disintegrator"
  {:events [{:event :pass-ice
             :req (req
                    (and (all-subs-broken? (:ice target))
                         (can-pay? state side eid card nil [:credit 2])))
             :effect (req (let [target-ice (:ice target)]
                            (continue-ability
                              state side
                              {:optional
                               {:prompt (msg "2[Credits]: Derez " (:title target-ice) " and end the run?")
                                :yes-ability {:cost [:credit 2]
                                              :async true
                                              :msg (msg "derez " (:title target-ice) " and end the run")
                                              :effect (effect (derez target-ice)
                                                              (end-run :runner eid card))}}}
                              card nil)))}]})

(defcard "ONR Dogcatcher"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Hellhound")
                                (break-sub 1 1 "Bloodhound")
                                (break-sub 1 1 "Watchdog")
                                (break-sub 1 1 "Pit Bull")
                                (strength-pump 1 1)]}))

(defcard "ONR Dropp [TM]"
  {:abilities [(break-sub 0 0 "All" {:additional-ability {:msg "end the run™"
                                                          :effect (effect (end-run :runner eid card))}})
               (strength-pump 1 1)]
   :implementation "Erratum - Should read '0[Credits] Break all subroutines of a piece
    of ice, and end the run. 1[Credits]: +1 strength."})

(defcard "ONR Dupré"
  (let [check-last-server {:req (req (let [last-server (get-in (get-card state card) [:special :last-server])]
                                       (and run last-server (not (= last-server (target-server (:run @state)))))))
                           :effect (req (update! state runner (assoc-in (get-card state card) [:special :last-server] nil))
                                        (add-counter state side (get-card state card) :power (- (get-counters (get-card state card) :power))))
                           :msg "remove all power counters"}]
    {:implementation "uses power counters"
     :abilities [(break-sub 1 1 "Code Gate" {:additional-ability {:req (req run)
                                                                  :async true
                                                                  :effect (req (update! state :runner (assoc-in (get-card state card) [:special :broke-sub] true))
                                                                               (continue-ability
                                                                                 state side
                                                                                 check-last-server
                                                                                 card nil))}})
                 ;; graft these together the hard way - yuck
                 {:label "add 1 strength"
                  :req (req true)
                  :cost [:credit 2]
                  :pump 1
                  :msg (msg "increase its strength from " (get-strength card)
                            " to " (+ 1 (get-strength card)))
                  :effect (effect (pump card 1)
                                  (continue-ability check-last-server card nil))}]
     :static-abilities [{:type :breaker-strength
                         :req (req (same-card? target card))
                         :value (req (get-counters (get-card state card) :power))}]
     :events [{:event :run-ends
               :req (req (get-in (get-card state card) [:special :broke-sub]))
               :msg (msg "place a +1 power counter on itself")
               :effect (req (add-counter state side card :power 1)
                            (update! state :runner (assoc-in (get-card state card) [:special :last-server] (target-server context))))}]}))

(defcard "ONR Dwarf"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Wall")
                                (strength-pump 1 1)]}))

(defcard "ONR Early Worm"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Wall")
                                (strength-pump 2 3)]}))

(defcard "ONR Emergency Self-Construct"
  (letfn [(prevent-event []
            {:event :pre-damage
             :ability-name "Self-Construct Damage Prevention"
             :unregister-once-resolved true
             :req (req (= target :meat))
             :effect (req
                       (system-msg state side "prevents all meat damage due to the effect of ONR Emergency Self-Construct")
                       (register-events state side card [(prevent-event)])
                       (damage-prevent state side :meat Integer/MAX_VALUE))})]
  {:implementation "Will ask for consent when you would get flatlined"
   :events [{:event :pre-flatline
             :optional {:prompt "Trash Self-Construct to prevent being flatlined?"
                        :no-ability {:msg "accept the consequences"}
                        :yes-ability {:msg "prevent being flatlined, remove all brain damage, gain one less action each turn, and prevent all meat damage for the remainder of the game"
                                      :cost [:trash-can]
                                      :effect (req (swap! state assoc-in [:damage :flatline-prevent] true)
                                                   ;; remove all brain damage
                                                   (swap! state update-in [:runner :brain-damage] #(- % %))
                                                   (lose state :runner :click-per-turn 1)
                                                   (handle-if-unique
                                                     state side (:identity runner)
                                                     (prevent-event)))}}}]}))

(defcard "ONR Enterprise, Inc., Shields"
  {:interactions {:prevent [{:type #{:net :brain}
                             :req (req true)}]}
   :abilities [{:cost [:credit 1]
                :msg "prevent up to 2 net damage"
                :effect (effect (damage-prevent :net 2))}
               {:label "Prevent 1 brain damage"
                :msg "prevent 1 brain damage"
                :cost [:credit 1]
                :effect (effect (damage-prevent :brain 1))}]})

(defcard "ONR Evil Twin"
  (letfn [(uses [state card]
            (or (get-in (get-card state card) [:special :usecount]) 0))
          (set-use [state side card x]
            (update! state side (assoc-in (get-card state card) [:special :usecount] x)))]
    (auto-icebreaker
      {:interactions {:prevent [{:type #{:net :brain}
                                 :req (req (if (< (uses state card) 2) true false))}]}
       :events [{:event :runner-turn-ends
                 :silent (req true)
                 :effect (req (set-use state side card 0))}
                {:event :corp-turn-ends
                 :silent (req true)
                 :effect (req (set-use state side card 0))}]
       :abilities [(break-sub 3 1 "Sentry")
                   (strength-pump 1 1)
                   {:label "Prevent 1 net damage"
                    :req (req (< (uses state card) 2))
                    :cost [:credit 0]
                    :msg "prevent 1 net damage"
                    :effect (req
                              (set-use state side card (inc (uses state card)))
                              (damage-prevent state side :net 1))}
                   {:label "Prevent 1 brain damage"
                    :req (req (< (uses state card) 2))
                    :msg "prevent 1 brain damage"
                    :cost [:credit 0]
                    :effect (req
                              (set-use state side card (inc (uses state card)))
                              (damage-prevent state side :brain 1))}]})))

(defcard "ONR Expert Schedule Analyzer"
  {:events [{:event :end-breach-server
             :req (req (= :hq (:breach-server target)))
             :msg (msg "reveal " (enumerate-str (sort (map :title (:hand corp)))) " from HQ")
             :async true
             :effect (effect (reveal eid (:hand corp)))}]})

(defcard "ONR Fait Accompli" {}) ;;TODO

(defcard "ONR False Echo"
  (letfn [(continue-rezzing [ices]
            {:async true
             :effect (req
                       (if (empty? ices)
                         (effect-completed state side eid)
                         (if (rezzed? (last ices))
                           (continue-ability state side (continue-rezzing (butlast ices)) card nil)
                           (wait-for (rez state :corp (last ices))
                                     (when-not async-result
                                       (system-msg state :corp (str " was unable to rez " (card-str state (last ices)))))
                                     (continue-ability
                                     state side
                                     (continue-rezzing (butlast ices))
                                     card nil)))))})]
  {:implementation "corp can not decline additional costs!"
   :abilities [{:req (req (and (pos? (count (:successful-run runner-reg)))))
                :cost [:credit 2]
                :label "Force the corp to rez"
                :async true
                :prompt "force the Corp to rez ice where?"
                :choices (req (cancellable (distinct (map zone->name (:successful-run runner-reg))) :sorted))
                :effect (req (let [ices (:ices (get-in @state (cons :corp (server->zone state target))))]
                               (continue-ability
                                 state side
                                 (continue-rezzing ices)
                                 card nil)))}]}))

(defcard "ONR Flak"
  (auto-icebreaker {:abilities [(break-sub 1 1 "AP")
                                (strength-pump 1 1)]}))

(defcard "ONR Force Shield"
  (letfn [(uses [state card]
            (or (get-in (get-card state card) [:special :usecount]) 0))
          (set-use [state side card x]
            (update! state side (assoc-in (get-card state card) [:special :usecount] x)))]
    {:interactions {:prevent [{:type #{:net :brain}
                               :req (req (if (< (uses state card) 2) true false))}]}
     :events [{:event :runner-turn-ends
               :silent (req true)
               :effect (req (set-use state side card 0))}
              {:event :corp-turn-ends
               :silent (req true)
               :effect (req (set-use state side card 0))}]
     :abilities [{:label "Prevent 1 net damage"
                  :req (req (< (uses state card) 2))
                  :cost [:credit 0]
                  :msg "prevent 1 net damage"
                  :effect (req
                            (set-use state side card (inc (uses state card)))
                            (damage-prevent state side :net 1))}
                 {:label "Prevent 1 brain damage"
                  :req (req (< (uses state card) 2))
                  :msg "prevent 1 brain damage"
                  :cost [:credit 0]
                  :effect (req
                            (set-use state side card (inc (uses state card)))
                            (damage-prevent state side :brain 1))}]}))

(defcard "ONR Forward's Legacy"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Sentry")]
                    :events [{:event :run
                              :async true
                              :effect (req (let [str (dice-roll)]
                                             (continue-ability
                                               state side
                                               {:msg (msg "gain " str " strength for the run")
                                                :effect (effect (pump card str :end-of-run))}
                                               card nil)))}]}))


(defcard "ONR Fubar"
  (auto-icebreaker
    (letfn [(chosen-type [card] (get-in card [:card-target]))]
      {:abilities [(break-sub 1 1 "All"
                              (assoc (lose-from-stealth 1)
                                     :req (req (and (chosen-type card)
                                                    (has-subtype? current-ice (chosen-type card))))))
                   (strength-pump 2 1)
                   {:prompt "Choose target type"
                    :req (req (not (:card-target card)))
                    :label "Choose subtype (once per game)"
                    :choices ["Sentry" "Code Gate" "Wall"]
                    :cost [:credit 0]
                    :msg (msg "choose to be able to break " target " subroutines")
                    :effect (effect (update! (assoc card :card-target target)))}]})))

(defcard "ONR Garbage In"
  {:implementation "Depends on you running the ONR Runner ID"
   :events [{:event :run-ends
             :req (req (and (= :rd (target-server context))
                            (:successful context)))
             :msg (msg "give the corp a Garbage counter")
             :effect (req
                       (add-counter state :corp (get-card state (:identity corp)) :garbage 1))}]})

(defcard "ONR Gremlins"
  {:events [{:event :successful-run
             :req (req (= :hq (target-server context)))
             :msg (msg "give the corp a Gremlin counter")
             :effect (req
                       (add-counter state :corp (get-card state (:identity corp)) :gremlin 1)
                       (register-effect-once
                         state side card
                         {:type :hand-size
                          :ability-name "Gremlins"
                          :req (req (= :corp side))
                          :value (req (- (int (/ (get-counters (get-card state (:identity corp)) :gremlin) 2))))}))}]})

(defcard "ONR Grubb"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Wall")
                                (strength-pump 2 1 :end-of-run)]}))

(defcard "ONR Hammer"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Wall" (lose-from-stealth 2))
                                (strength-pump 1 1)]}))

(defcard "ONR Highlighter"
  (letfn [(highlighter-event []
            {:event :breach-server
             :unregister-once-resolved true
             :side :runner
             :async true
             :ability-name "Highlighter Counters"
             :req (req (= target :rd))
             :effect (req (let [counters (get-counters (get-card state (:identity corp)) :highlighter)
                                add-access (dec counters)]
                            (when (pos? add-access)
                              (system-msg state side (str "accesses an additional " add-access " cards from R&D due to Highlighter counters (ONR Highlighter)"))
                              (access-bonus state side :rd add-access))
                            (when-not (zero? counters)
                              (register-events
                                state side
                                (:identity runner)
                                [(highlighter-event)]))
                            (effect-completed state side eid)))})]
    {:events [{:event :run-ends
               :req (req (and (= :rd (target-server context))
                              (:successful context)))
               :msg (msg "give the corp a Highlighter counter")
               :effect (req
                         (handle-if-unique state side (:identity runner) (highlighter-event) [:rd])
                         (add-counter state :corp (get-card state (:identity corp)) :highlighter 1))}]}))

(defcard "ONR Imp"
  {:implementation "MU Limit not enforced"
   :static-abilities [(host-with-negative-strength 1)]
   :abilities (daemon-abilities)})

(defcard "ONR Incubator" {}) ;;todo

(defcard "ONR Invisibility"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and run
                                               (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")
                                               (not (has-subtype? target "Noisy"))))
                                :type :recurring}}})

(defcard "ONR Jackhammer"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Wall" (lose-from-stealth 1))
                                (strength-pump 1 1)]}))

(defcard "ONR Japanese Water Torture"
  (auto-icebreaker {:implementation "boost is atomic instead of an x-fn"
                    :abilities [(break-sub 0 1 "Wall")
                                (strength-pump [:forgo-next-click 1 :credit 1] 1)]}))

(defcard "ONR Joan of Arc"
  {:interactions {:prevent [{:type #{:trash-program}
                             :req (req
                                    (let [target-card (:prevent-target target)]
                                      (if-not (same-card? target-card card)
                                        true
                                        false)))}]}
   :abilities [{:cost [:trash-can]
                :label "prevent a program trash"
                :effect (effect (trash-prevent :program 1))}
               {:cost [:credit 1 :return-to-hand]
                :label "prevent a program trash"
                :effect (effect (trash-prevent :program 1))}]})

(defcard "ONR Krash"
  (auto-icebreaker {:abilities [(break-sub 2 1 "All")
                                (strength-pump 2 1)]}))

(defcard "ONR Lockjaw"
  {:abilities [{:req (req (get-current-encounter state))
                :cost [:trash-can]
                :label "Give icebreaker +2 strength for the run"
                :prompt "Choose an installed non-AI icebreaker"
                :choices {:card #(and (has-subtype? % "Icebreaker")
                                      (installed? %))}
                :msg (msg "give +2 strength to " (:title target))
                :effect (effect (pump target 2 :end-of-run))}]})

(defcard "ONR Loony Goon"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 1 1)]}))

(defcard "ONR MS-todon"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 1 1)]
                    :events [{:event :subroutines-broken
                              :once :per-run
                              :req (req (and
                                          (has-subtype? target "Sentry")
                                          (any-subs-broken-by-card? target card)))
                              :msg "lose all stealth credits and take a tag"
                              :effect (req (wait-for (gain-tags state :runner 1)
                                                     (lose-all-stealth state side card eid)))}]}))

(defcard "ONR Matador"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 3 5)]}))

(defcard "ONR Microtech AI Interface"
  {:events [{:event :breach-server
             :async true
             :req (req (= target :rd))
             :effect (effect (continue-ability
                               {:req (req (< 1 (count (:deck corp))))
                                :prompt "How many cards from R&D do you cut to the bottom?"
                                :choices {:number (req (count (:deck corp)))
                                          :default (req 0)}
                                :msg (msg "cut "
                                          (when (= target (count (:deck corp))) "ALL ")
                                          (quantify target " card") " to the bottom of R&D")
                                :effect (req (let [cards (take target (:deck corp))]
                                               (doseq [c cards]
                                                 (move state :corp c :deck))))}
                               card nil))}]})

(defcard "ONR Morphing Tool"
  (auto-icebreaker
    (letfn [(chosen-type [card] (get-in card [:card-target]))
            (choose-type-abi []
              {:prompt "Choose target type"
               :label "Choose subtype (once per game)"
               :choices ["Sentry" "Code Gate" "Wall"]
               :msg (msg "choose to be able to break " target " subroutines")
               :effect (effect (update! (assoc card :card-target target)))})]
      {:on-install (choose-type-abi)
       :abilities [(break-sub 1 1 "All"
                              (assoc (lose-from-stealth 1)
                                     :req (req (and (chosen-type card)
                                                    (has-subtype? current-ice (chosen-type card))))))
                   (strength-pump 2 1)
                   (assoc (choose-type-abi) :cost [:credit 1 :click 1])]})))

(defcard "ONR Mouse"
  {:abilities [{:cost [:click 1]
                :choices {:card #(and (corp? %)
                                      (installed? %)
                                      (not (ice? %)))}
                :effect (effect (expose eid target))
                :msg "expose 1 card"}]})

(defcard "ONR Mystery Box"
  (letfn [(shuffle-end [shuffle-back]
            {:msg (msg "shuffle " (enumerate-str (map :title shuffle-back)) " into the stack")
             :effect (req
                       (doseq [c shuffle-back]
                         (move state side c :deck))
                       (shuffle! state side :deck))})]
    {:abilities [{:label "Install a program from the top 5"
                  :cost [:credit 0]
                  :req (req run)
                  :once :per-run
                  :async true
                  :waiting-prompt true
                  :effect (req (set-aside state side eid (take 5 (:deck runner)))
                               (let [set-aside-cards (sort-by :title (get-set-aside state side eid))
                                     programs (filter program? set-aside-cards)]
                                 (system-msg state side (str "uses " (get-title card)
                                                             " to set aside "
                                                             (enumerate-str (map get-title set-aside-cards))
                                                             " from the top of the stack"))
                                 (wait-for (resolve-ability state side
                                                            {:async true
                                                             :prompt (str "The set aside cards are: "
                                                                          (enumerate-str (map get-title set-aside-cards)))
                                                             :choices ["OK"]}
                                                            card nil)
                                           (if-not (empty? programs)
                                             (wait-for (trash state side card {:cause-card card})
                                                       (system-msg state side (str "trashes " (:title card)))
                                                       (continue-ability
                                                         state side
                                                         {:prompt "Choose a program to install, ignoring all costs"
                                                          :async true
                                                          :choices (req (concat programs ["Done"]))
                                                          :effect (req (if (= "Done" target)
                                                                         (continue-ability state side (shuffle-end set-aside-cards) card nil)
                                                                         (let [set-aside-cards (remove-once #(= % target) set-aside-cards)
                                                                               new-eid (assoc eid :source card :source-type :runner-install)]
                                                                           (wait-for (runner-install state side (make-eid state new-eid) target {:ignore-all-cost true})
                                                                                     (continue-ability state side (shuffle-end set-aside-cards) card nil)))))}
                                                         card nil))
                                             (continue-ability
                                               state side
                                               (shuffle-end set-aside-cards)
                                               card nil)))))}]}))

(defcard "ONR Netspace Inverter"
  {:events [{:event :run-ends
             :async true
             :effect (req (let [target-server (target-server context)
                                server-ice (:ices (get-in @state (cons :corp (server->zone state (zone->name target-server)))))]
                            (if (and (< 1 (count server-ice))
                                     (:successful context))
                              (continue-ability
                                state side
                                {:optional {:prompt (msg "Invert the ice on " (zone->name target-server))
                                            :yes-ability {:msg (msg "invert the ice on " (zone->name target-server))
                                                          :effect (req (let [half (int (/ (count server-ice) 2))
                                                                             first-half (take half server-ice)
                                                                             last-half (take half (reverse server-ice))
                                                                             swaps (map vector first-half last-half)]
                                                                         (doseq [pair swaps]
                                                                           (swap-ice state side (first pair) (second pair)))))}}}
                                card nil)
                              (effect-completed state side eid))))}]})

(defcard "ONR Newsgroup Filter"
  {:abilities [{:cost [:click 1]
                :keep-menu-open :while-clicks-left
                :async true
                :effect (effect (gain-credits eid 2))
                :msg "gain 2 [Credits]"}]})

(defcard "ONR Pattel's Virus"
  {:events [{:event :successful-run
             :req (req (let [events (map #(first %) (run-events state side :subroutines-broken))
                             all-subs-broke (fn [card]
                                              (empty? (remove :broken (:subroutines card))))
                             valid-targets (filter all-subs-broke events)]
                         (some #(and (ice? %) (installed? (get-card state %))) valid-targets)))
             :choices {:req (req (and
                                   (ice? target)
                                   (let [events (map #(first %) (run-events state side :subroutines-broken))
                                         all-subs-broke (fn [card]
                                                          (empty? (remove :broken (:subroutines card))))
                                         valid-targets (filter all-subs-broke events)]
                                     (some #(same-card? % target) valid-targets))))}
             :effect (req (add-counter state side (get-card state target) :pattel 1)
                          (register-effect-once
                            state side card
                            {:type :ice-strength
                             :ability-name "Pattel's Virus"
                             :value (req (- (get-counters (get-card state target) :pattel)))}))
             :msg (msg "place a pattel counter on " (card-str state target))}]})


(defcard "ONR Pile Driver"
  (auto-icebreaker {:abilities [(break-sub 3 4 "Wall" (lose-from-stealth 3))
                                (strength-pump 1 1)]}))

(defcard "ONR Poltergeist"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :runner-trash-corp-cards (:source-type eid))
                                               (asset? target)))
                                :type :recurring}}})

(defcard "ONR Pox" {}) ;;TODO

(defcard "ONR Psychic Friend"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Code Gate")
                                (strength-pump 2 1 :end-of-turn)]}))

(defcard "ONR R&D-Protocol Files"
  (let [ability (successful-run-replace-breach
                 {:target-server :rd
                  :mandatory true
                  :duration :end-of-run
                  :ability
                  {:prompt (msg "The top cards of R&D are" (enumerate-str (map :title (take 5 (:deck corp)))))
                   :choices ["Noted"]
                   :msg (msg "look at the top 5 cards of R&D")}})]
    {:implementation "requires successful run"
     :abilities [{:cost [:click 1]
                  :msg "make a run on R&D"
                  :makes-run true
                  :async true
                  :effect (effect (register-events card [ability])
                                  (make-run eid :rd card))}]}))

(defcard "ONR Rabbit"
  {:static-abilities [{:type :max-strength
                       :req (req (ice? target))
                       :value -1}]})

(defcard "ONR Raffles"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Code Gate")
                                (strength-pump 2 1)]}))

(defcard "ONR Ramming Piston"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Wall" (lose-from-stealth 2))
                                (strength-pump 1 1)]}))

(defcard "ONR Raptor"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Sentry")
                                (strength-pump 1 1)]}))

(defcard "ONR Redecorator"
  (auto-icebreaker {:abilities [(break-sub 1 2 "Sentry")
                                (strength-pump 3 1)]}))

(defcard "ONR Reflector"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Stun")
                                (break-sub 0 1 "Hellbolt")
                                (break-sub 0 1 "Knockout")]}))

(defcard "ONR Rent-I-Con"
  (auto-icebreaker
    {:abilities [(break-sub
                   1 1 "All"
                   {:additional-ability
                    {:msg "will trash itself when this run ends"
                     :effect (req
                               (register-events
                                 state :runner (get-card state card)
                                 [{:event :run-ends
                                   :duration :end-of-run
                                   :unregister-once-resolved true
                                   :async true
                                   :effect (effect (trash eid card {:cause :runner-ability
                                                                    :cause-card card}))}]))}})
                 (strength-pump 1 1)]}))

(defcard "ONR Replicator"
  {:implementation "Trace restriction not implemented"
   :abilities [(break-sub 0 1 "All" {:label "break 1 subroutine that traces"})
               (strength-pump 1 1)]})


(defcard "ONR Scaldan"
  (letfn [(scaldan-event []
            {:event :corp-turn-begins
             :unregister-once-resolved true
             :ability-name "Scaldan Counters"
             :async true
             :effect (req (let [counters (get-counters (get-card state (:identity corp)) :scaldan)
                                di (dice-roll counters)
                                bp (count (filter #(> % 4) di))]
                            (if-not (zero? counters)
                              (do (register-events state side (:identity runner) [(scaldan-event)])
                                  (system-msg state side (str "forces the Corp to roll " (seq di) " and gain " bp " Bad Publicity (ONR Scaldan)"))
                                  (gain-bad-publicity state :runner eid bp))
                              (effect-completed state side eid))))})]
    {:events [{:event :successful-run
               :req (req (= :hq (target-server context)))
               :msg (msg "give the corp a Scaldan counter")
               :effect (req
                         (handle-if-unique state side (:identity runner) (scaldan-event))
                         (add-counter state :corp (get-card state (:identity corp)) :scaldan 1))}]}))

(defcard "ONR Scatter Shot"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :runner-trash-corp-cards (:source-type eid))
                                               (upgrade? target)))
                                :type :recurring}}})

(defcard "ONR Schematics Search Engine"
  (letfn [(expose-chain [state side eid xs]
            (if-not (seq xs)
              (effect-completed state side eid)
              (wait-for (expose state side (make-eid state eid) (first xs))
                        (expose-chain state side eid (rest xs)))))]
    {:events [{:event :breach-server
               :async true
               :req (req (= target :hq))
               :effect (effect (continue-ability
                                 {:optional
                                  {:prompt "Expose all of the cards?"
                                   :req (req (some #(not (rezzed? %)) (all-installed state :corp)))
                                   :yes-ability {:msg "expose ALL of the cards"
                                                 :effect (req (expose-chain state side eid (filter #(not (rezzed? %)) (all-installed state :corp))))
                                                 :async true}}}
                                 card nil))}]}))

(defcard "ONR SeeYa"
  {:abilities [{:cost [:click 1 :credit 1]
                :choices {:card #(and (corp? %)
                                      (installed? %))}
                :effect (effect (expose eid target))
                :msg "expose 1 card"}]})

(defcard "ONR Self-Modifying Code"
  {:abilities [{:req (req (not (install-locked? state side)))
                :label "Install a program from the stack"
                :cost [:trash-can]
                :async true
                :effect (effect (continue-ability
                                  {:prompt "Choose a program to install"
                                   :msg (msg (if (= target "Done")
                                               "shuffle the stack"
                                               (str "install " (:title target) " from the stack")))
                                   :choices (req (concat
                                                   (->> (:deck runner)
                                                        (filter
                                                          #(and (program? %)
                                                                (can-pay? state side
                                                                          (assoc eid :source card :source-type :runner-install)
                                                                          % nil [:credit (install-cost state side %)])))
                                                        (sort-by :title)
                                                        (seq))
                                                  ["Done"]))
                                   :async true
                                   :effect (req (trigger-event state side :searched-stack nil)
                                                (shuffle! state side :deck)
                                                (if (= target "Done")
                                                  (effect-completed state side eid)
                                                  (runner-install state side (assoc eid :source card :source-type :runner-install) target nil)))}
                                  card nil))}]})

(defcard "ONR Shaka"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry")
                                (strength-pump 2 1)]}))

(defcard "ONR Shield"
  (letfn [(uses [state card]
            (or (get-in (get-card state card) [:special :usecount]) 0))
          (set-use [state side card x]
            (update! state side (assoc-in (get-card state card) [:special :usecount] x)))]
    {:interactions {:prevent [{:type #{:net}
                               :req (req (if (< (uses state card) 2) true false))}]}
     :events [{:event :runner-turn-ends
               :silent (req true)
               :effect (req (set-use state side card 0))}
              {:event :corp-turn-ends
               :silent (req true)
               :effect (req (set-use state side card 0))}]
     :abilities [{:label "Prevent 1 net damage"
                  :req (req (< (uses state card) 2))
                  :cost [:credit 0]
                  :msg "prevent 1 net damage"
                  :effect (req
                            (set-use state side card (inc (uses state card)))
                            (damage-prevent state side :net 1))}]}))

(defcard "ONR Shredder Uplink Protocol"
  {:abilities [{:cost [:click 1]
                :msg "make a run on Archives"
                :makes-run true
                :async true
                :effect (effect (register-events
                                  card
                                  [{:event :pre-successful-run
                                    :duration :end-of-run
                                    :unregister-once-resolved true
                                    :interactive (req true)
                                    :msg "change the attacked server to HQ"
                                    :req (req (= :archives (-> run :server first)))
                                    :effect (req (swap! state assoc-in [:run :server] [:hq])
                                                 (trigger-event state :corp :no-action))}])
                                (make-run eid :archives (get-card state card)))}]})

(defcard "ONR Signpost"
  {:events [{:event :trace-revealed
             :optional {:prompt "Gain 2 link?"
                        :waiting-prompt true
                        :yes-ability (boost-link-abi 1 2)}}]})

(defcard "ONR Skeleton Passkeys"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Code Gate")
                                (strength-pump 3 4)]}))

(defcard "ONR Skivviss"
  (letfn [(skivviss-event []
            {:event :corp-turn-begins
             :unregister-once-resolved true
             :ability-name "Skivviss Counters"
             :async true
             :effect (req (let [counters (get-counters (get-card state (:identity corp)) :skivviss)]
                            (if-not (zero? counters)
                              (do (register-events state side (:identity runner) [(skivviss-event)])
                                  (system-msg state side (str "forces the Corp to draw " counters "cards (ONR Skivviss)"))
                                  (draw state :corp eid counters))
                              (effect-completed state side eid))))})]
    {:implementation "draws are a start of turn event - if the timing is an issue, send me an @"
     :events [{:event :successful-run
               :req (req (= :rd (target-server context)))
               :msg (msg "give the corp a Skivviss counter")
               :effect (req
                         (handle-if-unique state side (:identity runner) (skivviss-event))
                         (add-counter state :corp (get-card state (:identity corp)) :skivviss 1))}]}))

(defcard "ONR Skullcap"
  {:interactions {:prevent [{:type #{:net :brain}
                             :req (req true)}]}
   :abilities [{:cost [:trash-can]
                :label "prevent any amount of net damage"
                :msg "prevent any amount of net damage"
                :effect (req (damage-prevent state :runner :net Integer/MAX_VALUE))}
               {:cost [:trash-can]
                :label "prevent any amount of brain damage"
                :msg "prevent any amount of brain damage"
                :effect (req (damage-prevent state :runner :brain Integer/MAX_VALUE))}]})

(defcard "ONR Smarteye"
    {:events [{:event :approach-ice
             :optional
             {:req (req (not (rezzed? (:ice context))))
              :prompt "Expose approached piece of ice?"
              :yes-ability
              {:async true
               :msg "expose the approached piece of ice"
               :effect (req (wait-for
                              (expose state side (:ice context))
                              (continue-ability state side (offer-jack-out) card nil)))}}}]})

(defcard "ONR Snowball"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Sentry"
                                           {:additional-ability {:msg "gain +1 strength for the remainder of the run"
                                                                 :effect (effect (pump card 1 :end-of-run))}})
                                (strength-pump 1 1)]}))

(defcard "ONR Speed Trap"
  {:events [{:event :rez
             :req (req (and (or (asset? (:card context))
                                (upgrade? (:card context)))
                            run))
             :async true
             :effect
             (effect
               (continue-ability
                 {:optional
                  {:player :runner
                   :waiting-prompt true
                   :prompt (msg "Jack out?")
                   :yes-ability
                   {:cost [:credit 0]
                    :msg (msg "jack out at light speed")
                    :async true
                    :effect (req (jack-out state side eid))}}}
                 card nil))}]})

(defcard "ONR Startup Immolator"
  {:events [{:event :subroutines-broken
             :req (req (and (same-card? (last run-ices) target)
                             (all-subs-broken? target)))
             :effect (req (let [target-ice target
                                ccost (:cost target)]
                            (continue-ability
                              state side
                              {:optional
                               {:prompt (msg "Trash this card, pay " ccost "[Credits]: trash " (:title target-ice) "?")
                                :req (req (can-pay? state side eid card nil [:credit ccost]))
                                :yes-ability
                                {:async true
                                 :cost [:credit ccost :trash-can]
                                 :msg (msg "trash " (card-str state target-ice))
                                 :effect (effect (trash eid target-ice {:cause-card card}))}}}
                              card nil)))
             :async true}]})

(defcard "ONR Succubus"
  {:implementation "MU Limit not enforced"
   :static-abilities [(host-with-negative-strength 0)]
   :abilities (daemon-abilities)})

(defcard "ONR Superglue"
  {:abilities [{:req (req (and (get-current-encounter state)
                               (rezzed? current-ice)
                               (all-subs-broken? current-ice)))
                :label "derez an ice"
                :cost [:trash-can]
                :msg (msg "derez " (:title current-ice))
                :effect (effect (derez current-ice))}]})

(defcard "ONR Taxman"
  (letfn [(tax-event []
            {:event :corp-turn-begins
             :unregister-once-resolved true
             :ability-name "Tax Counters"
             :async true
             :effect (req (let [counters (get-counters (get-card state (:identity corp)) :tax)
                                loss (quot counters 2)]
                            (if-not (zero? counters)
                              (do (register-events state side (:identity runner) [(tax-event)])
                                  (if (pos? loss)
                                    (do (system-msg state side (str "forces the Corp to lose " loss " [Credits] (ONR Taxman)"))
                                        (lose-credits state :corp eid loss))
                                    (effect-completed state side eid)))
                              (effect-completed state side eid))))})]
    {:events [{:event :successful-run
               :req (req (= :hq (target-server context)))
               :msg (msg "give the corp a Tax counter")
               :effect (req
                         (handle-if-unique state side (:identity runner) (tax-event))
                         (add-counter state :corp (get-card state (:identity corp)) :tax 1))}]}))

(defcard "ONR Tinweasel"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Code Gate")]}))

(defcard "ONR Vewy Vewy Quiet"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and run
                                               (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")
                                               (not (has-subtype? target "Noisy"))))
                                :type :recurring}}})

(defcard "ONR Vienna 22"
  (letfn [(vienna-event []
            {:event :breach-server
             :unregister-once-resolved true
             :side :runner
             :async true
             :ability-name "Vienna Counters"
             :req (req (= target :hq))
             :effect (req (let [counters (get-counters (get-card state (:identity corp)) :vienna)
                                add-access counters]
                            (when (pos? add-access)
                              (system-msg state side (str "accesses an additional " add-access " cards from HQ due to Vienna counters (ONR Vienna 22)"))
                              (access-bonus state side :hq add-access))
                            (when-not (zero? counters)
                              (register-events
                                state side
                                (:identity runner)
                                [(vienna-event)]))
                            (effect-completed state side eid)))})]
    {:events [{:event :run-ends
               :req (req (and (= :hq (target-server context))
                              (:successful context)))
               :msg (msg "give the corp a Vienna counter")
               :effect (req
                         (handle-if-unique state side (:identity runner) (vienna-event) [:hq])
                         (add-counter state :corp (get-card state (:identity corp)) :vienna 1))}]}))

(defcard "ONR Viral Pipeline" {:implementation "todo"}) ;;TODO

(defcard "ONR Wild Card"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Sentry")
                                (strength-pump 3 1)]}))

(defcard "ONR Wizard's Book"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Code Gate")
                                (strength-pump 2 1)]}))

(defcard "ONR Worm"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Wall")
                                (strength-pump 3 1)]}))

(defcard "ONR Wrecking Ball"
  (auto-icebreaker {:abilities [(break-sub 0 1 "Wall" (lose-from-stealth 1))
                                (strength-pump 2 1)]}))

(defcard "ONR Zetatech Software Installer"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :runner-install (:source-type eid))
                                               (program? target)))
                                :type :recurring}}})
