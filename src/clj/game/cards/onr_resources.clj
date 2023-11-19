(ns game.cards.onr-resources
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [game.core.access :refer [access-bonus access-n-cards breach-server steal
                             steal-cost-bonus]]
   [game.core.agendas :refer [update-all-advancement-requirements
                              update-all-agenda-points]]
   [game.core.bad-publicity :refer [gain-bad-publicity]]
   [game.core.board :refer [all-active all-active-installed all-installed
                            all-installed-runner card->server server->zone]]
   [game.core.card :refer [agenda? asset? assoc-host-zones card-index corp?
                           event? facedown? get-agenda-points get-card get-counters
                           get-title get-zone hardware? has-subtype? ice? identity? in-discard? in-hand?
                           installed? is-type? program? resource? rezzed? runner? upgrade? virus-program?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.charge :refer [can-charge charge-ability]]
   [game.core.cost-fns :refer [has-trash-ability? install-cost rez-cost
                               trash-cost]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage damage-prevent]]
   [game.core.def-helpers :refer [breach-access-bonus defcard offer-jack-out
                                  reorder-choice trash-on-empty do-net-damage]]
   [game.core.drawing :refer [draw click-draw-bonus draw-bonus]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid]]
   [game.core.engine :refer [not-used-once? pay register-events
                             register-once register-suppress resolve-ability
                             trigger-event trigger-event-sync unregister-events unregister-suppress-by-uuid checkpoint]]
   [game.core.events :refer [event-count first-event?
                             first-installed-trash-own? first-run-event?
                             first-successful-run-on-server? get-turn-damage no-event? second-event? turn-events]]
   [game.core.expose :refer [expose]]
   [game.core.flags :refer [card-flag? clear-persistent-flag!
                            has-flag? in-corp-scored?
                            register-persistent-flag! register-turn-flag! zone-locked?]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-clicks
                              lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+ hand-size runner-hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [break-sub break-subroutine! get-strength pump
                          unbroken-subroutines-choice update-all-ice
                          update-all-icebreakers update-breaker-strength]]
   [game.core.identities :refer [disable-card enable-card]]
   [game.core.initializing :refer [card-init make-card]]
   [game.core.installing :refer [install-locked? runner-can-install? runner-can-pay-and-install?
                                 runner-install]]
   [game.core.link :refer [get-link link+]]
   [game.core.mark :refer [identify-mark-ability is-mark?]]
   [game.core.memory :refer [available-mu]]
   [game.core.moving :refer [as-agenda flip-faceup forfeit mill move
                             remove-from-currently-drawing trash trash-cards
                             trash-prevent]]
   [game.core.optional :refer [get-autoresolve never? set-autoresolve]]
   [game.core.payment :refer [build-spend-msg can-pay?]]
   [game.core.pick-counters :refer [pick-virus-counters-to-spend]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable]]
   [game.core.props :refer [add-counter add-icon remove-icon]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [bypass-ice can-run-server? get-runnable-zones
                           gain-run-credits get-current-encounter
                           jack-out
                           update-current-encounter
                           make-run set-next-phase
                           successful-run-replace-breach total-cards-accessed]]
   [game.core.sabotage :refer [sabotage-ability]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name is-central? is-remote?
                              protecting-same-server? remote->name target-server unknown->kw
                              zone->name zones->sorted-names]]
   [game.core.set-aside :refer [set-aside get-set-aside set-aside-for-me]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.tags :refer [gain-tags lose-tags tag-prevent]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.threat :refer [threat-level]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [get-virus-counters number-of-runner-virus-counters]]
   [game.core.winning :refer [check-win-by-agenda]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [game.core.onr-trace :refer [boost-link set-base-link cancel-successful-trace]]
   [jinteki.utils :refer :all]
   [jinteki.validator :refer [legal?]]
   [medley.core :refer [find-first]]))

(defn- base-link-abi
  [cost val]
  (let [cost (if (integer? cost) [:credit cost] cost)]
    {:onr-base-link true
     :req (req true)
     :cost cost
     :base-link val
     :label (str "Base Link " val)
     :msg (str "set their Base Link to " val)
     :effect (req (set-base-link state val))}))

(defn- boost-link-abi
  [cost val]
  (let [cost (if (integer? cost) [:credit cost] cost)]
    {:onr-boost-link true
     :cost cost
     :label (str "+" val " Link")
     :msg (str "gain +" val " Link")
     :effect (req (boost-link state val))}))

;; card implementations

(defcard "ONR Access through Alpha"
  {:abilities [(base-link-abi 1 9)]})

(defcard "ONR Access to Arasaka"
  {:abilities [(base-link-abi 2 4)
               (boost-link-abi 2 1)]})

(defcard "ONR Access to Kiribati"
  {:abilities [(base-link-abi 1 1)
               (boost-link-abi 1 1)]})

(defcard "ONR Back Door to Hilliard"
  {:abilities [(base-link-abi 0 2)
               (boost-link-abi 3 1)]})

(defcard "ONR Back Door to Netwatch"
  {:abilities [(set-autoresolve :auto-fire "Back Door to Netwatch")]
   :events [{:event :successful-trace
             :async true
             :effect (req
                       (let [trace target]
                         (continue-ability
                           state side
                           {:optional
                            {:autoresolve (get-autoresolve :auto-fire)
                             :prompt "Cancel the effects of the trace?"
                             :yes-ability {:cost [:trash-can :credit 3]
                                           :async true
                                           :msg (msg "cancel the effect of the trace"
                                                     (when-not (:only-tags trace)
                                                       " and give the Corp 1 Bad Publicity point"))
                                           :effect (req (cancel-successful-trace state)
                                                        (if (:only-tags trace)
                                                          (gain-bad-publicity state :corp eid 1)
                                                          (effect-completed state side eid)))}}}
                           card nil)))}]})

(defcard "ONR Back Door to Orbital Air"
  {:abilities [(base-link-abi 1 2)
               (boost-link-abi 2 1)]})

(defcard "ONR Back Door to Rivals"
  {:abilities [(base-link-abi 0 2)
               (boost-link-abi 3 1)]
   :events [{:event :unsuccessful-trace
             :req (req (same-card? card (:base-link-card target)))
             :async true
             :msg "gain 1[Credit]"
             :effect (effect (gain-credits eid 1))}]})

(defcard "ONR Bolt-Hole"
  {:interactions {:prevent [{:type #{:meat}
                             :req (req true)}]}
   :abilities [{:label "Prevent 2 meat damage"
                :msg "prevent 2 meat damage"
                :cost [:trash-can]
                :effect (effect (damage-prevent :meat 2))}]})

(defcard "ONR Broker"
  {:abilities [{:cost [:click 1]
                :msg "store 3 [Credits]"
                :once :per-turn
                :effect (effect (add-counter card :credit 3))}
               {:cost [:click 1]
                :msg (msg "gain " (get-counters card :credit) " [Credits]")
                :once :per-turn
                :label "Take all credits"
                :async true
                :effect (effect (add-counter card :credit (- (get-counters card :credit)))
                                (gain-credits eid (get-counters card :credit)))}]})

(defcard "ONR Chiba Bank Account"
  {:abilities [{:label "Gain 4 [Credits]"
                :msg "gain 4 [Credits]"
                :cost [:trash-can :credit 1]
                :async true
                :effect (effect (gain-credits eid 4))}]})

(defcard "ONR Corporate Ally"
  {:static-abilities [{:type :advancement-requirement
                       :value 1}]
   :additional-cost [:agenda-point 1]})

(defcard "ONR Crash Everett, Inventive Fixer"
  {:events [{:event :pre-runner-draw
             :msg "draw 1 additional card"
             ;; The req catches draw events that happened before the card was installed
             :effect (req (draw-bonus state side 1))}
            {:event :runner-draw
             :interactive (req true)
             :async true
             :effect (req (let [drawn runner-currently-drawing]
                            (continue-ability
                              state side
                              (when (seq drawn)
                                {:waiting-prompt true
                                 :prompt (str "Choose a card to manipulate")
                                 :async true
                                 :choices {:max 1
                                           :card #(some (fn [c] (same-card? c %)) drawn)
                                           :all true}
                                 :effect (req (let [chosen-card target]
                                                  (continue-ability
                                                    state side
                                                    {:prompt "choose one"
                                                     :choices [(str "Trash " (:title chosen-card))
                                                               (str "Move " (:title chosen-card) " to top")]
                                                     :async true
                                                     :msg (msg (if (= target (str "Trash " (:title chosen-card)))
                                                                 (str "trash " (:title chosen-card))
                                                                 (str "add the "
                                                                      (pprint/cl-format nil "~:R" (inc (first (keep-indexed #(when (same-card? chosen-card %2) %1) drawn))))
                                                                      " card drawn to the top of the stack")))
                                                     :effect (req
                                                               (remove-from-currently-drawing state side chosen-card)
                                                               (if (= target (str "Trash " (:title chosen-card)))
                                                                 (trash state side eid chosen-card {:cause-card card})
                                                                 (do (move state side chosen-card :deck {:front true})
                                                                     (effect-completed state side eid))))}
                                                    chosen-card nil)))})
                              card nil)))}]})

(defcard "ONR Crash Space"
  {:implementation "both players still bid on traces as normal, it's just the result that's fixed"
   :leave-play (effect (system-msg "uses ONR Crash Space to lose 2[Credits]")
                       (lose-credits eid 2))
   :abilities [{:label "Trash this space"
                :cost [:click 1]
                :msg "trash itself"
                :async true
                :effect (effect (trash eid card {:cause-card card}))}]
   :events [{:event :runner-turn-begins
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}
            {:event :successful-trace
             :player :corp
             :msg "give the Runner an additional tag"
             :async true
             :effect (req (gain-tags state :runner eid 1))}]
   :static-abilities [{:type :trace-automatic-success
                       :req (req true)
                       :value true}]})

(defcard "ONR Credit Subversion"
  {:events [{:event :run-ends
             :optional {:req (req (and (:successful target)
                                       (= [:hq] (:server target))))
                        :prompt "Make the corp lose 3[Credits]?"
                        :yes-ability {:cost [:trash-can]
                                      :msg "force the corp to lose 3[Credits]"
                                      :effect (req (lose-credits state :corp eid (min 3 (:credit corp))))}}}]})

(defcard "ONR Danshi's Second ID"
  {:abilities [{:label "Remove 3 tags"
                :msg "remove 3 tags"
                :cost [:click 1 :trash-can]
                :async true
                :effect (effect (lose-tags :runner eid 3))}]})

(defcard "ONR Databroker"
  {:abilities [{:label "Gain 10 [Credits]"
                :msg "gain 10 [Credits]"
                :cost [:click 1 :agenda-point 1 :trash-can]
                :async true
                :effect (effect (gain-credits eid 10))}]})

(defcard "ONR Death from Above"
  {:events [(successful-run-replace-breach
              {:target-server :remote
               :ability {:cost [:trash-can]
                         :effect (effect (trash-cards eid (:content run-server)))
                         :msg "trash all cards in the server for no cost"
                         :async true}})]})

(defcard "ONR Diplomatic Immunity"
  (let [reset {:silent (req true)
               :effect (effect (update! (assoc-in (get-card state card) [:special :immunity-disabled] false)))}]
  {:corp-abilities [{:label "disable diplomatic immunity"
                     :msg "disable diplomatic immunity"
                     :cost [:agenda-point 1]
                     :effect (effect (update! (assoc-in (get-card state card) [:special :immunity-disabled] true)))}]
   :events [(assoc reset :event :runner-turn-ends)
            (assoc reset :event :corp-turn-ends)
            {:event :pre-damage
             :req (req (and (= target :meat)
                            (not (get-in card [:special :immunity-disabled]))))
             :msg "prevent all meat damage"
             :effect (effect (damage-prevent :meat Integer/MAX_VALUE))}]}))

(defcard "ONR Elena Laskova"
  {:implementation "Manual - use card ability"
   :abilities [{:label "Gain a credit (manual)"
                :msg "gain a credit (manual)"
                :effect (effect (gain-credits eid 1))}]})

(defcard "ONR N.E.T.O."
  (letfn [(shuffle-back [state side cards targets set-aside-eid]
            (doseq [c (get-set-aside state :runner set-aside-eid)]
              (move state :runner
                    c :deck))
            (system-msg state side (str "shuffles " (- (count cards) (count targets)) " cards back into the stack"))
            (shuffle! state side :deck))]
    {:abilities [{:cost [:click 1]
                  :label "look at the top 4 cards of the stack"
                  :async true
                  :effect (req (set-aside-for-me state :runner eid (take 4 (:deck runner)))
                               (let [cards (get-set-aside state :runner eid)
                                     set-aside-eid eid
                                     valid (count (filter #(or (event? %) (resource? %)) cards))
                                     max-amt (min valid
                                                  (total-available-credits state :runner eid card))]
                                 (continue-ability
                                   state side
                                   (if (zero? max-amt)
                                     {:waiting-prompt true
                                      :prompt "You can't do anything"
                                      :choices ["I understand"]
                                      :async true
                                      :effect (req (shuffle-back state side cards [] set-aside-eid)
                                                   (effect-completed state side eid))}
                                     {:waiting-prompt true
                                      :choices {:card #(and (or (event? %)
                                                                (resource? %))
                                                            (some (fn [c] (same-card? % c)) cards))
                                                :max max-amt}
                                      :async true
                                      :effect (req (wait-for (pay state :runner (make-eid state eid) card [:credit (count targets)])
                                                             (system-msg state side (str "pays " (count targets)
                                                                                         "[Credits] and moves "
                                                                                         (str/join ", " (map :title targets))
                                                                                         " to the grip"))
                                                             ;; showing is not revealing lmao
                                                             (doseq [c targets]
                                                               (move state side c :hand))
                                                             (shuffle-back state side cards targets set-aside-eid)
                                                             (effect-completed state side eid)))
                                      :cancel-effect (req (shuffle-back state side cards [] set-aside-eid)
                                                          (effect-completed state side eid))})
                                   card nil)))}]}))

(defcard "ONR Runner Sensei"
  {:abilities [(base-link-abi 2 4)
               (boost-link-abi 2 1)]
   :events [{:event :unsuccessful-trace
             :req (req (same-card? card (:base-link-card target)))
             :async true
             :msg "gain 1[Credit]"
             :effect (effect (gain-credits eid 1))}]})

(defcard "ONR Field Reporter for Ice and Data"
  {:events [{:event :runner-turn-ends
             :async true
             :effect (req (let [ice-summoned (count (filter #(ice? (:card (first %))) (turn-events state side :rez)))]
                            (if (pos? ice-summoned)
                              (continue-ability
                                state side
                                {:msg (msg "gain " ice-summoned "[Credits]")
                                 :effect (effect (gain-credits eid ice-summoned))}
                                card nil)
                              (effect-completed state side eid))))}]})

(defcard "ONR Junkyard BBS"
  {:abilities [{:cost [:credit 1 :click 1]
                :req (req (not (empty? (:discard runner))))
                :label "add the top card of your discard to your hand"
                :msg (msg "move " (:title (last (:discard runner))) " to their hand")
                :effect (effect (move :runner (last (:discard runner)) :hand)
                                (trigger-event :searched-stack nil))}]})

(defcard "ONR Submarine Uplink"
  ;; this forces you to jackout after the current encounter
  {:abilities [{:onr-base-link true
                :req (req run)
                :cost [:credit 0]
                :base-link 4
                :label (str "Base Link " 4)
                :msg (str "set their Base Link to " 4)
                :effect (req (set-base-link state 4)
                             (register-events
                               state side card
                              [{:event :end-of-encounter
                                :unregister-once-resolved true
                                :async true
                                :duration :end-of-run
                                :effect (req (jack-out state side eid))}]))}
               (assoc (boost-link-abi 2 1)  :req (req run))
               ]})

(defcard "ONR Swiss Bank Account"
  {:abilities [{:label "Gain 2 [Credits]"
                :msg "gain 2 [Credits]"
                :cost [:trash-can]
                :async true
                :effect (effect (gain-credits eid 2))}
               {:label "Gain 6 [Credits]"
                :msg "gain 6 [Credits]"
                :cost [:trash-can :credit 3]
                :async true
                :effect (effect (gain-credits eid 6))}]})

(defcard "ONR The Shell Traders"
  (let [shellpilled
        (fn [card]
          (and (pos? (get-counters card :shell))
               (= (:zone card) [:set-aside])))
        shellpilled-cards
        (fn [runner]
          (filter shellpilled (:set-aside runner)))
         remove-counter
         {:async true
          :req (req (not
                      (empty? (shellpilled-cards runner))))
          :msg (msg "remove 1 shell counter from " (:title target))
          :choices {:req (req (shellpilled-cards runner))}
          :effect (req (do (add-counter state side target :shell -1)
                           (if (pos? (get-counters (get-card state target) :shell))
                             (effect-completed state side eid)
                             (runner-install state side eid (dissoc target :counter) {:ignore-install-cost true}))))}]
     {:flags {:drip-economy true}
      :abilities [{:async true
                   :label "Set aside a program or piece of hardware"
                   :cost [:click 1]
                   :keep-menu-open :while-clicks-left
                   :prompt "Choose a program or piece of hardware in the grip"
                   :choices {:card #(and (or (program? %)
                                             (hardware? %))
                                         (in-hand? %)
                                         (runner? %))}
                   :effect (req (if (not (pos? (:cost target)))
                                  (runner-install state side (assoc eid :source card :source-type :runner-install) target nil)
                                  (do (set-aside state side eid [(assoc target :counter {:shell (:cost target)})])
                                      (effect-completed state side eid))))
                   :msg (msg "set aside " (:title target))}
                  (assoc remove-counter
                         :label "Remove 1 shell counter from a card"
                         :cost [:credit 1])
                  {:async true
                   :label "Remove a shell counter from a card"
                   :choices {:req (req (shellpilled-cards runner))}
                   :req (req (seq (shellpilled-cards runner)))
                   :effect (effect
                             (continue-ability
                               (let [paydowntarget target
                                     num-counters (get-counters (get-card state paydowntarget) :shell)]
                                 {:async true
                                  :prompt "How many shell counters do you want to remove?"
                                  :choices {:number (req (min num-counters
                                                              (total-available-credits state :runner eid card)))}
                                  :effect (req (wait-for
                                                 (pay state :runner (make-eid state eid) card [:credit target])
                                                 (if-let [payment-str (:msg async-result)]
                                                   (do (system-msg state side
                                                                   (str (build-spend-msg payment-str "use") (:title card)
                                                                        " to remove " (quantify target "shll counter")
                                                                        " from " (:title paydowntarget)))
                                                       (if (= num-counters target)
                                                         (runner-install state side (assoc eid :source card :source-type :runner-install) (dissoc paydowntarget :counter) {:ignore-install-cost true})
                                                         (do (add-counter state side paydowntarget :shell (- target))
                                                             (effect-completed state side eid))))
                                                   (effect-completed state side eid))))})
                               card nil))}]
      :implementation "this can put your cards in limbo. This is the intended design"
      :events [(assoc remove-counter :event :runner-turn-begins)]}))
