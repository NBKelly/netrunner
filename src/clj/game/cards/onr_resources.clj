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
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-icon remove-icon]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [active-encounter? bypass-ice can-run-server? get-runnable-zones continue
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
   [game.core.winning :refer [check-win-by-agenda win]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [game.core.onr-trace :refer [boost-link set-base-link cancel-successful-trace]]
   [jinteki.utils :refer :all]
   [jinteki.validator :refer [legal?]]
   [medley.core :refer [find-first]]
   [game.core.onr-utils :refer :all]
   ))

;; card implementations

(defcard "ONR Access through Alpha"
  {:abilities [(base-link-abi 1 9)]})

(defcard "ONR Access to Arasaka"
  {:abilities [(base-link-abi 2 4)
               (boost-link-abi 2 1)]})

(defcard "ONR Access to Kiribati"
  {:abilities [(base-link-abi 1 1)
               (boost-link-abi 1 1)]})

(defcard "ONR Airport Locker"
  {:abilities [{:req (req (not (install-locked? state side)))
                :label "Install a program from the stack"
                :cost [:trash-can :credit 5]
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

(defcard "ONR Aujourd'Oui"
    (letfn [(shuffle-back [state side cards targets set-aside-eid]
            (doseq [c (get-set-aside state :runner set-aside-eid)]
              (move state :runner
                    c :deck))
            (system-msg state side (str "shuffles " (- (count cards) (count targets)) " cards back into the stack"))
            (shuffle! state side :deck))]
    {:abilities [{:cost [:click 1]
                  :label "look at the top 5 cards of the stack"
                  :async true
                  :effect (req (set-aside-for-me state :runner eid (take 5 (:deck runner)))
                               (let [cards (get-set-aside state :runner eid)
                                     set-aside-eid eid
                                     valid (count (filter #(or (program? %)) cards))
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
                                      :choices {:card #(and (or (program? %))
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

(defcard "ONR Executive File Clerk"
  {:abilities [{:cost [:credit 2 :trash-can]
                :label "look at the cards in HQ"
                :msg (msg "reveal " (enumerate-str (sort (map :title (:hand corp)))) " from HQ")
                :async true
                :effect (effect (reveal eid (:hand corp)))}]})

(defcard "ONR Expendable Family Member"
  {:interactions {:prevent [{:type #{:tag}
                             :req (req true)}]}
   :abilities [{:async true
                :label "avoid 1 tag"
                :cost [:trash-can :credit 1]
                :msg "avoid 1 tag"
                :effect (effect (tag-prevent :runner eid 1))}]})

(defcard "ONR Fall Guy"
  {:interactions {:prevent [{:type #{:tag}
                             :req (req true)}]}
   :abilities [{:async true
                :cost [:trash-can]
                :msg "avoid 1 tag"
                :effect (effect (tag-prevent :runner eid 1))}]})

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

(defcard "ONR Floating Runner BBS"
  {:events [{:event :runner-turn-begins
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "ONR Get Ready to Rumble"
  {:events [{:event :damage
             :optional {:waiting-prompt true
                        :req (req (and
                                    (pos? (:amount context))
                                    (= :corp (:side context))
                                    (= :meat (:damage-type context))))
                        :prompt "Force the corp to discard 2 at random?"
                        :yes-ability {:cost [:trash-can]
                                      :msg "force the Corp to discard 2 cards at random"
                                      :async true
                                      :effect (effect (trash-cards :corp eid (take 2 (shuffle (:hand corp))) {:cause-card card}))}}}]})

(defcard "ONR HQ Mole"
  {:events [{:event :breach-server
             :async true
             :req (req (#{:hq} target))
             :optional {:req (req (can-pay? state side (assoc eid :source card :source-type :ability) card nil [:credit 4]))
                        :prompt (msg "Pay 4[Credits]: Access 2 additional cards from HQ?")
                        :yes-ability {:cost [:credit 4 :trash-can]
                                      :msg "access 2 additional cards from HQ"
                                      :effect (effect (access-bonus :hq 2))}}}]})

(defcard "ONR Hell's Run"
  {:recurring 1
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(defcard "ONR Junkyard BBS"
  {:abilities [{:cost [:credit 1 :click 1]
                :req (req (not (empty? (:discard runner))))
                :label "add the top card of your discard to your hand"
                :msg (msg "move " (:title (last (:discard runner))) " to their hand")
                :effect (effect (move :runner (last (:discard runner)) :hand)
                                (trigger-event :searched-stack nil))}]})

(defcard "ONR Karl de Veres, Corporate Stooge"
  {:events [{:event :successful-run
             :silent (req true)
             :async true
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits eid 1))}]})

(defcard "ONR Leland, Corporate Bodyguard"
  {:interactions {:prevent [{:type #{:tag :meat}
                             :req (req true)}]}
   :abilities [{:label "Prevent 1 meat damage"
                :msg "prevent 1 meat damage"
                :cost [:credit 1]
                :effect (effect (damage-prevent :meat 1))}
               {:async true
                :cost [:trash-can]
                :msg "avoid 1 tag"
                :effect (effect (tag-prevent :runner eid 1))}]})

(defcard "ONR Liberated Savings Account"
  {:abilities [{:label "Gain 11 [Credits]"
                :msg "gain 11 [Credits]"
                :cost [:trash-can :credit 7]
                :async true
                :effect (effect (gain-credits eid 11))}]})

(defcard "ONR Loan from Chiba"
  {:flags {:runner-phase-12 (req (some #(card-flag? % :drip-economy true) (all-active-installed state :runner)))}
   :on-install {:async true
                :msg "gain 12 [Credits]"
                :effect (effect (gain-credits eid 12))}
   :events [{:event :runner-turn-begins
             :msg (msg "lose " (if (zero? (get-in @state [:runner :credit]))
                                 "0 [Credits] (runner has no credits to lose)"
                                 "1 [Credits]"))
             :once :per-turn
             :async true
             :effect (effect (lose-credits eid 1))}
            {:event :runner-turn-ends
             :interactive (get-autoresolve :auto-fire (complement never?))
             :silent (get-autoresolve :auto-fire never?)
             :optional {:waiting-prompt "Runner to decide to trash ONR Loan from Chiba"
                        :prompt "Trash ONR Loan from Chiba?"
                        :player :runner
                        :autoresolve (get-autoresolve :auto-fire)
                        :yes-ability {:msg "trash ONR Loan from Chiba"
                                      :async true
                                      :effect (effect (trash eid card {:cause :runner-ability}))}
                        :no-ability {:effect (effect (system-msg "chooses not to trash ONR Loan from Chiba"))}}}]
   :abilities [;; set autoresolve
               (set-autoresolve :auto-fire "ONR Loan from Chiba")
               ;; manually lose 1 credit (for ordering)
               {:label "Lose 1 [Credits] (start of turn)"
                :msg (msg (if (zero? (get-in @state [:runner :credit]))
                            "lose 0 [Credits] (runner has no credits to lose)"
                            "lose 1 [Credits]"))
                :req (req (:runner-phase-12 @state))
                :once :per-turn
                :async true
                :effect (effect (lose-credits eid 1))}]
   ;; when chiba leaves play, runner pays 10 or loses
   :uninstall (effect
                (continue-ability
                  {:player :runner
                   :async true
                   :waiting-prompt "Runner to choose an option"
                   :prompt "Pay 10 to avoid losing the game?"
                   :effect (req
                             (clear-wait-prompt state :corp)
                             (if (= target "Pay 10")
                               (wait-for (pay state side (make-eid state eid) card [:credit 10])
                                         (system-msg state side
                                                     (str "pays 10 [Credits] to avoid losing the game due to ONR Loan from Chiba"))
                                         (complete-with-result state side eid card))
                               (do
                                 (system-msg state side "loses the game after being unable to repay ONR Loan from Chiba")
                                 (win state :corp "\"default\"")
                                 (complete-with-result state side eid card))))
                   ;; only offer payment choice if it can be made
                   :choices (req (if (can-pay? state :runner (assoc eid :source card
                                                                    :source-type :ability)
                                               card (:title target) [:credit 10])
                                   ["Pay 10" "Lose the game"]
                                   ["Lose the game"]))}
                  card nil))})

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

(defcard "ONR Nomad Allies"
  {:interactions {:prevent [{:type #{:tag}
                             :req (req true)}]}
   :abilities [{:label "Remove 1 tag"
                :msg "remove 1 tags"
                :cost [:click 1 :credit 1]
                :async true
                :effect (effect (lose-tags :runner eid 1))}
               {:async true
                :cost [:trash-can]
                :msg "avoid 1 tag"
                :effect (effect (tag-prevent :runner eid 1))}]})

(defcard "ONR Precision Bribery"
  {:implementation "No new remotes installs is manual"
   :static-abilities [{:type :prevent-new-remote
                       :req (req true)
                       :value true}]
   :corp-abilities [{:label "Trash Precision Bribery"
                     :async true
                     :cost [:click 1 :credit 4]
                     :req (req (= :corp side))
                     :effect (effect (system-msg :corp "spends [Click] and 4 [Credits] to trash Precision Bribery")
                                     (trash :corp eid card {:cause-card card}))}]})

(defcard "ONR Preying Mantis"
  (let [ability {:msg "gain [Click]"
                 :once :per-turn
                 :label "Gain [Click] (start of turn)"
                 :effect (effect (gain-clicks 1)
                                 (update! (assoc-in card [:special :joshua-b] true)))}]
    {:flags {:runner-phase-12 (req true)}
     :events [{:event :runner-turn-begins
               :optional {:prompt "Gain [Click]?"
                          :once :per-turn
                          :yes-ability ability}}
              {:event :runner-turn-ends
               :interactive (req true)
               :req (req (get-in card [:special :joshua-b]))
               :async true
               :effect (effect
                         (update! (assoc-in card [:special :joshua-b] false))
                         (damage eid :brain 1 {:unpreventable true}))
               :msg "suffer 1 brain damage"}]
     :abilities [ability]}))

(defcard "ONR Quest for Cattekin"
  {:events [{:event :runner-turn-begins
             :async true
             :effect (req (let [di (dice-roll)]
                            (continue-ability
                              state side
                              (cond
                                (= di 6)
                                {:msg (msg "roll a 6(1d6), trashes " (:title card) " and will gain an additional action for the remainder of the game")
                                 :async true
                                 :effect (req (wait-for (trash state side (make-eid state eid) card {:cause-card card})
                                                        (gain-clicks state :runner 1)
                                                        (gain state :runner :click-per-turn 1)
                                                        (effect-completed state side eid)))}
                                (= di 1)
                                {:msg (msg "roll a 1(1d6), and suffers 1 brain damage")
                                 :async true
                                 :effect (effect (damage eid :brain 1))}
                                (= di 2)
                                {:msg (msg "roll a 2(1d6), and suffers 1 net damage")
                                 :async true
                                 :effect (effect (damage eid :net 1))}
                                :else
                                {:msg (msg "roll a " di "(1d6), and continues questing")
                                 :async true})
                              card nil)))}]})

(defcard "ONR R&D Mole"
  {:events [{:event :breach-server
             :async true
             :req (req (#{:rd} target))
             :optional {:req (req (can-pay? state side (assoc eid :source card :source-type :ability) card nil [:credit 4]))
                        :prompt (msg "Pay 4[Credits]: Access 2 additional cards from R&D?")
                        :yes-ability {:cost [:credit 4 :trash-can]
                                      :msg "access 2 additional cards from R&D"
                                      :effect (effect (access-bonus :rd 2))}}}]})

(defcard "ONR Restrictive Net Zoning"
  {:on-install {:prompt "Choose a server"
                :choices (req servers)
                :effect (effect (update! (assoc card :card-target target)))}
   :static-abilities [{:type :install-cost
                       :req (req (let [serv (:server (second targets))]
                                   (and (= serv (:card-target card))
                                        (ice? target))))
                       :value 2}]})

(defcard "ONR Rigged Investments"
  (let [ability {:once :per-turn
                 :label "Take 1 [Credits] (start of turn)"
                 :req (req (:runner-phase-12 @state))
                 :msg (msg "gain " (min 1 (get-counters card :credit)) " [Credits]")
                 :async true
                 :effect (req (let [credits (min 1 (get-counters card :credit))]
                                (add-counter state side card :credit (- credits))
                                (gain-credits state :runner eid credits)))}]
    {:data {:counter {:credit 12}}
     :flags {:drip-economy true}
     :abilities [ability]
     :events [(assoc ability :event :runner-turn-begins)
              (trash-on-empty :credit)]}))

(defcard "ONR Ronin Around"
  (letfn [(shuffle-back [state side cards targets set-aside-eid]
            (doseq [c (get-set-aside state :runner set-aside-eid)]
              (move state :runner
                    c :deck))
            (system-msg state side (str "shuffles " (- (count cards) (count targets)) " cards back into the stack"))
            (shuffle! state side :deck))]
    {:abilities [{:cost [:click 1]
                  :label "look at the top 5 cards of the stack"
                  :async true
                  :effect (req (set-aside-for-me state :runner eid (take 5 (:deck runner)))
                               (let [cards (get-set-aside state :runner eid)
                                     set-aside-eid eid
                                     valid (count (filter #(or (hardware? %)) cards))
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
                                      :choices {:card #(and (or (hardware? %))
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
                                   card nil)))}
                 {:cost [:click 1 :credit 2]
                  :choices {:card #(and (corp? %)
                                        (installed? %))}
                  :effect (effect (expose eid target))
                  :msg "expose 1 card"}]}))

(defcard "ONR Runner Sensei"
  {:abilities [(base-link-abi 2 4)
               (boost-link-abi 2 1)]
   :events [{:event :unsuccessful-trace
             :req (req (same-card? card (:base-link-card target)))
             :async true
             :msg "gain 1[Credit]"
             :effect (effect (gain-credits eid 1))}]})

(defcard "ONR Sandbox Dig"
  {:abilities [{:cost [:trash-can :credit 3]
                :label "look at the top 3 cards of R&D"
                :msg "look at the top 3 cards of R&D"
                :async true
                :effect (req (continue-ability
                               state side
                               {:waiting-prompt true
                                :prompt (msg "The top 3 cards of R&D are " (str/join ", " (map :title (take 3 (:deck corp)))))
                                :choices ["I understand"]}
                               card nil))}]})

(defcard "ONR Short-Term Contract"
  {:data {:counter {:credit 12}}
   :events [(trash-on-empty :credit)]
   :abilities [{:cost [:click 1]
                :keep-menu-open :while-clicks-left
                :label "gain 2 [Credits]"
                :msg (msg "gain " (min 2 (get-counters card :credit)) " [Credits]")
                :async true
                :effect (req (let [credits (min 2 (get-counters card :credit))]
                               (add-counter state side card :credit (- credits))
                               (gain-credits state :runner eid credits)))}]})

(defcard "ONR Silicon Saloon Franchise"
  {:abilities [{:cost [:click 1]
                :msg "gain 1 [Credits] and draw 1 card"
                :async true
                :effect (req (wait-for (gain-credits state side 1)
                                       (draw state side eid 1)))}]})


(defcard "ONR Simulacrum"
  {:abilities [{:label (str "Bypass AP being encountered")
                :cost [:trash-can]
                :req (req (and (active-encounter? state)
                               (has-subtype? current-ice "AP")))
                :msg (msg "bypass " (:title current-ice))
                :effect (req (bypass-ice state)
                             (continue state :runner nil))}]})

(defcard "ONR Smith's Pawnshop"
  (let [ability {:async true
                 :label "trash a card to gain 2 [Credits]"
                 :once :per-turn
                 :req (req (>= (count (all-installed state :runner)) 2))
                 :choices {:not-self true
                           :req (req (and (runner? target)
                                          (installed? target)))}
                 :msg (msg "trash " (:title target) " and gain 2 [Credits]")
                 :cancel-effect (effect (system-msg (str "declines to use " (:title card)))
                                        (effect-completed eid))
                 :effect (req (wait-for (trash state side target {:unpreventable true :cause-card card})
                                        (gain-credits state side eid 2)))}]
    {:flags {:runner-phase-12 (req (>= (count (all-installed state :runner)) 2))}
     :events [(assoc ability
                     :event :runner-turn-begins
                     :interactive (req true))]
     :abilities [ability]}))

(defcard "ONR Streetware Distributor"
  (let [start-of-turn-ability {:once :per-turn
                               :label "Take 1 [Credits] (start of turn)"
                               :req (req (and (:runner-phase-12 @state)
                                              (pos? (get-counters card :credit))))
                               :msg "take 1 [Credits]"
                               :async true
                               :effect (req (add-counter state side card :credit -1)
                                         (gain-credits state side eid 1))}]
    {:flags {:drip-economy (req (pos? (get-counters card :credit)))}
     :abilities [{:cost [:click 1]
                  :msg "place 3 [Credits]"
                  :req (req (not (:runner-phase-12 @state)))
                  :effect (req (add-counter state side card :credit 3))}
                 start-of-turn-ability]
     :events [(assoc start-of-turn-ability :event :runner-turn-begins)]}))

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

(defcard "ONR Technician Lover"
  {:abilities [{:cost [:click 1]
                :label "look at the top card of R&D"
                :msg "look at the top card of R&D"
                :async true
                :effect (req (continue-ability
                               state side
                               {:waiting-prompt true
                                :prompt (msg "The top card of R&D is " (:title (first (:deck corp))))
                                :choices ["I understand"]}
                               card nil))}]})

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

(defcard "The Short Circuit"
  {:abilities [{:label "Search the stack for a program and add it to the grip"
                :prompt "Choose a program"
                :msg (msg "add " (:title target) " from the stack to the grip")
                :choices (req (cancellable (filter #(and (program? %))
                                                   (:deck runner)) :sorted))
                :cost [:click 1 :credit 1]
                :keep-menu-open :while-clicks-left
                :effect (effect (trigger-event :searched-stack nil)
                                (shuffle! :deck)
                                (move target :hand))}]})

(defcard "ONR The Springboard"
  {:events [{:event :trace-revealed
             :optional {:prompt "Gain 1 link?"
                        :waiting-prompt true
                        :yes-ability (boost-link-abi 1 1)}}]})

(defcard "ONR Time to Collect"
  {:interactions {:prevent [{:type #{:trash-resource}
                             :req (req (= :corp (:active-player @state)))}]}
   :abilities [{:label "Prevent another installed resource from being trashed"
                :cost [:trash-can]
                :effect (effect (trash-prevent :resource 1))}]})

(defcard "ONR Top Runner's Conference"
  {:events [{:event :run
             :msg "trash itself"
             :async true
             :effect (effect (trash eid card))}
            {:event :runner-turn-begins
             :async true
             :effect (effect (gain-credits eid 2))}]})

(defcard "ONR Trauma Team [TM]"
  {:on-install {:async true
                :effect (req (add-counter state :runner card :trauma 2)
                             (effect-completed state :runner eid))}
   :interactions {:prevent [{:type #{:meat}
                             :req (req true)}]}
   :abilities [{:label "Add a trauma counter"
                :cost [:click 1]
                :msg "place a trauma counter on itself"
                :effect (req (add-counter state :runner card :trauma 1))}
               {:label "Prevent 1 meat damage"
                :cost [:trauma 1]
                :effect (req (damage-prevent state side :meat 1))}]})

(defcard "ONR Umbrella Policy"
  {:interactions {:prevent [{:type #{:trash-program :trash-hardware}
                             :req (req true)}]}
   :abilities [{:cost [:trash-can]
                :label "prevent a program trash"
                :effect (effect (trash-prevent :program 1)
                                (trash-prevent :hardware 1))}]})

(defcard "ONR Wired Switchboard"
  {:events [{:event :trace-revealed
             :optional {:prompt "Gain 3 link?"
                        :waiting-prompt true
                        :yes-ability (boost-link-abi [:trash-can] 3)}}]})

(defcard "ONR Wilson, Weeflerunner Apprentice"
  {:implementation "Credit spent limit not enforced. You can forgo this action."
   :interactions {:prevent [{:type #{:meat}
                             :req (req true)}]}
   :abilities [{:label "Gain an action for making a run"
                :once :per-turn
                :async true
                :effect (req (wait-for (gain-or-forgo state side (make-eid state eid))
                                       (continue-ability
                                         state side
                                         (if (= "Forgo Click" async-result)
                                           {:msg "forgo an action"}
                                           {:msg "gain and action, which must be used to make a run"})
                                         card nil)))}
               {:async true
                :label "Avoid a tag"
                :cost [:trash-can]
                :msg "avoid a tag"
                :effect (effect (tag-prevent :runner eid 1))}
               {:label "Prevent any amount of meat damage"
                :msg "prevent all meat damage"
                :cost [:trash-can]
                :effect (effect (damage-prevent :meat Integer/MAX_VALUE))}]})
