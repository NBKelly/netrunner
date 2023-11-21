(ns game.cards.onr-operations
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.access :refer [access-card steal-cost-bonus]]
   [game.core.actions :refer [advance score]]
   [game.core.bad-publicity :refer [gain-bad-publicity lose-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed
                            get-all-installed get-remote-names get-remotes
                            installable-servers server->zone]]
   [game.core.card :refer [active? agenda? asset? can-be-advanced? card-index corp? corp-installable-type?
                           event? facedown? faceup? get-advancement-requirement
                           get-card get-counters get-title get-zone hardware? has-subtype? ice? identity?
                           in-discard? in-hand? installed? is-type? operation? program? resource?
                           rezzed? runner? upgrade?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.cost-fns :refer [play-cost trash-cost]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage damage-bonus]]
   [game.core.def-helpers :refer [corp-recur defcard do-brain-damage
                                  reorder-choice get-x-fn]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect gather-effects]]
   [game.core.eid :refer [effect-completed make-eid make-result]]
   [game.core.engine :refer [pay register-events resolve-ability gather-events]]
   [game.core.events :refer [first-event? last-turn? no-event? not-last-turn?
                             turn-events]]
   [game.core.flags :refer [can-score? clear-persistent-flag! in-corp-scored?
                            in-runner-scored? is-scored? prevent-jack-out
                            register-persistent-flag! register-turn-flag! when-scored? zone-locked?]]
   [game.core.gaining :refer [gain-clicks gain-credits gain-debt lose-clicks lose-debt
                              lose-credits]]
   [game.core.hand-size :refer [runner-hand-size+]]
   [game.core.ice :refer [add-extra-sub! remove-extra-subs! update-all-ice]]
   [game.core.identities :refer [disable-identity enable-identity]]
   [game.core.initializing :refer [ability-init card-init]]
   [game.core.installing :refer [corp-install corp-install-list
                                 corp-install-msg install-as-condition-counter]]
   [game.core.memory :refer [mu+ update-mu]]
   [game.core.moving :refer [as-agenda mill move swap-agendas swap-ice trash
                             trash-cards]]
   [game.core.payment :refer [can-pay? cost-target]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable clear-wait-prompt show-wait-prompt]]
   [game.core.props :refer [add-counter add-prop]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [end-run make-run]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [is-remote? remote->name zone->name]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck
                                shuffle-into-rd-effect]]
   [game.core.tags :refer [gain-tags]]
   [game.core.threat :refer [threat-level]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [number-of-virus-counters]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]

   [game.core.onr-utils :refer [base-link-abi boost-link-abi dice-roll
                                deep-merge generic-prevent-damage
                                handle-if-unique
                                onr-trace-tag onr-trace-ability
                                ]]
   ))

(defn- give-tags
  "Basic give runner n tags subroutine."
  [n]
  {:label (str "Give the Runner " (quantify n "tag"))
   :msg (str "give the Runner " (quantify n "tag"))
   :async true
   :effect (effect (gain-tags :corp eid n))})


(defn- handle-debt-collections
  ([debt]
   (letfn [(debt-abi []
             {:event :corp-gain
              :async true
              :duration :true
              :unregister-once-resolved true
              :req (req (and (= :credit (first target))
                             (pos? (second target)))) ;; only if we actually gain creds!
              :msg (msg
                     (let [creds-owed (:debt corp)
                           creds-gained (second target)
                           siphoned (if (>= creds-owed creds-gained) creds-gained creds-owed)
                           remainder (- creds-owed siphoned)]
                       (str "forfeit " siphoned "[credits] to cover their debts"
                            (when (pos? remainder) (str " (they still owe " remainder "[credits])")))))
              :effect (req (let [creds-owed (:debt corp)
                                 creds-gained (second target)
                                 siphoned (if (>= creds-owed creds-gained) creds-gained creds-owed)
                                 remainder (- creds-owed siphoned)]
                             (wait-for (lose-credits state side (make-eid state eid) siphoned)
                                       (when (pos? remainder)
                                         (register-events
                                           state side card
                                           [(debt-abi)]))
                                       (lose-debt state side eid siphoned))))})]
     {:msg (msg "incur a debt of " debt "[Credits]"(when (pos? (:debt corp)) (str " (they now owe " (+ debt (:debt corp)) "[Credits])")))
      :async true
      :effect (req
                (when (zero? (:debt corp))
                  (register-events
                    state side (get-card state card)
                    [(debt-abi)]))
                (gain-debt state side eid debt))})))

;; card implementations

(defcard "ONR Accounts Receivable"
  {:on-play
   {:msg "gain 9 [Credits]"
    :async true
    :effect (effect (gain-credits eid 9))}})

(defcard "ONR Annual Reviews"
  {:on-play
   {:msg "draw 3 cards"
    :async true
    :effect (effect (draw eid 3))}})

(defcard "ONR Audit of Call Records"
  {:on-play
   {:req (req (< 1 (count (last-turn? state :runner :made-run))))
    :async true
    :effect (effect
              (continue-ability (onr-trace-tag 5) card nil))}})

(defcard "ONR Badtimes"
  {:on-play {:req (req tagged)
             :msg "force the Runner to lose 2[mu] until the end of the turn"
             :effect (req (register-lingering-effect
                            state :corp card
                            (assoc (mu+ -2) :duration :end-of-turn))
                          (update-mu state))}})

(defcard "ONR Chance Observation"
  {:on-play
   {:req (req (< 0 (count (last-turn? state :runner :made-run))))
    :async true
    :effect (effect
              (continue-ability (onr-trace-tag 5) card nil))}})

(defcard "ONR Closed Accounts"
  {:on-play
   {:req (req tagged)
    :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]")
    :async true
    :effect (effect (lose-credits :runner eid :all))}})

(defcard "ONR Corporate Detective Agency"
  {:on-play
   {:req (req tagged)
    :msg (msg "trash " (enumerate-str (map :title (sort-by :title targets))))
    :choices {:max 2
              :card #(and (installed? %)
                          (resource? %))}
    :async true
    :effect (effect (trash-cards :runner eid targets {:cause-card card}))}})

(defcard "ONR Corporate Guard(R) Temps"
  (letfn [(gain-click-event [x]
            {:event :corp-turn-begins
             :duration true
             :req (req true)
             :unregister-once-resolved true
             :async true
             :msg (msg "gain an action" (when (> x 1) (str " (" (dec x) " remain)")))
             :effect (req (gain-clicks state :corp 1)
                          (when (> x 1)
                            (register-events
                              state side card
                              [(gain-click-event (dec x))]))
                          (effect-completed state side eid))})]
    {:on-play {:req (req (> (:credit corp) 1))
               :prompt (msg "Pay 2X[credit] to gain actions for the next X turns and forfeit the next X[credit] you gain")
               :choices {:number (req (int (/ (:credit corp) 2)))}
               :async true
               :effect (req
                         (let [x target]
                           (continue-ability
                             state side
                             (when (pos? x)
                               {:cost [:credit (* 2 x)]
                                :msg (msg "gain an additional action for the next " x " turns, and forfeit the next " x " credits they gain")
                                :async true
                                :effect (req
                                          (register-events
                                            state side card
                                            [(gain-click-event x)])
                                          (continue-ability
                                            state side
                                            (handle-debt-collections x)
                                            card nil))})
                             card nil)))}}))

(defcard "ONR Corporate Shuffle"
  {:on-play
   {:async true
    :effect (req (wait-for
                   (draw state side 5)
                   (system-msg state side (str "uses " (:title card) " to draw "
                                               (quantify (count async-result) "card")))
                   (continue-ability
                     state side
                     {:prompt "Choose a cards in HQ to shuffle into R&D"
                      :choices {:max 1
                                :all true
                                :card #(and (corp? %)
                                            (in-hand? %))}
                      :msg (msg "shuffle " (quantify (count targets) "card") " from HQ into R&D")
                      :effect (req (doseq [c targets]
                                     (move state side c :deck))
                                   (shuffle! state side :deck))}
                     card nil)))}})

(defcard "ONR Credit Consolidation"
  {:on-play
   {:msg "gain 15 [Credits]"
    :async true
    :effect (effect (gain-credits eid 15))}})

(defcard "ONR Data Sifters"
  {:implementation "Doesn't enforce asset (node) only"
   :on-play
   {:req (req (last-turn? state :runner :trashed-card))
    :msg "give the runner a tag"
    :async true
    :effect (req
              (system-msg state side (last-turn? state :runner :trashed-card))
              (gain-tags state :corp eid 1))}})

(defcard "ONR Datapool (R) by Zetatech"
  {:on-play
   {:req (req tagged)
    :msg "give the Runner 2 tags"
    :async true
    :effect (effect (gain-tags :corp eid 2))}})

(defcard "ONR Day Shift"
  {:on-play
   {:msg "gain 1 [Credits] and draw 2 cards"
    :async true
    :effect (req (wait-for (gain-credits state side 1)
                           (draw state side eid 2)))}})

(defcard "ONR Edgerunner, Inc., Temps"
  {:implementation "You may forgo these clicks to purge viruses. Manually adjust clicks you don't use."
   :on-play
   {:msg "gain [Click][Click][Click] for install actions"
    :effect (effect (gain-clicks 3))}})

(defcard "ONR Efficiency Experts"
  {:on-play
   {:msg "gain 3 [Credits]"
    :async true
    :effect (effect (gain-credits eid 3))}})

(defcard "ONR Emergency Rig"
  (letfn [(kludge-event []
            {:event :corp-turn-begins
             :unregister-once-resolved true
             :ability-name "Kludge Counters"
             :async true
             :effect (req (let [cards-with-counters (filter #(pos? (get-counters % :kludge))
                                                            (all-installed state :corp))]
                            (when-not (empty? cards-with-counters)
                              (system-msg state side "removes a Kludge counter from each piece of ice (ONR Emergency Rig)")
                              (register-events
                                state side (:identity corp)
                                [(kludge-event)]))
                            (doseq [c cards-with-counters]
                              (let [new-counters (dec (get-counters c :kludge))]
                                (add-counter state side c :kludge -1)
                                (when (zero? new-counters)
                                  (do
                                    ;;(system-msg state :corp
                                    ;;            (str "derezzes " (card-str state c) " when removing Kludge counters"))
                                    (derez state :corp (get-card state c))))))
                            (effect-completed state side eid)))})]
    {:on-play
     {:choices :credit
      :prompt "Pay how much?"
      :async true
      :effect (req
                (let [ccost target]
                  (if (zero? ccost)
                    (effect-completed state side eid)
                    (continue-ability
                      state side
                      {:prompt "choose an ice to rez"
                       :choices {:card #(and (not (rezzed? %))
                                             (ice? %))}
                       :async true
                       :msg (msg "rez " (card-str state target) " at no cost and place " ccost " Kludge counters on it")
                       :effect (req (wait-for (rez state side target {:ignore-cost :all-costs})
                                              (handle-if-unique state side (:identity corp) (kludge-event))
                                              (add-counter state side (get-card state target) :kludge ccost)
                                              (effect-completed state side eid)))}
                      card nil))))}}))

(defcard "ONR Falsified-Transactions Expert"
  {:on-play
   {:prompt "Choose an installed card you can advance"
    :req (req (let [advanceable (some can-be-advanced? (get-all-installed state))
                    num-installed (count (get-all-installed state))]
                 (and advanceable
                      (> num-installed 1))))
    :choices {:card #(and (can-be-advanced? %)
                          (installed? %))}
    :async true
    :effect (effect
              (continue-ability
                (let [card-to-advance target]
                  {:async true
                   :prompt "Choose another installed card"
                   :choices {:card #(and (not (same-card? card-to-advance %))
                                         (installed? %))}
                   :effect (effect
                             (continue-ability
                               (let [source target]
                                 {:prompt "How many advancement counters do you want to move?"
                                  :choices (take (inc (get-counters source :advancement)) ["0" "1" "2" "3"])
                                  :msg (msg "move " target " advancement counters from "
                                            (card-str state source) " to " (card-str state card-to-advance))
                                  :effect (effect (add-prop :corp card-to-advance :advance-counter (str->int target) {:placed true})
                                                  (add-prop :corp source :advance-counter (- (str->int target)) {:placed true}))})
                               card nil))})
                card nil))}})

(defcard "ONR Management Shake-Up"
  (letfn [(ability [x]
            {:prompt (msg "Choose an installed card to place advancement counters on (" x " remaining)")
             :async true
             :waiting-prompt true
             :choices {:card #(and (corp? %)
                                   (can-be-advanced? %)
                                   (installed? %))}
             :msg (msg "place 1 advancement counter on " (card-str state target))
             :effect (req (wait-for (add-prop state side target :advance-counter 1 {:placed true})
                                    (if (> x 1)
                                      (continue-ability state side (ability (dec x)) card nil)
                                      (effect-completed state side eid))))})]
    {:on-play
     {:async true
      :effect (effect (continue-ability (ability 3) card nil))}}))

;; this is a nearprint of midseasons - a good test for the trace system
(defcard "ONR Manhunt"
  {:on-play
   {:onr-trace
    {:req (req (last-turn? state :runner :made-run))
     :max-strength 6
     :only-tags true
     :label "Trace 6 - Give the Runner X tags"
     :successful {;:msg "give the Runner X tags"
                  :async true
                  :effect (effect (system-msg
                                    (str "gives the Runner " (quantify (- target (second targets)) "tag")))
                                  (gain-tags eid (- target (second targets))))}}}})

(defcard "ONR Netwatch Credit Voucher"
  {:on-play
   {:req (req tagged)
    :msg "give the Runner a tag and gain [Credit]"
    :async true
    :effect (req (wait-for (gain-tags state :corp (make-eid state eid) 1)
                           (gain-credits state side eid 1)))}})

(defcard "ONR New Blood"
  (letfn [(sun []
            {:prompt "Choose 2 pieces of ice to swap"
             :choices {:card #(and (installed? %)
                                   (ice? %))
                       :max 2}
             :async true
             :effect (req (if (= (count targets) 2)
                            (do (swap-ice state side (first targets) (second targets))
                                ;;(system-msg state side
                                ;;            (str "uses " (:title card) " to swap "
                                ;;                 (card-str state (first targets))
                                ;;                 " with "
                                ;;                 (card-str state (second targets))))
                                (continue-ability state side (sun) card nil))
                            (do (system-msg state side "has finished rearranging ice")
                                (effect-completed state side eid))))})]
    {:implementation "Exposed cards are already concealed - I might add a public info system a bit later - for now I'm just not printing swaps at all"
     :on-play
     {:prompt "Choose a server"
      :choices (req servers)
      :msg (msg "rearrange ice")
      :async true
      :effect (req (continue-ability state side (sun) card nil))}}))

(defcard "ONR Night Shift"
  {:on-play
   {:msg "gain 2 [Credits] and draw 1 cards"
    :async true
    :effect (req (wait-for (gain-credits state side 2)
                           (draw state side eid 1)))}})

(defcard "ONR Off-Site Backups"
  {:on-play (corp-recur)})

(defcard "ONR Overtime Incentives"
  {:on-play
   {:msg "gain [Click][Click]"
    :effect (effect (gain-clicks 2))}})

(defcard "ONR Planning Consultants"
  {:on-play
   {:msg "rearrange the top 5 cards of R&D"
    :waiting-prompt true
    :async true
    :effect (effect (continue-ability
                      (let [from (take 5 (:deck corp))]
                        (when (pos? (count from))
                          (reorder-choice :corp :runner from '()
                                          (count from) from)))
                      card nil))}})

(defcard "ONR Power Grid Overload"
  {:on-play
   {:req (req tagged)
    :effect (req (let [cards (filter #(and (hardware? %)
                                           ;; onr has the S!
                                           (not (has-subtype? % "Cybernetics"))
                                           (not (has-subtype? % "Cybernetic")))
                                     (all-installed state :runner))
                       max-amt (min (count cards) (total-available-credits state :corp (assoc eid :source card :source-type :play) card))]
                   (continue-ability
                     state side
                     (if (zero? max-amt)
                       {:waiting-prompt true
                        :prompt "You can't do anything"
                        :choices ["I understand"]}
                       {:waiting-prompt true
                        :choices {:card #(some (fn [c] (same-card? % c)) cards)
                                  :max max-amt}
                        :async true
                        :effect (req (wait-for (pay state :corp (make-eid state (assoc eid :source card :source-type :play)) card [:credit (count targets)])
                                               (system-msg state side (str "pays " (count targets)
                                                                           "[Credits] to use " (:title card) " to trash "
                                                                           (str/join ", " (map :title targets))))
                                               (trash-cards state side eid targets)))})
                     card nil)))}})

(defcard "ONR Project Consultants"
  (letfn [(ability [x]
            {:prompt (msg "Choose an installed card to place advancement counters on (" x " remaining)")
             :async true
             :waiting-prompt true
             :choices {:card #(and (corp? %)
                                   (can-be-advanced? %)
                                   (installed? %))}
             :msg (msg "place 1 advancement counter on " (card-str state target))
             :effect (req (wait-for (add-prop state side target :advance-counter 1 {:placed true})
                                    (if (> x 1)
                                      (continue-ability state side (ability (dec x)) card nil)
                                      (effect-completed state side eid))))})]
    {:on-play
     {:async true
      :effect (effect (continue-ability (ability 4) card nil))}}))

(defcard "ONR Punitive Counterstrike"
  {:on-play
   {:req (req (<= 1 (count-tags state)))
    :msg "do 2 meat damage"
    :async true
    :effect (effect (damage eid :meat 2 {:card card}))}})

(defcard "ONR Reclamation Project"
  {:on-play
   {:req (req (some ice? (:discard corp)))
    :async true
    :effect (req (let [max-ct (count (filter ice? (:discard corp)))]
                   (continue-ability
                     state side
                     {:prompt "Choose ice to add to HQ"
                      :waiting-prompt true
                      :show-discard true
                      :choices {:card #(and (ice? %)
                                            (in-discard? %))
                                :max max-ct}
                      :async true
                      :msg (msg "adds " (str/join ", " (map :title targets)) " to HQ")
                      :effect (req (wait-for (reveal state side targets)
                                             (doseq [c targets]
                                               (move state :corp c :hand))
                                             (effect-completed state side eid)))}
                     card nil)))}})

(defcard "ONR Rent-to-Own Contract"
  (letfn [(term-general-event []
            {:event :runner-turn-ends
             :silent (req true)
             :unregister-once-resolved true
             :ability-name "Term Counters"
             :async true
             :effect (req (let [cards-with-counters (filter #(pos? (get-counters % :term)) (all-installed state :corp))]
                            (when-not (zero? (count cards-with-counters))
                               (register-events
                                 state side
                                 (:identity corp)
                                 [(term-general-event)]))
                            (doseq [target-card cards-with-counters]
                              (let [abi-name (str "Term Counters (" (:title target-card) ")")]
                                (register-events
                                  state side
                                  (:identity corp)
                                  [{:event :corp-turn-begins
                                    :unregister-once-resolved true
                                    :ability-name abi-name
                                    :silent false
                                    :req (req true)
                                    :interactive (req true)
                                    :async true
                                    :effect (req (let [new-target (get-card state target-card)]
                                                   (if (zero? (get-counters new-target :term))
                                                     (effect-completed state side eid)
                                                     (continue-ability
                                                       state side
                                                       {:msg (msg (if (> 2 (:credit corp))
                                                                    "add a Term counter to "
                                                                    "lose 2 credits and remove a Term counter from ")
                                                                  (card-str state new-target))
                                                        :async true
                                                        :effect (req (if (> 2 (:credit corp))
                                                                       (do (add-counter state side new-target :term 1)
                                                                           (effect-completed state side eid))
                                                                       (do (add-counter state side new-target :term -1)
                                                                           (lose-credits state side eid 2))))}
                                                       card nil))))}])))
                            (effect-completed state side eid)))})]
    {:on-play
     {:req (req (some #(and (ice? %) (not (rezzed? %))) (all-installed state :corp)))
      :choices {:card #(and (ice? %)
                            (not (rezzed? %))
                            (installed? %))}
      :msg (msg "rezzes " (card-str state target) " at no cost, and place " (:cost target) " Term counters on it")
      :effect (req (wait-for (rez state side target {:ignore-cost :all-costs :no-msg true})
                             (handle-if-unique state side (:identity corp) (term-general-event))
                             (add-counter state side (get-card state target) :term (:cost target))
                             (effect-completed state side eid)))}}))

(defcard "ONR Schlaghund Pointers"
  {:implementation "Restriction is not enforced" ;; todo - add a "not first turn"
   :on-play {:onr-trace {:max-strength 3
                         :label "Give the Runner a tag"
                         :corp-extra-bid-cost 1
                         :only-tags true
                         :successful (give-tags 1)}}})

(defcard "ONR Scorched Earth"
  {:on-play
   {:req (req tagged)
    :msg "do 4 meat damage"
    :async true
    :effect (effect (damage eid :meat 4 {:card card}))}})

(defcard "ONR Silver Lining Recovery Protocol"
  {:on-play
   {:req (req (pos? (:stolen-agenda-advancements runner-reg-last 0)))
    :msg (msg "gain " (* 3 (:stolen-agenda-advancements runner-reg-last 0)) " [Credits]")
    :effect (effect (gain-credits eid (* 3 (:stolen-agenda-advancements runner-reg-last 0))))}})

(defcard "ONR Systematic Layoffs"
  (letfn [(ability [x]
            {:prompt (msg "Choose an installed card to place advancement counters on (" x " remaining)")
             :async true
             :waiting-prompt true
             :choices {:card #(and (corp? %)
                                   (can-be-advanced? %)
                                   (installed? %))}
             :msg (msg "place 1 advancement counter on " (card-str state target))
             :effect (req (wait-for (add-prop state side target :advance-counter 1 {:placed true})
                                    (if (> x 1)
                                      (continue-ability state side (ability (dec x)) card nil)
                                      (effect-completed state side eid))))})]
    {:on-play
     {:async true
      :effect (effect (continue-ability (ability 2) card nil))}}))

(defcard "ONR Team Restructuring"
  {:on-play
   {:choices {:max 2
              :card #(and (corp? %)
                          (installed? %)
                          (can-be-advanced? %))}
    :msg (msg "place 1 advancement token on " (quantify (count targets) "card"))
    :effect (req (doseq [t targets]
                   (add-prop state :corp t :advance-counter 1 {:placed true})))}})

(defcard "ONR Trojan Horse"
  {:on-play
   {:req (req (last-turn? state :runner :stole-agenda))
    :msg "give the runner a tag"
    :async true
    :effect (req (gain-tags state :corp eid 1))}})

(defcard "ONR Underworld Mole"
  {:implementation "None of the restrictions are enforced!"
   :on-play
   {:onr-trace
    {:max-strength 4
     :req (req (last-turn? state :runner :installed-resource))
     :successful {:msg (msg "trash " (:title target) " and give the Runner a tag")
                  :prompt "trash a card"
                  :choices {:card #(and (installed? %)
                                        (resource? %))}
                  :async true
                  :cancel-effect (req
                                   (system-msg state side "gives the Runner a tag")
                                   (gain-tags state :corp eid 1))
                  :effect (req (wait-for (trash state side target {:cause-card card})
                                         (gain-tags state :corp eid 1)))}}}})

(defcard "ONR Urban Renewal"
  {:on-play
   {:req (req tagged)
    :msg "do 5 meat damage"
    :async true
    :effect (effect (damage eid :meat 5 {:card card}))}})
