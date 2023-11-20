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
   [jinteki.utils :refer :all]))

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

(defn- handle-if-unique
  ([state side card handler] (handle-if-unique state side card handler nil))
  ([state side card handler targets] (handle-if-unique state side card handler targets false))
  ([state side card handler targets debug]
   (let [matching-events
         (seq (filter #(= (:ability-name handler) (:ability-name (:ability %)))
                      (gather-events state side (:event handler) targets)))]
     (when debug
       (do
         (system-msg state side (str "event type: " (:event handler)))
         (system-msg state side (str (gather-events state side (:event handler) targets)))
         ))
     (when-not matching-events
       (do (when debug (system-msg state side (str "registered " (:ability-name handler))))
           (register-events state side card [handler]))))))

(defn- register-effect-once [state side card effect]
  (let [em (gather-effects state side (:type effect))
        matches (filter #(= (:ability-name %) (:ability-name effect)) em)]
    (when (empty? matches)
      (register-lingering-effect
        state side card
        effect))))

(defn- give-tags
  "Basic give runner n tags subroutine."
  [n]
  {:label (str "Give the Runner " (quantify n "tag"))
   :msg (str "give the Runner " (quantify n "tag"))
   :async true
   :effect (effect (gain-tags :corp eid n))})


(defn- trace-ability
  "Run a trace with specified base strength.
  If successful trigger specified ability"
  ([base {:keys [label] :as ability}]
   {:label (str "Trace " base " - " label)
    :trace {:base base
            :label label
            :successful ability}})
  ([base ability un-ability]
   (let [label (str (:label ability) " / " (:label un-ability))]
     {:label (str "Trace " base " - " label)
      :trace {:base base
              :label label
              :successful ability
              :unsuccessful un-ability}})))

(defn- tag-trace
  "Trace ability for giving a tag, at specified base strength"
  ([base] (tag-trace base 1))
  ([base n] (trace-ability base (give-tags n))))

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
              (continue-ability (tag-trace 5) card nil))}})

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
              (continue-ability (tag-trace 5) card nil))}})

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
  {:on-play
   {:req (req (last-turn? state :runner :trashed-card))
    :implementation "Doesn't enforce asset (node) only"
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
   {:msg "gain 2 [Credits] and draw 1 card"
    :async true
    :effect (req (wait-for (gain-credits state side 2)
                           (draw state side eid 1)))}})

(defcard "ONR Edgerunner, Inc., Temps"
  {:on-play
   {:msg "gain [Click][Click][Click] for install actions"
    :implementation "You may forgo these clicks to purge viruses. Manually adjust clicks you don't use."
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

(defcard "ONR Scorched Earth"
  {:on-play
   {:req (req tagged)
    :msg "do 4 meat damage"
    :async true
    :effect (effect (damage eid :meat 4 {:card card}))}})
