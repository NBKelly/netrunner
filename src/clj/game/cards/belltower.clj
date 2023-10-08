(ns game.cards.belltower
  (:require
   [clojure.string :as str]
   [game.cards.ice :refer [do-psi end-the-run end-the-run-unless-runner gain-credits-sub runner-loses-credits runner-loses-click runner-pays runner-trash-installed-sub trash-resource-sub trash-hardware-sub trash-program-sub give-tags gain-variable-subs reset-variable-subs]] ;;reffing the other card file might not be ideal
   [game.core.access :refer [access-bonus breach-server set-only-card-to-access steal-cost-bonus]]
   [game.core.actions :refer [score]]
   [game.core.agendas :refer [update-all-agenda-points]]
   [game.core.board :refer [all-active-installed all-installed get-all-cards get-remotes get-remote-names card->server  server->zone server-list]]
   [game.core.card-defs :refer [card-def]]
   [game.core.card :refer [active? agenda? can-be-advanced? card-index console? corp? corp-installable-type? event? facedown? faceup? get-card get-counters get-zone hardware? has-subtype? ice? installed? in-discard? in-hand? is-type? operation? program? rezzed? resource? runner?]]
   [game.core.cost-fns :refer [trash-cost install-cost play-cost rez-cost]]
   [game.core.damage :refer [damage damage-prevent]]
   [game.core.def-helpers :refer [breach-access-bonus corp-recur corp-rez-toast defcard do-brain-damage do-net-damage trash-on-empty]]
   [game.core.drawing :refer [draw]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.effects :refer [any-effects register-lingering-effect]]
   [game.core.engine :refer [checkpoint not-used-once? pay register-default-events register-events register-once register-suppress resolve-ability trigger-event unregister-events unregister-suppress-by-uuid]]
   [game.core.events :refer [first-event? first-run-event? no-event? run-events last-turn?]]
   [game.core.expend :refer [expend]]
   [game.core.finding :refer [find-cid]]
   [game.core.flags :refer [register-turn-flag! register-run-flag! can-host? zone-locked?]]
   [game.core.gaining :refer [gain-clicks gain-credits lose-clicks lose-credits]]
   [game.core.hand-size :refer [hand-size runner-hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [add-sub! all-subs-broken-by-card? any-subs-broken? auto-icebreaker breaker-strength-bonus break-sub break-subroutine! get-strength ice-strength ice-strength-bonus remove-subs! resolve-subroutine! strength-pump unbroken-subroutines-choice update-all-ice update-all-icebreakers]]
   [game.core.installing :refer [corp-install corp-install-list corp-install-msg install-locked? runner-can-install? runner-can-pay-and-install? runner-install install-as-condition-counter]]
   [game.core.memory :refer [mu+]]
   [game.core.moving :refer [as-agenda flip-facedown flip-faceup mill move swap-cards trash trash-cards]]
   [game.core.payment :refer [can-pay?]]

   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable]]
   [game.core.props :refer [add-counter add-prop set-prop]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [bypass-ice continue force-ice-encounter get-current-encounter end-run make-run set-next-phase successful-run-replace-breach start-next-phase]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name in-same-server? is-central? is-remote? from-same-server? same-server? target-server remote->name unknown->kw zone->name zones->sorted-names]]
   [game.core.set-aside :refer [set-aside set-aside-for-me get-set-aside]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck]]
   [game.core.tags :refer [gain-tags lose-tags]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.threat :refer [threat threat-level]]
   [game.core.update :refer [update!]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

;; helpers
(defn not-tagged-req
  [state]
  (= 0 (count-tags state)))

(defn cond-breaker
  "The breakers which rely on an event having happened"
  [keyword fn]
  {:req (req (not (no-event? state side keyword fn)))})

;; Anarch cards
(defcard "Sebastião Pessoa: Activist Organiser"
  ;; Whenever you take 1 or more tags during your turn, if you were not tagged,
  ;; you may install 1 connection resource from your grip, paying 2[Credits] less.
  ;; As an additional cost to trash connection resources, the Corp must trash 1 card
  ;; from HQ. (I'm assuming a multi-trash requires a multi-cost)
  {:implementation "2v3. Cost to trash connections not implemented"
   ;; todo - cost to trash connection resources by the corp - look at hacktivist?
   :events [{:event :runner-gain-tag
             :async true
             :req (req (and (not (install-locked? state side))
                            (= :runner (:active-player @state))
                            (= target (count-tags state)))) ;; every tag is one that was just gained
             :effect (req (continue-ability
                            state :runner
                            {:prompt "Choose a connection to install (for 2[Credits] less)"
                             :player :runner
                             :choices {:card #(and (has-subtype? % "Connection")
                                                   (resource? %)
                                                   (in-hand? %))}
                             :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -2}))}
                            card nil))}]})

(defcard "[Eye for an Eye]"
  ;; Play only if you are not tagged.
  ;; Run archives. If successful, instead of breaching Archives,
  ;; take 1 tag and the corp trashes 1 of their install cards.
  ;; Threat 5 - if you took a tag this way, the Corp trashes another 1 of their installed cards.
  ;; Remove this event from the game
  (letfn [(trash-x-installed [x state]
            (if (<= x (count (all-installed state :corp)))
              {:player :corp
               :prompt (str "Choose " (quantify x "card") " to trash")
               :waiting-prompt "Corp to trash a card"
               :msg (msg "trash " (:title target))
               :choices {:max x
                         :card #(and (installed? %)
                                     (corp? %))}
               :async true
               :effect (req (trash-cards state side eid targets {:cause :forced-to-trash}))}
              {:player :corp
               :msg "has no installed cards to trash"}))]
    {:makes-run true
     :implementation "2v3"
     :rfg-instead-of-trashing true
     :on-play {:req (req (and archives-runnable
                              (not-tagged-req state)))
               :async true
               :effect (effect (make-run eid :archives card))}
     :events [(successful-run-replace-breach
                {:target-server :archives
                 :this-card-run true
                 :ability
                 {:async true
                  :effect
                  (req (let [old-tags (count-tags state)]
                         (wait-for (gain-tags state :runner 1)
                                   (let [new-tags (count-tags state)]
                                     (wait-for (resolve-ability
                                                 state side
                                                 (trash-x-installed 1 state)
                                                 card nil)
                                               (if (and (= new-tags (+ 1 old-tags))
                                                        (threat-level 5 state))
                                                 (continue-ability
                                                   state side
                                                   (trash-x-installed 1 state)
                                                   card nil)
                                                 (effect-completed state side eid)))))))}})]}))

(defcard "[HQ Occupation]"
  ;; Play only if you are not tagged.
  ;; Run HQ. If successful, take 1 tag and access 2 additional cards when you breach HQ.
  {:implementation "2v3"
   :makes-run true
   :on-play {:req (req (and hq-runnable
                            (not-tagged-req state)))
             :async true
             :effect (effect (make-run eid :hq card))}
   :interactions {:access-ability
                  {:label "Trash card"
                   :msg (msg "trash " (:title target) " at no cost")
                   :async true
                   :effect (effect (trash eid (assoc target :seen true) {:cause-card card}))}}
   :events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :hq (target-server context))
                            this-card-run))
             :async true
             :msg "take 1 tag and access 1 additional card"
             :effect (req
                       (wait-for (gain-tags state :runner 1 {:unpreventable true})
                                 (register-events
                                   state side
                                   card [(breach-access-bonus :hq 1 {:duration :end-of-run})])
                                 (effect-completed state side eid)))}]})

(defcard "[Achaean Crew]"
  ;; Each piece of ice gets -1 strength for each trojan it hosts.
  ;;
  ;; Threat 3 - [trash]: Trash currently encountered ice with strength 0 or less.
  ;; Use this ability only during encounters with ice hosting trojans.
  {:implementation "2v2"
   :static-abilities [{:type :ice-strength
                       :req (req (and (get-current-encounter state)
                                      (same-card? current-ice target)))
                       :value (req (count (filter #(has-subtype? % "trojan")
                                                  (:hosted (get-card state target)))))}]
   :abilities [{:req (req (and (get-current-encounter state)
                               (threat-level 3 state)
                               (not (pos? (ice-strength state side current-ice)))
                               (pos? (count (filter #(has-subtype? % "trojan")
                                                    (:hosted current-ice))))))
                :label "trash encountered ice"
                :msg (msg "trash " (str current-ice))
                :cost [:trash-can]
                :effect (effect (trash eid current-ice {:cause-card card}))}]})

(defcard "[Onion]"
  ;; +1 mu
  ;;
  ;; When your turn begins, if you are tagged, you may choose 2 cards in the heap.
  ;; The Corp removes one from the game. Host the other on this hardware (it is not installed).
  ;;
  ;; The first time each turn you remove a tag, you may add a hosted card to the grip.
  {:static-abilities [(mu+ 1)]
   :events [{:event :runner-lose-tag
             :req (req (first-event? state side :runner-lose-tag))
             :async true
             :effect (req (continue-ability
                            state side
                            {:choices (req (cancellable (:hosted card)))
                             :async true
                             :prompt "Choose a hosted card"
                             :msg (msg "move " (:title target) " to their grip")
                             :effect (effect (move target :hand)
                                             (effect-completed eid))}
                            card nil))}
            {:event :runner-turn-begins
             :optional
             {:req (req (and (not (not-tagged-req state))
                            (<= 2 (count (:discard runner)))
                            (not (zone-locked? state :runner :discard))))
              :prompt (msg "Use " (:title card) " to select cards?")
              ;:autoresolve (get-autoresolve :auto-fire)
              :yes-ability
              {:interactive (req true)
               :async true
               :prompt "Choose 2 hosted cards"
               :choices {:max 2
                         :all true
                         :card #(and (in-discard? %)
                                     (runner? %))}
               :effect
               (effect (continue-ability
                         (let [c1 (first targets)
                               c2 (second targets)]
                           {:waiting-prompt "Corp to make a decision"
                            :prompt "Choose which card to remove from the game"
                            :player :corp
                            :choices [c1 c2]
                            :msg (msg (let [[chosen other](if (= target c1)
                                                            [c1 c2]
                                                            [c2 c1])]
                                        (str "add " (:title other) " to onion."
                                             " Corp removes " (:title chosen) " from the game")))
                            :effect (req (let [[chosen other] (if (= target c1)
                                                                [c1 c2]
                                                                [c2 c1])]
                                           (host state :runner card other)
                                           (move state :runner chosen :rfg)))})
                         card nil))}}}]})

(defcard "The Wizard's Chest"
  ;; When you install this hardware, search your stack for 2 differently named cards
  ;; and host them faceup on this hardware.
  ;; When you steal an agenda, choose 1 hosted card at random.
  ;; You may trash that card. If you do, install the remaining hosted card, ignoring all costs,
  ;; then trash this hardware.
  (letfn [(search-and-host [x]
            {:prompt (msg "Choose a card (" x " remaining)")
             :choices (req (filter #(not (contains? (set '(:hosted (get-card state card))) %)) (:deck runner)))
             :async true
             :msg (msg "host " (:title target) " on itself")
             :effect (req (host state side card target)
                          (if (> x 1)
                            (continue-ability state side (search-and-host (dec x)) card nil)
                            (effect-completed state side eid)))})]
    {:implementation "2v3"
     :on-install {:msg "search 2 cards from the stack"
                  :async true
                  :effect (req (wait-for (resolve-ability state side
                                                          (make-eid state eid)
                                                          (search-and-host 2)
                                                          card nil)
                                         (trigger-event state side :searched-stack nil)
                                         (shuffle! state side :deck)
                                         (system-msg state side "shuffle the stack")
                                         (effect-completed state side eid)))}
     :events [{:event :agenda-stolen
               :async true
               :effect (req (let [selected-card (first (shuffle (:hosted card)))]
                              (system-msg state side "agenda stolen")
                              (continue-ability
                                state side
                                {:optional
                                 {:prompt (msg "Install " (:title selected-card) ", ignoring all costs?")
                                  :waiting-prompt "Runner to make a decision"
                                  :req (req (runner-can-install? state side selected-card nil))
                                  ;;(can-pay? state side (assoc eid :source card :source-type :runner-install) selected-card nil [:credit (install-cost state side selected-card {:cost-bonus -3})])))
                                  :yes-ability {:msg (msg "install " (:title selected-card) ", ignoring all costs?"
                                                          (str/join ", " (map :title (filter #(not= (:title %) (:title selected-card)) (:hosted card))))
                                                          " is trashed as a result")
                                                :async true
                                                :effect (req (wait-for (runner-install state side (make-eid state (assoc eid :source card :source-type :runner-install)) selected-card {:ignore-all-cost true})
                                                                       (trash-cards state side (assoc eid :source card) [card]  {:unpreventable true :cause-card card})))}
                                  :no-ability {:msg (msg "does not install " (:title selected-card) ". ")}}}
                                card nil)))}]}))

(defcard "Heliamphora"
  ;; Whenever you breach Archives, you may host 2 non-agenda cards from Archives on this program.
  ;; Whenever the Corp purges virus counters, the Corp randomly trashes 2 cards from HQ,
  ;; then trash this program."
  ;; TODO - show corp discard when runner access this?
  ;;        perhaps a "show-opponent-discard" key?
  {:implementation "2v3"
   :events [{:event :breach-server
             :async true
             :interactive (req true)
             :req (req (and (= target :archives)
                            (not-empty (:discard corp))))
             :effect (req (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                          (continue-ability
                            state side
                            {:optional
                             {:prompt "Host cards on Heliamphora?"
                              :yes-ability {:prompt "Choose up to 2 cards in Archives to host on heliamphora"
                                            :choices {:req (req (and (corp? target)
                                                                     (not (agenda? target))
                                                                     (in-discard? target)))
                                                      :max 2}
                                            :msg (msg "host " (str/join ", " (map :title targets)))
                                            :async true
                                            :effect (req (when-not (zero? (count targets))
                                                           (do (host state side card (first targets))
                                                               (when-not (= 1 (count targets))
                                                                 (host state side card (second targets)))))
                                                         (effect-completed state side eid))}}}
                            card nil))}
            {:event :purge
             :msg "force the Corp to trash 2 cards from HQ at random, then trash itself"
             :async true
             :effect (req (wait-for (trash-cards state :corp (make-eid state eid) (take 2 (shuffle (:hand corp))) {:cause-card card})
                                    (trash state :runner eid card {:cause :purge :cause-card card})))}]})

(defcard "[Raccoon]"
  ;; If you trashed one of your installed cards this turn,
  ;; paid abilities on this icebreaker cost 1{credit} less to activate.
  ;;
  ;; Interface → 2{credit}: Break up to 2 sentry subroutines.
  ;; 2{credit}: +2 strength."
  (letfn [(raccoon-fn
            [target]
            (runner? (:card (first target))))]
    (auto-icebreaker {:implementation "2v3. Printed abilities only."
                      :abilities [(break-sub 1 2 "Sentry" (cond-breaker :runner-trash raccoon-fn))
                                  (break-sub 2 2 "Sentry")
                                  (strength-pump 1 2 :end-of-encounter (cond-breaker :runner-trash raccoon-fn))
                                  (strength-pump 2 2)]})))

(defcard "Boiúna"
  ;; Paid abilities on this program cost 1{c} less if you installed a connection this turn.
  ;;
  ;; Interface → 2{credit}: Break up to 2 code gate subroutines.
  ;; 2{credit}: +2 strength.
  (letfn [(boinga-fn
            [target]
            (has-subtype? (:card (first target)) "Connection"))]
    (auto-icebreaker {:implementation "2v3. Printed abilities only."
                      :abilities [(break-sub 1 2 "Code Gate" (cond-breaker :runner-install boinga-fn))
                                  (break-sub 2 2 "Code Gate")
                                  (strength-pump 1 2 :end-of-encounter (cond-breaker :runner-install boinga-fn))
                                  (strength-pump 2 2)]})))

(defcard "Friend of a Friend"
  ;;{click}, {trash}: Gain 7{credit}. Use this ability only if you are tagged.
  ;; Threat 4 — {trash}: Install 1 connection resource from your heap.
  ;; Use this ability only during your turn.
  {:implementation "2v3. Runner turn req not enforced."
   :abilities [{:label "Gain 7 [Credits]"
                :msg "gain 7 [Credits]"
                :cost [:click 1 :trash-can]
                :async true
                :req (req (pos? (count-tags state)))
                :effect (effect (gain-credits eid 7))}
               {:label "Install a connection from your heap"
                :prompt "Choose a Connection"
                :cost [:trash-can]
                :choices (req (cancellable (filter #(and
                                                      (has-subtype? % "Connection")
                                                      (can-pay? state side (assoc eid :source card :source-type :runner-install) (get-card state %) nil [:credit (install-cost state side %)]))
                                                   (:discard runner)) :sorted))
                :msg (msg "Install " (:title target) " from their heap")
                :req (req (threat-level 4 state))
                :async true
                :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target nil))}]})

(defcard "[Power Outage]"
  ;; Derez a piece of ice protecting any server, then run that server.
  ;;When that run ends, the Corp may rez that ice, ignoring all costs.
  {:makes-run true
   :implementation "2v3"
   :on-play
   {:msg (msg "derez " (:title target))
    :choices {:card #(and (ice? %)
                          (rezzed? %))}
    :async true
    :effect (req (let [target-ice target]
                   (derez state :runner target)
                   (register-events
                     card [{:event :run-ends
                            :duration :end-of-run
                            :unregister-once-resolved true
                            :optional {:prompt (msg "Rez " (:title target-ice) ", ignoring all costs?")
                                       :yes-ability {:async true
                                                     :req (req (and
                                                                 (installed? (get-card state target-ice))
                                                                 (not (rezzed? (get-card state target-ice)))))
                                                     :effect (req (rez state side eid target-ice {:ignore-cost true}))}
                                       :no-ability {:msg (str "declines to rez " (:title target-ice))}}}])
                    (make-run eid (second (get-zone target)) card)))}})

(defcard "[Rule of Three]"
  ;; When your turn ends, if you made a successful run on HQ, R&D and Archives this turn, you may
  ;;   gain 3c. If you do, add this card to your score area as an agenda worth 0 points with
  ;;   "if you have 3 copies of {Rule of Three}" in your score area, you win the game."
  {:event :runner-turn-ends
   :implementation "2v3. Winning game is manual."
   :optional {:prompt (msg "Gain 3[Credits] and add " (:title card) " to your score area?")
              :yes-ability {:req (req (and
                                        (installed card)
                                        (some #{:hq} (:successful-run runner-reg))
                                        (some #{:rd} (:successful-run runner-reg))
                                        (some #{:archives} (:successful-run runner-reg))))
                            :msg (msg "gain 3[Credits] and add itself to the score area as an agenda worth 0 agenda points")
                            :async true
                            :effect (req (wait-for (gain-credits state :runner 3)
                                                   (as-agenda state :runner card 0)
                                                   (effect-completed state side eid)))}
              :no-ability {:msg "declines to use ."}}})

(defcard "[Wipe Down]"
  ;; Resolve 1 of the following:
  ;;  remove up to 2 tags
  ;;  Draw 3 cards. Add 1 card from the grip to the top or bottom of the stack.
  {:implementation "2v3 - you can choose to remove 0 tags if you want"
   :on-play {:prompt "Choose one"
             :req (req (or (pos? (count (:deck runner)))
                           (pos? (count-tags state))))
             :choices (req [(when (not= 0 (count-tags state) "Remove up to 2 tags")) "Draw 3 cards"])
             :effect (effect (continue-ability
                               (if (= target "Remove up to 2 tags")
                                 {:prompt "Remove how many tags?"
                                  :choices (req [0 1 (when (not= 1 (count-tags state)) 2)])
                                  :msg (msg "remove " target " tags.")
                                  :async true
                                  :effect (effect (lose-tags eid target))}
                                 {:msg "draw 3 cards"
                                  :async true
                                  :effect (req (wait-for (draw state side (make-eid state eid) 3)
                                                         (continue-ability
                                                           state side
                                                           {:prompt "select a card to return"
                                                            :choices {:card #(and (in-hand? %)
                                                                                  (runner? %))
                                                                      :all true}
                                                            :async true
                                                            :effect (req (let [chosen-card target]
                                                                           (continue-ability
                                                                             state side
                                                                             {:prompt "Choose one"
                                                                              :choices ["Top of Stack" "Bottom of stack"]
                                                                              :async true
                                                                              :effect (req (if (= target "Top of Stack")
                                                                                             (do (move state :runner chosen-card :deck {:front true})
                                                                                                 (system-msg state side "places a card on the top of the stack"))
                                                                                             (do (move state :runner chosen-card :deck)
                                                                                                 (system-msg state side "places a card on the bottom of the stack")))
                                                                                           (effect-completed state side eid))}
                                                                             card nil)))}
                                                           card nil)))})
                               card nil))}})

(defcard "[Alarm Clock]"
  ;; When your turn begins, you may run HQ
  (let [ability {:once :per-turn
                 :req (req (:runner-phase-12 @state))
                 :msg (msg "make a run on HQ")
                 :makes-run true
                 :async true
                 :effect (req (wait-for (make-run state :runner (make-eid state eid) :hq card)
                                        (effect-completed state side eid)))}]
    {:implementation "2v3"
     :flags {:runner-phase-12 (req true)}
     :events [{:event :runner-turn-begins
               :interactive (req true)
               :optional
               {:once :per-turn
                :prompt "Make a run on HQ?"
                :yes-ability ability}}]
     :abilities [ability]}))

(defcard "Fluke"
  ;; Install only on a piece of ice.
  ;; When you encounter host ice, if it is a code gate, you may pay 1[Credit] to bypass it.
  ;; Trash this program if the corp purges virus counters.
  {:implementation "2v3"
   :hosting {:card #(and (ice? %)
                         (can-host? %))}
   :events [{:event :encounter-ice
             :optional {:prompt "pay 1 [Credit] to bypass ice?"
                        :req (req (and (has-subtype? current-ice "Code Gate")
                                       (same-card? current-ice (:host card))
                                       (can-pay? state :runner eid card nil
                                                 [:credit 1])))
                        :yes-ability {:async true
                                      :msg (msg "pays 1 [Credits] to bypass " (:title current-ice))
                                      :effect (req (wait-for (pay state :runner (make-eid state eid) card [:credit 1])
                                                             (if-let [payment-str (:msg async-result)]
                                                               (do (bypass-ice state)
                                                                   (effect-completed state side eid))
                                                               (effect-completed state side eid))))}}}
            {:event :purge
             :async true
             :msg "trash itself"
             :effect (req (trash state :runner eid card {:cause :purge
                                                         :cause-card card}))}]})

(defcard "[Parrots]"
  ;;When you install this program, load 3 power counters onto it. When it is empty, trash it.
  ;; Whenever you encounter an ice with strength 2 or less,
  ;; you may spend 1 hosted power counter to bypass it. Use this ability only once per turn.
  ;; Threat 5 -- Whenever you encounter an ice, you may trash this program to bypass it.
  {:implementation "2v3 - abilities are named (but have the same timing)"
   :data {:counter {:power 3}}
   :events [(trash-on-empty :power)
            {:event :encounter-ice
             :interactive (req true)
             :ability-name "Parrots (Trash)"
             :optional {:prompt (msg "trash parrots to bypass?")
                        :req (req (threat-level 5 state))
                        :yes-ability {:cost [:trash-can]
                                      :msg (msg "bypass " (:title current-ice))
                                      :effect (req (bypass-ice state))}}}
            {:event :encounter-ice
             :interactive (req true)
             :ability-name "Parrots (Power Counter)"
             :optional {:prompt (msg "spend 1 power counter to bypass " (:title current-ice))
                        :once :per-turn
                        :req (req (and (>= 2 (ice-strength state side current-ice))
                                       (<= 1 (get-counters (get-card state card) :power))))
                        :yes-ability {:cost [:power 1]
                                      :msg (msg "bypass " (:title current-ice))
                                      :effect (req (bypass-ice state))}}}]})

(defcard "[Bindle]"
  ;; Limit 1 hosted card.
  ;;  Access -->0c: Host the accessed non-agenda card faceup on this program.
  ;; Whenever you breach HQ or R&D, you may trash 1 hosted corp card
  ;; and this program to access an additional card.
  {:implementation "2v3"
   :events [{:event :breach-server
            :async true
            :req (req (and (or (= :rd target)
                               (= :hq target))
                           (not (zero? (count (:hosted card))))))
            :effect (req
                      (let [target-server target]
                        (continue-ability
                          state side
                          {:optional {:req (req true)
                                      :prompt "Trash bindle and hosted card to access an additional card?"
                                      :yes-ability {:async true
                                                    :effect (effect (access-bonus target-server 1)
                                                                    (effect-completed eid))
                                                    :cost [:trash-can]
                                                    :msg "access an additional card"}}}
                          card nil)))}]
   :interactions {:access-ability {:label "Host a card"
                                   :req (req (and (zero? (count (:hosted card)))
                                                  (not (agenda? target))))
                                   :msg (msg "host " (:title target))
                                   :async true
                                   :effect (req (host state side card target)
                                                (swap! state dissoc :access)
                                                (effect-completed state side eid))}}})

(defcard "Amelia Earhart"
  ;; The first time each turn you may a successful run on a central server, place 1 credit on up to
  ;; 2 installed job and/or connection resources.
  {:implementation "2v3"
   :events [{:event :successful-run
             :req (req (and (is-central? (:server context))
                            (first-event? state side :successful-run
                                          (fn [targets]
                                            (let [context (first targets)]
                                              (is-central? (:server context)))))
                            (pos? (count (filter #(and (or (has-subtype? % "Job")
                                                           (has-subtype? % "Connection"))
                                                       (resource? %))
                                                 (all-installed state :runner))))))
             :msg "place 1 [Credit] on up to 2 job or connection resources"
             :async true
             :effect (req (let [valid-targets (filter #(and (or (has-subtype? % "Job")
                                                                (has-subtype? % "Connection"))
                                                            (resource? %))
                                                      (all-installed state :runner))
                                max-targets (min 2 (count valid-targets))]
                            (continue-ability
                              state side
                              {:choices {:card #(and (resource? %)
                                                     (installed? %)
                                                     (or (has-subtype? % "Job")
                                                         (has-subtype? % "Connection")))
                                         :max max-targets}
                               :msg (msg "place a credit on " (str/join " and " (map :title targets)))
                               :effect (req (doseq [c targets]
                                              (add-counter state :runner c :credit 1)))}
                              card nil)))}]})

(defcard "[Mrs Rocinha]"
  ;; When you install this resource, load 4 power counters onto it. When it is empty, trash it.
  ;; The first time each turn you take an action on an installed resource, remove 1 hosted power
  ;;  counter and gain [click]
  {:implementation "2v3. I still don't understand what this does."
   :data {:counter {:power 4}}
   :events [(trash-on-empty :power)
            {:event :runner-spent-click
             :req (req (let [all-cards (get-all-cards state)]
                         (and (resource? (find-cid (first target) all-cards))
                              (first-event? state side :runner-spent-click
                                            #(resource?
                                               (find-cid (first (first %)) all-cards))))))
             :msg "gain [Click]"
             :effect (effect (add-counter card :power -1)
                             (gain-clicks 1))}]})
