(ns game.cards.belltower
  (:require
   [clojure.string :as str]
   [game.cards.ice :refer [do-psi end-the-run end-the-run-unless-runner gain-credits-sub runner-loses-credits runner-loses-click runner-pays runner-trash-installed-sub trash-resource-sub trash-hardware-sub trash-program-sub give-tags gain-variable-subs reset-variable-subs]] ;;reffing the other card file might not be ideal
   [game.core.access :refer [access-bonus breach-server set-only-card-to-access steal-cost-bonus]]
   [game.core.actions :refer [score]]
   [game.core.agendas :refer [update-all-agenda-points]]
   [game.core.board :refer [all-active-installed all-installed get-all-cards get-remotes get-remote-names card->server  server->zone server-list]]
   [game.core.card-defs :refer [card-def]]
   [game.core.card :refer [active? agenda? can-be-advanced? card-index console? corp? corp-installable-type? event? facedown? faceup? get-card get-counters get-zone hardware? has-subtype? ice? installed? in-discard? in-hand? in-deck? is-type? operation? program? rezzed? resource? runner?]]
   [game.core.checkpoint :refer [fake-checkpoint]]
   [game.core.cost-fns :refer [trash-cost install-cost play-cost rez-cost]]
   [game.core.damage :refer [damage damage-prevent]]
   [game.core.def-helpers :refer [breach-access-bonus corp-recur corp-rez-toast defcard do-brain-damage do-net-damage trash-on-empty x-fn]]
   [game.core.diffs :refer [playable?]]
   [game.core.drawing :refer [draw]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.effects :refer [any-effects register-lingering-effect]]
   [game.core.engine :refer [checkpoint not-used-once? pay register-default-events register-events register-once register-suppress resolve-ability should-trigger? trigger-event unregister-events unregister-suppress-by-uuid]]
   [game.core.events :refer [first-event? first-run-event? no-event? run-events last-turn?]]
   [game.core.expend :refer [expend]]
   [game.core.finding :refer [find-cid]]
   [game.core.flags :refer [register-turn-flag! register-run-flag! can-host? zone-locked?]]
   [game.core.gaining :refer [gain-clicks gain-credits lose-clicks lose-credits]]
   [game.core.hand-size :refer [hand-size runner-hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [add-sub! add-extra-sub! all-subs-broken-by-card? any-subs-broken? auto-icebreaker breaker-strength-bonus break-sub break-subroutine! get-strength ice-strength ice-strength-bonus pump-ice remove-subs! remove-sub! resolve-subroutine! strength-pump unbroken-subroutines-choice update-all-ice update-all-icebreakers]]
   [game.core.installing :refer [corp-install corp-install-list corp-install-msg install-locked? runner-can-install? runner-can-pay-and-install? runner-install install-as-condition-counter]]
   [game.core.memory :refer [mu+]]
   [game.core.moving :refer [as-agenda flip-facedown flip-faceup mill move swap-cards trash trash-cards]]
   [game.core.payment :refer [can-pay?]]
   [game.core.play-instants :refer [can-play-instant? play-instant]]
   [game.core.prompts :refer [cancellable]]
   [game.core.props :refer [add-counter add-prop set-prop]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [bypass-ice continue force-ice-encounter get-current-encounter end-run make-run redirect-run set-next-phase successful-run-replace-breach start-next-phase]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name in-same-server? is-central? is-remote? from-same-server? protecting-same-server? same-server? target-server remote->name unknown->kw zone->name zones->sorted-names]]
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

(defn return-to-top
  "returns a set of cards to the top of the deck"
  ([set-aside-cards] (return-to-top set-aside-cards false))
  ([set-aside-cards reveal]
   {:prompt "choose a card to put ontop of R&D"
    :req (req (not (zero? (count set-aside-cards))))
    :choices {:all true
              :max 1
              :req (req (some #(same-card? % target) set-aside-cards))}
    :async true
    :waiting-prompt "corp to return cards to R&D"
    :msg (msg "place " (if reveal (:title target) "a card") " ontop of R&D")
    :effect (req (move state :corp target :deck {:front true})
                 (let [rem (seq (filter #(not (same-card? target %)) set-aside-cards))]
                   (if (not (empty? rem))
                     (continue-ability state side
                                       (return-to-top rem reveal)
                                       card nil)
                     (effect-completed state side eid))))}))

(defn return-to-top-or-bottom
  "returns a set of cards to the top or the bottom of the deck"
  ([set-aside-cards] (return-to-top-or-bottom set-aside-cards false))
  ([set-aside-cards reveal]
   {:prompt "choose a card to place on the top or bottom of R&D"
    :req (req (not (zero? (count set-aside-cards))))
    :choices {:all true
              :max 1
              :req (req (some #(same-card? % target) set-aside-cards))}
    :async true
    :waiting-prompt "corp to return cards to R&D"
    :effect (req (let [chosen-card target]
                   (continue-ability
                     state side
                     {:choices ["top of R&D" "bottom of R&D"]
                      :prompt (msg "place " (:title chosen-card) " where?")
                      :msg (msg "place " (if reveal (:title target) "a card") " on the " target)
                      :effect (req (move state :corp chosen-card :deck {:front (= target "top of R&D")})
                                   (let [rem (seq (filter #(not (same-card? chosen-card %))
                                                          set-aside-cards))]
                                     (if (not (empty? rem))
                                       (continue-ability
                                         state side
                                         (return-to-top-or-bottom rem reveal)
                                         card nil)
                                       (effect-completed state side eid))))}
                     card nil)))}))

(defn trojan-auto-hosts?
  [card]
  (not (or (has-subtype? card "Caïssa")
           (= (:title card) "Ika"))))

(def trash-resource-or-hardware-sub
  {:prompt "Choose a resource or hardware to trash"
   :label "Trash a resource or hardware"
   :msg (msg "trash " (:title target))
   :choices {:card #(and (installed? %)
                         (or (resource? %)
                             hardware? %))}
   :async true
   :effect (effect (trash eid target {:cause :subroutine}))})

;; ANARCH CARDS

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

;; CRIMINAL CARDS

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

;; SHAPER CARDS

(defcard "Spree"
  ;; Run any server. Whenever you approach a piece of ice during this run,
  ;; you may host 1 installed trojan program on that ice.
  {:implementation "2v3"
   :makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :approach-ice
             :prompt "select a trojan to host on encountered ice"
             :async true
             :choices {:card #(and (installed? %)
                                   (has-subtype? % "Trojan"))}
             :msg (msg "moves " (:title target) " and hosts it on " (card-str state current-ice))
             :effect (effect (host current-ice target)
                             (update-all-ice)
                             (effect-completed eid))}]})

(defcard "True Colors"
  ;; Run HQ. If successful, instead of breaching, the Corp reveals 3 cards from HQ at random.
  ;; Add 2 of the revealed cards to the top and/or bottom of R&D in any order."
  (letfn [(move-ab [chosen-cards n]
            {:prompt "select a card to move"
             :choices chosen-cards
             :async true
             :effect (req (let [target-card target]
                            (continue-ability
                              state side
                              {:prompt (msg "move " (:title target-card) " where?")
                               :choices ["Top of R&D" "Bottom of R&D"]
                               :async true
                               :msg (msg "move " (:title target-card) "to the "
                                         (if (= target "Top of R&D")
                                           "top of R&D"
                                           "bottom of R&D"))
                               :effect (req
                                         (if (= target "Top of R&D")
                                           (move state :corp target-card :deck {:front true})
                                           (move state :corp target-card :deck {:front false}))
                                         (if (= 1 n)
                                           (effect-completed state side eid)
                                           (continue-ability
                                             state side
                                             (move-ab
                                               (remove-once #(= % target-card) chosen-cards)
                                               (dec n))
                                             card nil)))}
                              card nil)))})]
    {:implementation "2v3"
     :makes-run true
     :on-play {:req (req hq-runnable)
               :async true
               :effect (effect (make-run eid :hq card))}
     :events [(successful-run-replace-breach
                {:target-server :hq
                 :this-card-run true
                 :ability
                 {:msg "reveal 3 cards from HQ"
                  ;; TODO - assuming that revealing 1 or 2 cards works!
                  :req (req (<= 1 (count (:hand corp))))
                  :async true
                  :effect (req (let [chosen-cards (take 3 (shuffle (:hand corp)))]
                                 (system-msg
                                   state side
                                   (str "reveals " (str/join  ", " (map :title chosen-cards))
                                        "from HQ"))
                                 (continue-ability
                                   state side
                                   (move-ab chosen-cards (min 2 (count chosen-cards)))
                                   card nil)))}})]}))

(defcard "[Free Wall]"
  ;; Run R&D. If successful, draw 1 card and when you breach R&D,
  ;;  access 1 additional card and gain 2{credit}.
  ;;  When that run ends, if it was successful, you may place this card on top of the stack."
  {:implementation "2v3"
   :makes-run true
   :on-play {:req (req rd-runnable)
             :async true
             :effect (effect (make-run eid :rd card))}
   :events [{:event :successful-run
             :silent (req true)
             :async true
             :req (req (and (= :rd (target-server context))
                            this-card-run))
             :effect (req (register-events
                            state side
                            card [(breach-access-bonus :rd 1 {:duration :end-of-run})])
                          (wait-for (draw state side 1)
                                    (gain-credits state side eid 2)))}
            {:event :run-ends
             :req (req this-card-run)
             :async true
             :effect (req (when (:successful target)
                            (continue-ability
                              state side
                              {:optional
                               {:prompt "Add Free Wall to the top of the stack?"
                                :yes-ability {:msg "add itself to the top of the stack"
                                              :effect (req (move state :runner card :deck {:front true})
                                                           (effect-completed state side eid))}
                                :no-ability {:msg "decline to add itself to the top of the stack"}}}
                              card nil)))}]})

(defcard "[Repurposer]"
  ;;You cannot use this hardware while breaching R&D or accessing a card in R&D.
  ;;  Access → Lose {click}: The Corp shuffles the non-agenda card you are accessing into R&D.
  ;;  Threat 5 → Whenever you access a non-agenda card, you may trash this hardware.
  ;;             If you do, the Corp shuffles that card into R&D."
  (letfn [(resolve-shuffle [state side target eid]
            (move state :corp target :deck)
            (shuffle! state :corp :deck)
            (effect-completed state side eid))]
  {:implementation "2v3"
   :interactions
   {:access-ability
    {:label "Shuffle a card into R&D"
     :req (req (and (not (in-deck? target))
                    (or (pos? (:click runner))
                        (threat-level 5 state))
                    (not= (:breach-server (:breach state)) :rd)))
     :async true
     :effect (req
               (let [shuffle-card target]
                 (continue-ability
                   state side
                   {:prompt "Pay how?"
                    :choices (req [(when (pos? (:click runner))
                                     "Lose Click")
                                   (when (threat-level 5 state)
                                     (str "Trash " (:title card)))])
                    :async true
                    :effect (req (if (= target "Lose Click")
                                   (wait-for
                                     (pay state :runner (make-eid state eid)
                                          card [:lose-click 1])
                                     (system-msg
                                       state side
                                       (str (:msg async-result) " to shuffle " (:title shuffle-card) "into R&D"))
                                     (resolve-shuffle state side shuffle-card eid))
                                   (wait-for
                                     (trash state :runner (make-eid state eid)
                                            card {:cause :runner-ability
                                                  :cause-card card})
                                     (system-msg
                                       state side
                                       (str "trashes " (:title card)
                                            " to shuffle " (:title shuffle-card) " into R&D"))
                                     (resolve-shuffle state side shuffle-card eid))))}
                   card nil)))}}}))

(defcard "Sketchpad"
  ;; Install only on a piece of ice.
  ;;
  ;; Host ice gains barrier, code gate, and sentry.
  ;;
  ;; Threat 4 — Lose {click}: Host this program on another piece of ice."
  {:hosting {:card #(and (ice? %)
                         (can-host? %))}
   :implementation "2v3"
   :abilities [{:label "Host on a piece of ice"
                :prompt "Choose a piece of ice"
                :cost [:lose-click 1]
                :req (req (threat-level 4 state))
                :choices {:card #(and (ice? %)
                                      (installed? %)
                                      (can-host? %))}
                :msg (msg "host itself on " (card-str state target))
                :effect (effect (host target card))}]
   :on-install {:msg (msg "make " (card-str state (:host card))
                          " gain Barrier, Code Gate and Sentry subtypes")}
   :static-abilities [{:type :gain-subtype
                       :req (req (same-card? target (:host card)))
                       :value ["Barrier" "Code Gate" "Sentry"]}]})

(defcard "Pressure Spike"
  ;; Threat 4 — This program gains 2 strength.

  ;; Interface → 1{credit}: Break 1 barrier subroutine.
  ;; 2{credit}: +1 strength."
  (auto-icebreaker {:implementation "2v3"
                    :x-fn (req (if (threat-level 4 state) 2 0))
                    :abilities [(break-sub 1 1 "Barrier")
                                (strength-pump 2 1)]
                    :static-abilities [(breaker-strength-bonus x-fn)]}))

(defcard "Brownie"
  ;; When you install this program, search your stack, heap, or grip for 1 icebreaker, trojan,
  ;;   or virus program. If it is a trojan, install it on a piece of ice,
  ;;   otherwise, install it on this program."
  (letfn [(brownie-fn [where]
            {:prompt "Choose a program to install"
             :msg (req (if (not= target "No install")
                         (str "install " (:title target))
                         (str "shuffle their Stack")))
             :choices (req (conj (filter #(and (can-pay? state side
                                                         (assoc eid :source card :source-type :runner-install)
                                                         % nil [:credit (install-cost state side %)])
                                               (or (has-subtype? % "Icebreaker")
                                                   (has-subtype? % "Trojan")
                                                   (has-subtype? % "Virus")))
                                         (vec (sort-by :title (filter program? (where runner)))))
                                 "No install"))
             :async true
             :effect (req (when (= :deck where)
                            (trigger-event state side :searched-stack nil)
                            (shuffle! state side :deck))
                          (if (not= target "No install")
                            ;; does the card need to be installed on brownie?
                            (if-not (has-subtype? target "Trojan")
                              (runner-install state side (assoc eid :source card :source-type :runner-install) target {:host-card (get-card state card)})
                              ;;otherwise, pick a target card to host the trojan on
                              (if (trojan-auto-hosts? target)
                                ;; if the trojan does it for free, so be it
                                (runner-install state side (assoc eid :source card :source-type :runner-install) target nil)
                                ;; do it the hard way
                                (let [target-card target]
                                  (continue-ability
                                    state side
                                    {:prompt (msg "choose an ice to host " (:title target-card))
                                     :choices {:card #(and (installed? %)
                                                           (ice? %))}
                                     :async true
                                     :effect (req (runner-install state side (assoc eid :source card :source-type :runner-install) target-card {:host-card (get-card state target)}))}
                                    card nil))))
                            ;;declined to install
                            (when (= where :deck)
                              (system-msg state side "shuffles the Stack"))))})]
    {:implementation "2v3"
     :on-install {:async true
                  :prompt "Install from where?"
                  :choices (req [(when-not (zone-locked? state :runner :discard) "Heap")
                                 "Grip" "Stack"])
                  :msg (msg "install a program from their " target)
                  :effect (effect (continue-ability
                                    (brownie-fn (if (= "Stack" target) :deck
                                                    (if (= "Grip" target) :hand
                                                        :discard)))
                                    card nil))}}))

(defcard "Kit's Toy"
  ;; Whenever this program fully breaks a code gate, place 1 power counter on this program.
  ;;
  ;; Interface → 1{credit}: Break 1 code gate subroutine.
  ;; Interface → 2{credit}, hosted power counter: Break up to 3 barrier subroutines.
  ;; 1{credit}: +2 strength."
  (auto-icebreaker {:abilities [(break-sub 1 1 "Code Gate")
                                (break-sub [:power 1 :credit 2] 3 "Barrier")
                                (strength-pump 1 2)]
                    :implementation "2v3"
                    :events [{:event :subroutines-broken
                              :req (req (and (all-subs-broken-by-card? target card)
                                             (has-subtype? target "Code Gate")))
                              :msg "place 1 power counter on itself"
                              :async true
                              :effect (effect (add-counter card :power 1)
                                              (effect-completed eid))}]}))

(defcard "\"Arsène\""
  ;; Whenever you breach R&D, if you will access 2 or more cards in R&D this breach,
  ;; access 1 additional card."
  {:implementation "2v3. Has a menu when you breach"
   :events [{:event :breach-server
             :req (req (and (= :rd target)))
             :prompt (msg "Access an additional card with " (:title card) "?")
             :choices {:number (req 1)
                       :default (req 0)}
             :msg (msg "access 1"
                       (quantify target "additional card")
                       " from R&D")
             :async true
             :effect (effect (access-bonus :rd target)
                             (effect-completed eid))}]})

;; NEUTRAL RUNNER

(defcard "AR Infinite Access"
  ;; Shuffle your grip and heap into your stack.
  ;; Remove the top 5 cards of your stack from the game, then draw 5 cards.
  ;; Remove AR Infinite Access from the game instead of trashing it."
  {:implementation "2v3"
   :on-play
   {:msg (msg (if (not (zone-locked? state :runner :discard))
                "shuffle their Grip and Heap into their Stack, remove the top 5 cards from the game, and then draw 5 cards"
                "shuffle their Grip into their Stack, remove the top 5 cards from the game, and then draw 5 cards"))
    :rfg-instead-of-trashing true
    :async true
    :effect (req (shuffle-into-deck state :runner :hand :discard)
                 (let [top-5 (vec (take 5 (:deck runner)))]
                   (doseq [c top-5]
                     (move state side c :rfg))
                   (system-msg state side
                               (str "removes "
                                    (str/join  ", " (map :title top-5))
                                    "from the game"))
                   (draw state :runner eid 5)))}})

(defcard "Valentina Ferreira"
  ;; The first time each turn you install a connection resource or remove a tag, gain 1{credit}.
  ;;
  ;; Threat 4 — When you install this resource, you may draw 2 cards."
  {:implementation "2v3"
   :on-install {:req (req (threat-level 4 state))
                :msg "draw 2"
                :async true
                :effect (effect (draw eid 2))}
   :events [{:event :runner-lose-tag
             :msg "gain 1 [Credit]"
             :req (req (first-event? state :runner :runner-lose-tag)
                       (no-event? state :runner :runner-install #(has-subtype? (:card (first %)) "Connection")))
             :async true
             :effect (effect (gain-credits eid 1))}
            {:event :runner-install
             :req (req (no-event? state :runner :runner-lose-tag)
                       (first-event? state :runner :runner-install #(has-subtype? (:card (first %)) "Connection")))
             :msg "gain 1 [Credit]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Thunderbolt Armaments"
  ;; Whenever you rez an AP or destroyer ice during a run,
  ;; it gets +2 strength and “[sub] The Runner must trash an installed resource or hardware.”
  ;; before its printed subroutines until the end of the run.
  ;; Each AP and destroyer ice in this deck costs 1 fewer influence."
  (let [thunderbolt-sub
        {:prompt "Choose a resource or hardware to trash"
         :label "Runner trashes a resource or hardware"
         :msg (msg "force the Runner to trash " (:title target))
         :player :runner
         :choices {:card #(and (installed? %)
                               (or (resource? %)
                                   (hardware? %)))}
         :async true
         :effect (effect (trash eid target {:cause :subroutine}))}]
    {:implementation "2v3"
     :events [{:event :run-ends
               :effect (req (let [cid (:cid card)
                                  ices (get-in card [:special :thunderbolt])]
                              (doseq [i ices]
                                (when-let [ice (get-card state i)]
                                  (remove-sub! state side ice #(= cid (:from-cid %))))))
                            (update! state side (dissoc-in card [:special :thunderbolt])))}
              {:event :rez
               :req (req (and run
                              (ice? (:card context))
                              (or (has-subtype? (:card context) "AP")
                                  (has-subtype? (:card context) "Destroyer"))))
               :msg (msg "give " (:title (:card context))
                         " +2 strength and \"[sub] The Runner must trash"
                         " an installed Resource or Hardware\" before it's printed subroutines")
               :async true
               :effect (effect (add-extra-sub! (get-card state (:card context))
                                               thunderbolt-sub
                                               (:cid card) {:front true})
                               (update! (update-in card [:special :thunderbolt]
                                                   #(conj % (:card context))))
                               (pump-ice (:card context) 2 :end-of-run)
                               (effect-completed eid))}]}))

;; HB HAAS BIOROID CARDS

(defcard "[Sleeper Control]"
  ;; When you score this agenda, place 2 agenda counters on it.
  ;; When a run begins, you may spend 1 hosted agenda counter to rez up to 2 ice
  ;; protecting that server, ignoring all costs.
  ;; When this turn ends, derez all ice protecting that server."
  (letfn [(sleeper-reset [zone]
            {:event :run-ends
             :duration :end-of-run
             :unregister-once-resolved true
             :msg (msg "derez all ice protecting " (zone->name [zone]))
             :async true
             :effect (req (let [target-ice (filter #(and (ice? %)
                                                         (rezzed? %)
                                                         (= (second (get-zone %)) zone))
                                                   (all-installed state :corp))]
                            (doseq [ice target-ice]
                              (derez state :corp ice))
                            (effect-completed state side eid)))})
          (sleeper-rez [state side targets card zone eid]
            (if (zero? (count targets))
              (do (register-events
                    state side card
                    [(sleeper-reset zone)])
                  (effect-completed state side eid))
              (wait-for (rez state :corp (make-eid state eid) (first targets) {:ignore-cost :all-costs})
                        (sleeper-rez state side (drop 1 targets) card zone eid))))]
    {:on-score {:effect (effect (add-counter card :agenda 2))
                :silent (req true)}
     :implementation "2v3"
     :events [{:event :run
               :async true
               :optional
               {:prompt (msg "Rez up to 2 ice protecting " (zone->name (:server context)))
                :yes-ability
                {:cost [:agenda 1]
                 :effect (req (let [current-server (first (:server (:run @state)))]
                                (continue-ability
                                  state side
                                  {:prompt (msg "select up to 2 ice protecting " (zone->name current-server) " to rez, ignoring all costs")
                                   :choices {:card #(and (ice? %)
                                                         (not (rezzed? %))
                                                         (system-msg state side current-server)
                                                         (= (second (get-zone %)) current-server))
                                             :max 2}
                                   :msg (msg "rez " (str/join " and " (map :title targets)))
                                   :async true
                                   :cancel-effect (req
                                                    (system-msg state side (str "uses " (:title card) " to rez 0 ice"))
                                                    (sleeper-rez state side [] card current-server eid))
                                   :effect (req (sleeper-rez state side targets card current-server eid))}
                                  card nil)))}}}]}))

(defcard "[\"Human\" Resources]"
  ;; When you rez this asset, load 3 power counters onto it. When it is empty, trash it.
  ;; [click], hosted power counter: Gain 2{credit} and draw 1 card. You may install a card from HQ."
  {:on-rez {:effect (effect (add-counter card :power 3))}
   :implementation "2v3"
   :abilities [{:cost [:click 1 :power 1]
                :msg "gain 2 [Credits] and draw 1 card"
                :async true
                :effect (req (wait-for (gain-credits state side 2)
                                       (wait-for (draw state side 1)
                                                 (continue-ability
                                                   state side
                                                   {:async true
                                                    :choices {:card #(and (corp? %)
                                                                          (in-hand? %)
                                                                          (not (operation? %)))}
                                                    :msg (msg (corp-install-msg target))
                                                    :effect (effect (corp-install eid target nil nil))
                                                    :cancel-effect (req (system-msg state side "declines to install a card")
                                                                        (effect-completed state side eid))}
                                                   card nil))))}]})

(defcard "[Will to Win]"
  ;; When your turn begins, you may derez a card.
  ;; Then, if there are no ice protecting this server, you may install a card from HQ.
  ;; You may not score that card this turn."
    (let [install {:req (req unprotected)
                 :prompt "install a card from HQ"
                 :async true
                 :choices {:card #(and (not (operation? %))
                                       (in-hand? %)
                                       (corp? %))}
                 :msg (msg (corp-install-msg target))
                 :effect (req (wait-for (corp-install state side (make-eid state eid) target nil nil)
                                        (let [installed-card async-result]
                                          (register-turn-flag!
                                            state side
                                            card :can-score
                                            (fn [state _ card]
                                              (if (same-card? card installed-card)
                                                ((constantly false) (toast state :corp "Cannot score due to Will to Win." "Warning"))
                                                true)))
                                          (effect-completed state side eid))))}
        derez {:label "derez a card (start of turn)"
               :async true
               :prompt "Derez a card"
               :choices {:card #(rezzed? %)}
               :effect (effect (derez target)
                               (continue-ability
                                 install
                                 card nil))
               :cancel-effect (effect (continue-ability install card nil))}]
    {:derezzed-events [corp-rez-toast]
     :implementation "2v3"
     :events [(assoc derez :event :corp-turn-begins)]
     :abilities [derez]}))

(defcard "[Cannonball]"
  ;; Subroutines on this ice cannot be broken by AI programs or by non-icebreaker cards.
  ;; {sub} Trash an installed resource.
  ;; {sub} If there are no installed resources, trash an installed piece of hardware.
  ;; {sub} If there are no installed pieces of hardware, trash an installed program.
  {:implementation "2v3 - breaking restriction not implemented"
   :subroutines [trash-resource-sub
                 (assoc trash-hardware-sub :req (req (not (some #(resource? %) (all-installed state :runner)))))
                 (assoc trash-program-sub :req (req (not (some #(hardware? %) (all-installed state :runner)))))]})

(defcard "Curare"
  ;; When you rez this ice, choose one or more of sentry, code gate, and barrier.
  ;;   This ice gains the chosen subtypes.
  ;; When a turn ends, derez this ice.
  ;; {sub} If this ice is a code gate, the Runner loses [click][click].
  ;; {sub} If this ice is a sentry, trash 1 program.
  ;; {sub} If this ice is a barrier, end the run."
  (letfn [(curare-choice [options]
            {:prompt "Choose a subtype for Curare or press 'Done'"
             :waiting-prompt "corp to add subtypes"
             :choices options
             :effect (req (if (= target "Done")
                            (effect-completed state side eid)
                            (do
                              (system-msg state side (str "uses " (:title card) " to make itself gain " target))
                              ;; feels spaghetti but it works
                              (update! state side (assoc card :subtype-target (cons target (:subtype-target (get-card state card)))))
                              (continue-ability
                                state side
                                (curare-choice (remove #{target} options))
                                card nil))))})]
    {:on-rez {:effect (effect (continue-ability (curare-choice ["Barrier" "Code Gate" "Sentry" "Done"]) card nil))}
     :implementation "2v3"
     :static-abilities [{:type :gain-subtype
                         :req (req (and (same-card? card target) (:subtype-target card)))
                         :value (req (:subtype-target card))}]
     :events [{:event :runner-turn-ends
               :req (req (rezzed? card))
               :effect (effect (derez :corp card))}
              {:event :corp-turn-ends
               :req (req (rezzed? card))
               :effect (effect (derez :corp card))}]
     :subroutines [{:label "(Code Gate) Force the Runner to lose [Click][Click]"
                    :msg "force the Runner to lose [Click][Click]"
                    :req (req (has-subtype? card "Code Gate"))
                    :effect (effect (lose-clicks :runner 2))}
                   {:prompt "Choose a program to trash"
                    :label "(Sentry) Trash a program"
                    :req (req (has-subtype? card "Sentry"))
                    :msg (msg "trash " (:title target))
                    :choices {:card #(and (installed? %)
                                          (program? %))}
                    :async true
                    :effect (effect (trash eid target {:cause :subroutine}))}
                   {:label "(Barrier) End the run"
                    :msg "end the run"
                    :req (req (has-subtype? card "Barrier"))
                    :async true
                    :effect (effect (end-run :corp eid card))}]}))

(defcard "[Tactical Ceasefire]"
  ;; Play only if the Runner trashed or stole a card during their last turn.
  ;; Derez a card to choose an installed Runner card with an install cost less than the
  ;;   printed rez cost of the derezzed card. Add the chosen Runner card to the grip.
  ;; Threat 4 — Reveal the grip. Put all cards in the Runner’s grip with the same type
  ;;   as the returned card on top of the stack in any order."
  (letfn [(cbi-final [chosen original]
            {:player :corp
             :prompt (str "The top cards of the Stack will be " (str/join  ", " (map :title chosen)) ".")
             :choices ["Done" "Start over"]
             :async true
             :effect (req (if (= target "Done")
                            (do
                              (system-msg state side (str "places " (str/join ", " (map :title chosen)) " on the top of the Stack (the first card is ontop)"))
                              (doseq [c (reverse chosen)]
                                (move state :runner c :deck {:front true}))
                              (effect-completed state side eid))
                            (continue-ability state side (cbi-choice original '() (count original) original)
                                              card nil)))})
          (cbi-choice [remaining chosen n original]
            {:player :corp
             :prompt "Choose a card to move next onto the Stack"
             :choices remaining
             :async true
             :effect (effect
                       (continue-ability
                         (let [chosen (cons target chosen)]
                           (if (< (count chosen) n)
                             (cbi-choice (remove-once #(= target %) remaining) chosen n original)
                             (cbi-final chosen original)))
                         card nil))})
          (choose-top-order [from]
            {:player :corp
             :waiting-prompt "Corp to make a decision"
             :async true
             :effect (req
                       (continue-ability
                         state side
                         (when (pos? (count from))
                           (cbi-choice from '() (count from) from))
                         card nil))})]
    {:on-play
     {:req (req (last-turn? state :runner :stole-agenda))
      :implementation "2v3"
      :prompt "Choose a card to derez"
      :choices {:card #(and (installed? %)
                            (rezzed? %))}
      :msg (msg "derez " (card-str state target))
      :async true
      :effect (req (let [rez-cost (:cost target)]
                     (derez state side target)
                     (continue-ability
                       state side
                       {:prompt (msg "return a runner card with cost less than " rez-cost " to the grip")
                        :choices {:card #(and (runner? %)
                                              (installed? %)
                                              (< (:cost %) rez-cost))}
                        :async true
                        :msg (msg "returns " (:title target) " to the grip")
                        :effect (req (move state :runner target :hand)
                                     (if (threat-level 4 state)
                                       (let [runner-hand (:hand (:runner @state))
                                             same-type (filter #(= (:type %) (:type target)) runner-hand)]
                                         (system-msg
                                           state :corp
                                           (str "uses " (:title card) " to reveal the Runner's Grip ("
                                                (str/join ", " (map :title (sort-by :title runner-hand)))
                                                ")"))
                                         (wait-for
                                           (reveal state side runner-hand)
                                           (continue-ability
                                             state side
                                             (choose-top-order same-type)
                                             card nil)))
                                       (effect-completed state side eid)))}
                       card nil)))}}))

(defcard "[Strategic Planning]"
  ;; As an additional cost to play this operation, spend {click}.
  ;; Draw 2 cards, gain 4{credit} and add 1 card from Archives to HQ."
  {:on-play {:msg "gain 4 [Credits] and draw 2 cards"
             :implementation "2v3"
             :async true
             :effect (req (wait-for (gain-credits state side 4)
                                    (wait-for (draw state side (make-eid state eid) 2)
                                              (continue-ability
                                                state side
                                                (corp-recur)
                                                card nil))))}})

(defcard "[Full Power Grid]"
  ;; When you rez an ice during a run against this server, you may derez another card.
  ;; If you do, the rezzed ice gains +X strength for the remainder of the run.
  ;; X is the printed rez cost of the derezzed card. Use this ability only once per run.
  ;; Limit 1 region per server."
  {:implementation "2v3"
   :events [{:event :rez
             :req (req (and (ice? (:card context))
                            this-server run
                            (some #(and (ice? %)
                                        (rezzed? %)
                                        (not (same-card? % (:card context))))
                                  (all-installed state :corp))))
             :effect (req
                       (let [rezzed-card (:card context)]
                         (continue-ability
                           state side
                           {:optional
                            {:prompt "derez another ice?"
                             :waiting-prompt (msg "corp to use " (:title card))
                             :once :per-run
                             :async true
                             :yes-ability {:choices {:card #(and (ice? %)
                                                                 (rezzed? %)
                                                                 (not (same-card? % rezzed-card)))}
                                           :async true
                                           :msg (msg "derez " (:title target) " to increase the strength of " (:title rezzed-card) " by " (:cost target) " for the remainder of the run")
                                           :effect (req (derez state side (get-card state target))
                                                        (pump-ice state side rezzed-card (:cost target) :end-of-run)
                                                        (effect-completed state side eid))}}}
                           card nil)))}]})

;; JINTEKI CARDS

(defcard "Psychic Profiling"
  ;; When you score this agenda, trash 1 installed resource and make a psi test.
  ;; (Players secretly bid up to 2{credit}. Then each player reveals and spends their bid.)
  ;; If the bids differ, give the Runner 1 tag."
  {:implementation "2v3"
   :on-score {:async true
              :msg (msg "trash " (:title target))
              :prompt "trash a resource"
              :choices {:card #(and (installed? %)
                                    (resource? %))}
              :effect (req (wait-for (trash state side target {:cause-card card})
                                     (continue-ability
                                       state side
                                       {:msg "start a psi game (give the runner a tag)"
                                        :async true
                                        :psi {:not-equal {:async true
                                                          :msg "give the runner a tag"
                                                          :effect (req (gain-tags state :runner eid 1))}}}
                                       card nil)))}})

(defcard "Puppet State"
  ;; The first time each turn the Runner passes a rezzed ice, you may pay 2{credit}.
  ;; If you do, the Runner encounters that ice again."
  {:implementation "2v3. This is a forced encounter."
   :events [{:event :pass-ice
             :req (req (and (rezzed? (:ice context))
                            (system-msg state side "Rezzed")
                            (first-event? state side :pass-ice
                                          (fn [targets]
                                            (let [context (first targets)]
                                              (rezzed? (:ice context)))))))
             :prompt (msg "pay 2 [Credits] to make the runner encounter " (:title (:ice context)) " again?")
             :choices (req [(when (can-pay? state :corp (assoc eid :source card :source-type :ability) card nil [:credit 2]) "Pay 2 [Credit]") "No thanks"])
             :effect (req (if (= target "No thanks")
                            (effect-completed state side eid)
                            (wait-for (pay state :corp (make-eid state eid) card :credit 2)
                                      (force-ice-encounter state side eid current-ice))))}]})

(defcard "[Carlos Izquierdo]"
  ;; You may advance this asset.
  ;; When your turn begins, you may remove 1 hosted advancement counter to
  ;; gain 4{credit} and draw 1 card.
  ;; {trash}, hosted advancement counter: Gain 2{credit} and draw 1 card."
  (let [ability {:label "gain 4/draw"
                 :optional {:once :per-turn
                            :prompt "take 4 [Credits] and draw a card?"
                            :req (req (pos? (get-counters card :advancement)))
                            :async true
                            :yes-ability {:msg "remove 1 advancement counter to gain 4 credits and draw a card"
                                          :async true
                                          :effect (req
                                                    (set-prop state side card :advance-counter (dec (get-counters card :advancement)))
                                                    (wait-for
                                                      (gain-credits state :corp (make-eid state eid) 4)
                                                      (draw state :corp eid 1)))}}}
        trash-ab {:cost [:advancement 1 :trash-can]
                  :label "Gain 2[Credits] and draw a card"
                  :effect (req (wait-for
                                 (gain-credits state :corp (make-eid state eid) 2)
                                 (draw state :corp eid 1)))}]
    {:advanceable :always
     :implementation "2v3"
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability trash-ab]}))

(defcard "Tributary"
  ;; Whenever a run begins on a remote server, you may move this ice to the
  ;; outermost position of the attacked server. (The Runner is now approaching this ice.)
  ;; Use this ability only once per turn.
  ;; [sub] You may install a piece of ice from HQ protecting another server.
  ;; [sub] Ice get +2 strength for the remainder of the run."
  {:implementation "2v3 - doesn't enforce NCIGS"
   :subroutines [{:label "Install a piece of ice from HQ protecting another server"
                  :prompt "Choose a piece of ice to install from HQ in another server"
                  :async true
                  :choices {:card #(and (ice? %)
                                        (in-hand? %))}
                  :effect (req (let [this (zone->name (second (get-zone card)))
                                     nice target]
                                 (continue-ability state side
                                                   {:prompt (str "Choose a location to install " (:title target))
                                                    :choices (req (remove #(= this %) (corp-install-list state nice)))
                                                    :async true
                                                    :effect (effect (corp-install eid nice target nil))}
                                                   card nil)))}
                 {:label "Give +2 strength to all ice for the remainder of the run"
                  :msg "give +2 strength to all ice for the remainder of the run"
                  :effect (effect (register-floating-effect
                                    card
                                    {:type :ice-strength
                                     :duration :end-of-run
                                     :value 2})
                                  (update-all-ice))}]
   :events [{:event :run
             :req (req (is-remote? (:server run)))
             :effect (req (let [target-server (:server run)]
                            (continue-ability
                              state side
                              {:optional
                               {:prompt (msg "move " (:title card) " to the outermost position of " (zone->name target-server) "?")
                                :yes-ability {:once :per-turn
                                              :msg (msg "move itself to the outermost position of " (zone->name target-server))
                                              :effect (req (let [moved-ice (move state side card [:servers (first target-server) :ices])];(conj target-server :ices))]
                                                             (system-msg state side moved-ice)
                                                             (redirect-run state side target-server)
                                                             (effect-completed state side eid)))}}}
                              card nil)))}]})

(defcard "Domino"
  ;; Threat 4 — This ice gets +2 strength.
  ;; {sub} Do 2 net damage.
  ;; {sub} You may trash 1 card from HQ to end the run.
  ;; {sub} You may trash 1 card from HQ to end the run."
  {:implementation "2v3"
   :constant-effects [(ice-strength-bonus (req (if (threat-level 4 state) 2 0)))]
   :subroutines [(do-net-damage 2)
                 {:label "trash a card from HQ to end the run"
                  :optional {:prompt "trash a card from HQ to end the run?"
                             :yes-ability {:cost [:trash-from-hand 1]
                                           :msg "end the run"
                                           :async true
                                           :effect (effect (end-run eid card))}}}
                 {:label "trash a card from HQ to end the run"
                  :optional {:prompt "trash a card from HQ to end the run?"
                             :yes-ability {:cost [:trash-from-hand 1]
                                           :msg "end the run"
                                           :async true
                                           :effect (effect (end-run eid card))}}}]})

(defcard "Hook"
  ;; When the Runner passes this ice, the Runner must choose a subroutine to
  ;;  resolve if this ice was rezzed this turn.
  ;; {sub} Do 3 net damage.
  ;; {sub} Give the Runner 2 tags.
  ;; {sub} Trash an installed resource or hardware."
  {:subroutines [(do-net-damage 3)
                 (give-tags 2)
                 trash-resource-or-hardware-sub]
   :events [{:event :pass-ice
             :req (req (and (= :this-turn (:rezzed card))
                            (same-card? (:ice context) card)))
             :msg "force the runner to choose a subroutine to resolve"
             :effect (effect (continue-ability
                               {:prompt "choose a subroutine"
                                :player :runner
                                :choices (req (map #(make-label (:sub-effect %)) (:subroutines card))) ;;(unbroken-subroutines-choice card))
                                :async true
                                :effect (req (let [sub (first (filter #(= target (make-label (:sub-effect %))) (:subroutines card)))]
                                               (resolve-subroutine! state side eid card (assoc sub :external-trigger true))))}
                               card nil))}]})

(defcard "[Protect the Family]"
  ;; Play only if the Runner made a successful run last turn.
  ;; After this operation is resolved, end your action phase.
  ;; The Runner adds 3 cards from their grip to the bottom of the stack in any order.
  ;; Threat 4 — The cards are randomly chosen from the grip instead."
  (letfn [(cbi-final [chosen original]
            {:player :runner
             :prompt (str "The bottom cards of the Stack will be " (str/join  ", " (map :title chosen)) " (last card is on the bottom).")
             :choices ["Done" "Start over"]
             :async true
             :effect (req (if (= target "Done")
                            (do
                              (system-msg state :runner (str "places " (count chosen) " cards on the bottom of the Stack"))
                              (doseq [c chosen]
                                (move state :runner c :deck {:back true}))
                              (effect-completed state side eid))
                            (continue-ability
                              state side (cbi-choice original '() (count original) original)
                              card nil)))})
          (cbi-choice [remaining chosen n original]
            {:player :runner
             :prompt "Choose a card to move next onto the bottom of the Stack"
             :choices remaining
             :async true
             :effect (effect
                       (continue-ability
                         (let [chosen (cons target chosen)]
                           (if (< (count chosen) n)
                             (cbi-choice (remove-once #(= target %) remaining) chosen n original)
                             (cbi-final chosen original)))
                         card nil))})
          (cbi-abi [from]
            (when (pos? (count from))
              (cbi-choice from '() (count from) from)))]
    {:async true
     :implementation "2v3"
     ;; this is a valid condition for taking 3 at random
     :effect (req (if (or (<= (count (:hand runner)) 3)
                          (threat-level 4 state))
                    (let [chosen-cards (take 3 (shuffle (:hand runner)))]
                      (continue-ability
                        state side
                        (cbi-abi chosen-cards)
                        card nil))
                    ;;runner chooses 3 of the cards
                    (continue-ability
                      state side
                      {:prompt (msg "choose 3 cards to add to the bottom of the Stack")
                       :player :runner
                       :choices {:card #(and (runner? %)
                                             (in-hand? %))
                                 :max 3
                                 :min 3}
                       :async true
                       :effect (req (continue-ability
                                      state side
                                      (cbi-abi targets)
                                      card nil))}
                      card nil)))}))

(defcard "Overseer Matrix";;"[Turnover]"
  ;; When your turn begins, choose 1 to resolve:
  ;; - Turn 1 faceup card in Archives facedown to gain 1{credit}.
  ;; - Turn 1 facedown card in Archives faceup to place 1 advancement counter on a installed card."
  {:implementation "2v3"
   :events [{:event :corp-turn-begins
             :prompt "Choose one"
             :interactive (req true)
             :choices (req [(when (some #(:seen %) (:discard corp)) "Turn a card facedown (gain 1)")
                            (when (some #(not (:seen %)) (:discard corp))
                              "Turn a card faceup (place 1 advancement)")
                            "Done"])
             :async true
             :effect (req (if (= target "Done")
                            (effect-completed state side eid)
                            (continue-ability
                              state side
                              (if (= target "Turn a card facedown (gain 1)")
                                {:prompt "Turn a card in Archives facedown"
                                 :choices {:card #(and (in-discard? %)
                                                       (corp? %)
                                                       (:seen %))}
                                 :msg (msg "turn " (:title target)
                                           " in archives facedown to gain 1 [Credits]")
                                 :show-discard true
                                 :effect (req (update! state side (assoc-in target [:seen] false))
                                              (gain-credits state side eid 1))}
                                {:prompt "Turn a card in Archives faceup"
                                 :choices {:card #(and (in-discard? %)
                                                       (corp? %)
                                                       (not (:seen %)))}
                                 :msg (msg "turn " (:title target) " in archives faceup")
                                 :show-discard true
                                 :async true
                                 :effect (req (update! state side (assoc-in target [:seen] true))
                                              (continue-ability
                                                state side
                                                {:prompt "Place 1 advancement token on an installed card"
                                                 :choices {:card #(and (corp? %)
                                                                       (installed? %))}
                                                 :msg (msg "place 1 advancement token on "
                                                           (card-str state target))
                                                 :effect (effect
                                                           (add-prop target
                                                                     :advance-counter 1
                                                                     {:placed true}))}
                                                card nil))})
                              card nil)))}]})

;; NBN CARDS

(defcard "[Fine Print Project]"
  ;; When you score this agenda, you may look at the top 3 cards of R&D;
  ;; from among those 3 cards or from HQ,
  ;; you may score an agenda with 1 or less printed agenda points.
  ;; Return the remaining looked at cards to the top of R&D in any order."
  {:implementation "2v3"
   :on-score
   {:optional
    {:prompt "Look at the top 3?"
     :yes-ability
     {:msg (msg "set aside top 3 cards of R&D")
      :async true
      :effect (req (set-aside-for-me state :corp eid (take 3 (:deck corp)))
                   (let [top-3 (get-set-aside state :corp eid)]
                     (continue-ability
                       state side
                       {:prompt "select an agenda worth 1 or less points"
                        :req (req true)
                        :async true
                        :choices {:req (req (and (agenda? target)
                                                 (>= 1 (:agendapoints target))
                                                 (or (some #{:hand} (:zone target))
                                                     (some #(same-card? % target) top-3))))}
                        :cancel-effect (req (continue-ability
                                              state side
                                              (return-to-top top-3)
                                              card nil))
                        :effect (req (wait-for (score state :corp (make-eid state eid) target {:no-req true})
                                               (let [rem (filter #(not (same-card? target %)) top-3)]
                                                 (if (not (empty? rem))
                                                   (continue-ability
                                                     state side
                                                     (return-to-top rem)
                                                     card nil)
                                                   (effect-completed state side eid)))))}
                       card nil)))}}}})

(defcard "[Policy Coordination]"
  ;; When you score this agenda, place 1 agenda counter on it for every advancement counter past 3.
  ;; Hosted agenda counter: Look at the top 3 cards of R&D and choose 1 of them;
  ;; install it or add it to HQ.
  ;; Return the remaining looked at cards to the top and/or bottom of R&D in any order.
  ;; Use this ability only during your turn."
  {:on-score {:silent (req true)
              :effect (effect (add-counter card :agenda (- (get-counters (:card context) :advancement) 3)))}
   :implementation "2v3"
   :abilities [{:cost [:agenda 1]
                :label "look at the top 3 cards"
                :msg (msg "set aside top 3 cards of R&D")
                :async true
                :effect (req (set-aside-for-me state :corp eid (take 3 (:deck corp)))
                             (let [top-3 (get-set-aside state :corp eid)]
                               (continue-ability
                                 state side
                                 {:prompt "Choose a card"
                                  :req (req true)
                                  :async true
                                  :choices {:req (req (some #(same-card? % target) top-3))}
                                  :cancel-effect (effect (continue-ability (return-to-top-or-bottom top-3) card nil))
                                  :effect (req
                                            (let [chosen-card target
                                                  rem (filter #(not (same-card? chosen-card %)) top-3)]
                                              (continue-ability
                                                state side
                                                {:prompt (msg "Install " (:title target) " or add it to HQ?")
                                                 :choices (req ["Add to HQ" (when-not (operation? chosen-card) (str "Install " (:title chosen-card)))])
                                                 :async true
                                                 :effect (req (if (= target "Add to HQ")
                                                                (do (system-msg state side "moves a card to HQ")
                                                                    (move state :corp (get-card state chosen-card) :hand)
                                                                    (continue-ability
                                                                      state side
                                                                      (return-to-top-or-bottom rem)
                                                                      card nil))
                                                                (do (wait-for (corp-install state :corp chosen-card nil)
                                                                              (continue-ability
                                                                                state side
                                                                                (return-to-top-or-bottom rem)
                                                                                card nil)))))}
                                                card nil)))}
                                 card nil)))}]})

(defcard "Theoretical Economist"
  ;; When your turn begins, place 3{credit} on this asset.
  ;; {click}: Take all credits from this asset. Add this asset to HQ. Then, you may install a card."
  (let [ability {:label "Place 3 credits (start of turn)"
                 :once :per-turn
                 :msg "place 3 credits on itself"
                 :effect (effect (add-counter card :credit 3 {:placed true}))}]
    {:derezzed-events [corp-rez-toast]
     :implementation "2v3"
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability
                 {:cost [:click 1]
                  :label "take all credits and add to HQ"
                  :async true
                  :msg (msg "take " (get-counters (get-card state card) :credit) " credits and add itself to HQ")
                  :effect (req (wait-for (gain-credits state side (make-eid state eid)
                                                       (get-counters (get-card state card) :credit))
                                         (move state :corp card :hand)
                                         (continue-ability
                                           state side
                                           {:async true
                                            :prompt "Choose a card to install from HQ"
                                            :choices {:card #(and (not (operation? %))
                                                                  (in-hand? %)
                                                                  (corp? %))}
                                            :msg (msg (corp-install-msg target))
                                            :effect (effect (corp-install eid target nil nil))}
                                           card nil)))}]}))

(defcard "Doomsday Clock"
  ;; When the Runner encounters this ice, they must either lose 3{credit},
  ;; take 2 net damage, or take 1 tag.
  ;; {sub} The Runner loses 3{credit}.
  ;; {sub} Do 2 net damage.
  ;; {sub} The Runner takes 1 tag."
  (let [encounter-ab
        {:prompt "choose one"
         :player :runner
         :choices ["lose 3 credits" "take 2 net damage" "gain 1 tag"]
         :msg (msg "force the runner to " target)
         :effect (req (cond
                        (= "lose 3 credits" target) (lose-credits state :runner eid 3)
                        (= "take 2 net damage" target) (damage state :corp eid :net 2 {:card card})
                        (= "gain 1 tag" target) (gain-tags state :corp eid 1)))}]
    {:on-encounter encounter-ab
     :implementation "2v3"
     :subroutines [(runner-loses-credits 3)
                   (do-net-damage 2)
                   (give-tags 1)]}))

(defcard "Ultra High Frequency"
  ;; {sub} You may draw 1 or 2 cards.
  ;; {sub} Do 1 net damage.
  ;; {sub} If there are more cards in HQ than cards in the grip, end the run."
  {:implementation "2v3"
   :subroutines [{:async true
                  :label "Draw up yo 2 cards"
                  :prompt "Draw how many cards?"
                  :choices {:number (req 2)
                            :max (req 2)
                            :min (req 0)
                            :default (req 2)}
                  :msg (msg "draw " (quantify target "card"))
                  :effect (effect (draw eid target))}
                 (do-net-damage 1)
                 {:label "End run HQ > Grip"
                  :async true
                  :effect (req (let [hq (count (:hand corp))
                                     grip (count (:hand runner))]
                                 (continue-ability
                                   state side
                                   (if (> hq grip)
                                     {:msg "end the run"
                                      :async true
                                      :effect (req (end-run state :corp eid card))}
                                     {:msg "do nothing"})
                                   card nil)))}]})

(defcard "Capacitor"
  ;; This ice gains +1 strength for each tag the Runner has.
  ;; {sub} Gain 1{credit} for each tag the Runner has.
  ;; {sub} End the run."
  {:implementation "2v3"
   :constant-effects [(ice-strength-bonus (req (count-tags state)))]
   :subroutines [{:label "Gain 1 [Credits] for each tag"
                  :async true
                  :msg (msg "Gain " (count-tags state) " [Credits]")
                  :effect (effect (gain-credits :corp eid (count-tags state)))}
                 end-the-run]})

(defcard "[Creative Business Practices]"
  ;; Set aside the top 3 cards of R&D facedown. You can look at those cards.
  ;; You may play 1 non-terminal operation from among the set-aside cards, paying 3{c} less.
  ;; Threat 4 -> You may play any number of other non-terminal operations from among
  ;; the set-aside cards.
  ;; Arrange the remaining set-aside cards in any order and add them to the top of R&D."
  (letfn [(maybe-play-op [set-aside discount]
          {:prompt (msg "Choose a non-terminal operation to play " (when discount (str "paying " discount "[Credits] less")))
           :choices (req (conj (filter #(and (operation? %)
                                             (not (has-subtype? % "Terminal"))
                                             (should-trigger? state :corp (assoc eid :source % :source-type :play) % nil (or (:on-play (card-def %)) {}))
                                             (can-pay? state side (assoc eid :source % :source-type :play) % nil [:credit (play-cost state side % {:cost-bonus discount})]))
                                       set-aside)
                               "Done"))
           :async true
           :effect (req
                     (if (= target "Done")
                       (continue-ability
                         state side
                         (return-to-top set-aside)
                         card nil)
                       (wait-for (play-instant state side (assoc (make-eid state eid) :source target :source-type :play) target
                                               {:cost-bonus [:credit discount]})
                                 (let [rem (filter #(not (same-card? % target)) set-aside)]
                                   (continue-ability
                                     state side
                                     (if (and (threat-level 4 state) (not (zero? (count rem))))
                                       (maybe-play-op rem 0)
                                       (return-to-top rem))
                                     card nil)))))})]
  {:implementation "2v3"
   :on-play {:msg (msg "set aside the top 3 cards of R&D facedown")
             :async true
             :effect (req (set-aside-for-me state :corp eid (take 3 (:deck corp)))
                          (let [top-3 (get-set-aside state :corp eid)]
                            (continue-ability
                              state side
                              (maybe-play-op top-3 -3)
                              card nil)))}}))

(defcard "[Holo Man]"
  ;; Persistent → Whenever the Runner accesses another card in the root of this server,
  ;; you may install the accessed card in the root of another new or existing server.
  ;; (It is no longer being accessed.)
  ;; When your turn ends, you may move this upgrade to the root of another server.
  (let [abi
        {:event :access
         :interactive (req true)
         :req (req (and (not (same-card? target card))
                        (installed? target)
                        (same-server? target card)))
         :async true
         :effect (req (let [accessed-card target]
                        (continue-ability
                          state side
                          {:optional
                           {:prompt (msg "Install " (:title accessed-card) " in the root of another server?")
                            :async true
                            :waiting-prompt "corp to use Holo Man"
                            :yes-ability {:prompt (str "Choose a location to install " (:title target))
                                          :waiting-prompt "corp to use Holo Man"
                                          :choices (req (remove #(= (zone->name (second (get-zone accessed-card))) %) (corp-install-list state accessed-card)))
                                          :async true
                                          :msg (msg (corp-install-msg accessed-card))
                                          :effect (req
                                                    (swap! state dissoc :access)
                                                    (corp-install state side eid accessed-card target nil))}}}
                          card nil)))}]
    {:implementation "2v3"
     :on-trash {:req (req (and (= :runner side)
                                (:run @state)))
                :effect (req (register-events
                              state side card
                              [(assoc abi :req (req (and (installed? target) (= (get-zone target) (:previous-zone card)))) :duration :end-of-run)]))}
     :events [abi
              {:event :corp-turn-ends
               :optional {:prompt (msg "move " (:title card) " to another server?")
                          :yes-ability {:async true
                                        :effect (effect (continue-ability
                                                          {:prompt "Choose a server"
                                                           :choices (server-list state)
                                                           :msg (msg "move to " target)
                                                           :effect (req (let [c (move state side card
                                                                                      (conj (server->zone state target) :content))]
                                                                          (unregister-events state side card)
                                                                          (register-default-events state side c)))}
                                                          card nil))}}}]}))

;; WEYLAND CARDS



;; NEUTRAL CORP
