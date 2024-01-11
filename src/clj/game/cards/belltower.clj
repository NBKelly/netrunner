(ns game.cards.belltower
  (:require
   [clojure.string :as str]
   [game.cards.upgrades :refer [mobile-sysop-event]]
   [game.cards.ice :refer [do-psi end-the-run end-the-run-unless-runner end-the-run-unless-runner-pays gain-credits-sub runner-loses-credits runner-loses-click runner-pays runner-trash-installed-sub trash-resource-sub trash-hardware-sub trash-program-sub give-tags gain-variable-subs reset-variable-subs trash-installed-sub]] ;;reffing the other card file might not be ideal
   [game.core.access :refer [access-bonus access-card breach-server set-only-card-to-access steal-cost-bonus]]
   [game.core.actions :refer [score]]
      [game.core.agendas :refer [update-all-advancement-requirements
                              update-all-agenda-points]]
   [game.core.board :refer [all-active-installed all-installed get-all-cards get-remotes get-remote-names card->server  server->zone server-list]]
   [game.core.card-defs :refer [card-def]]
   [game.core.card :refer [active? agenda? can-be-advanced? card-index console? corp? corp-installable-type? event? facedown? faceup? get-card get-counters get-zone hardware? has-subtype? ice? installed? in-discard? in-hand? in-deck? is-type? operation? program? rezzed? resource? runner?]]
   [game.core.checkpoint :refer [fake-checkpoint]]
   [game.core.cost-fns :refer [trash-cost install-cost play-cost rez-cost]]
   [game.core.damage :refer [damage damage-prevent]]
   [game.core.def-helpers :refer [breach-access-bonus corp-recur corp-rez-toast defcard do-brain-damage do-net-damage reorder-choice trash-on-empty get-x-fn rfg-on-empty]]
   [game.core.diffs :refer [playable?]]
   [game.core.drawing :refer [draw draw-up-to maybe-draw]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.effects :refer [any-effects register-lingering-effect unregister-lingering-effects unregister-effects-for-card]]
   [game.core.engine :refer [checkpoint not-used-once? pay register-default-events register-events register-once register-suppress resolve-ability should-trigger? trigger-event unregister-events unregister-suppress-by-uuid print-msg]]
   [game.core.events :refer [first-event? first-run-event? no-event? run-events last-turn?]]
   [game.core.expend :refer [expend]]
   [game.core.finding :refer [find-cid]]
   [game.core.flags :refer [register-turn-flag! register-run-flag! is-scored? can-host? zone-locked?]]
   [game.core.gaining :refer [gain-clicks gain-credits lose-clicks lose-credits]]
   [game.core.hand-size :refer [hand-size runner-hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [add-sub! add-extra-sub! all-subs-broken-by-card? any-subs-broken? auto-icebreaker breaker-strength-bonus break-sub break-subroutine! get-strength ice-strength ice-strength-bonus pump-ice remove-subs! remove-sub! resolve-subroutine! strength-pump unbroken-subroutines-choice update-all-ice update-all-icebreakers]]
   [game.core.initializing :refer [card-init]]
   [game.core.installing :refer [corp-install corp-install-list corp-install-msg install-locked? runner-can-install? runner-can-pay-and-install? runner-install install-as-condition-counter]]
   [game.core.memory :refer [mu+]]
   [game.core.moving :refer [as-agenda as-agenda-special flip-facedown flip-faceup mill move swap-cards trash trash-cards]]
   [game.core.payment :refer [can-pay? cost-value]]
   [game.core.play-instants :refer [can-play-instant? play-instant]]
   [game.core.prompts :refer [cancellable]]
   [game.core.props :refer [add-counter add-prop set-prop]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [active-encounter? bypass-ice continue force-ice-encounter get-current-encounter end-run make-run redirect-run set-next-phase successful-run-replace-breach start-next-phase total-cards-accessed get-runnable-zones]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name in-same-server? is-central? is-remote? from-same-server? protecting-same-server? same-server? target-server remote->name unknown->kw zone->name zones->sorted-names]]
   [game.core.set-aside :refer [set-aside set-aside-for-me get-set-aside]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck]]
   [game.core.tags :refer [gain-tags lose-tags]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.threat :refer [threat threat-level]]
   [game.core.update :refer [update!]]
   [game.core.winning :refer [check-win-by-agenda win]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

;; helpers
(defn not-tagged-req
  [state]
  (= 0 (count-tags state)))

;; helper for the faceup-archives-count cards
(defn faceup-archives-types [corp]
  (count (distinct (map :type (filter faceup? (:discard corp))))))

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

(defn return-to-top-or-bottom-of-either
  "returns a set of cards to the top or bottom or either deck with an arbitrary controller"
  ([player set-aside-cards] (return-to-top-or-bottom-of-either player set-aside-cards false))
  ([player set-aside-cards reveal]
   (return-to-top-or-bottom-of-either player set-aside-cards reveal 0 0))
  ([player set-aside-cards reveal sent-top sent-bot]
   (let [tp (:side (first set-aside-cards))
         td (if (= tp :corp) "R&D" "the Stack")]
     {:prompt (msg "Choose a card to place on the top or bottom of " td)
      :req (req (not (zero? (count set-aside-cards))))
      :player player
      :choices (req (sort-by :title set-aside-cards)) ;; they may not be in a public zone
      :not-distinct true
      :async true
      :waiting-prompt "opponent to manipulate cards"
      :effect (req (let [chosen-card target
                         ;; can't hurt - this is a fallback if the cards are non-homo
                         ;; sides sides for some reason
                         tp (keyword (decapitalize (:side chosen-card)))
                         td (if (= tp :corp) "R&D" "the Stack")]
                     (system-msg state side tp)
                     (continue-ability
                       state side
                       {:choices [(str "Top of " td) (str "Bottom of " td)]
                        :player player
                        :async true
                        :prompt (msg "place " (:title chosen-card) " where?")
                        :msg (msg "place " (if reveal (:title chosen-card) "a card") " on the "
                                  (decapitalize target))
                        :effect (req (move state tp chosen-card :deck
                                           {:front (= target (str "Top of " td))})
                                     (let [rem (seq (filter #(not (same-card? chosen-card %))
                                                            set-aside-cards))]
                                       (let [sent-bot (if (= target (str "Bottom of " td))
                                                        (inc sent-bot) sent-bot)
                                             sent-top (if (= target (str "Top of " td))
                                                        (inc sent-top) sent-top)]
                                       (if-not (empty? rem)
                                         (continue-ability
                                           state side
                                           (return-to-top-or-bottom-of-either
                                             player rem reveal sent-top sent-bot)
                                           card nil)
                                         ;; todo - if reveal, print the top/bottom in the log
                                         (do
                                           (let [tpl (tp @state)]
                                             (when (and reveal (not (zero? sent-top)))
                                               (system-msg
                                                 state side
                                                 (str "the top cards of " td " are (top to bottom): "
                                                      (str/join ", "
                                                                (map :title (take sent-top (:deck tpl)))))))
                                             (when (and reveal (not (zero? sent-bot)))
                                               (system-msg
                                                 state side
                                                 (str "the bottom cards of " td " are (top to bottom): "
                                                      (str/join ", "
                                                                (map :title (reverse (take sent-bot (reverse (:deck tpl))))))))))
                                           (effect-completed state player eid))))))}
                       card nil)))})))

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
                      :effect (req (move state (:side target) chosen-card :deck {:front (= target "top of R&D")})
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
  {:implementation "2v5. Cost to trash connections not implemented"
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

(defcard "[Rescue Mission]"
  ;;Play only if you are not tagged.
  ;; Run archives. If successful, instead of breaching,
  ;; take 1 tag and install a resource from your heap, paying 2{credits} less.
  ;;
  ;; Threat 3 -- Then, you may install a program or hardware from your heap.
  (letfn [(resolve-threat [state side card eid]
            (if-not (threat-level 3 state)
              (effect-completed state side eid)
              (continue-ability
                state side
                {:prompt (msg "Install a program or hardware from your heap")
                 :choices (req (concat
                                 (->> (:discard runner)
                                      (filter
                                        #(and (or (hardware? %)
                                                  (program? %))
                                              (can-pay? state side
                                                       (assoc eid :source card :source-type :runner-install)
                                                       % nil [:credit (install-cost state side %)])))
                                      (sort-by :title)
                                      (seq))
                                 ["Done"]))
                 :effect (req (if (= target "Done")
                                (effect-completed state side eid)
                                (runner-install state side (assoc eid :source card :source-type :runner-install) target)))}
                card nil)))]
    {:implementation "2v5"
     :makes-run true
     :on-play {:req (req (and hq-runnable
                              (not-tagged-req state)))
               :async true
               :effect (effect (make-run eid :archives card))}
     :events [(successful-run-replace-breach
                {:target-server :archives
                 :this-card-run true
                 :ability {:async true
                           :effect (req (wait-for
                                          (gain-tags state :runner 1)
                                          (continue-ability
                                            state side
                                            {:prompt (msg "Install a resource from your heap, "
                                                          "paying 2[Credits] less")
                                             :choices (req (concat
                                                             (->> (:discard runner)
                                                                  (filter
                                                                    #(and (resource? %)
                                                                          (can-pay? state side
                                                                                   (assoc eid :source card :source-type :runner-install)
                                                                                   % nil [:credit (max 0 (- (install-cost state side %) 2))])))
                                                                  (sort-by :title)
                                                                  (seq))
                                                             ["Done"]))
                                                :async true
                                             :msg (msg (if (= target "Done")
                                                         "decline to install a resource"
                                                         (str "install " (:title target) " from the heap, paying 2[Credits] less")))
                                             :effect (req (if (= target "Done")
                                                            (resolve-threat state side card eid)
                                                            (wait-for (runner-install state side (make-eid state (assoc eid :source card :source-type :runner-install)) target {:cost-bonus -2})
                                                                      ;; threat
                                                                      (resolve-threat state side card eid))))}
                                            card nil)))}})]}))

;; REPLACED in v4
;;
;;(defcard "Eye for an Eye"
  ;; Play only if you are not tagged.
  ;; Run archives. If successful, instead of breaching Archives,
  ;; take 1 tag and the corp trashes 1 of their install cards.
  ;; Threat 5 - if you took a tag this way, the Corp trashes another 1 of their installed cards.
  ;; Remove this event from the game
;;  (letfn [(trash-x-installed [x state]
;;            (if (<= x (count (all-installed state :corp)))
;;              {:player :corp
;;               :prompt (str "Choose " (quantify x "card") " to trash")
;;               :waiting-prompt "Corp to trash a card"
;;               :msg (msg "trash " (:title target))
;;               :choices {:max x
;;                         :card #(and (installed? %)
;;                                     (corp? %))}
;;               :async true
;;               :effect (req (trash-cards state side eid targets {:cause :forced-to-trash}))}
;;              {:player :corp
;;               :msg "has no installed cards to trash"}))]
;;    {:makes-run true
;;     :implementation "2v3"
;;     :rfg-instead-of-trashing true
;;     :on-play {:req (req (and archives-runnable
;;                              (not-tagged-req state)))
;;               :async true
;;               :effect (effect (make-run eid :archives card))}
;;     :events [(successful-run-replace-breach
;;                {:target-server :archives
;;                 :this-card-run true
;;                 :ability
;;                 {:async true
;;                  :effect
;;                  (req (let [old-tags (count-tags state)]
;;                         (wait-for (gain-tags state :runner 1)
;;                                   (let [new-tags (count-tags state)]
;;                                     (wait-for (resolve-ability
;;                                                 state side
;;                                                 (trash-x-installed 1 state)
;;                                                 card nil)
;;                                               (if (and (= new-tags (+ 1 old-tags))
;;                                                       (threat-level 5 state))
;;                                                 (continue-ability
;;                                                   state side
;;                                                   (trash-x-installed 1 state)
;;                                                   card nil)
;;                                                 (effect-completed state side eid)))))))}})]}))

(defcard "Eye for an Eye"
  ;; Play only if you are not tagged.
  ;; Run HQ. If successful, take 1 tag and access 1 additional card when you breach HQ.
  ;; Access -> Trash a card from your grip: trash the card you are accessing
  {:implementation "2v5"
   :makes-run true
   :on-play {:req (req (and hq-runnable
                            (not-tagged-req state)))
             :async true
             :effect (effect (make-run eid :hq card))}
   :interactions {:access-ability
                  {:label "Trash card"
                   :cost [:trash-from-hand 1]
                   :msg (msg "trash " (:title target))
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
  ;; During the first encounter each run, the encountered ice gets -1 strength.
  ;;
  ;; Threat 3 - [trash], take 1 tag: If the ice you are encountering has 0 or less strength,
  ;;                                 trash it.
  {:implementation "2v6.1"
   ;; :events [{:event :encounter-ice
   ;;           :once :per-run
   ;;           :effect (effect
   ;;                     (register-lingering-effect
   ;;                       card
   ;;                       (let [target-ice (:ice context)]
   ;;                         {:type :ice-strength
   ;;                          :duration :end-of-encounter
   ;;                          :req (req (same-card? target target-ice))
   ;;                          :value -1}))
   ;;                     (update-all-ice))}]
   ;; :static-abilities [{:type :ice-strength
   ;;                     :req (req (and
   ;;                                 (get-current-encounter state)
   ;;                                 (= (:cid target) (:cid current-ice))))
   ;;                     :value (req (* -1 (count (filter #(has-subtype? % "Trojan")
   ;;                                                       (:hosted (get-card state current-ice))))))}]
   :abilities [{:req (req (active-encounter? state))
                               ;; (not (pos? (ice-strength state side current-ice)))))
                ;; (pos? (count (filter #(has-subtype? % "Trojan")
                ;;                            (:hosted current-ice))))))
                :cost[:gain-tag 1]
                :once :per-run
                :label "give encountered ice -2 strength"
                :msg (msg "give " (card-str state current-ice) " -2 strength this run")
                :effect (effect (pump-ice current-ice -2 :end-of-run))}
               {:label "trash 0-strength encountered ice"
                :async true
                :req (req (and (active-encounter? state)
                               (not (pos? (ice-strength state side current-ice)))))
                :cost [:credit 2 :trash-can]
                :effect (effect (trash eid current-ice {:cause-card card}))
                :msg (msg "trash " (card-str state current-ice))}]})

(defcard "[Onion]"
  ;; +1 mu
  ;;
  ;; When your turn ends, if you are tagged, you may place 1 hosted power counter on this hardware.
  ;;
  ;; Whenever you remove a tag, you may spend 1 hosted power counter to draw 2 cards.
  {:static-abilities [(mu+ 1)]
   :implementation "2v5 - placing power counters is automatic, manually adjust if you don't want 'em"
   :events [{:event :runner-lose-tag
             :optional {:prompt "Draw 2 cards?"
                        :yes-ability {:cost [:power 1]
                                      :msg "draw 2 cards"
                                      :async true
                                      :effect (req (draw state :runner eid 2))}}}
             {:event :runner-turn-ends
              :req (req (not (not-tagged-req state)))
              :effect (req (add-counter state side (get-card state card) :power 1))}]})

(defcard "The Wizard's Chest"
  (letfn [(install-choice [state side eid card rev-str first-card second-card]
            (continue-ability
              state side
              {:prompt "Choose one"
               :choices [(str "Install " (:title first-card))
                         (str "Install " (:title second-card))
                         "No thanks"]
               :msg (msg "reveal " rev-str " from the top of the stack"
                         (when-not (= target "No thanks")
                           (str "and " (decapitalize target) ", ignoring all costs")))
               :effect (req (if-not (= target "No thanks")
                              (wait-for (runner-install
                                          state side
                                          (make-eid state {:source card :source-type :runner-install})
                                          (if (= target (str "Install " (:title first-card)))
                                            first-card second-card)
                                          {:ignore-all-cost true})
                                        (shuffle! state side :deck)
                                        (system-msg state side "shuffles the Stack")
                                        (effect-completed state side eid))
                              (do (shuffle! state side :deck)
                                  (system-msg state side "shuffles the Stack")
                                  (effect-completed state side eid))))}
              card nil))
          (wiz-search-fn [state side eid card remainder type rev-str first-card]
            (if (seq remainder)
              (let [revealed-card (first remainder)
                    rest-of-deck (rest remainder)
                    rev-str (if (= "" rev-str)
                              (:title revealed-card)
                              (str rev-str ", " (:title revealed-card)))]
                (if (is-type? revealed-card type)
                  (if-not first-card
                    (wiz-search-fn state side eid card rest-of-deck type rev-str revealed-card)
                    (install-choice state side eid card rev-str first-card revealed-card))
                  (wiz-search-fn state side eid card rest-of-deck type rev-str first-card)))
              (continue-ability
                state side
                {:msg (msg "reveal " rev-str " from the top of the stack")
                 :effect (effect (shuffle! :deck)
                                 (system-msg "shuffles the Stack"))}
                card nil)))]
    {:implementation "2v5"
     :abilities
     [{:cost [:trash-can]
       :label "Set aside cards"
       :prompt "Choose a type"
       :choices (req (cancellable ["Hardware" "Program" "Resource"]))
       :req (req (and (some #{:hq} (:successful-run runner-reg))
                      (some #{:rd} (:successful-run runner-reg))
                      (some #{:archives} (:successful-run runner-reg))))

       :async true
       :effect (effect (wiz-search-fn eid card (:deck runner) target "" nil))}]}))
  ;; When you install this hardware, search your stack for 2 differently named cards
  ;; Trash 1 of them at random. Host the other face-up on this hardware.
  ;; When you steal an agenda, you MAY install the hosted card, ignoring all costs, then trash this
  ;; hardware.
  ;; Threat 4 --> (4 clicks) - install the hosted card, ignoring all costs
  ;; (letfn [(search-and-host [chosen]
  ;;           (let [rem (- 2 (count chosen))]
  ;;             {:prompt (msg "Choose a card (" rem " remaining)")
  ;;              :choices (req (concat
  ;;                              (sort-by :title
  ;;                                       (filter #(or (empty? chosen)
  ;;                                                    (not= (:title %) (:title (first chosen))))
  ;;                                               (:deck runner)))
  ;;                              ["Done"]))
  ;;              :async true
  ;;              :effect (req (if (= target "Done")
  ;;                             ;; is there a card to trash
  ;;                             (do (system-msg state side "declines to find a card")
  ;;                                 (if (zero? (count chosen))
  ;;                                   (effect-completed state side eid)
  ;;                                   (do (system-msg state side
  ;;                                                   (str "trashes " (:title (first chosen))))
  ;;                                       (trash-cards state side eid chosen {:unpreventable :true
  ;;                                                                           :cause-card card}))))
  ;;                             (if (= rem 2)
  ;;                               (continue-ability
  ;;                                 state side
  ;;                                 (search-and-host (conj chosen target))
  ;;                                 card nil)
  ;;                               ;; we selected 2 and didn't click done
  ;;                               (let [selection (shuffle (conj chosen target))
  ;;                                     selected-card (first selection)
  ;;                                     trashed-card (second selection)]
  ;;                                 (system-msg state side (str "randomly selects "
  ;;                                                             (:title trashed-card) " to trash"))
  ;;                                 (wait-for (trash state side (make-eid state eid) trashed-card
  ;;                                                  {:unpreventable :true
  ;;                                                   :cause-card card})
  ;;                                           (system-msg state side (str "hosts "
  ;;                                                                       (:title selected-card)
  ;;                                                                       " on " (:title card)))
  ;;                                           (host state side card selected-card)
  ;;                                           (effect-completed state side eid))))))}))
  ;;         (minor-install [trashes]
  ;;           {:msg (msg "install " (:title (first (:hosted card))) ", ignoring all costs")
  ;;            :async true
  ;;            :effect (req (let [target-card (first (:hosted card))
  ;;                               new-eid (make-eid state (assoc eid :source card
  ;;                                                              :source-type :runner-install))]
  ;;                           (wait-for (runner-install
  ;;                                       state side
  ;;                                       new-eid target-card {:ignore-all-cost true})
  ;;                                     (if trashes
  ;;                                       (trash state side eid card {:cause-card card})
  ;;                                       (effect-completed state side eid)))))})
  ;;         (install-hosted [trashes]
  ;;           {:optional
  ;;            {:prompt (msg "Install " (:title (first (:hosted card))) ", ignoring all costs?")
  ;;             :waiting-prompt "Runner to make a decision"
  ;;             :req (req (and (not (event? (first (:hosted card))))
  ;;                            (runner-can-install? state side (first (:hosted card)) nil)))
  ;;             :yes-ability (minor-install trashes)
  ;;             :no-ability {:msg (msg "declines to install a card")}}})]
  ;;   {:implementation "2v4"
  ;;    :on-install {:msg "search 2 cards from the stack"
  ;;                 :async true
  ;;                 :effect (req (wait-for (resolve-ability state side
  ;;                                                         (make-eid state eid)
  ;;                                                         (search-and-host [])
  ;;                                                         card nil)
  ;;                                        (trigger-event state side :searched-stack nil)
  ;;                                        (shuffle! state side :deck)
  ;;                                        (system-msg state side "shuffle the stack")
  ;;                                        (effect-completed state side eid)))}
  ;;    :abilities [(assoc (minor-install false)
  ;;                       :cost [:click 4]
  ;;                       :label "(threat 4) install hosted card, ignoring all costs"
  ;;                       :req (req (and (seq (:hosted card))
  ;;                                      (threat-level 4 state)
  ;;                                      (not (event? (first (:hosted card))))
  ;;                                      (runner-can-install? state side
  ;;                                                           (first (:hosted card)) nil))))]
  ;;    :events [(assoc (install-hosted true) :event :agenda-stolen)]}))


(defcard "Heliamphora"
  ;; Whenever you breach Archives, you may remove 1 of the card you would access from the game
  ;; Whenever the Corp purges virus counters, the Corp trashes 2 cards from HQ,
  ;; then trash this program."
  ;; TODO - show corp discard when runner access this?
  ;;        perhaps a "show-opponent-discard" key?
  {:implementation "2v6"
   :events [{:event :breach-server
             :async true
             :interactive (req true)
             :req (req (and (= target :archives)
                            (not-empty (:discard corp))))
             :effect (req (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                          (continue-ability
                            state side
                            {:optional
                             {:prompt "Host a card on Heliamphora?"
                              :yes-ability {:prompt "Choose a card in Archives"
                                            :choices (req (:discard corp))
                                            :msg (msg "host " (:title target) " from the game")
                                            :effect (effect (host card target))}}}
                            ;;(move :corp target :rfg))}}}
                            card nil))}
            {:event :purge
             :msg "force the Corp to trash 2 cards from HQ at random, then trash itself"
             :async true
             :effect (req ;;(if (>= 2 (count (:hand corp)))
                       (wait-for
                         (trash-cards state :corp (make-eid state eid)
                                      (take 2 (shuffle (:hand corp))) {:cause-card card})
                         (trash state :runner eid card {:cause :purge :cause-card card})))}]})
                            ;; (continue-ability
                            ;;   state :corp
                            ;;   {:prompt "Choose 2 cards to trash from HQ"
                            ;;    :choices {:max 2
                            ;;              :all true
                            ;;              :card #(and (in-hand? %)
                            ;;                          (corp? %))}
                            ;;    :async true
                            ;;    :effect (req (wait-for (trash-cards
                            ;;                             :corp (make-eid state eid) targets
                            ;;                             {:unpreventable true
                            ;;                              :cause-card card
                            ;;                              :cause :forced-to-trash}))
                            ;;                 (trash state :runner eid card
                            ;;                        {:cause :purge :cause-card card}))}
                            ;;   card nil)))}]})

(defcard "Boi-tatá"
  ;; If you trashed one of your installed cards this turn,
  ;; paid abilities on this icebreaker cost 1{credit} less to activate.
  ;;
  ;; Interface → 2{credit}: Break up to 2 sentry subroutines.
  ;; 3{credit}: +3 strength."
  (letfn [(raccoon-fn
            [target]
            (runner? (:card (first target))))]
    (auto-icebreaker {:implementation "2v4. Printed abilities only."
                      :abilities [(break-sub 1 2 "Sentry" (cond-breaker :runner-trash raccoon-fn))
                                  (break-sub 2 2 "Sentry")
                                  (strength-pump 2 3 :end-of-encounter (cond-breaker :runner-trash raccoon-fn))
                                  (strength-pump 3 3)]})))

(defcard "Manuel"
  ;; When you breach HQ or R&d during a run, if you are tagged, you may access an additional card.
  ;;
  ;; Threat 3 - As an additional cost to trash this card using a basic action,
  ;; Corp must trash 1 from HQ
  {:implementation "2v5 - additional cost to trash not implemented"
   ;;                                                       sue me
   :events [{:event :breach-server
             :optional
             {:req (req (and (not (not-tagged-req state))
                             run
                             (or (= target :rd)
                                 (= target :hq))))
              :waiting-prompt true
              :prompt (msg "Access an additional card from " (if (= target :rd) "R&D" "HQ") "?")
              :yes-ability {:msg (msg "access 1 additional card from " (if (= target :rd)"R&D" "HQ"))
                            :effect (effect (access-bonus target 1))}}}]})

;;(defcard "Boiúna"
  ;; Paid abilities on this program cost 1{c} less if you installed a connection this turn.
  ;;
  ;; Interface → 2{credit}: Break up to 2 code gate subroutines.
  ;; 2{credit}: +2 strength.
;;  (letfn [(boinga-fn
;;            [target]
;;            (has-subtype? (:card (first target)) "Connection"))]
;;    (auto-icebreaker {:implementation "2v3. Printed abilities only."
;;                      :abilities [(break-sub 1 2 "Code Gate"
;;                                      (cond-breaker :runner-install boinga-fn))
;;                                  (break-sub 2 2 "Code Gate")
;;                                  (strength-pump 1 2 :end-of-encounter (cond-breaker :runner-install boinga-fn))
;;                                  (strength-pump 2 2)]})))

(defcard "[Friend of a Friend]"
  ;;{click}, {trash}: Gain 5{credit} and remove 1 tag.
  ;;{click}, {trash}: Gain 10{credit} and   take 1 tag.
  {:implementation "2v5"
   :abilities [{:label "Gain 5 [Credits], remove tag"
                :msg "gain 5 [Credits]"
                :cost [:click 1 :trash-can]
                :async true
                :effect (req (wait-for (gain-credits state side (make-eid state eid) 5)
                                       (lose-tags state :runner eid 1)))}

               {:label "Gain 10 [Credits] and a tag"
                :msg "gain 10 [Credits]"
                :cost [:click 1 :trash-can]
                :async true
                :effect (req (wait-for (gain-credits state side (make-eid state eid) 10)
                                       (gain-tags state :runner eid 1)))}]})

;; CRIMINAL CARDS

(defcard "[Power Outage]"
  (let [install-abi
        {:prompt "Choose a card to install"
         :waiting-prompt true
         :choices {:req (req (and (or (hardware? target)
                                      (program? target))
                                  (in-hand? target)
                                  (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                            [:credit (install-cost state side target nil{:cost-bonus 0})])))}
         :async true
         :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus 0}))}]
    {:makes-run true
     :implementation "2v6.1"
     :on-play {:async true
               :effect (req (wait-for
                              (resolve-ability state side install-abi card nil)
                              (continue-ability
                                state side
                                {:async true
                                 :req (req (seq (filter #(and (ice? %) (rezzed? %)) (all-installed state :corp))))
                                 :prompt "choose an ice to derez"
                                 :choices {:card #(and (rezzed? %) (ice? %))}
                                 :msg (msg "derez " (card-str state target) " and make a run on " (zone->name (second (get-zone target))))
                                 :effect (req (let [chosen-ice target
                                                    target-server (second (get-zone target))]
                                                (register-events
                                                  state side card
                                                  [{:event :run-ends
                                                    :duration :end-of-run
                                                    :unregister-once-resolved :true
                                                    :optional
                                                    {:player :corp
                                                     :waiting-prompt true
                                                     :req (req (and (installed? (get-card state chosen-ice))
                                                                    (not (rezzed? (get-card state chosen-ice)))))
                                                     :prompt (msg "rez " (:title chosen-ice) ", ignoring all costs?")
                                                     :yes-ability {:async true
                                                                   :effect (req (rez state :corp eid (get-card state chosen-ice) {:ignore-cost :all-costs}))}
                                                     :no-ability {:msg (msg "decline to rez " (card-str state (get-card state chosen-ice)))}}}])
                                                (derez state side target)
                                                (make-run state side eid target-server card)))}
                                card nil)))}}))

                            ;;     serv (second (server->zone state chosen-server))]
                            ;; (continue-ability
                            ;;   state side
                            ;;   {:prompt (msg "Derez up to 2 ice protecting " chosen-server)
                            ;;    :choices {:max 2
                            ;;              :card #(and (ice? %) (rezzed? %) (= serv (second (get-zone %))))}
                            ;;    :msg (msg (str "derez " (str/join " and " (map :title targets))
                            ;;                   " protecting " chosen-server " and make a run"))
                            ;;    :async true
                            ;;    :cancel-effect (req (system-msg state side (str "uses " (:title card) " to make a run on " chosen-server))
                            ;;                        (register-events
                            ;;                          state side card
                            ;;                          [(merge (corp-rez-ice-abi serv)
                            ;;                                  {:event :run-ends
                            ;;                                   :duration :end-of-run
                            ;;                                   :unregister-once-resolved true})
                            ;;                           (merge (corp-rez-ice-abi serv)
                            ;;                                  {:event :run-ends
                            ;;                                   :duration :end-of-run
                            ;;                                   :unregister-once-resolved true})])
                            ;;                        (make-run state side eid chosen-server (get-card state card)))
                            ;;    :effect (req (doseq [c targets]
                            ;;                   (derez state :runner c))
                            ;;                 (register-events
                            ;;                   state side card
                            ;;                   [(merge (corp-rez-ice-abi serv)
                            ;;                          {:event :run-ends
                            ;;                           :duration :end-of-run
                            ;;                           :unregister-once-resolved true})
                            ;;                    (merge (corp-rez-ice-abi serv)
                            ;;                           {:event :run-ends
                            ;;                            :duration :end-of-run
                            ;;                            :unregister-once-resolved true})])
                            ;;                 (make-run state side eid chosen-server (get-card state card)))}
                            ;;   card nil)))}}))

  ;; Derez up to 2 pieces of ice protecting the same server, then run that server.
  ;;When that run ends, the Corp may rez 2 pieces of ice protecting that server, ignoring all costs
  ;; {:makes-run true
  ;;  :implementation "2v5"
  ;;  :on-play
  ;;  {:msg (msg "derez " (:title target))
  ;;   :choices {:card #(and (ice? %)
  ;;                         (rezzed? %))}
  ;;   :async true
  ;;   :effect (req (let [target-ice target]
  ;;                  (derez state :runner target)
  ;;                  (register-events
  ;;                    card [{:event :run-ends
  ;;                           :duration :end-of-run
  ;;                           :unregister-once-resolved true
  ;;                           :optional {:prompt (msg "Rez " (:title target-ice) ", ignoring all costs?")
  ;;                                      :yes-ability {:async true
  ;;                                                    :req (req (and
  ;;                                                                (installed? (get-card state target-ice))
  ;;                                                                (not (rezzed? (get-card state target-ice)))))
  ;;                                                    :effect (req (rez state side eid target-ice {:ignore-cost true}))}
  ;;                                      :no-ability {:msg (str "declines to rez " (:title target-ice))}}}])
  ;;                   (make-run eid (second (get-zone target)) card)))}})

(defcard "Jeitinho"
  ;; When your turn ends, if you made a successful run on HQ, R&D and Archives this turn,
  ;; add this hardware to your score area as an agenda worth 0 points with
  ;;   "if you have 3 copies of {Rule of Three} in your score area, you win the game"
  ;; then, you may install a copy of this card from your grip or heap.
  {:implementation "2v6.1. Click ability is on the scored agenda. Priority not enforced."
   :flags {:has-abilities-when-stolen true}
   :abilities [{:cost [:click 1]
                :req (req (and (seq (filter #(= (:printed-title %) (:printed-title card)) (:discard runner)))
                               (is-scored? state :runner card)))
                :label "Install another copy of Jeitinho from the Heap"
                :async true
                :effect (req (let [target-card (first (filter #(= (:printed-title %) (:printed-title card)) (:discard runner)))]
                               (runner-install state side (assoc eid :source card :source-type :runner-install) target-card nil)))}]
   ;;              :player :runner
   ;;              :label (msg "Install another copy of " (:title card))
   ;;              :effect (req (effect-completed state side eid))}]
   :events [{:event :runner-turn-ends
             :req (req (and
                         (installed? card)
                         (some #{:hq} (:successful-run runner-reg))
                         (some #{:rd} (:successful-run runner-reg))
                         (some #{:archives} (:successful-run runner-reg))))
             :msg (msg "and add itself to the score area as an agenda worth 0 agenda points")
             :async true
             :effect (req (as-agenda-special state :runner card 0)
                          (if (= 3 (count (filter #(= (:printed-title %) (:printed-title card))
                                                  (:scored runner))))
                            (do (system-msg state side "wins the game by assembling exodia")
                                (win state :runner "assembling exodia")
                                (effect-completed state side eid))
                            (effect-completed state side eid)))}]})

(defcard "[Meeting of Minds]"
  (letfn [(tutor-abi [type]
            {:prompt (str "Choose a " (decapitalize type))
             :implementation "2v6.1"
             :choices (req (cancellable (filter #(has-subtype? % type)
                                                (:deck runner)) :sorted))
             :msg (msg "add " (:title target) " from the stack to the grip and shuffle the stack")
             :async true
             :effect (effect (trigger-event :searched-stack nil)
                             (continue-ability
                               (let [connection target]
                                 (if (can-pay? state side (assoc eid :source card :source-type :runner-install) connection nil
                                               [:credit (install-cost state side connection)])
                                   {:optional {:prompt (str "Install " (:title connection) "?")
                                               :yes-ability {:async true
                                                             :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) connection nil)
                                                                             (shuffle! :deck))}
                                               :no-ability {:effect (effect (move connection :hand)
                                                                            (shuffle! :deck))}}}
                                   {:effect (effect (move connection :hand)
                                                    (shuffle! :deck))}))
                               card nil))})]
  ;; Search your stack for a connection or virtual resource and add it to your grip
    {:makes-run true
     :on-play {:prompt "Choose one"
               :async true
               :choices ["Tutor a connection" "Run HQ to tutor a virtual"]
               :effect (req
                         (if (= target "Tutor a connection")
                           (continue-ability
                             state side
                             (tutor-abi "Connection")
                             card nil)
                           (do
                             (system-msg state side (str "uses " (:title card) " to make a run on HQ"))
                             (make-run state side eid :hq card))))}
     :events [{:event :successful-run
               :req (req (and this-card-run
                              (= :hq (target-server context))))
               :interactive (req true)
               :async true
               :effect (req (continue-ability state side (tutor-abi "Virtual") card nil))}]}))

;; (defcard "Wipe Down"
;;   ;; Resolve 1 of the following:
;;   ;;  remove up to 2 tags
;;   ;;  Draw 3 cards. Add 1 card from the grip to the top or bottom of the stack.
;;   {:implementation "2v3 - you can choose to remove 0 tags if you want"
;;    :on-play {:prompt "Choose one"
;;              :req (req (or (pos? (count (:deck runner)))
;;                            (pos? (count-tags state))))
;;              :choices (req [(when (not= 0 (count-tags state) "Remove up to 2 tags")) "Draw 3 cards"])
;;              :effect (effect (continue-ability
;;                                (if (= target "Remove up to 2 tags")
;;                                  {:prompt "Remove how many tags?"
;;                                   :choices (req [0 1 (when (not= 1 (count-tags state)) 2)])
;;                                   :msg (msg "remove " target " tags.")
;;                                   :async true
;;                                   :effect (effect (lose-tags eid target))}
;;                                  {:msg "draw 3 cards"
;;                                   :async true
;;                                   :effect (req (wait-for (draw state side (make-eid state eid) 3)
;;                                                          (continue-ability
;;                                                            state side
;;                                                            {:prompt "select a card to return"
;;                                                             :choices {:card #(and (in-hand? %)
;;                                                                                   (runner? %))
;;                                                                       :all true}
;;                                                             :async true
;;                                                             :effect (req (let [chosen-card target]
;;                                                                            (continue-ability
;;                                                                              state side
;;                                                                              {:prompt "Choose one"
;;                                                                               :choices ["Top of Stack" "Bottom of stack"]
;;                                                                               :async true
;;                                                                               :effect (req (if (= target "Top of Stack")
;;                                                                                              (do (move state :runner chosen-card :deck {:front true})
;;                                                                                                  (system-msg state side "places a card on the top of the stack"))
;;                                                                                              (do (move state :runner chosen-card :deck)
;;                                                                                                  (system-msg state side "places a card on the bottom of the stack")))
;;                                                                                            (effect-completed state side eid))}
;;                                                                              card nil)))}
;;                                                            card nil)))})
;;                                card nil))}})

(defcard "Alarm Clock"
  ;; When your turn begins, you may run HQ. The first time you encounter a piece of ice
  ;; during that run, you may spend [click][click] to bypass it
  (let [ability {:once :per-turn
                 :req (req (:runner-phase-12 @state))
                 :msg (msg "make a run on HQ")
                 :makes-run true
                 :async true
                 :effect (req (register-events
                                  state side card
                                  [{:event :encounter-ice
                                    :once :per-run
                                    :unregister-once-resolved true
                                    :duration :end-of-run

                                    :optional
                                    {:prompt "spend [Click][Click] to bypass this ice?"
                                     :yes-ability {:cost [:click 2]
                                                   :req (req (>= (:click runner) 2))
                                                   :msg (msg "bypass " (:title (:ice context)))
                                                   :effect (req (bypass-ice state))}}}])
                              (wait-for
                                (make-run state :runner (make-eid state eid) :hq card)
                                (effect-completed state side eid)))}]
    {:implementation "2v5"
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
  {:implementation "2v6"
   :hosting {:card #(and (ice? %)
                         (can-host? %))}
   :events [{:event :purge
             :async true
             :msg "trash itself"
             :effect (req (trash state :runner eid card {:cause :purge
                                                         :cause-card card}))}
            {:event :encounter-ice
             :optional {:prompt (msg "Pay " (count (:subroutines (get-card state current-ice)))
                                     " [Credits] to bypass " (:title current-ice) "?")
                        :req (req (and (not (has-subtype? current-ice "Barrier"))
                                       (same-card? current-ice (:host card))
                                       (can-pay? state :runner eid (:ice context) nil [:credit (count (:subroutines (get-card state current-ice)))])))
                        :yes-ability {:async true
                                      :msg (msg "pays 1 [Credits] to bypass " (:title current-ice))
                                      :effect (req (wait-for
                                                     (pay state side (make-eid state eid) card [:credit (count (:subroutines (get-card state current-ice)))])
                                                     (let [payment-str (:msg async-result)
                                                           msg-ab {:msg (str "bypass " (:title (:ice context)))}]
                                                       (print-msg state side msg-ab card nil payment-str))
                                                     (bypass-ice state)
                                                     (effect-completed state side eid)))}}}]})


(defcard "Malandragem"
  ;;When you install this program, load 2 power counters onto it. When it is empty, trash it.
  ;; Whenever you encounter an ice with strength 3 or less,
  ;; you may spend 1 hosted power counter to bypass it. Use this ability only once per turn.
  ;; Threat 4 -- Whenever you encounter an ice, you may trash this program to bypass it.
  {:implementation "2v5 - abilities are named (but have the same timing)"
   :data {:counter {:power 2}}
   :events [(rfg-on-empty :power)
            {:event :encounter-ice
             :interactive (req true)
             :ability-name "Parrots (rfg)"
             :optional {:prompt (msg "remove parrots from the game to bypass?")
                        :req (req (threat-level 4 state))
                        :yes-ability {:cost [:remove-from-game]
                                      :msg (msg "bypass " (:title current-ice))
                                      :effect (req (bypass-ice state))}}}
            {:event :encounter-ice
             :interactive (req true)
             :ability-name "Parrots (Power Counter)"
             :optional {:prompt (msg "spend 1 power counter to bypass " (:title current-ice))
                        :once :per-turn
                        :req (req (and (>= 3 (ice-strength state side current-ice))
                                       (<= 1 (get-counters (get-card state card) :power))))
                        :yes-ability {:cost [:power 1]
                                      :msg (msg "bypass " (:title current-ice))
                                      :effect (req (bypass-ice state))}}}]})

(defcard "[Bindle]"
  ;; Limit 1 hosted card.
  ;;  Access -->2c: Host the accessed non-agenda card faceup on this program.
  ;; Whenever you breach HQ or R&D, you may trash 1 hosted corp card
  ;; and this program to access an additional card.
  {:implementation "2v6.1"
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
                                       :prompt "Trash bindle and hosted card to access two additional cards?"
                                       :yes-ability {:async true
                                                     :effect (effect (access-bonus target-server 2)
                                                                     (effect-completed eid))
                                                     :cost [:trash-can]
                                                     :msg "access an additional card"}}}
                           card nil)))}]
   :interactions {:access-ability {:label "Host a card"
                                   :req (req (and (zero? (count (:hosted card)))
                                                  (not (agenda? target))))
                                   :cost [:credit 2]
                                   :msg (msg "host " (:title target))
                                   :async true
                                   :effect (req (host state side card target)
                                                (swap! state dissoc :access)
                                                (effect-completed state side eid))}}})

(defcard "Amelia Earhart"
  ;; The first time each turn you access 3 or more cards from HQ or R&D during a run,
  ;; place one power counter.
  ;; When your turn begins, if this resource has 3 or more hosted power counters, you may trash
  ;; this resource. If you do, the corp loses 10{credit}
  {:implementation "2v5 - condition is implemented as third card in a breach on HQ or RD during run"
   :flags {:runner-phase-12 (req true)}
   :events [{:event :run-ends
             :req (req (and (#{:hq :rd} (target-server context))
                          (>= (total-cards-accessed context) 3)))
             :once :per-turn
             :effect (effect (add-counter (get-card state card) :power 1))}
            {:event :runner-turn-begins
             :optional
             {:prompt (msg "Force the corp to lose 10 [Credits]?")
              :yes-ability
              {:req (req (>= (get-counters (get-card state card) :power) 3))
               :msg (msg "trash itself and force the corp to lose 10 [Credits]")
               :async true
               :effect (req (let [tolose 10]
                              (wait-for
                                (trash state side card {:cause-card card})
                                (lose-credits state :corp eid(min tolose (:credit corp))))))}}}]})

  ;; get-in breach cards-accessed zone

  ;; ;; The first time each turn you may a successful run on a central server, place 1 credit on up to
  ;; ;; 2 installed job and/or connection resources.
  ;; {:implementation "2v3"
  ;;  :events [{:event :successful-run
  ;;            :req (req (and (is-central? (:server context))
  ;;                           (first-event? state side :successful-run
  ;;                                         (fn [targets]
  ;;                                           (let [context (first targets)]
  ;;                                             (is-central? (:server context)))))
  ;;                           (pos? (count (filter #(and (or (has-subtype? % "Job")
  ;;                                                          (has-subtype? % "Connection"))
  ;;                                                      (resource? %))
  ;;                                                (all-installed state :runner))))))
  ;;            :msg "place 1 [Credit] on up to 2 job or connection resources"
  ;;            :async true
  ;;            :effect (req (let [valid-targets (filter #(and (or (has-subtype? % "Job")
  ;;                                                               (has-subtype? % "Connection"))
  ;;                                                           (resource? %))
  ;;                                                     (all-installed state :runner))
  ;;                               max-targets (min 2 (count valid-targets))]
  ;;                           (continue-ability
  ;;                             state side
  ;;                             {:choices {:card #(and (resource? %)
  ;;                                                    (installed? %)
  ;;                                                    (or (has-subtype? % "Job")
  ;;                                                        (has-subtype? % "Connection")))
  ;;                                        :max max-targets}
  ;;                              :msg (msg "place a credit on " (str/join " and " (map :title targets)))
  ;;                              :effect (req (doseq [c targets]
  ;;                                             (add-counter state :runner c :credit 1)))}
  ;;                             card nil)))}]})

(defcard "Juli"
  ;; When you install this resource, load 4 power counters onto it. When it is empty, trash it.
  ;; The first time each turn you take an action on an installed resource, remove 1 hosted power
  ;;  counter and gain [click]
  {:implementation "2v5. I still don't understand what this does."
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
  {:implementation "2v5"
   :data {:counter {:power 3}}
   :makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :abilities [{:cost [:power 1]
                :label "host a trojan on an ice protecting this server"
                :prompt "Choose a trojan"
                :choices {:card #(has-subtype? % "Trojan")}
                :async true
                :effect (req (let [trojan target]
                               (continue-ability
                                 state side
                                 {:prompt "Choose an ice"
                                  :choices {:card #(ice? %)}
                                  :msg (msg "host " (:title trojan) " on " (card-str state target))
                                  :effect (req (host state side target trojan)
                                               (update-all-ice state side))}
                                 card nil)))}]})

(defcard "[True Colors]"
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
    {:implementation "2v5"
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
  ;;  When that run ends, if it was successful, you may place this card on bottom of the stack."
  {:implementation "2v5"
   :makes-run true
   :on-play {:req (req rd-runnable)
             :async true
             :effect (effect (make-run eid :rd card))}
   :events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :rd (target-server context))
                            this-card-run))
             :msg (msg "gain 3 [Credits] and access an additional card")
             :effect (req (register-events
                            state side
                            card [(breach-access-bonus :rd 1 {:duration :end-of-run})])
                          (gain-credits state side eid 3))}
            {:event :run-ends
             :req (req this-card-run)
             :async true
             :effect (req (wait-for
                            ;;(draw state side (make-eid state eid) 1)
                            (maybe-draw state side (make-eid state eid) card 1)
                            (continue-ability
                              state side
                              {:optional
                               {:prompt "Add Free Wall to the bottom of the stack?"
                                :yes-ability {:msg "add itself to the bottom of the stack"
                                              :effect (req (move state :runner card :deck {:front false})
                                                           (effect-completed state side eid))}
                                :no-ability {:msg "decline to add itself to the bottom of the stack"}}}
                              card nil)))}]})

(defcard "[Indexer]"
  ;; When you install, load 3 power counters. Trash empty.
  ;; Click, power counter: Run R&D. If successful, instead of breaching, index the top 3.
  ;; You may spend 1 hosted counter to access the top card of R&D
  (let [ability (successful-run-replace-breach
                 {:target-server :rd
                  :mandatory false
                  :ability
                  {:async true
                   :msg "rearrange the top 3 cards of R&D"
                   :cost [:power 1]
                   :waiting-prompt true
                   :effect (req (wait-for
                                  (resolve-ability
                                    state side
                                    (let [from (take 3 (:deck corp))]
                                      (when (pos? (count from))
                                        (reorder-choice :corp :corp from '() (count from) from)))
                                    card nil)
                                  (if (and (installed? (get-card state card))
                                           (pos? (get-counters (get-card state card) :power))
                                           (pos? (count (take 3 (:deck corp)))))
                                    (continue-ability
                                      state :runner
                                      {:optional
                                       {:prompt "Access the top card?"
                                        :waiting-prompt true
                                        :yes-ability
                                        {:msg "access the top card of R&D"
                                         :async true
                                         :effect (req
                                                   (add-counter state side card :power -1)
                                                   (access-card state side eid (first (:deck corp))))}}}
                                      card nil)
                                    (effect-completed state side eid))))}})]
    {:implementation "2v6"
     ;; :on-install {:msg "look at the top 3 cards of R&D"
     ;;              :choices ["noted"]
     ;;:prompt (msg "The top 3 cards of R&D are " (str/join ", " (map :title (take 3 (:deck corp)))))}
     :data {:counter {:power 3}}
     :events [{:event :runner-turn-begins
               :req (req (zero? (get-counters (get-card state card) :power)))
               :msg (msg "gain a power counter")
               :effect (req (add-counter state side card :power 1))}
              ability]}))

(defcard "[Powercache]"
  ;; When installed, place 2 power counters.
  ;; Hosted counter: gain 2 credits. Only during your turn.
  {:implementation "2v5"
   :abilities [{:cost [:power 1]
                :req (req (= (:active-player @state) :runner))
                :async true
                :effect (effect (gain-credits eid 2))
                :msg "gain 2 [Credits]"}]
   :data {:counter {:power 2}}})

;; (defcard "[Repurposer]"
;;   ;;You cannot use this hardware while breaching R&D or accessing a card in R&D.
;;   ;;  Access → Lose {click}: The Corp shuffles the non-agenda card you are accessing into R&D.
;;   ;;  Threat 5 → Whenever you access a non-agenda card, you may trash this hardware.
;;   ;;             If you do, the Corp shuffles that card into R&D."
;;   (letfn [(resolve-shuffle [state side target eid]
;;             (move state :corp target :deck)
;;             (shuffle! state :corp :deck)
;;             (effect-completed state side eid))]
;;   {:implementation "2v3"
;;    :interactions
;;    {:access-ability
;;     {:label "Shuffle a card into R&D"
;;      :req (req (and (not (in-deck? target))
;;                     (or (pos? (:click runner))
;;                         (threat-level 5 state))
;;                     (not= (:breach-server (:breach state)) :rd)))
;;      :async true
;;      :effect (req
;;                (let [shuffle-card target]
;;                  (continue-ability
;;                    state side
;;                    {:prompt "Pay how?"
;;                     :choices (req [(when (pos? (:click runner))
;;                                      "Lose Click")
;;                                    (when (threat-level 5 state)
;;                                      (str "Trash " (:title card)))])
;;                     :async true
;;                     :effect (req (if (= target "Lose Click")
;;                                    (wait-for
;;                                      (pay state :runner (make-eid state eid)
;;                                           card [:lose-click 1])
;;                                      (system-msg
;;                                        state side
;;                                        (str (:msg async-result) " to shuffle " (:title shuffle-card) "into R&D"))
;;                                      (resolve-shuffle state side shuffle-card eid))
;;                                    (wait-for
;;                                      (trash state :runner (make-eid state eid)
;;                                             card {:cause :runner-ability
;;                                                   :cause-card card})
;;                                      (system-msg
;;                                        state side
;;                                        (str "trashes " (:title card)
;;                                             " to shuffle " (:title shuffle-card) " into R&D"))
;;                                      (resolve-shuffle state side shuffle-card eid))))}
;;                    card nil)))}}}))

;; (defcard "Sketchpad"
;;   ;; Install only on a piece of ice.
;;   ;;
;;   ;; Host ice gains barrier, code gate, and sentry.
;;   ;;
;;   ;; Threat 4 — Lose {click}: Host this program on another piece of ice."
;;   {:hosting {:card #(and (ice? %)
;;                          (can-host? %))}
;;    :implementation "2v3"
;;    :abilities [{:label "Host on a piece of ice"
;;                 :prompt "Choose a piece of ice"
;;                 :cost [:lose-click 1]
;;                 :req (req (threat-level 4 state))
;;                 :choices {:card #(and (ice? %)
;;                                       (installed? %)
;;                                       (can-host? %))}
;;                 :msg (msg "host itself on " (card-str state target))
;;                 :effect (effect (host target card))}]
;;    :on-install {:msg (msg "make " (card-str state (:host card))
;;                           " gain Barrier, Code Gate and Sentry subtypes")}
;;    :static-abilities [{:type :gain-subtype
;;                        :req (req (same-card? target (:host card)))
;;                        :value ["Barrier" "Code Gate" "Sentry"]}]})

(defcard "[Pressure Spike]"
  ;; Threat 4 — This program gains 2 strength.

  ;; Interface → 1{credit}: Break 1 barrier subroutine.
  ;; 2{credit}: +1 strength."
  (auto-icebreaker {:implementation "2v5"
                    ;;:x-fn (req (if (threat-level 4 state) 2 0))
                    :abilities [(break-sub 1 1 "Barrier")
                                (strength-pump 2 3)
                                (strength-pump 2 9 :end-of-encounter {:req (req (threat-level 3 state))})]}))
                    ;;:static-abilities [(breaker-strength-bonus (get-x-fn))]}))

(defcard "Muse"
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
                                               (not (has-subtype? % "Daemon")))
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

(defcard "Lobisomem"
  ;; Whenever this program fully breaks a code gate, place 1 power counter on this program.
  ;;
  ;; Interface → 1{credit}: Break 1 code gate subroutine.
  ;; Interface → 2{credit}, hosted power counter: Break up to 3 barrier subroutines.
  ;; 1{credit}: +2 strength."
  (auto-icebreaker {:data {:counter {:power 1}}
                    :abilities [(break-sub 1 1 "Code Gate")
                                {:label "Break X Barrier subroutines"
                                 :cost [:x-credits :power 1]
                                 :break-cost [:x-credits :power 1]
                                 :req (req (and
                                             (active-encounter? state)
                                             (<= (get-strength current-ice) (get-strength card))))
                                 :msg (msg "break " (quantify (cost-value eid :x-credits) "subroutine")
                                             " on " (card-str state current-ice))
                                 :effect (effect
                                             (continue-ability
                                               (when (pos? (cost-value eid :x-credits))
                                                 (break-sub nil (cost-value eid :x-credits) "Barrier"))
                                               card nil))}
                                ;;(break-sub [:power 1 :credit 2] 3 "Barrier")
                                (strength-pump 1 2)]
                    :implementation "2v5"
                    :events [{:event :subroutines-broken
                              :req (req (and (all-subs-broken-by-card? target card)
                                             (has-subtype? target "Code Gate")))
                              :msg "place 1 power counter on itself"
                              :async true
                              :effect (effect (add-counter card :power 1)
                                              (effect-completed eid))}]}))

(defcard "\"Pretty\" Mary"
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

(defcard "[AR Infinite Access]"
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
  ;; whenever you remove 1 or more tags, gain 1
  ;;
  ;; Threat 3 — When you install this resource, you may remove 1 tag."
  (letfn [(ab [ev] {;;:prompt "Choose one"
                    ;;:waiting-prompt true
                    :event ev
                    :player :runner
            ;;:choices ["Gain 1 [Credits]" "Draw 1 cards"]
                    :msg (msg "gain 1 [Credits]")
                    :async true
                    :interactive (req true)
                    ;; :once :per-turn
                    ;; :req (req (first-event? state :runner ev))
                    :effect (req (gain-credits state :runner eid 1))})]
    {:implementation "2v4"
     :on-install {:req (req (and (threat-level 3 state) (pos? (count-tags state))))
                  :msg "remove a tag"
                  :async true
                  :effect (effect (lose-tags eid 1))}
     :events [(ab :runner-lose-tag)]}))

(defcard "Thunderbolt Armaments"
  ;; Whenever you rez an AP or destroyer ice during a run,
  ;; it gets +1 strength and “[sub] End the run unless the runner trashes an installed card”
  ;; after its printed subroutines until the end of the run.
  (let [thunderbolt-sub
        (end-the-run-unless-runner-pays [:trash-installed 1])]
    {:implementation "2v4"
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
                         " +1 strength and \""
                         (:label thunderbolt-sub)
                         "\" after it's printed subroutines")
               :async true
               :effect (effect (add-extra-sub! (get-card state (:card context))
                                               thunderbolt-sub
                                               (:cid card) {:front false})
                               (update! (update-in card [:special :thunderbolt]
                                                   #(conj % (:card context))))
                               (pump-ice (:card context) 1 :end-of-run)
                               (effect-completed eid))}]}))

;; HB HAAS BIOROID CARDS

(defcard "[Sleeper Control]"
  ;; When you score this agenda, place 1 agenda counters on it.
  ;; When a run begins, you may spend 1 hosted agenda counter to rez up to 2 ice
  ;; protecting that server, ignoring all costs.
  ;; When this turn ends, derez two ice protecting that server"
  (letfn [(sleeper-reset [zone]
            {:event :run-ends
             :duration :end-of-run
             :unregister-once-resolved true
             :msg (msg "should derez two ice protecting " (zone->name [zone]))})
          (sleeper-rez [state side targets card zone eid]
            (if (zero? (count targets))
              (do (register-events
                    state side card
                    [(sleeper-reset zone)])
                  (effect-completed state side eid))
              (wait-for (rez state :corp (make-eid state eid) (first targets) {:ignore-cost :all-costs})
                        (sleeper-rez state side (drop 1 targets) card zone eid))))]
    {:on-score {:effect (effect (add-counter card :agenda 1))
                :silent (req true)}
     :implementation "2v6.1 - derez is manual"
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
  ;; When you rez a card, place 1 power counter on this asset.
  ;; [Click], 1 hosted power counter: gain 2[credit]
  ;; [trash, 6 hosted power counters: trash an installed resource
  {:implementation "2v6.1"
   :events [{:event :rez
             :silent (req true)
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:cost [:click 1 :power 1]
                :label "Gain 3 [Credits]"
                :msg "gain 3 [Credits]"
                :effect (req (gain-credits state side eid 3))}
               {:cost [:click 1 :power 4]
                :label "move an installed resource to the stack"
                :prompt "Choose a resource to add to the top of the stack"
                :choices {:card #(and (resource? %)
                                      (installed? %))}
                :async true
                :msg (msg "gain 7 [Credits] and add " (:title target) " to the top of the stack")
                :effect (req
                          (wait-for (gain-credits state side 7)
                                    (move state :runner target :deck {:front true})
                                    (effect-completed state side eid)))}]})

(defcard "[Will to Win]"
  ;; When your turn begins, you may derez a card.
  ;; Then, if there are no ice protecting this server, you may install a card from HQ.
  ;; You may not score that card this turn."
    (let [install {:prompt "install a card from HQ"
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
                 :req (req unprotected)
                 :async true
                 :prompt "Derez a card"
                 :choices {:card #(rezzed? %)}
                 :msg (msg "derez " (:title card) " to derez " (:title target))
                 :effect (effect
                           (derez card)
                           (derez target))}]
      {:derezzed-events [corp-rez-toast]
       :implementation "2v5"
       :events [{:event :corp-turn-begins
                 :async true
                 :effect (req (wait-for (resolve-ability state side install card nil)
                                        (continue-ability state side derez card nil)))}]}))

(defcard "Sorocaban Blade"
  ;; Subroutines on this ice cannot be broken by AI programs or by non-icebreaker cards.
  ;; {sub} Trash an installed resource.
  ;; {sub} If there are no installed resources, trash an installed piece of hardware.
  ;; {sub} If there are no installed pieces of hardware, trash an installed program.
  {:implementation "2v6.1 - trash restriction not implemented, be honest with the last sub"
   :subroutines [trash-resource-sub
                 (assoc trash-hardware-sub :req (req (not (some #(resource? %) (all-installed state :runner)))))
                 (assoc trash-program-sub :req (req (not (some #(hardware? %) (all-installed state :runner)))))
                 (assoc trash-installed-sub :req (req (installed? (get-card state card))))]})

(defcard "Curare"
  ;; When you rez this ice, choose one or more of sentry, code gate, and barrier.
  ;;   This ice gains the chosen subtypes.
  ;; When a turn ends, derez this ice.
  ;; {sub} If this ice is a code gate, the Runner loses [click][click].
  ;; {sub} If this ice is a sentry, trash 1 program.
  ;; {sub} If this ice is a barrier, gain 1c and end the run."
  ;; TODO = make the effects actually go away
  (letfn [(curare-choice [options]
            {:prompt "Choose a subtype for Curare or press 'Done'"
             :waiting-prompt "corp to add subtypes"
             :choices options
             :effect (req (if (= target "Done")
                            (effect-completed state side eid)
                            (do
                              ;; note - this is a lingering ability and persists so long as the card is rezzed
                              ;;  if the card is hushed, it will not derez, so the subtypes will stay!
                              (system-msg state side (str "uses " (:title card) " to make itself gain " target))
                              (register-lingering-effect
                                state side card
                                (let [ice card]
                                  {:type :gain-subtype
                                   :req (req (same-card? ice target))
                                   :value target}))
                              (continue-ability
                                state side
                                (curare-choice (remove #{target} options))
                                card nil))))})]
    {:on-rez {:effect (effect (continue-ability (curare-choice ["Barrier" "Code Gate" "Sentry" "Done"]) card nil))}
     :derez-effect {:effect (req (unregister-effects-for-card state side card #(= :gain-subtype (:type %))))}
     :implementation "2v4"
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
                   {:label "(Barrier) Gain 1 [Credit] and End the run"
                    :msg "end the run"
                    :req (req (has-subtype? card "Barrier"))
                    :async true
                    :effect (req (wait-for
                                   (gain-credits state :corp 1)
                                   (end-run state :corp eid card)))}]}))

(defcard "[Tactical Ceasefire]"
  (let [lose-click-abi
        {:msg "make the runner lose [Click] next turn"
         :async true
         :effect (req (swap! state update-in [:runner :extra-click-temp] (fnil #(- % 1) 0))
                      (if-not (threat-level 3 state)
                        (effect-completed state side eid)
                        (continue-ability
                          state side
                          {:optional {:prompt "Pay 2 [credits] to make the runner lose an additional [Click]?"
                                      :yes-ability {:cost [:credit 2]
                                                    :msg "make the runner have one less alloted [Click] next turn"
                                                    :effect (req (swap! state update-in [:runner :extra-click-temp] (fnil #(- % 1) 0)))}}}
                          card nil)))}]
  {:on-play {:req (req (or (last-turn? state :runner :trashed-card)
                           (last-turn? state :runner :stole-agenda)))
             :prompt "Choose a card to install"
             :choices {:card #(and (corp? %)
                                   (corp-installable-type? %)
                                   (in-hand? %))}
             :async true
             :msg (msg (corp-install-msg target))
             :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to install a card"))
                                    (continue-ability lose-click-abi card nil))
             :effect (req (wait-for (corp-install state side target nil nil)
                                    (continue-ability state side lose-click-abi card nil)))}}))

  ;; Play only if the Runner trashed or stole a card during their last turn.
  ;; Derez a card to choose an installed Runner card with an install cost less than the
  ;;   printed rez cost of the derezzed card. Add the chosen Runner card to the grip.
  ;; Threat 4 — Reveal the grip. Put all cards in the Runner’s grip with the same type
  ;;   as the returned card on top of the stack in any order."
  ;; (letfn [(cbi-final [chosen original]
  ;;           {:player :corp
  ;;            :prompt (str "The top cards of the Stack will be " (str/join  ", " (map :title chosen)) ".")
  ;;            :choices ["Done" "Start over"]
  ;;            :async true
  ;;            :effect (req (if (= target "Done")
  ;;                           (do
  ;;                             (system-msg state side (str "places " (str/join ", " (map :title chosen)) " on the top of the Stack (the first card is ontop)"))
  ;;                             (doseq [c (reverse chosen)]
  ;;                               (move state :runner c :deck {:front true}))
  ;;                             (effect-completed state side eid))
  ;;                           (continue-ability state side (cbi-choice original '() (count original) original)
  ;;                                             card nil)))})
  ;;         (cbi-choice [remaining chosen n original]
  ;;           {:player :corp
  ;;            :prompt "Choose a card to move next onto the Stack"
  ;;            :choices remaining
  ;;            :async true
  ;;            :effect (effect
  ;;                      (continue-ability
  ;;                        (let [chosen (cons target chosen)]
  ;;                          (if (< (count chosen) n)
  ;;                            (cbi-choice (remove-once #(= target %) remaining) chosen n original)
  ;;                            (cbi-final chosen original)))
  ;;                        card nil))})
  ;;         (choose-top-order [from]
  ;;           {:player :corp
  ;;            :waiting-prompt "Corp to make a decision"
  ;;            :async true
  ;;            :effect (req
  ;;                      (continue-ability
  ;;                        state side
  ;;                        (when (pos? (count from))
  ;;                          (cbi-choice from '() (count from) from))
  ;;                        card nil))})]
  ;;   {:on-play
  ;;    {:req (req (last-turn? state :runner :stole-agenda))
  ;;     :implementation "2v4. It's implemented exactly as written."
  ;;     :prompt "Choose a card to derez"
  ;;     :choices {:card #(and (installed? %)
  ;;                           (rezzed? %))}
  ;;     :msg (msg "derez " (card-str state target))
  ;;     :async true
  ;;     :effect (req (let [rez-cost (:cost target)]
  ;;                    (derez state side target)
  ;;                    (continue-ability
  ;;                      state side
  ;;                      {:prompt (msg "return a runner card with cost less than " rez-cost " to the grip")
  ;;                       :choices {:card #(and (runner? %)
  ;;                                             (installed? %)
  ;;                                             (< (:cost %) rez-cost))}
  ;;                       :async true
  ;;                       :msg (msg "returns " (:title target) " to the grip")
  ;;                       :effect (req (move state :runner target :hand)
  ;;                                    (if (threat-level 4 state)
  ;;                                      (let [runner-hand (:hand (:runner @state))
  ;;                                            same-type (filter #(= (:title %) (:title target)) runner-hand)]
  ;;                                        (system-msg
  ;;                                          state :corp
  ;;                                          (str "uses " (:title card) " to reveal the Runner's Grip ("
  ;;                                               (str/join ", " (map :title (sort-by :title runner-hand)))
  ;;                                               ")"))
  ;;                                        (wait-for
  ;;                                          (reveal state side runner-hand)
  ;;                                          (continue-ability
  ;;                                            state side
  ;;                                            (choose-top-order same-type)
  ;;                                            card nil)))
  ;;                                      (effect-completed state side eid)))}
  ;;                      card nil)))}}))

(defcard "[Strategic Planning]"
  ;; As an additional cost to play this operation, spend {click}.
  ;; Draw 2 cards, gain 4{credit} and add 1 card from Archives to HQ."
  {:on-play {:msg "gain 6 [Credits] and draw 2 cards"
             :implementation "2v5"
             :async true
             :effect (req (wait-for (gain-credits state side 6)
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
  {:implementation "2v6"
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
                             :once :per-turn
                             :async true
                             :yes-ability {:choices {:card #(and (ice? %)
                                                                 (rezzed? %)
                                                                 (not (same-card? % rezzed-card)))}
                                           :async true
                                           :msg (msg "derez " (:title target) " to increase the strength of " (:title rezzed-card) " by " 3 " for the remainder of the run")
                                           :effect (req (derez state side (get-card state target))
                                                        (pump-ice state side rezzed-card 3 :end-of-run)
                                                        (effect-completed state side eid))}}}
                           card nil)))}]})

;; JINTEKI CARDS

(defcard "[Psychic Profiling]"
  ;; When you score this agenda, trash 1 installed resource and make a psi test.
  ;; (Players secretly bid up to 2{credit}. Then each player reveals and spends their bid.)
  ;; If the bids differ, give the Runner 1 tag."
  {:implementation "2v6"
   :events [{:event :agenda-scored
             :interactive (req true)
             :msg "gain 2[Credits] and start a psi game (match-do 1 net damage/differ-give the runner a tag)"
             :async true
             :effect (req (wait-for
                            (gain-credits state side 2)
                            (continue-ability
                              state side
                              {:psi {:equal {:msg "do 1 net damage"
                                            :async true
                                            :effect (effect (damage eid :net 1 {:card card}))}
                                    :not-equal {:async true
                                                :msg "give the runner a tag"
                                                :effect (req (gain-tags state :runner eid 1))}}}
                              card nil)))}]})

(defcard "[Puppet State]"
  ;; The first time each turn the Runner passes a rezzed non-barrierice, you may pay 2{credit}
  ;; or trash a card from HQ
  ;; If you do, the Runner encounters that ice again."
  {:implementation "2v4. This is a forced encounter."
   :events [{:event :pass-ice
             :req (req (and (rezzed? (:ice context))
                            (or (has-subtype? (:ice context) "Code Gate")
                                (has-subtype? (:ice context) "Sentry"))
                            (first-event? state side :pass-ice
                                          (fn [targets]
                                            (let [context (first targets)]
                                              (rezzed? (:ice context)))))))
             :prompt (msg "Make the runner encounter " (:title (:ice context)) " again?")
             :choices (req [(when (can-pay? state :corp (assoc eid :source card :source-type :ability) card nil [:credit 1]) "Pay 1 [Credit]")
                            (when (can-pay? state :corp (assoc eid :source card :source-type :ability) card nil [:trash-from-hand 1]) "Trash a card from HQ")
                            "No thanks"])
             :async true
             :effect (req (if (= target "No thanks")
                            (effect-completed state side eid)
                            (let [enc-ice current-ice]
                              (continue-ability
                                state side
                                (assoc {:msg (msg "make the runner encounter " (:title enc-ice) " again")
                                        :async true
                                        :effect (req (force-ice-encounter state side eid enc-ice))}
                                       :cost (if (= target "Pay 1 [Credit]")
                                               [:credit 1]
                                               [:trash-from-hand 1]))
                                card nil))))}]})

(defcard "Charlotte"
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
                  :msg (msg "gain 2[Credits] and draw a card")
                  :effect (req (wait-for
                                 (gain-credits state :corp (make-eid state eid) 2)
                                 (draw state :corp eid 1)))}]
    {:advanceable :always
     :implementation "2v3"
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability trash-ab]}))

(defcard "Tributary"
  ;; First time each turn a run begins on another server, you may move this ice to the
  ;; outermost position of the attacked server. (The Runner is now approaching this ice.)
  ;; Use this ability only once per turn.
  ;; [sub] You may install a piece of ice from HQ protecting another server.
  ;; [sub] Ice get +2 strength for the remainder of the run."
  {:implementation "2v6.1 - doesn't enforce NCIGS"
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
                                                    :effect (effect (corp-install eid nice target {:ignore-install-cost true}))}
                                                   card nil)))}
                 {:label "Give +2 strength to all ice for the remainder of the run"
                  :msg "give +2 strength to all ice for the remainder of the run"
                  :effect (effect (register-lingering-effect
                                    card
                                    {:type :ice-strength
                                     :duration :end-of-run
                                     :value 2})
                                  (update-all-ice))}]
   :events [{:event :run
             :req (req (and (first-event? state side :run)))
             :once :per-turn
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

(defcard "Boto"
  ;; Threat 4 — This ice gets +2 strength.
  ;; {sub} Do 2 net damage.
  ;; {sub} You may trash 1 card from HQ to end the run.
  ;; {sub} You may trash 1 card from HQ to end the run."
  {:implementation "2v3"
   :static-abilities [(ice-strength-bonus (req (if (threat-level 4 state) 2 0)))]
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

(defcard "[Hook]"
  ;; When the Runner passes this ice, the Runner must choose a subroutine to
  ;;  resolve if this ice was rezzed this turn.
  ;; {sub} Do 3 net damage.
  ;; {sub} Give the Runner 2 tags.
  ;; {sub} Trash an installed card."
  {:subroutines [(do-net-damage 3)
                 (give-tags 2)
                 trash-installed-sub]
   :implementation "2v5"
   :events [{:event :end-of-encounter
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
  ;; Threat 3 — Reveal the grip, you may add 3 cards to the bottom of the stack in a random order
  (letfn [;; (cbi-final [chosen original]
          ;;   {:player :runner
          ;;    :prompt (str "The bottom cards of the Stack will be " (str/join  ", " (map :title chosen)) " (last card is on the bottom).")
          ;;    :choices ["Done" "Start over"]
          ;;    :async true
          ;;    :effect (req (if (= target "Done")
          ;;                   (do
          ;;                     (system-msg state :runner (str "places " (count chosen) " cards on the bottom of the Stack"))
          ;;                     (doseq [c chosen]
          ;;                       (move state :runner c :deck {:back true}))
          ;;                     (effect-completed state side eid))
          ;;                   (continue-ability
          ;;                     state side (cbi-choice original '() (count original) original)
          ;;                     card nil)))})
          ;; (cbi-choice [remaining chosen n original]
          ;;   {:player :runner
          ;;    :prompt "Choose a card to move next onto the bottom of the Stack"
          ;;    :choices remaining
          ;;    :async true
          ;;    :effect (effect
          ;;              (continue-ability
          ;;                (let [chosen (cons target chosen)]
          ;;                  (if (< (count chosen) n)
          ;;                    (cbi-choice (remove-once #(= target %) remaining) chosen n original)
          ;;                    (cbi-final chosen original)))
          ;;                card nil))})
          (hide-away [cards known]
            {:msg (msg "place " (if known (str/join ", " (map :title cards)) (quantify (count cards) " random card"))
                       " from the grip on the bottom of the stack in a random order")
             :effect (req (doseq [c (shuffle cards)]
                            (move state :runner c :deck {:back true})))})]
          ;; (choose-n [n chosen remaining]
          ;;   (if (or (zero? n)
          ;;           (zero? (count remaining)))
          ;;     ;; corp can resolve now
          ;;     (hide-away chosen true)
          ;;     (if (>= n (count remaining))
          ;;       (hide-away remaining true) ;;(return-to-top-or-bottom-of-either :corp remaining true)
          ;;       {:prompt (msg "Choose a runner card to place in the stack (" n " remaining)")
          ;;        :choices (req (sort-by :title remaining))
          ;;        :not-distinct true
          ;;        :async true
          ;;        :effect (req
          ;;                  (continue-ability
          ;;                    state side
          ;;                    (choose-n (dec n) (concat chosen [target])
          ;;                              (remove-once #(= target %) remaining))
          ;;                    card nil))})))]
    ;; return-to-top-or-bottom-of-either corp cards true
    {:on-play
     {:async true
      :implementation "2v6.1"
      :req (req (and (pos? (count (:hand runner)))
                     (or (last-turn? state :runner :trashed-card)
                           (last-turn? state :runner :stole-agenda))))
      :effect (req
                     ;; reveal the cards to the corp and do whatever
                     ;; (continue-ability
                     ;;   state :corp
                     ;;   {:label "Reveal the grip"
                     ;;    :msg (msg "reveal "
                     ;;              (enumerate-str (map :title (:hand runner)))
                     ;;              " from the grip")
                     ;;    :async true
                     ;;    :effect (req (wait-for
                     ;;                   (reveal state side (:hand runner))
                     ;;                   (continue-ability
                     ;;                     state side
                     ;;                     (choose-n (min 3 (count (:hand runner))) [] (:hand runner))
                     ;;                     card nil)))}
                     ;;   card nil)
                     ;;runner chooses 3 of the cards
                (let [maxcards (min (if (threat-level 3 state) 3 2)
                                    (count (:hand runner)))]
                  (continue-ability
                    state side
                    {:prompt (msg "choose " maxcards " cards to add to the bottom of the Stack")
                     :player :runner
                     :choices {:card #(and (runner? %)
                                           (in-hand? %))
                               :max maxcards
                               :min maxcards}
                     :async true
                     :effect (req (continue-ability
                                    state side
                                    (hide-away targets false)
                                    card nil))}
                    card nil)))}}))

(defcard "[Turnover]"
  ;; When your turn begins, choose 1 to resolve:
  ;; - Turn 1 faceup card in Archives facedown to gain 1{credit}.
  ;; - Turn 1 facedown card in Archives faceup to place 1 advancement counter on a installed card."
  {:implementation "2v6.1"
   :events [{:event :corp-turn-begins
             :prompt "Choose one"
             :interactive (req true)
             :choices (req [(when (seq (:hand corp)) "Trash from HQ (gain 2 draw 1)")
                            (when (some #(not (:seen %)) (:discard corp))
                              "Turn a card faceup (place 1 advancement)")
                            "Done"])
             :async true
             :effect (req (if (= target "Done")
                            (effect-completed state side eid)
                            (continue-ability
                              state side
                              (if (= target "Trash from HQ (gain 1 draw 1)")
                                {:prompt "Turn a card in Archives facedown"
                                 :cost [:trash-from-hand 1]
                                 :msg "gain 2 [Credits] and draw 1"
                                 :effect (req (wait-for (gain-credits state side
                                                                      (make-eid state eid) 2)
                                                        (draw state side eid 1)))}
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
  (let [add-abi
        {:prompt "select an agenda worth 1 or less points"
         :req (req true)
         :async true
         :choices {:req (req (and (agenda? target)
                                  (>= 1 (:agendapoints target))
                                  (some #{:hand} (:zone target))))}
         :waiting-prompt true
         :msg (msg "score " (:title target))
         :effect (effect (score eid target {:no-req true}))
                   ;; (let [c (move state :corp target :scored)]
                   ;;      (card-init state :corp c {:resolve-effect false
                   ;;                                :init-data true}))
                   ;;    (update-all-advancement-requirements state)
                   ;;    (update-all-agenda-points state)
                   ;;    (check-win-by-agenda state side)
                   ;;    (effect-completed state side eid))
         :cancel-effect (effect (system-msg "does not get a free agenda")
                                (effect-completed eid))}]
    {:implementation "2v6"
     :on-score {:async true
                :effect (req (wait-for
                               (draw-up-to state side card 3)
                               (continue-ability state side add-abi card nil)))}}))

(defn- policy-coordination-card
  [cred-gain]
  (let [score-abi {:msg (msg "gain " cred-gain" [Credits]")
                   :interactive (req true)
                   :async true
                   :waiting-prompt false
                   :effect (req (wait-for
                                  (gain-credits state side (make-eid state eid) cred-gain)
                                  (continue-ability
                                    state side
                                    {:choices {:card #(installed? %)}
                                     :msg (msg "place an advancement token on"
                                               (card-str state target))
                                     :effect (effect (add-prop :corp target :advance-counter 1
                                                               {:placed true}))}
                                    card nil)))}]
    {:on-score score-abi
     :derezzed-events [{:event :corp-install
                        :req (req (and
                                    (not= [:hand] (:previous-zone card))
                                    (same-card? (:card target) card)))
                        :async true
                        :waiting-prompt false
                        :effect (effect
                                  (continue-ability
                                    {:waiting-prompt false
                                     :optional
                                     {:prompt "Resolve the when-scored ability?"
                                      :waiting-prompt false
                                      :async true
                                      :yes-ability score-abi}}
                                    card nil))}]
     :implementation "2v6"}))

(defcard "[Policy Coordination A]"
  (policy-coordination-card 4))

(defcard "[Policy Coordination B]"
  (policy-coordination-card 2))

(defcard "Theoretical Economist"
  ;; When your turn begins, place 3{credit} on this asset.
  ;; {click}: Take all credits from this asset. Add this asset to HQ. Then, you may install a card."
  (let [ability {:label "Place 3 credits (start of turn)"
                 :once :per-turn
                 :msg "place 3 credits on itself"
                 :effect (effect (add-counter card :credit 3 {:placed true}))}]
    {:derezzed-events [corp-rez-toast]
     :implementation "2v4"
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

(defcard "Seraphim"
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

(defcard "[Ultra High Frequency]"
  ;; {sub} You may draw 1 card.
  ;; {sub} Do 1 net damage.
  ;; {sub} If there are more cards in HQ than cards in the grip, end the run."
  (let [draw-sub {:async true
                  :label "You may draw a card"
                  :effect (effect (maybe-draw eid card 1))}]
  {:implementation "2v4"
   :subroutines [draw-sub
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
                                   card nil)))}]}))

(defcard "Capacitor"
  ;; This ice gains +1 strength for each tag the Runner has.
  ;; {sub} Gain 21{credit} if the runner is tagged.
  ;; {sub} End the run."
  {:implementation "2v4"
   :static-abilities [(ice-strength-bonus (req (if (pos? (count-tags state)) 2 0)))]
   :subroutines [{:label "Gain 2 [Credits] if the runner is tagged"
                  :async true
                  ;:msg (msg "Gain " (count-tags state) " [Credits]")
                  :effect (req
                            (if-not (zero? (count-tags state))
                              (do
                                (system-msg state side (str "uses " (:title card) "to gain 2 [Credits]"))
                                (gain-credits :corp eid 2))
                              (effect-completed state side eid)))}
                 end-the-run]})

(defcard "[Creative Business Practices]"
  (let [play-instant-second {:optional
                             {:prompt (msg "Pay 4 to gain a click?")
                              :yes-ability {:cost [:credit 5]
                                            :msg (msg "gain [Click]")
                                            :effect (effect (gain-clicks 1))}}}
                             ;; :choices (req (conj (filter #(and (operation? %)
                             ;;                                   (should-trigger? state :corp (assoc eid :source % :source-type :play) % nil (or (:on-play (card-def %)) {}))
                             ;;                                   (can-pay? state side (assoc eid :source % :source-type :play) % nil [:credit (+ 2 (play-cost state side % nil))]))
                             ;;                             (:hand corp))
                             ;;                     "Done"))
                             ;; :async true
                             ;; :effect (req (if (= target "Done")
                             ;;                (effect-completed state side eid)
                             ;;                (wait-for (pay state side (make-eid state eid) card :credit 2)
                             ;;                          (system-msg state side "pays 2 [credits] to play another operation from HQ")
                             ;;                          (play-instant state side (assoc eid :source target :source-type :play) target nil))))}
        play-instant-first {:prompt (msg "Choose a non-terminal operation")
                            :choices (req (conj (filter #(and (operation? %)
                                                              (not (has-subtype? % "Terminal"))
                                                              (should-trigger? state :corp (assoc eid :source % :source-type :play) % nil (or (:on-play (card-def %)) {}))
                                                              (can-pay? state side (assoc eid :source % :source-type :play) % nil [:credit (play-cost state side % nil)]))
                                                        (:hand corp))
                                                "Done"))
                            :async true
                            :effect (req (if (= target "Done")
                                           (effect-completed state side eid)
                                           (wait-for (play-instant state side (assoc (make-eid state eid) :source target :source-type :play) target nil)
                                                     (if-not (threat-level 4 state)
                                                       (effect-completed state side eid)
                                                       (continue-ability
                                                         state side
                                                         play-instant-second
                                                         card nil)))))}]
    {:on-play {:msg "Draw 2 cards"
               :implementation "2v4"
               :effect (req (wait-for (draw state side (make-eid state eid) 2)
                                      (continue-ability
                                        state side
                                        play-instant-first
                                        card nil)))}}))
  
  ;; Draw 2 cards. You may play 1 non-terminal operation from HQ.
  ;; Threat 4 -- you may pay 2[credits] to play a second operation from HQ.
  ;; (letfn [(maybe-play-op [set-aside discount]
  ;;         {:prompt (msg "Choose a non-terminal operation to play " (when discount (str "paying " discount "[Credits] less")))
  ;;          :choices (req (conj (filter #(and (operation? %)
  ;;                                            (not (has-subtype? % "Terminal"))
  ;;                                            (should-trigger? state :corp (assoc eid :source % :source-type :play) % nil (or (:on-play (card-def %)) {}))
  ;;                                            (can-pay? state side (assoc eid :source % :source-type :play) % nil [:credit (play-cost state side % {:cost-bonus discount})]))
  ;;                                      set-aside)
  ;;                              "Done"))
  ;;          :async true
  ;;          :effect (req
  ;;                    (if (= target "Done")
  ;;                      (continue-ability
  ;;                        state side
  ;;                        (return-to-top set-aside)
  ;;                        card nil)
  ;;                      (wait-for (play-instant state side (assoc (make-eid state eid) :source target :source-type :play) target
  ;;                                              {:cost-bonus [:credit discount]})
  ;;                                (let [rem (filter #(not (same-card? % target)) set-aside)]
  ;;                                  (continue-ability
  ;;                                    state side
  ;;                                    (if (and (threat-level 4 state) (not (zero? (count rem))))
  ;;                                      (maybe-play-op rem 0)
  ;;                                      (return-to-top rem))
  ;;                                    card nil)))))})]
  ;; {:implementation "2v3"
  ;;  :on-play {:msg (msg "set aside the top 3 cards of R&D facedown")
  ;;            :async true
  ;;            :effect (req (set-aside-for-me state :corp eid (take 3 (:deck corp)))
  ;;                         (let [top-3 (get-set-aside state :corp eid)]
  ;;                           (continue-ability
  ;;                             state side
  ;;                             (maybe-play-op top-3 -3)
  ;;                             card nil)))}}))

(defcard "The Holo Man"
  ;; Persistent → Whenever the Runner accesses another card in the root of this server,
  ;; you may install the accessed card in the root of another new or existing server.
  ;; (It is no longer being accessed.)
  ;; When your turn ends, you may move this upgrade to the root of another server.
  (let [abi
        ;; {:event :successful-run
        ;;  :interactive (req true)
        ;;  :req (req
        ;;         (system-msg state side (:zone card))
        ;;         (system-msg state side (second (:zone card)))
        ;;         (system-msg state side (:server target))
        ;;         (= (second (:zone card)) (first (:server target))))
        ;;  :async true
        ;;  :prompt "Choose a card to move"
        ;;  :choices {:req (req (and (installed? target)
        ;;                           (in-same-server? target card)))}
        ;;  :msg (msg "reveals " (:title target) " and moves it to the root of another server")
        ;;  :effect (req (let [chosen-card target]
        ;;                 (wait-for
        ;;                   (reveal state side target)
        ;;                   (continue-ability
        ;;                     state side
        ;;                     {:prompt (str "Choose a location to move " (:title target))
        ;;                      :waiting-prompt "corp to use Holo Man"
        ;;                      :choices
        ;;                      (req (remove #(= (zone->name (second (get-zone chosen-card))) %)
        ;;                                   (corp-install-list state chosen-card)))
        ;;                      ;:async true
        ;;                                 ;:msg (msg (corp-install-msg chosen-card))
        ;;                      :msg (msg "move " (:title target) " to the root of " target)
        ;;                      :effect (req (let [c (move state side (get-card state chosen-card)
        ;;                                                 (conj (server->zone state target) :content))]
        ;;                                     (unregister-events state side card)
        ;;                                     (register-default-events state side c)))}
        ;;                     card nil))))}]
        {:cost [:click :credit 4]
         :label "place advancement tokens on a card in or protecting this server"
         :once :per-turn
         :req (req (= :corp (:active-player @state)))
         :choices {:req (req (same-server? card target))}
         :msg (msg "place advancement tokens on " (card-str state target))
         :effect (req
                   (if (no-event? state side :corp-install #(= [:hand] (:previous-zone (:card (first %)))))
                     (add-prop state side eid target :advance-counter 3 {:placed true})
                     (add-prop state side eid target :advance-counter 2 {:placed true})))}]
    {:implementation "2v6.1"
     ;; :on-trash {:req (req (and (= :runner side)
     ;;                            (:run @state)))
     ;;            :effect (req (register-events
     ;;                          state side card
     ;;                          [(assoc abi :req (req (and (installed? target) (= (get-zone target) (:previous-zone card)))) :duration :end-of-run)]))}
     :abilities [abi]
     :events [(assoc mobile-sysop-event :event :corp-turn-begins)]}))

;; WEYLAND CARDS

(defcard "Nuvem SA"
  ;; Whenever you play an operation or take an action on an expendable card,
  ;; look at the top card of R&D. You may trash the top card of R&D to gain 1{credit}.
  ;; (Do this after paying costs for the operation or action, but before resolving its effects.) "
  (let [abi2 {:once :per-turn
              :event :corp-trash
              :req (req
                     (= :corp (:active-player @state))
                     (= [:deck] (:zone (:card target))))
              :msg "gain 2 [Credit]"
              :async true
              :effect (req (gain-credits state :corp eid 2))}
        abi1 {:prompt (msg "The top card of R&D is " (:title (first (:deck corp))))
              :choices ["Trash it" "Done"]
              :async true
              :effect (req (if (= target "Done")
                             (effect-completed state side eid)
                             (do (system-msg state side "trashes the top card of R&D")
                                 (mill state :corp eid :corp 1))))}]
    {:implementation "2v6 - nuvem resolves on play instead of after resolution, I'll fix this later"
     :events [(assoc abi1 :event :expended)
              (assoc abi1 :event :play-operation)
              abi2]}))

(defcard "[Rapid Activation Project]"
  ;; {click}, 1{credit}, reveal and trash this agenda from HQ:
  ;;  Install and rez a card from HQ, reducing the total cost by 6.
  ;; When you score this agenda, you may search R&D for a card.
  ;;  Install and rez that card, ignoring all costs."
  (let [expend-abi {:req (req (some ice? (:hand corp)))
                    :cost [:credit 1]
                    :prompt "install and rez a card, paying 5 less"
                    :choices {:card #(and (in-hand? %)
                                   (ice? %))}
                    :msg (msg "install and rez a card, paying 5 less")
                    :async true
                    :effect (req (corp-install state side (make-eid state eid) target nil
                                               {:install-state :rezzed
                                                :combined-credit-discount 5}))}
        score-abi {:interactive (req true)
                   :optional
                   {:prompt "Search R&D for a card to install and rez, ignoring all costs?"
                    :yes-ability
                    {:async true
                     :effect (effect
                               (continue-ability
                                 (if (not-empty (filter #(not (operation? %)) (:deck corp)))
                                   {:async true
                                    :prompt "Choose a card to install"
                                    :choices (req (filter #(not (operation? %)) (:deck corp)))
                                    :effect (effect (shuffle! :deck)
                                                    (corp-install eid target nil
                                                                  {:install-state :rezzed-no-cost
                                                                   :ignore-all-cost true}))}
                                   {:prompt "You have no installables in R&D!"
                                    :choices ["Carry on!"]
                                    :prompt-type :bogus
                                    :effect (effect (shuffle! :deck))})
                                 card nil))}}}]
    {:implementation "2v3"
     :on-score score-abi
     :expend expend-abi}))

(defn- buildcon-98
  []
  {:on-score {:effect (effect (add-counter card :agenda 2))
              :silent (req true)}
   :stolen {:async true
              :effect (effect (continue-ability (corp-recur) card nil))}
   :flags {:has-abilities-when-stolen true}
   :implementation "2v6: cost is paid after you pick the target - I'll fix this later"
   :abilities [(into (corp-recur) {:cost [:trash-from-deck 1 :agenda 1]})]})

(defcard "[BuildCon 98]"
  (buildcon-98))

;; (defcard "[BuildCon 98 B]"
;;   (buildcon-98))
  ;; {:on-score {:effect (effect (add-counter card :agenda 3))
  ;;             :silent (req true)}
  ;;  :stolen {:effect (effect (add-counter card :agenda 1))
  ;;           :silent (req true)}
  ;;  :flags {:has-abilities-when-stolen true}
  ;;  :implementation "2v5: cost is paid after you pick the target - I'll fix this later"
  ;;  :abilities [(into (corp-recur) {:cost [:trash-from-deck 1 :agenda 1]})]})

(defcard "[Thought Leader Conference]"
  ;; When your turn begins, you may move 1 advancement counter from one of your installed cards
  ;;  to another of your installed cards.
  ;; Then, if this server was not protected by ice, place 1 advancement counter on one of
  ;; your installed cards. "
  (let [political {:req (req unprotected)
                   :prompt "Place 1 advancement token on a card"
                   :choices {:card #(and (can-be-advanced? %)
                                         (installed? %))}
                   :msg (msg "place 1 advancement token on " (card-str state target))
                   :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]
    {:derezzed-events [corp-rez-toast]
     :implementation "2v3"
     :flags {:corp-phase-12 (req true)}
     :abilities [{:label "Manipulate tokens (start of turn)"
                  :once :per-turn
                  :waiting-prompt "Corp to make a decision"
                  :prompt "move a token"
                  :choices {:card #(and (installed? %)
                                        (get-counters % :advancement))}
                  :effect (effect
                            (continue-ability
                              (let [from-ice target]
                                {:prompt "Move to where?"
                                 :choices {:card #(and (installed? %)
                                                       (not (same-card? from-ice %)))}
                                 :msg (msg "move an advancement token from "
                                           (card-str state from-ice)
                                           " to "
                                           (card-str state target))
                                 :effect (effect (add-prop :corp target :advance-counter 1)
                                                 (add-prop :corp from-ice :advance-counter -1)
                                                 (continue-ability political card nil))
                                 :cancel-effect (effect (continue-ability political card nil))})
                              card nil))
                  :cancel-effect (effect (continue-ability political card nil))}]}))

(defcard "Hangman"
  ;; {click}, 1{credit}, reveal and trash this ice from HQ:
  ;;  Gain 2{credit}. Reveal up to 2 agendas from HQ and/or Archives and shuffle them into R&D.
  ;; When your turn begins, you may add this ice to HQ.
  ;;
  ;; {sub} End the run."
  (let [shuffle-ab {:prompt "Reveal up to 2 agendas in HQ or archives"
                    :label "reveal and shuffle agendas"
                    :cost [:credit 1]
                    :choices {:max 2
                              :card #(and (agenda? %)
                                          (or (in-hand? %)
                                              (in-discard? %)))}
                    :async true
                    :show-discard true
                    :cancel-effect (req (effect-completed eid))
                    :effect (req (wait-for
                                   (reveal state side targets)
                                   (doseq [c targets]
                                     (move state :corp c :deck))
                                   (shuffle! state :corp :deck)
                                   (let [from-hq (map :title (filter in-hand? targets))
                                         from-archives (map :title (filter in-discard? targets))]
                                     (system-msg
                                       state side
                                       (str "uses Preacher to shuffle "
                                            (str/join
                                              " and "
                                              (filter identity
                                                      [(when (not-empty from-hq)
                                                         (str (str/join " and " from-hq)
                                                              " from HQ"))
                                                       (when (not-empty from-archives)
                                                         (str (str/join " and " from-archives)
                                                              " from Archives"))]))
                                            " into R&D")))
                                   (effect-completed state side eid)))}]
    {:implementation "2v6.1"
     :events [{:event :corp-turn-begins
               :interactive (req true)
               :req (req (rezzed? card))
               :optional {:prompt (msg "add " (:title card) " to HQ?")
                          :yes-ability {:effect (req (move state side card :hand))
                                        :msg (msg "adds " (:title card) " to HQ")}}}]
     :expend shuffle-ab
     :subroutines [end-the-run]}))

(defcard "Hammer"
  ;; Subroutines on this ice cannot be broken by AI programs or by non-icebreaker cards.
  ;;  {sub} Give the Runner 1 tag.
  ;;  {sub} Trash 1 installed hardware or virtual resource.
  ;; {sub} Trash 1 installed non-icebreaker program."
  {:implementation "2v6.1. Breaking restriction is manual"
   :subroutines [(give-tags 1)
                 {:label "trash 1 hardware or virtual resource"
                  :msg (msg "trash " (:title target))
                  :prompt (req (str "trash a hardware or virtual resource"))
                  :choices {:req (req (and (installed? target)
                                           (or (hardware? target)
                                               (and (resource? target)
                                                    (has-subtype? target "Virtual")))))}
                  :async true
                  :effect (effect (trash eid target {:cause :subroutine}))}
                 {:label "trash 1 non-icebreaker program"
                  :msg (msg "trash " (:title target))
                  :prompt (req (str "trash 1 non-icebreaker program"))
                  :choices {:req (req (and (installed? target)
                                           (program? target)
                                           (not (has-subtype? target "Icebreaker"))))}
                  :async true
                  :effect (effect (trash eid target {:cause :subroutine}))}]})

(defcard "Logjam"
  ;; Threat 3 — This ice gets +1 strength for each card type faceup in Archives.
  ;;
  ;; {sub} Gain 2{credit}.
  ;; {sub} End the run.
  ;; {sub} End the run.
  ;; {sub} End the run.
  ;; {sub} End the run.
  {:implementation "2v6.1"
   :static-abilities [(ice-strength-bonus (req (faceup-archives-types corp)))]
   :on-rez {:req (req (threat-level 3 state))
            :prompt "Select cards in archives to turn faceup"
            :show-discard true
            :choices {:max (req (count (filter #(not (:seen %)) (:discard corp))))
                      :card #(and (corp? %)
                                  (not (:seen %))
                                  (in-discard? %))}
            :msg (msg "turns " (str/join ", " (map :title targets)) " in archives faceup")
            :effect (req (doseq [t targets]
                           (update! state side (assoc-in t [:seen] true))))}
   :subroutines [{:msg "gain 2 [Credits] and end the run"
                  :async true
                  :effect (req (wait-for (gain-credits state side 2)
                                         (end-run state side eid card)))}
                 end-the-run
                 end-the-run
                 end-the-run]})

(defcard "[Director’s Visit]"
  ;; Choose one to resolve:
  ;; - Remove all virus counters from an installed card
  ;; - Place 1 advancement counter on up to 2 installed cards that can be advanced.
  ;;
  ;; Threat 3 — You may do both."
  (let [faux-purge {:choices {:req (req (and (installed? target)
                                             (pos? (get-counters target :virus))))}
                    :effect (effect (add-counter target :virus (* -1 (get-counters target :virus))))
                    :msg (msg "remove all virus counters from " (:title target))}
        kaguya {:choices {:max 2
                          :card #(and (corp? %)
                                      (installed? %)
                                      (can-be-advanced? %))}
                      :msg (msg "place 1 advancement token on " (quantify (count targets) "card"))
                      :effect (req (doseq [t targets]
                                     (add-prop state :corp t :advance-counter 1 {:placed true})))}]
  {:implementation "2v4"
   :on-play
   {:prompt "Choose one"
    :choices (req ["Remove virus counters from a card"
                   "Place 1 advancement on up to two cards"
                   (when (threat-level 3 state) "Do both")])
    :async true
    :effect (req (if (or (= target "Remove virus counters from a card") (= target "Do both"))
                   (wait-for (resolve-ability state side faux-purge card nil)
                             (continue-ability
                               state side (when (= target "Do both") kaguya)
                               card nil))
                   (continue-ability state side kaguya card nil)))}}))

(defcard "[Isaac Coroa]"
  ;; Advanced ice protecting this server get +2 strength.
  ;; When your turn ends, you may move this upgrade to another server.
  ;; If you do, place 1 advancement counter on a piece of ice protecting the new server."
  (let [motility {:prompt "Choose a server"
                  :waiting-prompt true
                  :choices (req (remove #(= (zone->name (second (get-zone card))) %)
                                        (server-list state)))
                  :msg (msg "move itself to " target)
                  :effect (req (let [c (move state side card
                                             (conj (server->zone state target) :content))]
                                 (unregister-events state side card)
                                 (register-default-events state side c)))}
        ability {:interactive (req (->> (all-installed state :corp)
                                        (filter #(and (ice? %)
                                                      (same-server? (get-card state card) %)))
                                        count
                                        pos?))
                 :label "place 1 advancement counter (then move)"
                 :async true
                 :effect
                 (effect
                   (continue-ability
                     (when (->> (all-installed state :corp)
                                (filter #(and (ice? %)
                                              (same-server? (get-card state card) %)))
                                count
                                pos?)
                       {:prompt (str "Place 1 advancement counter on an ice protecting " (zone->name (second (get-zone card))))
                        :choices {:card #(and (ice? %)
                                              (same-server? % card))}
                        :msg (msg "place 1 advancement counter on " (card-str state target))
                        :async true
                        :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                        (continue-ability motility card nil))})
                     card nil))}]
    {:implementation "2v3"
     :static-abilities [{:type :ice-strength
                         :req (req (and (ice? target)
                                        (= (card->server state card) (card->server state target))))
                         :value (req (if (pos? (get-counters target :advancement)) 2 0))}]
     :events [{:event :corp-turn-ends
               :optional {:prompt (msg "place an advancement counter?")
                          :yes-ability ability}}]}))
                          ;; :yes-ability {:async true
                          ;;               :effect (effect (continue-ability
                          ;;                                 {:prompt "Choose a server"
                          ;;                                  :choices (server-list state)
                          ;;                                  :msg (msg "move to " target)
                          ;;                                  :async true
                          ;;                                  :effect (req (let [c (move state side card
                          ;;                                                             (conj (server->zone state target) :content))]
                          ;;                                                 (unregister-events state side card)
                          ;;                                                 (register-default-events state side c)
                          ;;                                                 (continue-ability
                          ;;                                                   state side
                          ;;                                                   ability
                          ;;                                                   (get-card state card) nil)))}
                          ;;                                 card nil))}}}]}))

;; NEUTRAL CORP

(defcard "[Party Endorsement]"
  ;; Whenever you score an agenda, you may install 1 card from Archives or HQ,
  ;; ignoring the install cost
  {:implementation "2v4"
   :events [{:event :agenda-scored
             :prompt "Choose a card from Archives or HQ to install"
             :show-discard true
             :interactive (req true)
             :async true
             :choices {:card #(and (not (operation? %))
                                   (corp? %)
                                   (or (in-hand? %)
                                       (in-discard? %)))}
             :msg (msg (corp-install-msg target))
             :effect (effect (corp-install eid target nil {:ignore-install-cost true}))}]})
;; When you rez this card, load 3 power counters on it. When it is empty, trash it.
  ;; Whenever an agenda is scored or stolen, you may remove 1 hosted power
  ;; counter to install a card from HQ or Archives in a new or existing remote server."
  ;; (let [abi {:prompt "Choose a card from HQ or Archives to install in a new remote"
  ;;            :show-discard true
  ;;            :interactive (req true)
  ;;            :async true
  ;;            :cost [:power 1]
  ;;            :choices {:card #(and (not (operation? %))
  ;;                                  (corp? %)
  ;;                                  (or (in-discard? %)
  ;;                                      (in-hand? %)))}
  ;;            :msg (msg (corp-install-msg target))
  ;;            :effect (effect (corp-install eid target nil))}]
  ;;   {:data {:counter {:power 3}}
  ;;    :implementation "2v3 - remote enforcement is manual"
  ;;    :events [(trash-on-empty :power)
  ;;             (assoc abi :event :agenda-scored)
  ;;             (assoc abi :event :agenda-stolen)]}))
