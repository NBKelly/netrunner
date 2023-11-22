(ns game.cards.onr-agendas
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.access :refer [steal-cost-bonus steal]]
   [game.core.actions :refer [score]]
   [game.core.agendas :refer [update-all-advancement-requirements
                              update-all-agenda-points]]
   [game.core.bad-publicity :refer [gain-bad-publicity lose-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed all-installed-corp
                            all-installed-runner-type get-remote-names installable-servers server->zone]]
   [game.core.card :refer [agenda? asset? can-be-advanced?
                           corp-installable-type? corp? facedown? faceup? get-agenda-points
                           get-card get-counters get-title get-zone has-subtype? ice? in-discard? in-hand?
                           in-scored? installed? operation? program? resource? rezzed? runner? upgrade?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.cost-fns :refer [rez-cost install-cost]]
   [game.core.damage :refer [damage damage-bonus damage-prevent]]
   [game.core.def-helpers :refer [corp-recur defcard do-net-damage
                                  offer-jack-out reorder-choice get-x-fn]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid]]
   [game.core.engine :refer [pay register-events resolve-ability
                             unregister-events]]
   [game.core.events :refer [first-event? first-run-event? no-event? run-events run-event-count turn-events]]
   [game.core.finding :refer [find-latest]]
   [game.core.flags :refer [in-runner-scored? is-scored? register-run-flag!
                            register-turn-flag! when-scored? zone-locked?]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-clicks lose-click-debt
                              lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+ runner-hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [add-extra-sub! insert-extra-sub! remove-sub!
                          insert-extra-sub! remove-subs!
                          update-all-ice update-all-icebreakers]]
   [game.core.initializing :refer [card-init]]
   [game.core.installing :refer [corp-install corp-install-list
                                 corp-install-msg]]
   [game.core.moving :refer [as-program as-agenda as-agenda-card-only forfeit mill move move-zone swap-cards swap-ice
                             trash trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay?]]
   [game.core.prompts :refer [cancellable clear-wait-prompt show-wait-prompt]]
   [game.core.props :refer [add-counter add-prop add-icon]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [end-run jack-out-prevent]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [is-remote? target-server zone->name]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck
                                shuffle-into-rd-effect]]
   [game.core.tags :refer [gain-tags]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.core.winning :refer [check-win-by-agenda]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]
   [game.core.onr-utils :refer [dice-roll gain-or-forgo onr-trace-tag]]
   ))

(defn- add-agenda-point-counters
  "Adds a number of agenda counters to an agenda that checks for a win"
  [state side card counters]
  (add-counter state side card :agenda counters)
  (update-all-agenda-points state side)
  (check-win-by-agenda state side))

;; card impl's

(defcard "ONR AI Board Member"
  {:events [{:event :corp-turn-begins
             :interactive (req true)
             :async true
             :effect (req (let [di (dice-roll)
                                action (cond
                                         (= di 1) "install a card"
                                         (= di 2) "gain a credit"
                                         (= di 3) "gain a credit"
                                         :else "draw a card")]
                            (continue-ability
                              state side
                              {:optional
                               {:prompt (msg "Gain a click (" action ")")
                                :yes-ability
                                {:async true
                                 :effect (req (wait-for
                                                (gain-or-forgo state side (make-eid state eid))
                                                (system-msg state side async-result)
                                                (continue-ability
                                                  state side
                                                  (if (= async-result "Forgo Click")
                                                    {:msg (msg "roll a " di "(1d6), and gains an action ("action") - but they Forgo that action!")}
                                                    {:msg (msg "roll a " di "(1d6), and gains an action ("action"), which they must take this turn")})
                                                  card nil)))}
                                :no-ability {:msg (msg "roll a " di "(1d6), and refuse to take that action")}}}
                              card nil)))}]})

(defcard "ONR AI Chief Financial Officer"
  {:abilities [{:cost [:click 1 :agenda 1]
                :msg "shuffle HQ into R&D, then draw 5 cards"
                :keep-menu-open :while-agenda-tokens-left
                :async true
                :effect (effect
                          (shuffle-into-deck :hand)
                          (draw eid 5))}]})

(defcard "ONR Artificial Security Directors"
  {:static-abilities [{:type :advancement-requirement
                       :req (req (and (agenda? target)
                                      (has-subtype? target "Black Ops")))
                       :value -1}]})

(defcard "ONR Bioweapons Engineering"
  {:events [{:event :pre-damage
             :req (req (and (= target :meat)))
                            ;; this doesn't specificy corp side!!!
                            ;;(= side :corp)))
             :msg "do 1 additional meat damage"
             :effect (effect (damage-bonus :meat 1))}]})

(defcard "ONR Black Ice Quality Assurance"
  {:static-abilities [{:type :ice-strength
                       :req (req (has-subtype? target "Black Ice"))
                       :value 2}]})

(defcard "ONR Charity Takeover"
  {:on-score {:msg "gain 9 [Credits] and take 1 bad publicity"
              :async true
              :effect (req (wait-for (gain-credits state side 9)
                                     (gain-bad-publicity state :corp eid 1)))
              :interactive (req true)}})

(defcard "ONR Corporate Boon"
  {:on-score {:silent (req true)
              :effect (effect (add-counter card :agenda 4))}
   :implementation "uses agenda counters instead of boon counters"
   :abilities [{:cost [:agenda 1]
                :req (req (= :corp (:active-player @state)))
                :once :per-turn
                :effect (effect (gain-clicks 1))
                :msg "gain [Click]"}]})

(defcard "ONR Corporate Coup"
    {:data {:counter {:credit 15}}
     :abilities [{:label "Take 3 [Credits] from this card"
                  :cost [:click 1]
                  :keep-open :while-clicks-left
                  :msg (msg "gain " (min 3 (get-counters card :credit)) " [Credits]")
                  :async true
                  :effect (req (let [credits (min 3 (get-counters card :credit))]
                                 (wait-for (gain-credits state :corp (make-eid state eid) credits)
                                           (add-counter state side card :credit (- credits) {:placed true})
                                           (effect-completed state side eid))))}]})

(defcard "ONR Corporate Downsizing"
  {:on-score {:async true
              :effect
              (req (let [max-ch (count (filter agenda? (:hand corp)))]
                     (continue-ability
                       state side
                       {:choices {:max max-ch
                                  :card #(and (corp? %)
                                              (agenda? %)
                                              (in-hand? %))}
                        :msg (msg "reveal " (enumerate-str (map :title (sort-by :title targets))) " from HQ and gain " (* 2 (reduce + 0 (map :agendapoints targets))) " [Credits], then shuffle them into R&D")
                        :async true
                        :effect (req (wait-for
                                       (reveal state side targets)
                                       (wait-for
                                         (gain-credits state side (make-eid state eid) (* 2 (reduce + 0 (map :agendapoints targets))))
                                         (doseq [c targets]
                                           (move state side c :deck))
                                         (shuffle! state side :deck)
                                         (effect-completed state side eid))))}
                       card nil)))}})

(defcard "ONR Corporate Headhunters"
  {:abilities [{:req (req tagged)
                :cost [:click 1]
                :keep-menu-open :while-clicks-left
                :effect (req (wait-for (damage state side (make-eid state eid) :meat 1 {:card card})
                                       (let [cards-trashed async-result]
                                         (when (seq cards-trashed)
                                           (do (system-msg state side (str "uses " (:title card) " to reduce the Runner's maximum hand size by 1"))
                                               (register-lingering-effect
                                                 state side card
                                                 {:type :hand-size
                                                  :req (req (= :runner side))
                                                  :value -1})))
                                         (effect-completed state side eid))))
                :msg "do 1 meat damage"}]})

(defcard "ONR Corporate Retreat"
  (let [ev {:silent (req true)
           :effect (effect (update! (assoc-in card [:special :corporate-retreat-disabled] true)))}]
    {:events [(assoc ev :event :rez)
              (assoc ev :event :corp-install)]
     :abilities [{:cost [:click 1]
                  :req (req (not (get-in card [:special :corporate-retreat-disabled])))
                  :async true
                  :keep-menu-open :while-clicks-left
                  :effect (effect (gain-credits eid 2))
                  :msg "gain 2 [Credits]"}]}))

(defcard "ONR Corporate War"
  {:on-score
   {:msg (msg (if (> (:credit corp) 11) "gain 12 [Credits]" "lose all credits"))
    :interactive (req true)
    :async true
    :effect (req (if (> (:credit corp) 11)
                   (gain-credits state :corp eid 12)
                   (lose-credits state :corp eid :all)))}})

(defcard "ONR Data Fort Reclamation"
  (letfn [(install-ability [server-name n]
            {:prompt "Choose a card from HQ to install"
             :choices {:card #(and (corp? %)
                                   (not (operation? %))
                                   (or (in-hand? %)))}
             :msg (msg (corp-install-msg target)
                       (when (zero? n)
                         ", creating a new remote server")
                       ", ignoring all install costs")
             :async true
             :effect (req (wait-for (corp-install state side target server-name {:ignore-all-cost true})
                                    ;; option to rez the card, using those creds on this card
                                    (let [installed-card async-result]
                                      (continue-ability
                                        state side
                                        {:optional {:prompt (msg "Rez " (:title installed-card))
                                                    :async true
                                                    :no-ability {:effect (req (continue-ability
                                                                                state side
                                                                                (when (< n 3)
                                                                                  (install-ability (last (get-remote-names state)) (inc n)))
                                                                                card nil))}
                                                    :yes-ability {:effect (req (wait-for (rez state side (make-eid state eid) installed-card)
                                                                                         (continue-ability
                                                                                           state side
                                                                                           (when (< n 3)
                                                                                             (install-ability (last (get-remote-names state)) (inc n)))
                                                                                           card nil)))
                                                                  :async true}}}
                                        card nil))))})]
    {:interactions {:pay-credits {:req (req (= :rez (:source-type eid)))
                                  :type :credit}}
     :implementation "10 credits are placed on the card, instead of gained"
     :on-score
     {:optional
      {:prompt "Install cards in a new remote server?"
       :yes-ability {:async true
                     :effect (req (add-counter state side (get-card state card) :credit 10 {:placed true})
                                  (wait-for
                                    (resolve-ability
                                      state side
                                      (install-ability "New remote" 0)
                                      card nil)
                                    (add-counter state side (get-card state card) :credit (- (get-counters (get-card state card) :credit)) {:placed true})
                                    (effect-completed state side eid)))}}}}))

(defcard "ONR Data Fort Remapping"
  {:implementation "uses agenda counters"
   :on-score {:silent (req true)
              :effect (effect (add-counter card :agenda 1))}
   :abilities [{:req (req (:run @state))
                :cost [:agenda 1]
                :msg "end the run"
                :async true
                :effect (effect (end-run eid card))}]})

(defcard "ONR Detroit Police Contract"
  (let [e {:req (req (pos? (get-counters card :credit)))
           :msg "gain 2 [Credits]"
           :async true
           :effect (req (add-counter state side card :credit -2)
                        (gain-credits state :corp eid 2))}]
    {:on-score {:effect (effect (add-counter card :credit 12))
                :silent (req true)}
     :events [(assoc e :event :corp-turn-begins)]}))

(defcard "ONR Employee Empowerment"
  (let [ability {:once :per-turn
                 :async true
                 :label "Draw 1 card (start of turn)"
                 :interactive (req true)
                 :effect (effect (continue-ability
                                   {:optional
                                    {:prompt (msg "Use " (:title card) " to draw 1 card?")
                                     :yes-ability {:async true
                                                   :msg "draw 1 card"
                                                   :effect (effect (draw eid 1))}}}
                                   card nil))}]
    {:flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability
                 {:cost [:click 1]
                  :label "draw 2 cards"
                  :msg "draw 2 cards"
                  :async true
                  :effect (effect (draw eid 2))}]}))

(defcard "ONR Executive Extraction"
  {:static-abilities [{:type :advancement-requirement
                       :req (req (and (agenda? target)
                                      (has-subtype? target "Gray Ops")))
                       :value -1}]})

(defcard "ONR Fetal AI"
  {:flags {:rd-reveal (req true)}
   :on-access {:async true
               :req (req (not (in-discard? card)))
               :msg "do 2 net damage"
               :effect (effect (damage eid :net 2 {:card card}))}
   :steal-cost-bonus (req [:credit 2])})

(defcard "ONR Genetics-Visionary Acquisition"
  {:static-abilities [{:type :advancement-requirement
                       :req (req (and (agenda? target)
                                      (has-subtype? target "Research")))
                       :value -1}]})

(defcard "ONR Hostile Takeover"
  {:on-score {:msg "gain 5 [Credits]"
              :async true
              :effect (req (gain-credits state side eid 5))
              :interactive (req true)}})

(defcard "ONR Ice Transmutation"
  {:implementation "Only doubles subroutines natural to the card (even if non-printed). I've chosen to apply this on encounter."
   :on-score {:req (req (some #(and (ice? %) (rezzed? %)) (all-installed-corp state)))
              :waiting-prompt true
              :prompt "choose an ice to transmute"
              :choices {:card #(and (installed? %)
                                    (rezzed? %)
                                    (ice? %))}
              :msg (msg "transmute " (card-str state target))
              :effect (effect
                        (add-icon card target "T" (faction-label card))
                        (update! (assoc-in card [:special :transmutation] target)))}
   :events [{:event :encounter-ice
             :once :per-encounter
             :interactive (req true)
             :req (req (same-card? (get-in card [:special :transmutation]) (:ice context)))
             :msg (msg "duplicate subroutines on the ice")
             :async true
             :effect (req
                       (doseq [sub (reverse (filter #(= (:from-cid %) (:cid (:ice context))) (:subroutines (:ice context))))]
                         (insert-extra-sub! state side (get-card state (:ice context)) sub (:cid card) (:index sub) {:printed false :variable true}))
                       (register-events
                         state side card
                         [{:event :end-of-encounter
                           :unregister-once-resolved true
                           :req (req true)
                           :duration :end-of-run
                           :effect (req
                                     (remove-subs! state side (get-in card [:special :transmutation]) #(= (:cid card) (:from-cid %))))}])
                       (effect-completed state side eid))}]
   :static-abilities [{:type :ice-strength
                       :req (req (same-card? target (get-in card [:special :transmutation])))
                       :value 1}]})

(defcard "ONR Main-Office Relocation"
  {:on-score {:silent (req true)
              :msg "increase their maximum hand size by 2"}
   :static-abilities [(corp-hand-size+ 2)]})

(defcard "ONR Marine Arcology"
  {:abilities [{:cost [:click 2]
                :msg "gain 3 [Credits]"
                :async true
                :keep-open :while-2-clicks-left
                :effect (effect (gain-credits eid 3))}]})

(defcard "ONR Marked Accounts"
    {:flags {:rd-reveal (req true)}
     :on-access {:msg "give the Runner 1 tag"
                 :async true
                 :effect (effect (gain-tags eid 1))}})

(defcard "ONR Netwatch Operations Office"
  {:abilities [(assoc (onr-trace-tag 2) :cost [:click 1])]})


(defcard "ONR Please Don't Choke Anyone"
  {:interactions {:prevent [{:type #{:net :meat :brain}
                             :req (req (= :corp (:side target)))}]}
   :implementation "actually uses agenda counters"
   :abilities [{:label (msg "Prevent 1 damage to place PDCA counter on " (:title card))
                :msg (msg "prevent 1 damage and place a PDCA counter on " (:title card))
                :async true
                :req (req true)
                :effect (req (add-counter state side card :agenda 1)
                             (damage-prevent state :corp :net 1)
                             (damage-prevent state :corp :meat 1)
                             (damage-prevent state :corp :brain 1))}
               {:label (msg "Gain a click")
                :cost [:agenda 1]
                :req (req (= :corp (:active-player @state)))
                :effect (effect (gain-clicks 1))
                :msg "gain [Click]"}]})

(defcard "ONR Political Coup"
    {:data {:counter {:credit 12}}
     :abilities [{:label "Take 3 [Credits] from this card"
                  :cost [:click 1]
                  :keep-open :while-clicks-left
                  :msg (msg "gain " (min 3 (get-counters card :credit)) " [Credits]")
                  :async true
                  :effect (req (let [credits (min 3 (get-counters card :credit))]
                                 (wait-for (gain-credits state :corp (make-eid state eid) credits)
                                           (add-counter state side card :credit (- credits) {:placed true})
                                           (effect-completed state side eid))))}]})

(defcard "ONR Political Overthrow"
  {:abilities [{:cost [:click 1]
                :async true
                :keep-menu-open :while-clicks-left
                :effect (effect (gain-credits eid 3))
                :msg "gain 3 [Credits]"}]})

(defcard "ONR Polymer Breakthrough"
  {:events [{:event :corp-turn-begins
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "ONR Priority Requisition"
  {:on-score {:interactive (req true)
              :choices {:card #(and (ice? %)
                                    (not (rezzed? %))
                                    (installed? %))}
              :async true
              :effect (effect (rez eid target {:ignore-cost :all-costs}))}})

(defcard "ONR Private Cybernet Police"
  {:abilities [(assoc (onr-trace-tag 5) :cost [:click 1])]})

(defcard "ONR Project Babylon"
  {:agendapoints-runner (req 2)
   :agendapoints-corp (req (+ 1 (get-counters card :agenda)))
   :on-score {:interactive (req true)
              :effect (effect (add-agenda-point-counters card (quot (- (get-counters (:card context) :advancement) 3) 2)))}})

(defcard "ONR Project Venice"
  {:events [{:event :corp-turn-begins
             :req (req (pos? (get-counters (get-card state card) :agenda)))
             :msg (msg "gain " (repeat (get-counters card :agenda) "[Click]"))
             :effect (req (gain-clicks state side (get-counters card :agenda)))}]
   :on-score {:interactive (req true)
              :effect (effect (add-agenda-point-counters card (quot (- (get-counters (:card context) :advancement) 4) 3)))}})

(defcard "ONR Project Zurich"
  {:events [{:event :corp-turn-begins
             :req (req (pos? (get-counters (get-card state card) :agenda)))
             :msg (msg "gain " (repeat (get-counters card :agenda) "[Credits]"))
             :async true
             :effect (req (gain-credits state side eid (get-counters card :agenda)))}]
   :on-score {:interactive (req true)
              :effect (effect (add-agenda-point-counters card (quot (- (get-counters (:card context) :advancement) 3) 2)))}})

(defcard "ONR Security Purge"
  (letfn [(abt [choices]
            {:async true
             :prompt "Choose a card to install and rez at no cost"
             :choices (cancellable (filter ice? choices) :sorted)
             :cancel-effect (effect (unregister-events card)
                                    (system-msg (str "declines to use " (get-title card) " to install any of the top 3 cards or R&D"))
                                    (trash-cards eid (map #(assoc % :seen true) choices) {:unpreventable true :cause-card card}))
             :effect (req (wait-for (corp-install state side target nil
                                                  {:ignore-all-cost true
                                                   :install-state :rezzed-no-cost})
                                    (let [choices (remove-once #(= target %) choices)]
                                      (cond
                                        ;; Shuffle ends the ability
                                        (get-in (get-card state card) [:special :shuffle-occurred])
                                        (do (unregister-events state side card)
                                            (trash-cards state side eid choices {:unpreventable true :cause-card card}))
                                        ;; There are still ice left
                                        (seq (filter ice? choices))
                                        (continue-ability state side (abt choices) card nil)
                                        ;; Trash what's left
                                        :else
                                        (do (unregister-events state side card)
                                            (wait-for
                                              (trash-cards state side eid (map #(assoc % :seen true) choices) {:unpreventable true :cause-card card})
                                              (system-msg state side async-result)))))))})]
    {:on-score
     {:interactive (req true)
      :async true
      :msg "reveal at the top 3 cards of R&D"
      :effect (req (register-events
                     state side card
                     [{:event :corp-shuffle-deck
                       :effect (effect (update! (assoc-in card [:special :shuffle-occurred] true)))}])
                   (let [choices (take 3 (:deck corp))]
                    (wait-for
                      (resolve-ability state side
                                       {:async true
                                        :msg (msg "reveals " (str/join ", " (map :title choices)) " from the top of R&D")
                                        :prompt (str "The top cards of R&D are (top->bottom): "
                                                     (enumerate-str (map get-title choices)))
                                        :choices ["OK"]}
                                       card nil)
                      (continue-ability state side (abt choices) card nil))))}}))

(defcard "ONR Strike Force Kali"
  {:abilities [{:req (req tagged)
                :cost [:click 1]
                :keep-menu-open :while-clicks-left
                :effect (effect (damage eid :meat 2 {:card card}))
                :msg "do 2 meat damage"}]})

(defcard "ONR Subsidiary Branch"
  {:move-zone (req (when (and (in-scored? card)
                              (= :corp (:scored-side card)))
                     (system-msg state side (str "uses " (:title card) " to gain 1 additional [Click] per turn"))
                     (when (= :corp (:active-player @state))
                       (gain-clicks state :corp 1))
                     (gain state :corp :click-per-turn 1)))
   :leave-play (req (lose state :corp
                          :click 1
                          :click-per-turn 1))})

(defcard "ONR Theorem Proof"
  {:steal-cost-bonus (req [:credit 999]) ;; on-access is apparently too slow to register the inability to steal it...
   :leave-play (effect (continue-ability
                         {:req (req (and (program? card) ;;lmao
                                         (not (get-in card [:special :being-scored]))))
                          :once :per-turn
                          :msg "remove itself from the game"
                          :effect (effect (move (get-card state card) :rfg))}
                         card nil))
   :abilities [{:player :runner
                :label "Score theorem proof"
                :req (req (program? card))
                :cost [:click 1]
                :async true
                :effect (req
                          (update! state side (assoc-in card [:special :being-scored] true))
                          (steal state side eid (as-agenda-card-only state :runner (get-card state card) 3)))}]
   :on-access {:optional {:prompt "Install theorem proof as a program worth 2 memory?"
                          :player :runner
                          :yes-ability {:msg (msg "Installs " (:title card) " as a program worth 2 memory")
                                        :effect (req (as-program state :runner card 2))}}}})

(defcard "ONR Tycho Extension" {:implementation "PSYCHO TYCHO"}) ;; no special implementation

(defcard "ONR Unlisted Research Lab"
  (let [ability {:msg "draw 1 card"
                 :label "draw 1 card (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (req (draw state :corp eid 1))}]
    {:flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "ONR World Domination"
  {:on-score {:msg "gain an additional 4 agenda points 👀"}
   :agendapoints-runner (req 3)
   :agendapoints-corp (req 7)})

              ;; :effect (req (register-lingering-effect
              ;;                state side nil
              ;;                {:type :user-agenda-points
              ;;                 ;; `target` is either `:corp` or `:runner`
              ;;                 :req (req (= :corp target))
              ;;                 :value 4})
              ;;              (update-all-agenda-points state side)
              ;;              (check-win-by-agenda state side))}})
