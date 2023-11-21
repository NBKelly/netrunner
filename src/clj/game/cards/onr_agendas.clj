(ns game.cards.onr-agendas
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.access :refer [steal-cost-bonus]]
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
   [game.core.damage :refer [damage damage-bonus]]
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
   [game.core.moving :refer [forfeit mill move move-zone swap-cards swap-ice
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
   [game.core.onr-utils :refer [dice-roll gain-or-forgo]]
   ))

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
     :abilities [{:label "Take 3 [Credits] from this Corporate Coup"
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

(defcard "ONR Polymer Breakthrough"
  {:events [{:event :corp-turn-begins
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})


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

(defcard "ONR World Domination"
  {:on-score {:msg "gain and additional 4 agenda points 👀"
              :effect (req (register-lingering-effect
                             state side nil
                             {:type :user-agenda-points
                              ;; `target` is either `:corp` or `:runner`
                              :req (req (= :corp target))
                              :value 4})
                           (update-all-agenda-points state side)
                           (check-win-by-agenda state side))}})
