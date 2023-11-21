(ns game.cards.onr-assets
  (:require
   [clojure.pprint :as pprint]
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.access :refer [access-card installed-access-trigger]]
   [game.core.actions :refer [score]]
   [game.core.agendas :refer [update-all-advancement-requirements
                              update-all-agenda-points]]
   [game.core.bad-publicity :refer [bad-publicity-prevent gain-bad-publicity
                                    lose-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed get-remotes
                            installable-servers]]
   [game.core.card :refer [agenda? asset? can-be-advanced? corp? event? corp-installable-type?
                           faceup? fake-identity? get-advancement-requirement
                           get-agenda-points get-card get-counters get-title get-zone hardware? has-subtype? ice?
                           identity? in-deck? in-discard? in-hand? in-server? installed? is-type?
                           operation? program? resource? rezzed? runner? upgrade?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.damage :refer [damage damage-prevent]]
   [game.core.def-helpers :refer [corp-recur corp-rez-toast defcard
                                  reorder-choice trash-on-empty get-x-fn]]
   [game.core.drawing :refer [draw first-time-draw-bonus max-draw draw-bonus
                              remaining-draws]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [complete-with-result effect-completed is-basic-advance-action? make-eid]]
   [game.core.engine :refer [pay register-events resolve-ability]]
   [game.core.events :refer [first-event? no-event? turn-events]]
   [game.core.expose :refer [expose-prevent]]
   [game.core.flags :refer [lock-zone prevent-current
                            prevent-draw
                            register-turn-flag! release-zone]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-clicks
                              lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+ runner-hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [add-extra-sub! remove-extra-subs! update-all-ice
                          update-ice-strength]]
   [game.core.identities :refer [disable-card enable-card]]
   [game.core.initializing :refer [card-init]]
   [game.core.installing :refer [corp-install corp-install-msg]]
   [game.core.moving :refer [as-agenda mill move remove-from-currently-drawing
                             swap-cards swap-installed trash trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay? cost-value]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable]]
   [game.core.props :refer [add-counter add-icon add-prop remove-icon set-prop]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [end-run]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [is-central? is-remote? target-server zone->name]]
   [game.core.set-aside :refer [get-set-aside set-aside-for-me swap-set-aside-cards]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck
                                shuffle-into-rd-effect]]
   [game.core.tags :refer [gain-tags]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.core.winning :refer [check-win-by-agenda win]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]
   [game.core.link :refer [get-link]]
   [game.cards.assets :refer [campaign]]
   ))

(defcard "ONR Braindance Campaign"
  (campaign 12 2))

(defcard "ONR Department of Truth Enhancement"
  {:abilities [{:cost [:click 1]
                :msg "place 3 [Credits]"
                :label "place 3 [Credits]"
                :async true
                :effect (req (add-counter state side eid card :credit 3 nil))}
               {:cost [:click 1]
                :req (req (pos? (get-counters card :credit)))
                :label "take all credits"
                :msg (msg "take " (get-counters card :credit) " credits")
                :async true
                :effect (req (let [creds (get-counters card :credit)]
                               (add-counter state side card :credit (- creds))
                               (gain-credits state :corp eid creds)))}]})

(defcard "ONR ESA Contract"
  {:abilities [{:cost [:click 1]
                :msg "draw 2 cards"
                :async true
                :effect (effect (draw eid 2))}]})

(defcard "ONR Fortress Architects"
  {:static-abilities [{:type :install-cost
                       :req (req (ice? target))
                       :value -1}]})

(defcard "ONR I Got a Rock"
  {:abilities [{:cost [:click 1 :agenda-point 3]
                :label "Do 15 meat damage"
                :msg "do 15 meat damage"
                :req (req (<= 2 (count-tags state)))
                :async true
                :effect (effect (damage eid :meat 15 {:card card}))}]})

(defcard "ONR Investment Firm"
  (let [ability {:msg "take 1 [Credits]"
                 :label "Take 1 [Credits] (start of turn)"
                 :once :per-turn
                 :req (req (pos? (get-counters card :credit)))
                 :async true
                 :effect (effect (add-counter card :credit -1)
                                 (gain-credits eid 1))}]
    {:implementation "Manual - click ONR Investment Firm to place credits counters on itself"
     :abilities [ability
                 {:label "Place 2 credits"
                  :cost [:credit 1]
                  :msg "place 2 credits on itself"
                  :effect (effect (add-counter card :credit 2))}]
     :events [(assoc ability :event :corp-turn-begins)]}))

(defcard "ONR Krumz"
  {:recurring 1
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(defcard "ONR Nevinyrral"
  {:in-play [:click-per-turn 1]
   :on-rez {:effect (req (system-msg state side (str "uses " (:title card) " to gain 1 additional [Click] per turn"))
                         (when (= :corp (:active-player @state))
                           (gain-clicks state :corp 1))
                         (gain state :corp :click-per-turn 1))}
   :leave-play (req (when (rezzed? card)
                      (win state :runner "\"Executive Termination\"")))})

(defcard "ONR Rescheduler"
  {:abilities [{:cost [:click 1]
                :label  "shuffle all cards in HQ into R&D and draw that many cards"
                :async true
                :msg (msg "shuffle HQ into R&D and draw " (count (:hand corp)) " cards")
                :effect (req (let [cards (count (:hand corp))]
                               (shuffle-into-deck state side :hand)
                               (draw state side eid cards)))}]})

(defcard "ONR Rockerboy Promotion"
  {:data {:counter {:credit 15}}
   :events [(trash-on-empty :credit)]
   :abilities [{:label "Take 3 [Credits] from this asset"
                :cost [:click 1]
                :keep-menu-open :while-clicks-left
                :msg (msg "gain " (min 3 (get-counters card :credit)) " [Credits]")
                :async true
                :effect (req (let [credits (min 3 (get-counters card :credit))]
                               (wait-for (gain-credits state :corp (make-eid state eid) credits)
                                         (add-counter state side card :credit (- credits) {:placed true})
                                         (effect-completed state side eid))))}]})

(defcard "ONR Rustbelt HQ Branch"
  {:static-abilities [(corp-hand-size+ 2)]})

(defcard "ONR Solo Squad"
  {:abilities [{:req (req tagged)
                :cost [:click 1]
                :keep-menu-open :while-clicks-left
                :effect (effect (damage eid :meat 1 {:card card}))
                :msg "do 1 meat damage"}]})

(defcard "ONR Stereogram Antibody"
  {:on-access {:req (req (in-discard? card))
               :msg "do 1 net damage and shuffle itself into R&D"
               :async true
               :effect (req (wait-for (damage state side (make-eid state eid) :net 1 {:card card})
                                      (move state :corp card :deck nil)
                                      (shuffle! state :corp :deck)
                                      (effect-completed state side eid)))}})

(defcard "ONR Strategic Planning Group"
  {:derezzed-events [corp-rez-toast]
   :events [{:event :pre-corp-draw
             :msg "draw 1 additional card"
             ;; The req catches draw events that happened before the card was installed
             :effect (req (draw-bonus state side 1))}
            {:event :corp-draw
             :interactive (req true)
             :async true
             :effect (req (let [drawn corp-currently-drawing]
                            (continue-ability
                              state side
                              (when (seq drawn)
                                {:waiting-prompt true
                                 :prompt (str "Choose a card to add to the bottom of R&D")
                                 :choices {:max 1
                                           :card #(some (fn [c] (same-card? c %)) drawn)
                                           :all true}
                                 :effect (req (doseq [c (reverse targets)]
                                                (system-msg state side
                                                            (str "uses " (:title card) " to add the "
                                                                 (pprint/cl-format nil "~:R" (inc (first (keep-indexed #(when (same-card? c %2) %1) drawn))))
                                                                 " card drawn to the bottom of R&D"))
                                                (move state side c :deck)
                                                (remove-from-currently-drawing state side c)))})
                              card nil)))}]})
