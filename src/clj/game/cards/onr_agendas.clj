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
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [pay register-events resolve-ability
                             unregister-events]]
   [game.core.events :refer [first-event? first-run-event? no-event? run-events run-event-count turn-events]]
   [game.core.finding :refer [find-latest]]
   [game.core.flags :refer [in-runner-scored? is-scored? register-run-flag!
                            register-turn-flag! when-scored? zone-locked?]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-clicks
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
   [jinteki.utils :refer :all]))

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
