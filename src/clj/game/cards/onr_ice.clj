(ns game.cards.onr-ice
  (:require
   [clojure.string :as str]
   [cond-plus.core :refer [cond+]]
   [game.cards.ice :refer [end-the-run]]
   [game.core.access :refer [access-bonus access-card breach-server max-access]]
   [game.core.bad-publicity :refer [gain-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed all-installed-runner-type card->server
                            get-all-cards get-all-installed server->zone]]
   [game.core.card :refer [active? agenda? asset? can-be-advanced? card-index
                           corp? corp-installable-type? faceup?
                           get-card get-counters get-zone
                           hardware? has-subtype? ice? in-discard? in-hand? installed? is-type? operation?
                           program? protecting-a-central? protecting-archives? protecting-hq? protecting-rd?
                           resource? rezzed? runner?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage]]
   [game.core.def-helpers :refer [combine-abilities corp-recur defcard
                                  do-brain-damage do-net-damage offer-jack-out
                                  reorder-choice get-x-fn]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [get-effects register-lingering-effect unregister-static-abilities]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid]]
   [game.core.engine :refer [gather-events pay register-events resolve-ability
                             trigger-event trigger-event-simult unregister-events]]
   [game.core.events :refer [run-events]]
   [game.core.finding :refer [find-cid]]
   [game.core.flags :refer [can-rez? card-flag? prevent-draw prevent-jack-out
                            register-run-flag! register-turn-flag! run-flag? zone-locked?]]
   [game.core.gaining :refer [gain-credits lose-clicks lose-credits]]
   [game.core.hand-size :refer [hand-size]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [add-sub add-sub! any-subs-broken? break-sub get-current-ice ice-strength-bonus
                          remove-sub! remove-subs! resolve-subroutine
                          set-current-ice unbroken-subroutines-choice update-all-ice update-all-icebreakers
                          update-ice-strength]]
   [game.core.identities :refer [disable-card enable-card]]
   [game.core.initializing :refer [card-init]]
   [game.core.installing :refer [corp-install corp-install-list
                                 corp-install-msg]]
   [game.core.memory :refer [available-mu init-mu-cost]]
   [game.core.moving :refer [as-agenda mill move swap-cards swap-cards-async
                             swap-ice swap-installed trash
                             trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay? cost->string build-cost-label]]
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-icon add-prop remove-icon]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez get-rez-cost rez]]
   [game.core.runs :refer [bypass-ice encounter-ends end-run
                           force-ice-encounter get-current-encounter prevent-access
                           redirect-run set-next-phase]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name protecting-same-server?
                              target-server zone->name]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.subtypes :refer [update-all-subtypes]]
   [game.core.tags :refer [gain-tags lose-tags sum-tag-effects]]
   [game.core.threat :refer [threat threat-level]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

(defcard "ONR Code Corpse"
  {:subroutines [(do-brain-damage 1)
                (do-brain-damage 1)
                end-the-run]})

(defcard "ONR Quandary"
  {:subroutines [end-the-run]})
