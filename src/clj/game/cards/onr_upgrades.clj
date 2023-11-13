(ns game.cards.onr-upgrades
  (:require
   [clojure.string :as str]
   [cond-plus.core :refer [cond+]]
   [game.core.access :refer [access-bonus set-only-card-to-access
                             installed-access-trigger
                             steal-cost-bonus]]
   [game.core.bad-publicity :refer [lose-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed card->server
                            get-remotes server->zone server-list]]
   [game.core.card :refer [agenda? asset? can-be-advanced?
                           corp-installable-type? corp? get-card get-counters get-zone
                           has-subtype? ice? in-discard? in-hand? installed? operation? program? resource? rezzed?
                           runner? upgrade?]]
   [game.core.cost-fns :refer [install-cost rez-cost]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage]]
   [game.core.def-helpers :refer [corp-rez-toast defcard offer-jack-out
                                  reorder-choice get-x-fn]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [effect-completed get-ability-targets is-basic-advance-action? make-eid]]
   [game.core.engine :refer [dissoc-req pay register-default-events
                             register-events resolve-ability unregister-events]]
   [game.core.events :refer [first-event? first-run-event? turn-events]]
   [game.core.expose :refer [expose-prevent]]
   [game.core.finding :refer [find-cid find-latest]]
   [game.core.flags :refer [clear-persistent-flag! is-scored? register-persistent-flag!
                            register-run-flag!]]
   [game.core.gaining :refer [gain-credits lose-clicks lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+]]
   [game.core.ice :refer [all-subs-broken? get-run-ices pump-ice resolve-subroutine!
                          unbroken-subroutines-choice update-all-ice update-all-icebreakers]]
   [game.core.installing :refer [corp-install corp-install-list]]
   [game.core.moving :refer [mill move remove-from-currently-drawing
                             swap-cards swap-ice trash trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay? cost-value]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-prop set-prop]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [rez]]
   [game.core.runs :refer [end-run force-ice-encounter jack-out redirect-run
                           set-next-phase start-next-phase]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->zone from-same-server? in-same-server?
                              is-central? protecting-same-server? same-server?
                              target-server unknown->kw zone->name]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.tags :refer [gain-tags]]
   [game.core.threat :refer [threat-level]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

(defcard "ONR Twenty-Four-Hour Surviellance"
  ;; TODO - find a way to do this. I know it can be done.
  {:implementation "Unimplemented/Manual Implementation"})

(defcard "ONR Weapons Depot"
  {:static-abilities [{:type :advancement-requirement
                       :req (req (and (in-same-server? card target)
                                      (has-subtype? target "Black Ops")))
                       :value -1}]})
