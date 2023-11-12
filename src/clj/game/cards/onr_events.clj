(ns game.cards.onr-events
  (:require
   [game.core.card :refer [agenda? asset? can-be-advanced? corp? event? corp-installable-type?
                           faceup? fake-identity? get-advancement-requirement
                           get-agenda-points get-card get-counters get-title get-zone hardware? has-subtype? ice?
                           identity? in-deck? in-discard? in-hand? in-server? installed? is-type?
                           operation? program? resource? rezzed? runner? upgrade?]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-clicks
                                                            lose-credits]]
   [game.core.props :refer [add-counter add-icon add-prop remove-icon set-prop]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   ))

(defcard "ONR Networking"
  {:on-play
   {:msg "gain 9 [Credits]"
    :async true
    :effect (effect (gain-credits eid 9))}})
