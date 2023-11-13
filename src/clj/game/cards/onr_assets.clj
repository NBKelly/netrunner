(ns game.cards.onr-assets
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
