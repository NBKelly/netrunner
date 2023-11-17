(ns game.cards.onr-upgrades-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest onr-olivia-salazar
  (do-game
    (new-game {:corp {:hand ["ONR Olivia Salazar" "ONR Data Wall"]}})
    (play-from-hand state :corp "ONR Olivia Salazar" "HQ")
    (play-from-hand state :corp "ONR Data Wall" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [liz (get-content state :hq 0)
          wall (get-ice state :hq 0)]
      (rez state :corp liz)
      (card-ability state :corp (refresh liz) 0)
      (changes-val-macro
        0 (:credit (get-corp))
        "spent 0 to rez"
        (click-card state :corp wall))
      (is (rezzed? (refresh wall)))
      (run-continue state :encounter-ice)
      (card-subroutine state :corp wall 0)
      (is (not (rezzed? (refresh wall)))))))

(deftest onr-tokyo-chiba-infighting
  (do-game
    (new-game {:corp {:hand ["ONR Tokyo-Chiba Infighting"]}})
    (play-from-hand state :corp "ONR Tokyo-Chiba Infighting" "New remote")
    (let [card (get-content state :remote1 0)]
      (is (rezzed? (refresh card)) "regions auto-rez")
      (take-credits state :corp)
      (changes-val-macro
        2 (:credit (get-corp))
        "gained 2 for unsuccessful run"
        (run-on state :remote1)
        (run-jack-out state)))))

(deftest onr-twenty-four-hour-surveillance
  ;; ONR Twenty-Four-Hour Surviellance
  ;; no implementation
  true
  )
