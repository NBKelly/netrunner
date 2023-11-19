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

(deftest onr-red-herrings
  ;; Red Herrings
  (do-game
      (new-game {:corp {:deck ["ONR Red Herrings" "House of Knives"]}})
      (play-from-hand state :corp "ONR Red Herrings" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (let [rh (get-content state :remote1 0)
            hok (get-content state :remote1 1)]
        (rez state :corp rh)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner hok)
        ;; prompt should be asking for the 5cr cost
        (is (= "House of Knives" (:title (:card (prompt-map :runner))))
            "Prompt to pay 5cr")
        (click-prompt state :runner "No action")
        (is (= 5 (:credit (get-runner))) "Runner was not charged 5cr")
        (is (zero? (count (:scored (get-runner)))) "No scored agendas")
        (click-prompt state :runner "No action")
        (run-empty-server state "Server 1")
        (click-card state :runner hok)
        (click-prompt state :runner "Pay to steal")
        (is (zero? (:credit (get-runner))) "Runner was charged 5cr")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest onr-red-herrings-cost-increase-even-when-trashed
    ;; Cost increase even when trashed
    (do-game
      (new-game {:corp {:deck [(qty "ONR Red Herrings" 3) (qty "House of Knives" 3)]}})
      (play-from-hand state :corp "ONR Red Herrings" "New remote")
      (play-from-hand state :corp "House of Knives" "Server 1")
      (take-credits state :corp 1)
      (core/gain state :runner :credit 1)
      (let [rh (get-content state :remote1 0)]
        (rez state :corp rh)
        (run-empty-server state "Server 1")
        ;; runner now chooses which to access.
        (click-card state :runner rh)
        (click-prompt state :runner "Pay 1 [Credits] to trash") ; pay to trash
        ;; should now have prompt to pay 5cr for HoK
        (click-prompt state :runner "Pay to steal")
        (is (zero? (:credit (get-runner))) "Runner was charged 5cr")
        (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

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
