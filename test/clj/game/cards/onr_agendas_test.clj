(ns game.cards.onr-agendas-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core.eid :refer [make-eid]]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest onr-bioweapons-engineering
  ;; The Cleaners
  (do-game
      (new-game {:corp {:deck ["ONR Bioweapons Engineering" "Scorched Earth"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 3)]}})
      (play-and-score state "ONR Bioweapons Engineering")
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (zero? (count (:hand (get-runner)))) "5 damage dealt to Runner")))

;; Hostile Takeover
(deftest onr-charity-takeover
  (do-game
    (new-game {:corp {:deck ["ONR Charity Takeover"]}})
    (play-and-score state "ONR Charity Takeover")
    (is (= 14 (:credit (get-corp))) "Gain 9 credits")
    (is (= 1 (count-bad-pub state)) "Take 1 bad publicity")))

(deftest onr-corporate-retreat
  ;; Government Takeover
  (do-game
    (new-game {:corp {:deck ["ONR Corporate Retreat" "Ice Wall"]}})
    (play-and-score state "ONR Corporate Retreat")
    (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
    (let [gt-scored (get-scored state :corp 0)]
      (card-ability state :corp gt-scored 0)
      (is (= 7 (:credit (get-corp))) "Should gain 2 credits from 5 to 7")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (card-ability state :corp gt-scored 0)
      (is (= 7 (:credit (get-corp))) "Should not be able to use the ability!"))))

(deftest onr-corporate-war
  ;; Corporate War
  (do-game
    (new-game {:corp {:deck [(qty "ONR Corporate War" 2)]}})
    (is (= 5 (:credit (get-corp))))
    (play-and-score state "ONR Corporate War")
    (is (zero? (:credit (get-corp))) "Lost all credits")
    (core/gain state :corp :credit 12)
    (play-and-score state "Corporate War")
    (is (= 24 (:credit (get-corp))) "Had 12 credits when scoring, gained another 12")))

(deftest onr-ice-transmutation
  (do-game
    (new-game {:corp {:hand ["Hydra" "ONR Ice Transmutation"]
                      :credits 15}})
    (play-from-hand state :corp "Hydra" "HQ")
    (let [hydra (get-ice state :hq 0)]
      (rez state :corp hydra)
      (play-from-hand state :corp "ONR Ice Transmutation" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-card state :corp (refresh hydra))
      (is (= 7 (get-strength (refresh hydra))))
      (is (= 3 (count (:subroutines (refresh hydra)))))
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state :encounter-ice)
      (is (= 6 (count (:subroutines (refresh hydra)))))
      (run-continue state :pass-ice)
      (is (= 3 (count (:subroutines (refresh hydra))))))))

(deftest onr-polymer-breakthrough
  ;; Rezeki - gain 1c when turn begins
  (do-game
    (new-game {:corp {:deck ["ONR Polymer Breakthrough"]}})
    (play-and-score state "ONR Polymer Breakthrough")
    (take-credits state :corp)
    (let [credits (:credit (get-corp))]
      (take-credits state :runner)
      (is (= (:credit (get-corp)) (+ credits 1)) "Gain 1 from Polymer Breakthrough"))))

(deftest onr-subsidiary-branch
    ;; Gain an additional click
    (do-game
      (new-game {:corp {:deck ["ONR Subsidiary Branch"
                               "Melange Mining Corp."]}})
      (play-and-score state "ONR Subsidiary Branch")
      (is (= 1 (:agenda-point (get-corp))))
      (play-from-hand state :corp "Melange Mining Corp." "New remote")
      (let [mmc (get-content state :remote2 0)]
        (rez state :corp mmc)
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 4 (:click (get-corp))))
        (card-ability state :corp mmc 0)
        (is (= 1 (:click (get-corp)))))))

(deftest onr-world-domination
  (do-game
    (new-game {:corp {:deck ["ONR World Domination"]}})
    (play-and-score state "ONR World Domination")
    (is (= 7 (:agenda-point (get-corp))))))
