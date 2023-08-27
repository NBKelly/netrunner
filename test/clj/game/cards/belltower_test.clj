(ns game.cards.belltower-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

;; anarch cards

;;(deftest sebastio  ) - no test here, I can't figure out the accents

(deftest hq-occupation
  ;; HQ Occupation
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Hostile Takeover" 3)]}
               :runner {:hand ["[HQ Occupation]"]}})
    (take-credits state :corp)
    (is (= 0 (count-tags state)))
    (play-run-event state "[HQ Occupation]" :hq)
    (dotimes [_ 3]
      (click-prompt state :runner "Steal"))
    (is (= 1 (count-tags state)))
    (is (not (:run @state)) "Run has finished")))

(deftest eye-for-and-eye-basic
  (do-game
    (new-game {:corp {:hand ["Ice Wall" "Enigma"]}
               :runner {:hand ["[Eye for an Eye]"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Enigma" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "[Eye for an Eye]")
    (is (= 0 (count-tags state)))
    (run-continue state :success)
    (click-prompt state :runner "[Eye for an Eye]")
    (is (= 1 (count-tags state)))
    (click-card state :corp (get-ice state :hq 0))
    (is (no-prompt? state :corp))
    (is (not (:run @state)) "Run ended")
    (is (= 1 (count (:discard (get-corp)))))))

(deftest eye-for-and-eye-basic-threat
  (do-game
    (new-game {:corp {:hand ["Ice Wall" "Enigma" "City Works Project" "Project Atlas"]}
               :runner {:hand ["[Eye for an Eye]"]}})
    (play-and-score state "City Works Project")
    (play-and-score state "Project Atlas")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Enigma" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "[Eye for an Eye]")
    (is (= 0 (count-tags state)))
    (run-continue state :success)
    (click-prompt state :runner "[Eye for an Eye]")
    (is (= 1 (count-tags state)))
    (click-card state :corp (get-ice state :hq 0))
    (click-card state :corp (get-ice state :rd 0))
    (is (no-prompt? state :corp))
    (is (not (:run @state)) "Run ended")
    (is (= 1 (count (:discard (get-corp)))))))
