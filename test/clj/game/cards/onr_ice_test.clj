(ns game.cards.onr-ice-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest onr-fetch-4.0.1
  ;; Subroutine is trace 3 give a tag
  (do-game
    (new-game {:corp {:deck ["ONR Fetch 4.0.1"]}})
    (play-from-hand state :corp "ONR Fetch 4.0.1" "HQ")
    (let [resistor (get-ice state :hq 0)]
      (rez state :corp resistor)
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (fire-subs state resistor)
      (click-prompt state :corp "0")
      (is (= 1 (count-tags state)) "Runner has gained 1 tag"))))

(deftest onr-networking
  ;; ONR Networking
  (do-game
    (new-game {:corp {:deck ["ONR Code Corpse" (qty "Hedge Fund" 1)]}
               :runner {:hand [(qty "ONR Networking" 3)]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "ONR Code Corpse" "HQ")
    (take-credits state :corp)
    (let [cc (get-ice state :hq 0)]
      (rez state :corp cc)
      (run-on state "HQ")
      (run-continue state)
      (card-subroutine state :corp (refresh cc) 0)
      (is (= 1 (:brain-damage (get-runner))) "Runner took 1 core damage")
      (card-subroutine state :corp (refresh cc) 1)
      (is (= 2 (:brain-damage (get-runner))) "Runner took 1 more core damage")
      (card-subroutine state :corp (refresh cc) 2)
      (is (not (:run @state)) "Run is ended"))))
