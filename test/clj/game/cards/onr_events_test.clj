(ns game.cards.onr-events-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest onr-networking
  ;; ONR Networking
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["ONR Networking"]}})
    (take-credits state :corp)
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "ONR Networking")
      (is (= 2 (:click (get-runner))) "ONR Networking is a double")
      (is (= (+ credits -3 9) (:credit (get-runner))) "Runner should spend 3 and gain 9"))))
