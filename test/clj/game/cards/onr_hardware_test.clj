(ns game.cards.onr-hardware-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest bodyweight-data-creche
  ;; Doppelgänger - run again when successful
  (do-game
    (new-game {:runner {:deck ["ONR Bodyweight [TM] Data Crèche"]}})
    (core/gain state :corp :bad-publicity 1)
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Bodyweight [TM] Data Crèche")
    (run-empty-server state :hq)
    (click-prompt state :runner "No action")
    (is (zero? (:run-credit (get-runner))) "Runner lost BP credits")
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "R&D")
    (is (:run @state) "New run started")
    (is (= [:rd] (:server (:run @state))) "Running on R&D")
    (is (= 1 (:run-credit (get-runner))) "Runner has 1 BP credit")))

(deftest onr-mram-chip
  (do-game
    (new-game {:runner {:hand ["ONR MRAM Chip"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR MRAM Chip")
    (is (= 7 (hand-size :runner)))))

(deftest onr-raven-microcyb-eagle
  (do-game
    (new-game {:runner {:hand ["ONR Raven Microcryb Eagle", "Sure Gamble", "Corroder"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "ONR Raven Microcyb Eagle")
    (play-from-hand state :runner "Corroder")
    (let [corr (get-program state 0)]
      (card-ability state :runner (refresh corr) 1)
      (run-on state :hq)
      (card-ability state :runner (refresh corr) 1)
      (click-card state :runner (get-hardware state 0)))))
