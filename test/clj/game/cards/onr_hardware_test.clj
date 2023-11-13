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
