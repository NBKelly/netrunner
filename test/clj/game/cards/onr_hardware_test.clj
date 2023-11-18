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

(deftest onr-parraline-5750
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["ONR Parraline 5750" "Corroder"]}
                 :credits 15})
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Parraline 5750")
      (play-from-hand state :runner "Corroder")
      (run-on state :hq)
      (let [ppvp (get-hardware state 0)]
        (changes-val-macro -1 (:credit (get-runner))
                           "Used 1 credit from "
                           (card-ability state (get-program state 0) 0)
                           (click-card state :runner ppvp)))))

(deftest onr-raven-microcyb-eagle
  (do-game
    (new-game {:runner {:hand ["ONR Raven Microcyb Eagle", "Sure Gamble", "Corroder"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "ONR Raven Microcyb Eagle")
    (play-from-hand state :runner "Corroder")
    (let [corr (get-program state 0)]
      (card-ability state :runner (refresh corr) 1)
      (run-on state :hq)
      (card-ability state :runner (refresh corr) 1)
      (click-card state :runner (get-hardware state 0)))))

(deftest onr-the-deck
  ;; Trace 6 - Give the runner 1 tag for each point your trace exceeded their link
  (do-game
    (new-game {:corp {:deck ["ONR Manhunt"]}
               :runner {:hand ["ONR The Deck" (qty "Sure Gamble" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "ONR The Deck")
    (run-empty-server state :rd)
    (take-credits state :runner)
    ;; 7 - 4 means that our max trace is 3
    ;; the runner should also have 6 credits
    (play-from-hand state :corp "ONR Manhunt")
    (click-prompt state :corp "3")
    (click-prompt state :runner "ONR The Deck")
    (changes-val-macro
      -0 (:credit (get-runner))
      "spent 0c to boost link to 5"
      (click-prompt state :runner "0 [Credits]: Base Link 5"))
    (changes-val-macro
      -1 (:credit (get-runner))
      "spent 1c to boost link to 6"
      (click-prompt state :runner "1 [Credits]: +1 Link"))
    (click-prompt state :runner "Done")
    (is (= 0 (count-tags state)) "Runner should have 0 tags")))

(deftest zetatech-portastation-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["ONR Zetatech Portastation" "Dirty Laundry"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Zetatech Portastation")
      (let [ppvp (get-hardware state 0)]
        (changes-val-macro -1 (:credit (get-runner))
                           "Used 1 credit from "
                           (play-from-hand state :runner "Dirty Laundry")
                           (click-card state :runner ppvp)))))
