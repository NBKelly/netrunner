(ns game.cards.onr-hardware-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(defn- provides-memory
  [card x]
  (do-game
    (new-game {:runner {:hand [card]
                        :credits 50}})
    (take-credits state :corp)
    (play-from-hand state :runner card)
    (is (= (+ 4 x) (core/available-mu state)) (str "Gain " x " memory"))))

(defn- provides-hand-size
  [card x]
  (do-game
    (new-game {:runner {:credits 50
                        :hand [card]}})
    (take-credits state :corp)
    (play-from-hand state :runner card)
    (is (= (+ 5 x) (hand-size :runner)) (str card " provides " x " hand size"))))

;;; tests below here


(deftest onr-armadillo-armored-road-home
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["ONR \"Armadillo\" Armored Road Home"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "ONR \"Armadillo\" Armored Road Home")
      (gain-tags state :corp 1)
      (let [cs (get-hardware state 0)]
        (changes-val-macro 0 (:credit (get-runner))
                           "Used 2 credit from Crash Space"
                           (remove-tag state :runner)
                           (click-card state :runner cs)
                           (click-card state :runner cs)))))

(deftest onr-drifter-mobile-environment
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["ONR \"Drifter\" Mobile Environment"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "ONR \"Drifter\" Mobile Environment")
      (gain-tags state :corp 1)
      (let [cs (get-hardware state 0)]
        (changes-val-macro 0 (:credit (get-runner))
                           "Used 2 credit from Crash Space"
                           (remove-tag state :runner)
                           (click-card state :runner cs)
                           (click-card state :runner cs)))))

(deftest onr-armored-fridge
  ;; Plascrete Carapace - Prevent meat damage
  (do-game
    (new-game {:corp {:deck ["Scorched Earth"]}
               :runner {:deck ["ONR Armored Fridge" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Armored Fridge")
    (let [plas (get-hardware state 0)]
      (is (= 7 (get-counters (refresh plas) :power)) "7 counters on install")
      (take-credits state :runner)
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Scorched Earth")
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (is (= 3 (get-counters (refresh plas) :power)) "3 counters left!"))))

(deftest onr-artemis-2020-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:credits 15
                          :deck ["ONR Artemis 2020" "Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Artemis 2020")
      (play-from-hand state :runner "Corroder")
      (run-on state :hq)
      (let [corr (get-program state 0)
            ppvp (get-hardware state 0)]
        (changes-val-macro
          0 (:credit (get-runner))
          "Used 1 credit from "
          (card-ability state :runner (refresh corr) 1)
          (click-card state :runner ppvp)))))

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

(deftest onr-corolla-speed-chip
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["ONR Corolla Speed Chip" "Bukhgalter"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Corolla Speed Chip")
      (play-from-hand state :runner "Bukhgalter")
      (let [sil (get-hardware state 0)
            dag (get-program state 0)]
        (changes-val-macro 0 (:credit (get-runner))
                           "Used 1 credit from Silencer"
                           (card-ability state :runner dag 1)
                           (click-card state :runner sil)))))

(deftest onr-eurocorpse-spin-chip-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:credits 10
                          :hand ["ONR Eurocorpse [TM] Spin Chip" "Inti"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Eurocorpse [TM] Spin Chip")
      (let [omni (get-hardware state 0)]
          (card-ability state :runner omni 0)
          (click-card state :runner (find-card "Inti" (:hand (get-runner))))
          (let [inti (first (:hosted (refresh omni)))]
            (changes-val-macro 0 (:credit (get-runner))
                               "Used 2 credits from Omni-drive"
                               (card-ability state :runner inti 1)
                               (click-card state :runner omni)
                               (click-card state :runner omni))))))

(deftest lucidrine-drip-feed
  ;; Stim Dealer - Take 1 brain damage when it accumulates 2 power counters
  (do-game
    (new-game {:runner {:credits 10
                        :deck ["ONR Lucidrine [TM] Drip Feed" "Sure Gamble" "Feedback Filter"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Feedback Filter")
    (play-from-hand state :runner "ONR Lucidrine [TM] Drip Feed")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [sd (get-hardware state 1)]
      (is (= 1 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-runner))) "Gained 1 click")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-runner))) "Gained 1 click")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (zero? (get-counters (refresh sd) :power)) "Lost all counters")
      (is (no-prompt? state :runner) "No Feedback Filter brain dmg prevention possible")
      (is (= 1 (:brain-damage (get-runner))) "Took 1 core damage")
      (is (= 4 (:click (get-runner))) "Didn't gain extra click"))))

(deftest onr-mram-chip
  (provides-hand-size "ONR MRAM Chip" 2))

(deftest onr-mram-chip
  (provides-hand-size "ONR Militech MRAM Chip" 3))

(deftest onr-parraline-5750
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:credits 15
                          :deck ["ONR Parraline 5750" "Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Parraline 5750")
      (play-from-hand state :runner "Corroder")
      (run-on state :hq)
      (let [corr (get-program state 0)
            ppvp (get-hardware state 0)]
        (changes-val-macro
          0 (:credit (get-runner))
          "Used 1 credit from "
          (card-ability state :runner (refresh corr) 1)
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

(deftest onr-tycho-mem-chip
  (provides-memory "ONR Tycho Mem Chip" 3))

(deftest onr-wutech-mem-chip
  (provides-memory "ONR WuTech Mem Chip" 1))

(deftest onr-zetatech-mem-chip
  (provides-memory "ONR Zetatech Mem Chip" 2))

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
