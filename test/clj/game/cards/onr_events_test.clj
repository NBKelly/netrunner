(ns game.cards.onr-events-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest onr-all-hands
  ;; Legwork
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Hostile Takeover" 5)]}
               :runner {:hand ["ONR All-Hands"]}})
    (take-credits state :corp)
    (play-run-event state "ONR All-Hands" :hq)
    (dotimes [_ 4]
      (click-prompt state :runner "Steal"))
    (is (not (:run @state)) "Run has finished")))

(deftest onr-anonymous-tip
  ;; Emergency Shutdown
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["ONR Brain Drain"]}
               :runner {:hand ["ONR Anonymous Tip"]}})
    (play-from-hand state :corp "ONR Brain Drain" "New remote")
    (let [iw (get-ice state :remote1 0)]
      (rez state :corp iw)
      (is (rezzed? (refresh iw)) "Ice Wall is rezzed")
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Anonymous Tip")
      (is (not (no-prompt? state :runner)) "Runner has a derez prompt")
      (click-card state :runner (refresh iw))
      (is (not (rezzed? (refresh iw))) "Ice Wall is derezzed"))))

(deftest onr-blackmail
  ;; Notoriety - Run all 3 central servers successfully and play to gain 1 agenda point
  (do-game
    (new-game {:corp {:deck ["Hedge Fund"]}
               :runner {:deck ["ONR Blackmail"]
                        :credits 15}})
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (play-run-event state "ONR Blackmail" :hq)
    (is (= 1 (count (:scored (get-runner)))) "Notoriety moved to score area")
    (is (= 1 (:agenda-point (get-runner))) "Notoriety scored for 1 agenda point")))

(deftest onr-bodyweight-tm-synthetic-blood
  ;; Quality Time
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["ONR Bodyweight [TM] Synthetic Blood"]}})
    (take-credits state :corp)
    (is (= 1 (count (:hand (get-runner)))) "Runner should have 1 card in hand")
    (play-from-hand state :runner "ONR Bodyweight [TM] Synthetic Blood")
    (is (= 5 (count (:hand (get-runner)))) "Runner should draw 5 cards")))

(deftest onr-networking
  ;; ONR Networking
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["ONR Livewire's Contacts"]}})
    (take-credits state :corp)
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "ONR Livewire's Contacts")
      (is (= 8 (:credit (get-runner))) "Runner should spend 0 and gain 3"))))

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
