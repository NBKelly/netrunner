(ns game.cards.belltower-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

;; ANARCH CARDS

;;(deftest sebastio  ) - no test here, I can't figure out the accents

(deftest hq-occupation
  ;; HQ Occupation
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Hostile Takeover" 3)]}
               :runner {:hand ["HQ Occupation"]}})
    (take-credits state :corp)
    (is (= 0 (count-tags state)))
    (play-run-event state "HQ Occupation" :hq)
    (click-prompt state :runner "[HQ Occupation] Trash card")
    (click-prompt state :runner "[HQ Occupation] Trash card")
    (is (= 1 (count-tags state)))
    (is (not (:run @state)) "Run has finished")))

(deftest eye-for-and-eye-basic
  (do-game
    (new-game {:corp {:hand ["Ice Wall" "Enigma"]}
               :runner {:hand ["Eye for an Eye"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Enigma" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Eye for an Eye")
    (is (= 0 (count-tags state)))
    (run-continue state :success)
    (click-prompt state :runner "Eye for an Eye")
    (is (= 1 (count-tags state)))
    (click-card state :corp (get-ice state :hq 0))
    (is (no-prompt? state :corp))
    (is (not (:run @state)) "Run ended")
    (is (= 1 (count (:discard (get-corp)))))))

(deftest eye-for-and-eye-basic-threat
  (do-game
    (new-game {:corp {:hand ["Ice Wall" "Enigma" "City Works Project" "Project Atlas"]}
               :runner {:hand ["Eye for an Eye"]}})
    (core/gain-clicks state :corp 1)
    (play-and-score state "City Works Project")
    (play-and-score state "Project Atlas")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Enigma" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Eye for an Eye")
    (is (= 0 (count-tags state)))
    (run-continue state :success)
    (click-prompt state :runner "Eye for an Eye")
    (is (= 1 (count-tags state)))
    (click-card state :corp (get-ice state :hq 0))
    (click-card state :corp (get-ice state :rd 0))
    (is (no-prompt? state :corp))
    (is (not (:run @state)) "Run ended")
    (is (= 2 (count (:discard (get-corp)))))))

(deftest eye-for-and-eye-basic-threat-avoided-tag
  (do-game
    (new-game {:corp {:hand ["Ice Wall" "Enigma" "City Works Project" "Project Atlas"]}
               :runner {:id "Jesminder Sareen: Girl Behind the Curtain"
                        :hand ["Eye for an Eye"]}})
    (core/gain-clicks state :corp 1)
    (play-and-score state "City Works Project")
    (play-and-score state "Project Atlas")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Enigma" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Eye for an Eye")
    (is (= 0 (count-tags state)))
    (run-continue state :success)
    (click-prompt state :runner "Eye for an Eye")
    (is (= 0 (count-tags state)))
    (click-card state :corp (get-ice state :hq 0))
    (is (no-prompt? state :corp))
    (is (not (:run @state)) "Run ended")
    (is (= 1 (count (:discard (get-corp)))))))

(deftest achaean-crew-basic-functionality
  (do-game
    (new-game {:corp {:hand ["Vanilla" "City Works Project"]}
               :runner {:hand ["Achaean Crew" (qty "Sketchpad" 3)] :credits 10}})
    (play-from-hand state :corp "Vanilla" "R&D")
    (play-and-score state "City Works Project")
    (take-credits state :corp)
    (core/gain-clicks state :runner 10)
    (let [iwall (get-ice state :hq 0)
          vanil (get-ice state :rd 0)]
      (rez state :corp vanil)
      (is (= 0 (get-strength (refresh vanil))) "Vanilla starts at 0 strength")
      (play-from-hand state :runner "Sketchpad")
      (click-card state :runner (refresh vanil))
      (is (= 0 (get-strength (refresh vanil))) "Vanilla still at 0 strength")
      (play-from-hand state :runner "Achaean Crew")
      (run-on state "R&D")
      (run-continue state :encounter-ice)
      (is (= -1 (get-strength (refresh vanil))) "Vanilla at -1")
      (fire-subs state vanil)
      (play-from-hand state :runner "Sketchpad")
      (click-card state :runner (refresh vanil))
      (run-on state "R&D")
      (run-continue state :encounter-ice)
      (is (= -2 (get-strength (refresh vanil))) "Vanilla at -2")
      (fire-subs state vanil)
      (run-on state "R&D")
      (run-continue state :encounter-ice)
      (card-ability state :runner (get-resource state 0) 0)
      (is (= 1 (count (:discard (get-corp)))) "Vanilla trashed")
      (is (= 3 (count (:discard (get-runner)))) "Hopes and dreams trashed"))))

;; CRIM CARDS

(deftest alarm-clock-test
  (do-game
    (new-game {:runner {:hand ["Alarm Clock"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Alarm Clock")
    (take-credits state :runner)
    (take-credits state :corp)
    (end-phase-12 state :runner)
    ;;(click-card state :runner (get-hardware state 0))
    (click-prompt state :runner "Yes")
    (is (:run @state) "Run has started")))
