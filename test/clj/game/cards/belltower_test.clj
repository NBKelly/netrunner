(ns game.cards.belltower-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

;; ANARCH CARDS

;;(deftest sebastio  ) - no test here, I can't figure out the accents

(deftest ^:kaocha/pending achaean-crew-basic-functionality
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

;; event

(deftest ar-infinite-access
  (do-game
    (new-game {:runner {:hand ["AR Infinite Access" (qty "Sure Gamble" 5)]
                        :deck [(qty "Sure Gamble" 20)]
                        :discard [(qty "Sure Gamble" 20)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "AR Infinite Access")
    (is (= 0 (count (:discard (get-runner)))) "Heap is empty")
    (is (= 5 (count (:hand (get-runner)))) "Grip is full")
    (is (= 35 (count (:deck (get-runner)))) "Stack is back")
    (is (= 6 (count (:rfg (get-runner)))))))

(deftest eye-for-an-eye
  (do-game
    (new-game {:corp {:hand ["Ice Wall" "Enigma"]}
               :runner {:hand ["Eye for an Eye" (qty "Sure Gamble" 2)]}})
    (take-credits state :corp)
    (gain-tags state :runner 1)
    (play-from-hand state :runner "Eye for an Eye")
    (is (not (:run @state)) "Cannot play Eye for an Eye when tagged")
    (remove-tag state :runner)
    (play-run-event state "Eye for an Eye" :hq)
    (click-prompt state :runner "[Eye for an Eye] Trash 1 card from your hand: Trash card")
    (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
    (click-prompt state :runner "[Eye for an Eye] Trash 1 card from your hand: Trash card")
    (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
    (is (not (:run @state)) "Run ended")
    (is (= 1 (count-tags state)))
    (is (= 2 (count (:discard (get-corp)))))
    (is (= 3 (count (:discard (get-runner)))) "Whole world is blind")))

(deftest true-colors
  (do-game
    (new-game {:corp {:hand [(qty "Hedge Fund" 4)]}
               :runner {:hand ["True Colors"]}})
    (take-credits state :corp)
    (play-run-event state "True Colors" :hq)
    (is (last-log-contains? state "reveals Hedge Fund, Hedge Fund, Hedge Fund from HQ"))
    (changes-val-macro 2 (count (:deck (get-corp)))
                       "2 cards added to R&D"
                       (click-prompt state :runner "Hedge Fund")
                       (click-prompt state :runner "Top of R&D")
                       (click-prompt state :runner "Hedge Fund")
                       (click-prompt state :runner "Bottom of R&D"))))

;; hardware

(deftest alarm-clock
  (do-game
    (new-game {:corp {:hand ["Ice Wall"]}
               :runner {:hand ["Alarm Clock"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Alarm Clock")
    (take-credits state :runner)
    (take-credits state :corp)
    (end-phase-12 state :runner)
    (click-prompt state :runner "Yes")
    (is (:run @state) "Run has started")
    (run-continue state)
    (changes-val-macro -2 (:click (get-runner))
                       "Costs 2 clicks"
                       (click-prompt state :runner "Yes"))
    (is (= :movement (:phase (get-run))) "Run has bypassed Ice Wall")))

(deftest the-wizards-chest
  (do-game
    (new-game {:runner {:hand ["The Wizard's Chest"]
                        :discard ["Legwork" "Corroder" "Ice Carver" "Prepaid VoicePAD" "Femme Fatale" "Egret" "Earthrise Hotel"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "The Wizard's Chest")
    (let [chest (get-hardware state 0)]
      ;; TODO: make this a helper or something for consistently ordered starting deck
      (doseq [card-name ["Legwork" "Corroder" "Ice Carver" "Prepaid VoicePAD" "Femme Fatale" "Egret" "Earthrise Hotel"]]
        (core/move state :runner (find-card card-name (get-in @state [:runner :discard])) :deck))
      (card-ability state :runner chest 0)
      (is (no-prompt? state :runner) "Cannot trigger The Wizard's Chest until all centrals ran")
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (card-ability state :runner (refresh chest) 0)
      (is (= ["Hardware" "Program" "Resource" "Cancel"] (prompt-buttons :runner)))
      (click-prompt state :runner "Program")
      (is (= ["Install Corroder" "Install Femme Fatale" "No thanks"] (prompt-buttons :runner)))
      (changes-val-macro 0 (:credit (get-runner))
                         "Install at no cost"
                         (click-prompt state :runner "Install Femme Fatale"))
      (is (= "Femme Fatale" (:title (get-program state 0))) "Femme Fatale is installed")
      (is (second-last-log-contains? state (str "Runner uses The Wizard's Chest"
                                                " to reveal Legwork, Corroder, Ice Carver, Prepaid VoicePAD, Femme Fatale from the top of the stack"
                                                " and install Femme Fatale, ignoring all costs."))))))

;; program

(deftest powercache
  (do-game
    (new-game {:runner {:hand ["[Powercache]"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "[Powercache]")
    (changes-val-macro 2 (:credit (get-runner)) "Runner gains 2 credits"
                       (card-ability state :runner (get-program state 0) 0))
    (is (= 1 (get-counters (get-program state 0) :power)))
    (take-credits state :runner)
    (changes-val-macro 0 (:credit (get-runner)) "Cannot use [Powercache] to gain credits on Corp turn"
                       (card-ability state :runner (get-program state 0) 0))))

;; resources

;; wrote this test not realizing Arsène was just a prompt, this should hopefully match card intention
(deftest ^:kaocha/pending arsene
  (do-game
    (new-game {:corp {:hand ["Hedge Fund"] :deck [(qty "Hedge Fund" 10)]}
               :runner {:hand ["Arsène" "Jailbreak"] :deck ["Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Arsène")
    (run-empty-server state "R&D")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")
    (play-from-hand state :runner "Jailbreak")
    (click-prompt state :runner "R&D")
    (run-continue state)
    (click-prompt state :runner "No action")
    (click-prompt state :runner "No action")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))

(deftest friend-of-a-friend
  (do-game
    (new-game {:runner {:hand [(qty "[Friend of a Friend]" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "[Friend of a Friend]")
    (changes-val-macro 10 (:credit (get-runner)) "Runner gains 10 credits"
                       (card-ability state :runner (get-resource state 0) 1))
    (is (= 1 (count-tags state)))
    (play-from-hand state :runner "[Friend of a Friend]")
    (changes-val-macro 5 (:credit (get-runner)) "Runner gains 5 credits"
                       (card-ability state :runner (get-resource state 0) 0))
    (is (= 0 (count-tags state)))
    (is (= 2 (count (:discard (get-runner)))) "Both trashed")))
