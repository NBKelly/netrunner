(ns game.cards.onr-resources-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest onr-access-through-alpha
  (do-game
    (new-game {:corp {:deck ["ONR Manhunt"]}
               :runner {:hand ["ONR Access through Alpha" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "ONR Access through Alpha")
    (run-empty-server state :rd)
    (take-credits state :runner)
    ;; 7 - 4 means that our max trace is 3
    ;; the runner should also have 2 credits
    (play-from-hand state :corp "ONR Manhunt")
    (click-prompt state :corp "3")
    (click-prompt state :runner "ONR Access through Alpha")
    (changes-val-macro
      -1 (:credit (get-runner))
      "spent 1c to boost link to 9"
      (click-prompt state :runner "1 [Credits]: Base Link 9"))
    (click-prompt state :runner "Done")
    (is (= 0 (count-tags state)) "Runner should have 0 tags")))

(deftest onr-access-to-arasaka
  (do-game
    (new-game {:corp {:deck ["ONR Manhunt"]}
               :runner {:hand ["ONR Access to Arasaka" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "ONR Access to Arasaka")
    (run-empty-server state :rd)
    (take-credits state :runner)
    ;; 7 - 4 means that our max trace is 3
    ;; the runner should also have 2 credits
    (play-from-hand state :corp "ONR Manhunt")
    (click-prompt state :corp "3")
    (click-prompt state :runner "ONR Access to Arasaka")
    (changes-val-macro
      -2 (:credit (get-runner))
      "spent 2c to boost link to 4"
      (click-prompt state :runner "2 [Credits]: Base Link 4"))
    (changes-val-macro
      -2 (:credit (get-runner))
      "spent 2c to boost link to 5"
      (click-prompt state :runner "2 [Credits]: +1 Link"))
    (click-prompt state :runner "Done")
    (is (= 0 (count-tags state)) "Runner should have 0 tags")))

(deftest onr-access-to-kiribati
  (do-game
    (new-game {:corp {:deck ["ONR Manhunt"]}
               :runner {:hand ["ONR Access to Kiribati" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "ONR Access to Kiribati")
    (run-empty-server state :rd)
    (take-credits state :runner)
    ;; 7 - 4 means that our max trace is 3
    ;; the runner should also have 2 credits
    (play-from-hand state :corp "ONR Manhunt")
    (click-prompt state :corp "3")
    (click-prompt state :runner "ONR Access to Kiribati")
    (changes-val-macro
      -1 (:credit (get-runner))
      "spent 1c to boost link to 1"
      (click-prompt state :runner "1 [Credits]: Base Link 1"))
    (changes-val-macro
      -1 (:credit (get-runner))
      "spent 1c to boost link to 2"
      (click-prompt state :runner "1 [Credits]: +1 Link"))
    (click-prompt state :runner "Done")
    (is (= 1 (count-tags state)) "Runner should have 1 tag1")))

(deftest onr-back-door-to-hilliard
  ;; Trace 6 - Give the runner 1 tag for each point your trace exceeded their link
  (do-game
    (new-game {:corp {:deck ["ONR Manhunt"]}
               :runner {:hand ["ONR Back Door to Hilliard"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Back Door to Hilliard")
    (run-empty-server state :rd)
    (take-credits state :runner)
    ;; 7 - 4 means that our max trace is 3
    ;; the runner should also have 6 credits
    (play-from-hand state :corp "ONR Manhunt")
    (click-prompt state :corp "3")
    (click-prompt state :runner "ONR Back Door to Hilliard")
    (changes-val-macro
      -0 (:credit (get-runner))
      "spent 0c to boost link to 2"
      (click-prompt state :runner "0 [Credits]: Base Link 2"))
    (changes-val-macro
      -3 (:credit (get-runner))
      "spent 1c to boost link to 3"
      (click-prompt state :runner "3 [Credits]: +1 Link"))
    (click-prompt state :runner "Done")
    (is (= 0 (count-tags state)) "Runner should have 0 tags")))

(deftest onr-back-door-to-orbital-air
  ;; Trace 6 - Give the runner 1 tag for each point your trace exceeded their link
  (do-game
    (new-game {:corp {:deck ["ONR Manhunt"]}
               :runner {:hand ["ONR Back Door to Orbital Air"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Back Door to Orbital Air")
    (run-empty-server state :rd)
    (take-credits state :runner)
    ;; 7 - 4 means that our max trace is 3
    ;; the runner should also have 6 credits
    (play-from-hand state :corp "ONR Manhunt")
    (click-prompt state :corp "3")
    (click-prompt state :runner "ONR Back Door to Orbital Air")
    (changes-val-macro
      -1 (:credit (get-runner))
      "spent 1c to boost link to 2"
      (click-prompt state :runner "1 [Credits]: Base Link 2"))
    (changes-val-macro
      -2 (:credit (get-runner))
      "spent 2c to boost link to 3"
      (click-prompt state :runner "2 [Credits]: +1 Link"))
    (click-prompt state :runner "Done")
    (is (= 0 (count-tags state)) "Runner should have 0 tags")))

(deftest onr-back-door-to-rivals
  ;; Trace 6 - Give the runner 1 tag for each point your trace exceeded their link
  (do-game
    (new-game {:corp {:deck ["ONR Manhunt"]}
               :runner {:hand ["ONR Back Door to Rivals"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Back Door to Rivals")
    (run-empty-server state :rd)
    (take-credits state :runner)
    ;; 7 - 4 means that our max trace is 3
    ;; the runner should also have 6 credits
    (play-from-hand state :corp "ONR Manhunt")
    (click-prompt state :corp "2")
    (click-prompt state :runner "ONR Back Door to Rivals")
    (changes-val-macro
      -0 (:credit (get-runner))
      "spent 0c to boost link to 2"
      (click-prompt state :runner "0 [Credits]: Base Link 2"))
    (changes-val-macro
      -3 (:credit (get-runner))
      "spent 1c to boost link to 3"
      (click-prompt state :runner "3 [Credits]: +1 Link"))
    (changes-val-macro
      +1 (:credit (get-runner))
      "gained 1c from beating trace"
      (click-prompt state :runner "Done"))
    (is (= 0 (count-tags state)) "Runner should have 0 tags")))

(deftest onr-runner-sensei
  ;; Trace 6 - Give the runner 1 tag for each point your trace exceeded their link
  (do-game
    (new-game {:corp {:deck ["ONR Manhunt"]}
               :runner {:hand ["ONR Runner Sensei"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Runner Sensei")
    (run-empty-server state :rd)
    (take-credits state :runner)
    ;; 7 - 4 means that our max trace is 3
    ;; the runner should also have 6 credits
    (play-from-hand state :corp "ONR Manhunt")
    (click-prompt state :corp "3")
    (click-prompt state :runner "ONR Runner Sensei")
    (changes-val-macro
      -2 (:credit (get-runner))
      "spent 2c to boost link to 4"
      (click-prompt state :runner "2 [Credits]: Base Link 4"))
    (changes-val-macro
      +1 (:credit (get-runner))
      "gained 1c from beating trace"
      (click-prompt state :runner "Done"))
    (is (= 0 (count-tags state)) "Runner should have 0 tags")))

(deftest onr-swiss-bank-account
  (do-game
    (new-game {:runner {:hand [(qty "ONR Swiss Bank Account" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Swiss Bank Account")
    (is (= 5 (:credit (get-runner))))
    (let [fall (get-resource state 0)]
      (card-ability state :runner fall 0)
      (is (= 7 (:credit (get-runner)))))
    (play-from-hand state :runner "ONR Swiss Bank Account")
    (let [fall (get-resource state 0)]
      (card-ability state :runner fall 1)
      (is (= 10 (:credit (get-runner)))))))
