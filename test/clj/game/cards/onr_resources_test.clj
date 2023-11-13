(ns game.cards.onr-resources-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

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
