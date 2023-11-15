(ns game.cards.onr-operations-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest onr-accounts-recievable
  (do-game
    (new-game {:corp {:hand ["ONR Accounts Receivable"]}})
    (is (= 5 (:credit (get-corp))))
    (play-from-hand state :corp "ONR Accounts Receivable")
    (is (= 9 (:credit (get-corp))))))

(deftest onr-corporate-guard-temps
  (let [name "ONR Corporate Guard(R) Temps"]
    (do-game
      (new-game {:corp {:hand ["ONR Corporate Guard(R) Temps"]}})
      (play-from-hand state :corp "ONR Corporate Guard(R) Temps")
      (changes-val-macro
        -4 (:credit (get-corp))
        "spent 4"
        (click-prompt state :corp "2"))
      (is (= 1 (:credit (get-corp))))
      (is (= 2 (:debt (get-corp))))
      (changes-val-macro 0 (:credit (get-corp))
                         "we owe 2"
                         (take-credits state :corp))
      (take-credits state :runner)
      (changes-val-macro 4 (:credit (get-corp))
                         "+1 click"
                         (take-credits state :corp))
      (take-credits state :runner)
      (changes-val-macro 4 (:credit (get-corp))
                         "+1 click"
                         (take-credits state :corp))
      (take-credits state :runner)
      (changes-val-macro 3 (:credit (get-corp))
                         "+0 clicks"
                         (take-credits state :corp)))))


(deftest onr-manhunt
  ;; Trace 6 - Give the runner 1 tag for each point your trace exceeded their link
  (do-game
    (new-game {:corp {:deck ["ONR Manhunt"]}})
    (take-credits state :corp)
    (run-empty-server state :rd)
    (take-credits state :runner)
    ;; 7 - 4 means that our max trace is 3
    (play-from-hand state :corp "ONR Manhunt")
    (click-prompt state :corp "3")
    (is (= 3 (count-tags state)) "Runner should have 3 tags")))
