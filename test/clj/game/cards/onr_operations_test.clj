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
