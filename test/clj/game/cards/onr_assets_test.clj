(ns game.cards.onr-assets-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest onr-department-of-truth-enhancement
  (do-game
    (new-game {:corp {:hand ["ONR Department of Truth Enhancement"]}})
    (play-from-hand state :corp "ONR Department of Truth Enhancement" "New remote")
    (let [n (+ 1 (rand-int 15))
          dept (get-content state :remote1 0)]
      (rez state :corp dept)
      (core/gain state :corp :click n)
      (dotimes [x n]
        (card-ability state :corp (refresh dept) 0)
        (is (= (* 3 (+ x 1)) (get-counters (refresh dept) :credit)) (str (* 3 (+ x 1)) " counters on DOTE")))
      (changes-val-macro
        (* 3 n) (:credit (get-corp))
        (str "Corp gains 3 * " n " credits from Department of Truth Enhancement")
        (card-ability state :corp (refresh dept) 1)
        (is (zero? (get-counters (refresh dept) :credit)) "0 counters on DOTE")))))
