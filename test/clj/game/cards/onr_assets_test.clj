(ns game.cards.onr-assets-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest onr-braindance-campaign
  ;; Adonis Campaign
  (do-game
      (new-game {:corp {:hand ["ONR Braindance Campaign" "Hedge Fund"]}})
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "ONR Braindance Campaign" "New remote")
      (let [ac (get-content state :remote1 0)]
        (changes-val-macro
          -6 (:credit (get-corp))
          "spent 6 to rez braindance"
          (rez state :corp ac))
        (is (= 12 (get-counters (refresh ac) :credit)) "12 counters on Braindance")
        (take-credits state :corp)
        (dotimes [_ 6]
          (let [credits (:credit (get-corp))
                counters (get-counters (refresh ac) :credit)]
            (take-credits state :runner)
            (is (= (+ credits 2) (:credit (get-corp))) "Gain 2 from Adonis")
            (is (= (- counters 2) (get-counters (refresh ac) :credit)) "-2 counters on braindance"))
          (take-credits state :corp))
        (is (nil? (refresh ac)) "Braindance Campaign should be trashed")
        (is (= "ONR Braindance Campaign" (->> (get-corp) :discard second :title))))))


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

(deftest onr-rockerboy-promotion
  ;; Regolith Mining License
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand ["ONR Rockerboy Promotion"]}})
   (play-from-hand state :corp "ONR Rockerboy Promotion" "New remote")
   (let [rml (get-content state :remote1 0)]
     (rez state :corp (refresh rml))
     (changes-val-macro
      3 (:credit (get-corp))
      "Corp gains 3 credits"
      (card-ability state :corp rml 0)))))
