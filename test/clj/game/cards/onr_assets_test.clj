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

(deftest onr-i-got-a-rock
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["ONR I Got a Rock"]}
               :runner {:hand [(qty "Sure Gamble" 16)]}})
    (play-from-hand state :corp "ONR I Got a Rock" "New remote")
    (let [rock (get-content state :remote1 0)]
      (rez state :corp rock)
      (card-ability state :corp (refresh rock) 0)
      (is (= 16 (count (:hand (get-runner)))) "BOOM! should not be played as runner has no tags")
      (gain-tags state :runner 2)
      (is (zero? (count (:discard (get-runner)))) "Runner should have 0 cards in discard")
      (card-ability state :corp (refresh rock) 0)
      (is (= 15 (count (:discard (get-runner)))) "Runner should take 7 damage"))))

(deftest onr-krumz-test
  (do-game
    (new-game {:corp {:hand ["ONR Krumz" "ONR Manhunt"]}})
    (take-credits state :corp)
    (run-empty-server state :rd)
    (take-credits state :runner)
    (play-from-hand state :corp "ONR Krumz" "New remote")
    (let [krumz (get-content state :remote1 0)]
      (rez state :corp krumz)
      (play-from-hand state :corp "ONR Manhunt")
      (click-prompt state :corp "3")
      (click-card state :corp (refresh krumz)))))

(deftest onr-rescheduler
  (do-game
    (new-game {:corp {:hand [(qty "ONR Rescheduler" 7)]
                      :deck [(qty "ONR Accounts Receivable" 40)]}})
    (play-from-hand state :corp "ONR Rescheduler" "New remote")
    (let [sch (get-content state :remote1 0)]
      (rez state :corp sch)
      (card-ability state :corp (refresh sch) 0)
      (is (= 6 (count (:hand (get-corp)))))
      (is (some #(= (:title %) "ONR Accounts Receivable") (:hand (get-corp))) "Bad luck!"))))

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

(deftest onr-rustbelt-hq-branch
  ;; Cybernetics Court
  (do-game
    (new-game {:corp {:deck ["ONR Rustbelt HQ Branch"]}})
    (play-from-hand state :corp "ONR Rustbelt HQ Branch" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (is (= 7 (hand-size :corp)) "Corp should have hand size of 7")))
