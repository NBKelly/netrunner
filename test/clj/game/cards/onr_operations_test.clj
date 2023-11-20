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

(deftest anonymous-tip
  ;; Anonymous Tip
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["ONR Annual Reviews"]}})
    (is (= 1 (count (:hand (get-corp)))) "Corp starts with 1 card in HQ")
    (is (zero? (count (:discard (get-corp)))) "Corp starts with 0 cards in Archives")
    (play-from-hand state :corp "ONR Annual Reviews")
    (is (= 3 (count (:hand (get-corp)))) "Corp should draw 3 cards")
    (is (= 1 (count (:discard (get-corp)))) "Corp has 1 card in Archives")))

(deftest onr-badtimes
  ;; Bad Times
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["ONR Badtimes"]}})
      (is (= 4 (core/available-mu state)) "Runner should start with 4 MU")
      (play-from-hand state :corp "ONR Badtimes")
      (is (= 4 (core/available-mu state)) "Corp can't play without a tag")
      (gain-tags state :runner 1)
      (play-from-hand state :corp "ONR Badtimes")
      (is (= 2 (core/available-mu state)) "Runner should lose 2 available MU")
      (take-credits state :corp)
      (is (= 4 (core/available-mu state)) "Runner should regain 2 available MU")))

(deftest onr-closed-accounts
  ;; Closed Accounts - Play if Runner is tagged to make Runner lose all credits
  (do-game
    (new-game {:corp {:deck ["ONR Closed Accounts"]}})
    (play-from-hand state :corp "ONR Closed Accounts")
    (is (and (= 3 (:click (get-corp)))
             (= 5 (:credit (get-runner))))
        "Closed Accounts precondition not met; card not played")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "ONR Closed Accounts")
    (is (zero? (:credit (get-runner))) "Runner lost all credits")))

(deftest onr-corporate-detective-agency
  ;; Freelancer
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["ONR Corporate Detective Agency"]}
               :runner {:hand ["Kati Jones" "Net Mercur"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Kati Jones")
    (play-from-hand state :runner "Net Mercur")
    (take-credits state :runner)
    (play-from-hand state :corp "ONR Corporate Detective Agency")
    (is (= ["ONR Corporate Detective Agency" "Hedge Fund"] (->> (get-corp) :hand (map :title))) "Freelancer shouldn't be playable without a tag")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "ONR Corporate Detective Agency")
    (click-card state :corp "Kati Jones")
    (click-card state :corp "Net Mercur")
    (is (zero? (count (get-resource state))) "Runner should have no resources left in play")))

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

(deftest onr-corporate-shuffle
  ;; Sprint
  (do-game
      (new-game {:corp {:deck ["Hedge Fund" "Restructure" "NGO Front" "Ice Wall" "Fire Wall"]
                        :hand ["ONR Corporate Shuffle" (qty "IPO" 3) "Ice Wall"]}})
      (play-from-hand state :corp "ONR Corporate Shuffle")
      (is (zero? (count (:deck (get-corp)))) "Corp should draw 5 cards")
      (is (= 9 (count (:hand (get-corp)))) "Corp should draw 5 cards")
      (is (last-log-contains? state "Corp uses ONR Corporate Shuffle to draw 5 cards"))
      (click-card state :corp "NGO Front")
      (is (= 1 (count (:deck (get-corp)))) "1 cards shuffled into deck")
      (is (= 8 (count (:hand (get-corp)))) "1 cards shuffled into deck")))

(deftest onr-credit-consolidation
  (do-game
    (new-game {:corp {:credits 7
                      :deck ["ONR Credit Consolidation"]}})
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "ONR Credit Consolidation")
    (is (= 15 (:credit (get-corp))))))

(deftest onr-data-sifters ;;TODO - actually fix this card
    ;; Winning Trace - Trashing 2 cards
    (do-game
      (new-game {:corp {:deck ["Dedicated Response Team" "ONR Data Sifters"]}})
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (take-credits state :runner)
      (is (zero? (-> (get-runner) :discard count)) "heap should be empty")
      (play-from-hand state :corp "ONR Data Sifters")
      (is (= 1 (count-tags state)) "Runner should have 1 tags")))

(deftest onr-datapool-r-by-zetatech
  ;; Big Brother - Give the Runner 2 tags if already tagged
  (do-game
    (new-game {:corp {:deck ["ONR Datapool (R) by Zetatech"]}})
    (play-from-hand state :corp "ONR Datapool (R) by Zetatech")
    (is (= 1 (count (:hand (get-corp)))) "Card not played because Runner has no tags")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "ONR Datapool (R) by Zetatech")
    (is (= 3 (count-tags state)) "Runner gained 2 tags")))

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

(deftest onr-scorched-earth
  ;; Scorched Earth
  (do-game
      (new-game {:corp {:deck ["ONR Scorched Earth"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (gain-tags state :runner 1)
      (play-from-hand state :corp "ONR Scorched Earth")
      (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")))

(deftest onr-scorched-earth-not-tagged
    ;; not tagged
    (do-game
      (new-game {:corp {:deck ["ONR Scorched Earth"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (play-from-hand state :corp "ONR Scorched Earth")
      (is (= 3 (:click (get-corp))) "Corp not charged a click")
      (is (= 5 (count (:hand (get-runner)))) "Runner did not take damage")))
