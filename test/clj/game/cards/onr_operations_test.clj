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

(deftest onr-day-shift
  ;; Green Level Clearance
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["ONR Day Shift"]}})
    (play-from-hand state :corp "ONR Day Shift")
    (is (= (+ 5 +1) (:credit (get-corp))) "Corp should gain net 1 credits")
    (is (= 2 (count (:hand (get-corp)))) "Corp should draw 2 cards")))

(deftest onr-efficiency-experts
  ;; Beanstalk Royalties
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["ONR Efficiency Experts"]}})
    (let [credits (:credit (get-corp))]
      (play-from-hand state :corp "ONR Efficiency Experts")
      (is (= (+ credits 3) (:credit (get-corp))) "Corp should gain 3"))))

(deftest onr-falsified-transactions-expert
  ;; Trick of Light
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["ONR Falsified-Transactions Expert" "Ice Wall" "NGO Front"]
                        :credits 10}})
      (core/gain state :corp :click 6)
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "NGO Front" "New remote")
      (let [ngo (get-content state :remote1 0)
            iw (get-ice state :hq 0)]
        (advance state (refresh ngo) 3)
        (is (= 3 (get-counters (refresh ngo) :advancement)) "NGO Front should have 3 counters")
        (play-from-hand state :corp "ONR Falsified-Transactions Expert")
        (click-card state :corp iw)
        (click-card state :corp ngo)
        (click-prompt state :corp "3")
        (is (= 3 (get-counters (refresh iw) :advancement)) "Ice Wall is now advanced")
        (is (zero? (get-counters (refresh ngo) :advancement)) "NGO Front should have 0 counters"))))

(deftest onr-management-shakeup
  ;; Shipment from Vladisibirsk
  (do-game
    (new-game {:corp {:credits 20
                      :hand [(qty "ONR Management Shake-Up" 2) "Ice Wall" "Hostile Takeover" "PAD Campaign" "Bio Vault"]}})
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Bio Vault" "New remote")
    (play-from-hand state :corp "ONR Management Shake-Up")
    (click-card state :corp "Ice Wall")
    (click-card state :corp "Hostile Takeover")
    (click-card state :corp "Hostile Takeover")
    (is (= 1 (get-counters (get-ice state :hq 0) :advancement)))
    (is (= 2 (get-counters (get-content state :remote1 0) :advancement)))
        (play-from-hand state :corp "ONR Management Shake-Up")
    (dotimes [_ 3]
      (click-card state :corp "Bio Vault"))
    (is (= 3 (get-counters (get-content state :remote3 0) :advancement)))))

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

(deftest onr-netwatch-credit-voucher
  ;; Big Brother - Give the Runner 2 tags if already tagged
  (do-game
    (new-game {:corp {:deck ["ONR Netwatch Credit Voucher"]}})
    (play-from-hand state :corp "ONR Netwatch Credit Voucher")
    (is (= 1 (count (:hand (get-corp)))) "Card not played because Runner has no tags")
    (gain-tags state :runner 1)
    (changes-val-macro
      +1 (:credit (get-corp))
      "gained 1 from play"
      (play-from-hand state :corp "ONR Netwatch Credit Voucher")
      (is (= 2 (count-tags state)) "Runner gained a tag"))))

(deftest onr-night-shift
  ;; Green Level Clearance
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["ONR Night Shift"]}})
    (play-from-hand state :corp "ONR Night Shift")
    (is (= (+ 5 +2) (:credit (get-corp))) "Corp should gain net 2 credits")
    (is (= 1 (count (:hand (get-corp)))) "Corp should draw 1 card")))

(deftest onr-off-site-backups
  ;; Archived Memories
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["ONR Off-Site Backups"]
                      :discard ["Hostile Takeover"]}})
    (play-from-hand state :corp "ONR Off-Site Backups")
    (click-card state :corp "Hostile Takeover")
    (is (= ["Hostile Takeover"] (->> (get-corp) :hand (map :title))) "Hostile Takeover should be in HQ")))

(deftest onr-overtime-incentives
  ;; Biotic Labor - Gain 2 clicks
  (do-game
    (new-game {:corp {:deck ["ONR Overtime Incentives"]}})
    (play-from-hand state :corp "ONR Overtime Incentives")
    (is (= 1 (:credit (get-corp))))
    (is (= 4 (:click (get-corp))) "Spent 1 click to gain 2 additional clicks")))

(deftest onr-planning-consultants
  ;; Precognition - Full test
  (do-game
    (new-game {:corp {:deck ["ONR Planning Consultants" "Caprice Nisei" "Adonis Campaign"
                             "Quandary" "Jackson Howard" "Global Food Initiative"]}})
    (starting-hand state :corp ["ONR Planning Consultants"])
    (play-from-hand state :corp "ONR Planning Consultants")
    (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Adonis Campaign" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Quandary" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Jackson Howard" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Global Food Initiative" (:deck (get-corp))))
    ;; try starting over
    (click-prompt state :corp "Start over")
    (click-prompt state :corp (find-card "Global Food Initiative" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Jackson Howard" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Quandary" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Adonis Campaign" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp)))) ;this is the top card of R&D
    (click-prompt state :corp "Done")
    (is (= "Caprice Nisei" (:title (first (:deck (get-corp))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-corp))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-corp)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-corp))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-corp)))))))))))

(deftest onr-project-consultants
  ;; Shipment from Vladisibirsk
  (do-game
    (new-game {:corp {:credits 30
                      :hand [(qty "ONR Project Consultants" 2) "Ice Wall" "Hostile Takeover" "PAD Campaign" "Bio Vault"]}})
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Bio Vault" "New remote")
    (play-from-hand state :corp "ONR Project Consultants")
    (click-card state :corp "Ice Wall")
    (click-card state :corp "Hostile Takeover")
    (click-card state :corp "Hostile Takeover")
    (click-card state :corp "Hostile Takeover")
    (is (= 1 (get-counters (get-ice state :hq 0) :advancement)))
    (is (= 3 (get-counters (get-content state :remote1 0) :advancement)))
        (play-from-hand state :corp "ONR Project Consultants")
    (dotimes [_ 4]
      (click-card state :corp "Bio Vault"))
    (is (= 4 (get-counters (get-content state :remote3 0) :advancement)))))

(deftest onr-punitive-counterstrike
  ;; Traffic Accident
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["ONR Punitive Counterstrike"]}
               :runner {:hand [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "ONR Punitive Counterstrike")
    (is (= 5 (count (:hand (get-runner)))) "Runner shouldn't take damage as they're not tagged")
    (gain-tags state :runner 1)
    (play-from-hand state :corp "ONR Punitive Counterstrike")
    (is (= 3 (count (:hand (get-runner)))) "Runner should take 2 damage")))


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

(deftest onr-systematic-layoffs
  ;; Shipment from Vladisibirsk
  (do-game
    (new-game {:corp {:credits 30
                      :hand [(qty "ONR Systematic Layoffs" 2) "Ice Wall" "Hostile Takeover" "PAD Campaign" "Bio Vault"]}})
    (core/gain state :corp :click 3)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Bio Vault" "New remote")
    (play-from-hand state :corp "ONR Systematic Layoffs")
    (click-card state :corp "Ice Wall")
    (click-card state :corp "Hostile Takeover")
    (is (= 1 (get-counters (get-ice state :hq 0) :advancement)))
    (is (= 1 (get-counters (get-content state :remote1 0) :advancement)))
        (play-from-hand state :corp "ONR Systematic Layoffs")
    (dotimes [_ 2]
      (click-card state :corp "Bio Vault"))
    (is (= 2 (get-counters (get-content state :remote3 0) :advancement)))))

(deftest onr-team-restructuring
  ;; NGO Front
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["ONR Team Restructuring" "Ice Wall" "NGO Front"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "NGO Front" "New remote")
    (play-from-hand state :corp "ONR Team Restructuring")
    (click-card state :corp "Ice Wall")
    (click-card state :corp "NGO Front")
    (is (= 1 (get-counters (get-ice state :hq 0) :advancement)) "Ice Wall should be advanced")
    (is (= 1 (get-counters (get-content state :remote1 0) :advancement)) "NGO should be advanced")))

(deftest onr-trojan-horse
  ;; Distributed Tracing
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["ONR Trojan Horse" "Hostile Takeover"]}})
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (play-from-hand state :corp "ONR Trojan Horse")
    (is (no-prompt? state :corp) "Corp should have no prompt without agenda stolen")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (play-from-hand state :corp "ONR Trojan Horse")
    (is (= 1 (count-tags state)) "Runner took 1 tag")))

(deftest onr-urban-renewal
  ;; Scorched Earth
  (do-game
    (new-game {:corp {:credits 6
                      :deck ["ONR Urban Renewal"]}
               :runner {:hand [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]}})
      (gain-tags state :runner 1)
      (play-from-hand state :corp "ONR Urban Renewal")
      (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")))
