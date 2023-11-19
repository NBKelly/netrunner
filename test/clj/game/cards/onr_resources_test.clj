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

(deftest onr-back-door-to-netwatch
  (do-game
    (new-game {:corp {:deck ["ONR Manhunt"]}
               :runner {:hand ["ONR Back Door to Netwatch"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Back Door to Netwatch")
    (run-empty-server state :rd)
    (take-credits state :runner)
    (play-from-hand state :corp "ONR Manhunt")
    (click-prompt state :corp "3")
    (changes-val-macro
      -3 (:credit (get-runner))
      "spent 3 to cancel the trace"
      (click-prompt state :runner "Yes"))
    (is (zero? (count-tags state)) "The trace effect was prevented!")))

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

(deftest onr-broker
  ;; Kati Jones - Click to store and take
  (do-game
    (new-game {:runner {:deck ["ONR Broker"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Broker")
    (is (= 2 (:credit (get-runner))))
    (let [kati (get-resource state 0)]
      (card-ability state :runner kati 0)
      (is (= 2 (:click (get-runner))))
      (is (= 3 (get-counters (refresh kati) :credit)) "Store 3cr on Kati")
      (card-ability state :runner kati 0)
      (is (= 2 (:click (get-runner))) "Second use of Kati should not be allowed")
      (is (= 3 (get-counters (refresh kati) :credit)) "Second use of Kati should not be allowed")
      (take-credits state :runner 2)
      (is (= 4 (:credit (get-runner))) "Pass turn, take 2cr")
      (take-credits state :corp)
      (card-ability state :runner kati 0)
      (is (= 6 (get-counters (refresh kati) :credit)) "Store 3cr more on Kati")
      (take-credits state :runner 3)
      (is (= 7 (:credit (get-runner))) "Pass turn, take 3cr")
      (take-credits state :corp)
      (card-ability state :runner (refresh kati) 1)
      (is (= 13 (:credit (get-runner))) "Take 6cr from Kati")
      (is (zero? (get-counters (refresh kati) :credit)) "No counters left on Kati"))))

(deftest onr-executive-file-clerk
  ;; Executive Wiretaps
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hostile Takeover" "PAD Campaign" "Ice Wall" "Hedge Fund" "Cayambe Grid"]}
               :runner {:hand ["ONR Executive File Clerk"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Executive File Clerk")
    (card-ability state :runner (get-resource state 0) 0)
    (is (last-log-contains? state "Runner pays 2 [Credits] and trashes ONR Executive File Clerk to use ONR Executive File Clerk to reveal Cayambe Grid, Hedge Fund, Hostile Takeover, Ice Wall, and PAD Campaign from HQ."))))

(deftest onr-expendable-family-member
  ;; Decoy - Trash to avoid 1 tag
  (do-game
    (new-game {:corp {:deck ["SEA Source"]}
               :runner {:deck ["ONR Expendable Family Member"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Expendable Family Member")
    (run-empty-server state :archives)
    (take-credits state :runner)
    (play-from-hand state :corp "SEA Source")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (not (no-prompt? state :runner)) "Runner prompted to avoid tag")
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 1 (count (:discard (get-runner)))) "Decoy trashed")
    (is (zero? (count-tags state)) "Tag avoided")))

(deftest onr-fall-guy
  ;; Decoy - Trash to avoid 1 tag
  (do-game
    (new-game {:corp {:deck ["SEA Source"]}
               :runner {:deck ["ONR Fall Guy"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Fall Guy")
    (run-empty-server state :archives)
    (take-credits state :runner)
    (play-from-hand state :corp "SEA Source")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (not (no-prompt? state :runner)) "Runner prompted to avoid tag")
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 1 (count (:discard (get-runner)))) "Decoy trashed")
    (is (zero? (count-tags state)) "Tag avoided")))

(deftest onr-floating-runner-bbs
  ;; Rezeki - gain 1c when turn begins
  (do-game
    (new-game {:runner {:deck ["ONR Floating Runner BBS"]
                        :credits 6}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Floating Runner BBS")
    (take-credits state :runner)
    (let [credits (:credit (get-runner))]
      (take-credits state :corp)
      (is (= (:credit (get-runner)) (+ credits 1)) "Gain 1 from ONR Floating Runner BBS"))))

(deftest onr-field-reporter-for-ice-and-data
  (do-game
    (new-game {:corp {:hand [(qty "Vanilla" 3)]}
               :runner {:hand ["ONR Field Reporter for Ice and Data"]}})
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Vanilla" "R&D")
    (play-from-hand state :corp "Vanilla" "Archives")
    (take-credits state :corp)
    (rez state :corp (get-ice state :hq 0))
    (play-from-hand state :runner "ONR Field Reporter for Ice and Data")
    (changes-val-macro
      +4 (:credit (get-runner))
      "+1 drip from Journo"
      (take-credits state :runner))
    (take-credits state :corp)
    (rez state :corp (get-ice state :archives 0))
    (rez state :corp (get-ice state :rd 0))
    (changes-val-macro
      +6 (:credit (get-runner))
      "+2 drip from Journo"
      (take-credits state :runner))))

(deftest onr-karl-de-veres-your-name-is-long
  ;; Desperado - Gain 1 MU and gain 1 credit on successful run
  (do-game
    (new-game {:runner {:deck [(qty "ONR Karl de Veres, Corporate Stooge" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Karl de Veres, Corporate Stooge")
    (run-empty-server state :archives)
    (is (= 4 (:credit (get-runner))) "Got 1c for successful run on Desperado")))

(deftest onr-preying-mantis
  ;; Joshua B. - Take 1 tag at turn end if you choose to gain the extra click
  (do-game
    (new-game {:runner {:deck [(qty "ONR Preying Mantis" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Preying Mantis")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 4 (:click (get-runner))) "Runner has 4 clicks")
    (is (:runner-phase-12 @state) "Runner is in Step 1.2")
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 5 (:click (get-runner))) "Gained extra click from PM")
    (end-phase-12 state :runner)
    (is (zero? (count (:discard (get-runner)))) "Runner has no discards")
    (take-credits state :runner)
    (is (= 1 (count (:discard (get-runner)))) "Runner took brain")))

(deftest onr-restrictive-net-zoning
  ;; Diwan - Full test
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3) (qty "Fire Wall" 3) (qty "Crisium Grid" 2)]
                      :hand []}
               :runner {:deck ["ONR Restrictive Net Zoning"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Restrictive Net Zoning")
    (click-prompt state :runner "HQ")
    (take-credits state :runner)
    (is (= 8 (:credit (get-corp))) "8 credits for corp at start of second turn")
    (starting-hand state :corp ["Ice Wall"])
    (play-from-hand state :corp "Ice Wall" "R&D")
    (is (= 8 (:credit (get-corp))) "Restrictive Net Zoning did not charge extra for install on another server")
    (starting-hand state :corp ["Ice Wall"])
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (= 6 (:credit (get-corp))) "Restrictive Net Zoning charged 2cr to install ice protecting the named server")
    (starting-hand state :corp ["Crisium Grid"])
    (play-from-hand state :corp "Crisium Grid" "HQ")
    (is (= 6 (:credit (get-corp))) "Restrictive Net Zoning did not charge")))

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

(deftest onr-silicon-saloon-franchise
  ;; Professional Contacts - Click to gain 1 credit and draw 1 card
  (do-game
    (new-game {:runner {:credits 8
                        :deck [(qty "ONR Silicon Saloon Franchise" 3)
                               (qty "Sure Gamble" 2)
                               (qty "Shiv" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Silicon Saloon Franchise")
    (let [proco (get-resource state 0)]
      (card-ability state :runner proco 0)
      (is (= 2 (:click (get-runner))) "Spent 1 click")
      (is (= 1 (:credit (get-runner))) "Gained 1 credit")
      (is (= 5 (count (:hand (get-runner)))) "Drew 1 card")
      (card-ability state :runner proco 0)
      (is (= 1 (:click (get-runner))) "Spent 1 click")
      (is (= 2 (:credit (get-runner))) "Gained 1 credit")
      (is (= 6 (count (:hand (get-runner)))) "Drew 1 card"))))

;; Aesop's Pawnshop
(deftest smith-s-pawnshop-manual-use
  ;; Manual use
  (do-game
   (new-game {:runner {:deck ["ONR Smith's Pawnshop" "Cache"]}})
   (take-credits state :corp)
   (play-from-hand state :runner "ONR Smith's Pawnshop")
   (play-from-hand state :runner "Cache")
   (let [orig-credits (:credit (get-runner))
         ap (get-resource state 0)
         cache (get-program state 0)]
     (card-ability state :runner ap 0)
     (click-card state :runner cache)
     (card-ability state :runner ap 0)
     (is (not (prompt-is-type? state :runner :select)) "Aesop's has already been used this turn")
     (let [ap (get-resource state 0)
           cache (get-in @state [:runner :discard 0])]
       (is (= (+ 2 orig-credits) (:credit (get-runner))) "Should have only gained 3 credits")
       (is (not= cache nil) "Cache should be in Heap")
       (is (not= ap nil) "Aesops should still be installed")))))

(deftest smith-s-pawnshop-triggered-at-start-of-turn
  ;; Triggered at start of turn
  (do-game
   (new-game {:runner {:deck ["ONR Smith's Pawnshop" "Daily Casts"]}})
   (take-credits state :corp)
   (play-from-hand state :runner "ONR Smith's Pawnshop")
   (play-from-hand state :runner "Daily Casts")
   (take-credits state :runner)
   (take-credits state :corp)
   (end-phase-12 state :runner)
   (let [dc (get-resource state 1)]
     (changes-val-macro
      2 (:credit (get-runner))
      "ONR Smith's sells Daily Casts before it triggers so only 2 credits gained"
      (click-prompt state :runner "Aesop's Pawnshop")
      (click-card state :runner dc))
     (is (= (refresh dc) nil) "Daily Casts should be in Heap"))))

(deftest onr-streetware-distributor-basic-functionality
    ;; basic functionality
    (do-game
      (new-game {:runner {:deck ["ONR Streetware Distributor"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Streetware Distributor")
      (changes-val-macro
        3 (get-counters (get-resource state 0) :credit)
        "Gains 3 credits"
        (card-ability state :runner (get-resource state 0) 0))
      (take-credits state :runner)
      (changes-val-macro
        1 (:credit (get-runner))
        "Gain 1 credit from Smartware Distributor"
        (take-credits state :corp))
      (is (= 2 (get-counters (get-resource state 0) :credit)) "Smartware Distributor has 2 credits left")))

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

(deftest onr-umbrella-policy
  ;; Sacrificial Construct - Trash to prevent trash of installed program or hardware
  (do-game
    (new-game {:runner {:deck [(qty "ONR Umbrella Policy" 2) "Cache"
                               "Motivation" "Astrolabe"]}})
    (take-credits state :corp)
    (core/gain state :runner :click 1)
    (play-from-hand state :runner "ONR Umbrella Policy")
    (play-from-hand state :runner "ONR Umbrella Policy")
    (play-from-hand state :runner "Cache")
    (play-from-hand state :runner "Motivation")
    (play-from-hand state :runner "Astrolabe")
    (take-credits state :runner)
    (trash state :runner (get-resource state 2))
    (is (no-prompt? state :runner) "Sac Con not prompting to prevent resource trash")
    (trash state :runner (get-program state 0))
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 2 (count (:discard (get-runner)))) "Sac Con trashed")
    (is (= 1 (count (get-program state))) "Cache still installed")
    (trash state :runner (get-hardware state 0))
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 3 (count (:discard (get-runner)))) "Sac Con trashed")
    (is (= 1 (count (get-hardware state))) "Astrolabe still installed")))
