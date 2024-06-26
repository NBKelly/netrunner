(ns game.cards.onr-events-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest onr-all-hands
  ;; Legwork
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Hostile Takeover" 5)]}
               :runner {:hand ["ONR All-Hands"]}})
    (take-credits state :corp)
    (play-run-event state "ONR All-Hands" :hq)
    (dotimes [_ 4]
      (click-prompt state :runner "Steal"))
    (is (not (:run @state)) "Run has finished")))

(deftest onr-anonymous-tip
  ;; Emergency Shutdown
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["ONR Brain Drain"]}
               :runner {:hand ["ONR Anonymous Tip"]}})
    (play-from-hand state :corp "ONR Brain Drain" "New remote")
    (let [iw (get-ice state :remote1 0)]
      (rez state :corp iw)
      (is (rezzed? (refresh iw)) "Ice Wall is rezzed")
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Anonymous Tip")
      (is (not (no-prompt? state :runner)) "Runner has a derez prompt")
      (click-card state :runner (refresh iw))
      (is (not (rezzed? (refresh iw))) "Ice Wall is derezzed"))))

(deftest onr-blackmail
  ;; Notoriety - Run all 3 central servers successfully and play to gain 1 agenda point
  (do-game
    (new-game {:corp {:deck ["Hedge Fund"]}
               :runner {:deck ["ONR Blackmail"]
                        :credits 15}})
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (play-run-event state "ONR Blackmail" :hq)
    (is (= 0 (count (:scored (get-runner)))) "we score without moving the card")
    (is (= 1 (:agenda-point (get-runner))) "Notoriety scored for 1 agenda point")))

(deftest onr-bodyweight-tm-synthetic-blood
  ;; Quality Time
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["ONR Bodyweight [TM] Synthetic Blood"]}})
    (take-credits state :corp)
    (is (= 1 (count (:hand (get-runner)))) "Runner should have 1 card in hand")
    (play-from-hand state :runner "ONR Bodyweight [TM] Synthetic Blood")
    (is (= 5 (count (:hand (get-runner)))) "Runner should draw 5 cards")))

(deftest onr-cruising-for-netwatch
  ;; Build Script
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["ONR Cruising for Netwatch"]}})
    (take-credits state :corp)
    (let [credits (:credit (get-runner))
          hand (dec (count (:hand (get-runner))))]
      (play-from-hand state :runner "ONR Cruising for Netwatch")
      (is (= (inc credits) (:credit (get-runner))) "Gained 1 credit")
      (is (= (+ 2 hand) (count (:hand (get-runner)))) "Drew 2 cards"))))

(deftest onr-custodial-position
  (do-game
     (new-game {:corp {:deck [(qty "Quandary" 5)]
                       :hand [(qty "Quandary" 5)]}
                :runner {:deck ["ONR Custodial Position"]}})
     (take-credits state :corp)
     (play-run-event state "ONR Custodial Position" :rd)
     (is (accessing state "Quandary"))
     (click-prompt state :runner "No action")
     (is (accessing state "Quandary"))
     (click-prompt state :runner "No action")
     (is (accessing state "Quandary"))
     (click-prompt state :runner "No action")
     (is (not (:run @state)))))

(deftest onr-edited-shipping-manifests-steal
    ;; Use ability
    (do-game
      (new-game {:runner {:deck [(qty "ONR Edited Shipping Manifests" 3)]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      ;; play Account Siphon, use ability
      (play-run-event state "ONR Edited Shipping Manifests" :hq)
      ;;(click-prompt state :runner "Account Siphon")
      (is (= 1 (count-tags state)) "Runner took 1 tags")
      (is (= 14 (:credit (get-runner))) "Runner gained 10 credits")
      (is (= 7 (:credit (get-corp))) "Corp lost 1 credits")))

(deftest onr-executive-wiretaps
  ;; Legwork
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Hostile Takeover" 3)]}
               :runner {:hand ["ONR Executive Wiretaps"]}})
    (take-credits state :corp)
    (play-run-event state "ONR Executive Wiretaps" :hq)
    (dotimes [_ 3]
      (click-prompt state :runner "Steal"))
    (is (not (:run @state)) "Run has finished")))

(deftest onr-edited-shipping-manifests-access
    ;; Access
    (do-game
      (new-game {:runner {:deck [(qty "ONR Edited Shipping Manifests" 3)]}})
      (take-credits state :corp) ; pass to runner's turn by taking credits
      (core/lose state :corp :credit 8)
      (is (= 0 (:credit (get-corp))) "Corp has 0 credits")
      ;; play another Siphon, do not use ability
      (play-run-event state "ONR Edited Shipping Manifests" :hq)
      ;;(click-prompt state :runner "Breach HQ")
      ;;(is (zero? (count-tags state)) "Runner did not take any tags")
      (is (= 4 (:credit (get-runner))) "Runner did not gain any credits")
      (is (= 0 (:credit (get-corp))) "Corp did not lose any credits")))

(deftest onr-forged-activation-orders-corp-chooses-to-trash-the-ice
    ;; Corp chooses to trash the ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["ONR Forged Activation Orders"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Forged Activation Orders")
      (click-card state :runner "Ice Wall")
      (click-prompt state :corp "Trash ice protecting HQ at position 0")
      (is (= "Ice Wall" (:title (get-discarded state :corp 0))) "Ice Wall is trashed")))

(deftest onr-forged-activation-orders-corp-chooses-to-rez-the-ice
    ;; Corp chooses to rez the ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["ONR Forged Activation Orders"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Forged Activation Orders")
      (click-card state :runner "Ice Wall")
      (click-prompt state :corp "Rez ice protecting HQ at position 0")
      (is (rezzed? (get-ice state :hq 0)) "Ice Wall is rezzed")))

(deftest onr-forged-activation-orders-corp-cannot-rez-the-ice
    ;; Corp cannot rez the ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Chiyashi"]}
                 :runner {:hand ["ONR Forged Activation Orders"]}})
      (play-from-hand state :corp "Chiyashi" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Forged Activation Orders")
      (click-card state :runner "Chiyashi")
      (is (= ["Trash ice protecting HQ at position 0"]
           (mapv :value (:choices (prompt-map :corp)))))))

(deftest onr-gideons-pawnshop
  ;; Archived Memories
  (do-game
    (new-game {:runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["ONR Gideon's Pawnshop"]
                        :discard ["ONR Networking"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Gideon's Pawnshop")
    (click-card state :runner "ONR Networking")
    (is (= ["ONR Networking"] (->> (get-runner) :hand (map :title))) "ONR Networking should be in HQ")))

(deftest onr-hot-tip-for-wns
  (do-game
    (new-game {:corp {:hand ["ONR Black Ice Quality Assurance"]}
               :runner {:hand ["ONR Hot Tip for WNS"]}})
    (take-credits state :corp)
    (changes-val-macro
      0 (:agenda-point (get-runner))
      "can't play yet"
      (play-from-hand state :runner "ONR Hot Tip for WNS"))
    (run-empty-server state :hq)
    (click-prompt state :runner "Steal")
    (changes-val-macro
      1 (:agenda-point (get-runner))
      "+1 AP"
      (play-from-hand state :runner "ONR Hot Tip for WNS"))))


(deftest onr-inside-job
  ;; Inside Job
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["ONR Inside Job"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Inside Job")
      (click-prompt state :runner "HQ")
      (is (:run @state) "A run has been initiated")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (= :movement (:phase (get-run))) "Run has bypassed Ice Wall")))

(deftest onr-jack-n-joe
  ;; Diesel
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["ONR Jack 'n' Joe"]}})
    (take-credits state :corp)
    (let [hand (count (:hand (get-runner)))]
      (play-from-hand state :runner "ONR Jack 'n' Joe")
      (is (= (+ hand -1 3) (count (:hand (get-runner)))) "Runner plays Diesel and draws 3 cards"))))

(deftest onr-lucidrine-tm-booster-drug
  ;; Stimhack - Gain 9 temporary credits and take 1 brain damage after the run
  (do-game
    (new-game {:corp {:deck ["Eve Campaign"]}
               :runner {:deck ["ONR Lucidrine [TM] Booster Drug" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Lucidrine [TM] Booster Drug")
    (click-prompt state :runner "HQ")
    (is (= [:hq] (get-in @state [:run :server])) "Run initiated on HQ")
    (run-continue state)
    (is (= 14 (:credit (get-runner))))
    (is (= 9 (:run-credit (get-runner))) "Gained 9 credits for use during the run")
    (click-prompt state :runner "Pay 5 [Credits] to trash") ; choose to trash Eve
    (is (and (zero? (count (:hand (get-corp))))
             (= 1 (count (:discard (get-corp)))))
        "Corp hand empty and Eve in Archives")
    (is (= 5 (:credit (get-runner))))
    (is (zero? (count (:hand (get-runner)))) "Lost card from Grip to core damage")
    (is (= 4 (hand-size :runner)))
    (is (= 1 (:brain-damage (get-runner))))))

(deftest onr-mit-west-tier
  ;; Levy AR Lab Access
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck ["Magnum Opus"]
                        :hand ["ONR MIT West Tier" "Easy Mark"]
                        :discard [(qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR MIT West Tier")
    (is (= 5 (count (:hand (get-runner)))) "Runner should draw 5 cards")
    (is (zero? (count (:deck (get-runner)))) "Stack should be empty")
    (is (zero? (count (:discard (get-runner)))) "Heap should be empty")
    (is (= "ONR MIT West Tier" (:title (get-rfg state :runner 0))) "Levy should be rfg'd")))

(deftest onr-meat-upgrade
  ;; Lawyer Up - Lose 2 tags and draw 3 cards
  (do-game
    (new-game {:runner {:deck ["ONR Meat Upgrade" (qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (gain-tags state :runner 3)
    (play-from-hand state :runner "ONR Meat Upgrade")
    (is (= 3 (count (:hand (get-runner)))) "Drew 3 cards")
    (is (= 2 (:click (get-runner))) "Spent 2 clicks")
    (is (= 1 (count-tags state)) "Lost 2 tags")))

(deftest onr-kilroy-was-here
  ;; Demolition Run - Trash at no cost
  (do-game
    (new-game {:corp {:deck ["False Lead"
                             "Shell Corporation"
                             (qty "Hedge Fund" 3)]}
               :runner {:deck ["ONR Kilroy Was Here"]}})
    (core/move state :corp (find-card "False Lead" (:hand (get-corp))) :deck) ; put False Lead back in R&D
    (play-from-hand state :corp "Shell Corporation" "R&D") ; install upgrade with a trash cost in root of R&D
    (take-credits state :corp 2) ; pass to runner's turn by taking credits
    (play-from-hand state :runner "ONR Kilroy Was Here")
    (is (= 5 (:credit (get-runner))) "Paid 0 credits for the event")
    (is (= [:rd] (get-in @state [:run :server])) "Run initiated on R&D")
    (run-continue state)
    (click-prompt state :runner "Unrezzed upgrade")
    (click-prompt state :runner "[ONR Kilroy Was Here] Trash card")
    (is (= 5 (:credit (get-runner))) "Trashed Shell Corporation at no cost")
    (click-prompt state :runner "[ONR Kilroy Was Here] Trash card")
    (is (zero? (:agenda-point (get-runner))) "Didn't steal False Lead")
    (is (= 2 (count (:discard (get-corp)))) "2 cards in Archives")
    (is (no-prompt? state :runner) "Run concluded")))

(deftest onr-networking
  ;; ONR Networking
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["ONR Livewire's Contacts"]}})
    (take-credits state :corp)
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "ONR Livewire's Contacts")
      (is (= 8 (:credit (get-runner))) "Runner should spend 0 and gain 3"))))

(deftest onr-networking
  ;; ONR Networking
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["ONR Networking"]}})
    (take-credits state :corp)
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "ONR Networking")
      (is (= 2 (:click (get-runner))) "ONR Networking is a double")
      (is (= (+ credits -3 9) (:credit (get-runner))) "Runner should spend 3 and gain 9"))))

(deftest networking
  ;; Networking
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["ONR Open-Ended(R) Mileage Program"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Open-Ended(R) Mileage Program")
    (is (= "ONR Open-Ended(R) Mileage Program" (-> (get-runner) :hand first :title)) "ONR Open-Ended(R) Mileage Program shouldn't be played")
    (gain-tags state :runner 4)
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "ONR Open-Ended(R) Mileage Program")
      (is (= 3 (count-tags state)) "Runner should lose 1 tag")
      (click-prompt state :runner "Yes")
      (is (= (dec credits) (:credit (get-runner))) "Runner should spend 1 on Networking ability")
      (is (zero? (count (:discard (get-runner)))) "Runner's discard should be empty")
      (is (= "ONR Open-Ended(R) Mileage Program" (-> (get-runner) :hand first :title))))
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "ONR Open-Ended(R) Mileage Program")
      (is (= 2 (count-tags state)) "Runner should lose 1 tag")
      (click-prompt state :runner "No")
      (is (= credits (:credit (get-runner))) "Runner should spend 1 on ONR Open-Ended(R) Mileage Program ability")
      (is (= 1 (count (:discard (get-runner)))) "Runner's discard should be empty")
      (is (= "ONR Open-Ended(R) Mileage Program" (-> (get-runner) :discard first :title)) "ONR Open-Ended(R) Mileage Program should be in heap"))))

(deftest onr-prearranged-drop
  ;;
  (do-game
    (new-game {:corp {:deck ["NAPD Contract" "Hostile Takeover" "PAD Campaign"]}
               :runner {:deck ["ONR Prearranged Drop"]}})
    (play-from-hand state :corp "NAPD Contract" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (core/lose state :runner :credit 2)
    (play-from-hand state :runner "ONR Prearranged Drop")
    (is (= 3 (:credit (get-runner))) "Can't afford to steal NAPD")
    (changes-val-macro
        0 (:credit (get-runner))
        "No credits gained from accessing non-agenda cards"
        (run-empty-server state "Server 2"))
    (click-prompt state :runner "No action")
    (run-empty-server state "Server 1")
    (is (= 9 (:credit (get-runner))) "Gained 6c on access, can steal NAPD")
    (click-prompt state :runner "Pay to steal")
    (is (= 2 (:agenda-point (get-runner))) "Stole agenda")
    (is (= 5 (:credit (get-runner))))
    (run-empty-server state "HQ")
    (click-prompt state :runner "Steal")
    (is (= 5 (:credit (get-runner))) "No credits gained from 2nd agenda access")))

(deftest onr-priority-wreck
  ;; Vamp - Run HQ and use replace access to pay credits to drain equal amount from Corp
  (do-game
    (new-game {:runner {:deck ["ONR Priority Wreck" (qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (is (= 8 (:credit (get-corp))))
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Sure Gamble")
    (is (= 13 (:credit (get-runner))))
    (play-run-event state "ONR Priority Wreck" :hq)
    ;;(click-prompt state :runner "ONR Priority Wreck") - this one is mandatory!
    (click-prompt state :runner "8")
    (is (= 5 (:credit (get-runner))) "Paid 8 credits")
    (is (zero? (:credit (get-corp))) "Corp lost all 8 credits")))

(deftest onr-running-interference
  ;; Running Interference
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall" "Archer" "Hostile Takeover"]
                      :credits 100}
               :runner {:hand ["ONR Running Interference"]}})
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :corp "Archer" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Running Interference")
    (click-prompt state :runner "HQ")
    (let [archer (get-ice state :hq 0)
          credits (:credit (get-corp))]
      (rez state :corp archer)
      (click-card state :corp (get-scored state :corp 0))
      (is (no-prompt? state :corp) "Only 1 agenda required to rez")
      (is (= (- credits (* 2 (:cost archer))) (:credit (get-corp))) "Rezzing Archer costs double")
      (is (rezzed? (refresh archer)) "Archer is rezzed"))
    (run-continue-until state :movement)
    (run-jack-out state)
    (let [iw (get-ice state :hq 1)
          credits (:credit (get-corp))]
      (run-on state "HQ")
      (rez state :corp iw)
      (is (no-prompt? state :corp))
      (is (= (- credits (:cost iw)) (:credit (get-corp))) "Rezzing Ice Wall costs normal"))))

(deftest onr-score
  ;; Sure Gamble
  (do-game
    (new-game {:runner {:deck ["ONR Score!"]}})
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))))
    (play-from-hand state :runner "ONR Score!")
    (is (= 9 (:credit (get-runner))))))

(deftest onr-sneak-preview-programs-hosted-after-install-get-returned-to-stack-issue-1081
    ;; Programs hosted after install get returned to Stack. Issue #1081
    (do-game
      (new-game {:corp {:deck ["Wraparound"]}
                 :runner {:deck [(qty "ONR Sneak Preview" 2) "Morning Star"
                                 "Knight" "Leprechaun"]}})
      (play-from-hand state :corp "Wraparound" "HQ")
      (let [wrap (get-ice state :hq 0)]
        (rez state :corp wrap))
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (trash-from-hand state :runner "Morning Star")
      (trash-from-hand state :runner "Knight")
      (let [ms (find-card "Morning Star" (:discard (get-runner)))]
        (play-from-hand state :runner "Leprechaun")
        (play-from-hand state :runner "ONR Sneak Preview")
        (click-prompt state :runner "Heap")
        (click-prompt state :runner ms))
      (let [lep (get-program state 0)
            ms (get-program state 1)]
        (card-ability state :runner lep 1)
        (click-card state :runner ms)
        (is (= "Morning Star" (:title (first (:hosted (refresh lep))))) "Morning Star hosted on Lep"))
      (take-credits state :runner)
      (is (= "Morning Star" (:title (first (:deck (get-runner))))) "Morning Star returned to Stack from host")
      (take-credits state :corp)
      (let [kn (find-card "Knight" (:discard (get-runner)))]
        (play-from-hand state :runner "ONR Sneak Preview")
        (click-prompt state :runner "Heap")
        (click-prompt state :runner kn))
      (let [wrap (get-ice state :hq 0)
            kn (get-program state 1)]
        (card-ability state :runner kn 0)
        (click-card state :runner wrap)
        (is (= "Knight" (:title (first (:hosted (refresh wrap))))) "Knight hosted on Wraparound")
        (take-credits state :runner)
        (is (= "Knight" (:title (first (:deck (get-runner))))) "Knight returned to Stack from host ice"))))

(deftest onr-sneak-preview-make-sure-program-remains-installed-if-scavenged
    ;; Make sure program remains installed if Scavenged
    (do-game
      (new-game {:runner {:hand ["ONR Sneak Preview" "Scavenge" "Inti"]
                          :discard ["Morning Star"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Sneak Preview")
      (click-prompt state :runner "Heap")
      (click-prompt state :runner "Morning Star")
      (is (= 2 (:credit (get-runner))) "Program installed for free")
      (play-from-hand state :runner "Scavenge")
      (click-card state :runner "Morning Star")
      (click-card state :runner "Morning Star")
      (take-credits state :runner)
      (is (empty? (:deck (get-runner))) "Morning Star not returned to Stack")
      (is (= "Morning Star" (:title (get-program state 0))) "Morning Star still installed")))

(deftest onr-sneak-preview-heap-locked-test
    ;; Heap Locked Test
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5) "Blacklist"]}
                 :runner {:hand [(qty "ONR Sneak Preview" 3) "Scavenge" "Inti"]
                          :deck ["Pelangi"]
                          :discard ["Morning Star"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Sneak Preview")
      (is (= "Install a program from the stack?" (:msg (prompt-map :runner))) "Stack is only option")
      (is (= 1 (-> (prompt-map :runner) :choices count)) "Runner has 1 choice")
      (is (= ["Stack"] (prompt-buttons :runner)) "Runner's only choice is Stack")))

(deftest onr-stakeout
  ;; Process Automation
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["ONR Stakeout"]}})
    (take-credits state :corp)
    (let [credits (:credit (get-runner))
          hand (dec (count (:hand (get-runner))))]
      (play-from-hand state :runner "ONR Stakeout")
      (is (= (+ 2 credits) (:credit (get-runner))) "Should gain 2 credits")
      (is (= (inc hand) (count (:hand (get-runner)))) "Should draw 1 card"))))
