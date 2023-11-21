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
    (is (= 1 (count (:scored (get-runner)))) "Notoriety moved to score area")
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
