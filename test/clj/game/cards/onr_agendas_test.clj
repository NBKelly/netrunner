(ns game.cards.onr-agendas-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core.eid :refer [make-eid]]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest onr-bioweapons-engineering
  ;; The Cleaners
  (do-game
      (new-game {:corp {:deck ["ONR Bioweapons Engineering" "Scorched Earth"]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 3)]}})
      (play-and-score state "ONR Bioweapons Engineering")
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Scorched Earth")
      (is (zero? (count (:hand (get-runner)))) "5 damage dealt to Runner")))

;; Hostile Takeover
(deftest onr-charity-takeover
  (do-game
    (new-game {:corp {:deck ["ONR Charity Takeover"]}})
    (play-and-score state "ONR Charity Takeover")
    (is (= 14 (:credit (get-corp))) "Gain 9 credits")
    (is (= 1 (count-bad-pub state)) "Take 1 bad publicity")))

(deftest onr-corporate-retreat
  ;; Government Takeover
  (do-game
    (new-game {:corp {:deck ["ONR Corporate Retreat" "Ice Wall"]}})
    (play-and-score state "ONR Corporate Retreat")
    (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
    (let [gt-scored (get-scored state :corp 0)]
      (card-ability state :corp gt-scored 0)
      (is (= 7 (:credit (get-corp))) "Should gain 2 credits from 5 to 7")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (card-ability state :corp gt-scored 0)
      (is (= 7 (:credit (get-corp))) "Should not be able to use the ability!"))))

(deftest onr-corporate-war
  ;; Corporate War
  (do-game
    (new-game {:corp {:deck [(qty "ONR Corporate War" 2)]}})
    (is (= 5 (:credit (get-corp))))
    (play-and-score state "ONR Corporate War")
    (is (zero? (:credit (get-corp))) "Lost all credits")
    (core/gain state :corp :credit 12)
    (play-and-score state "ONR Corporate War")
    (is (= 24 (:credit (get-corp))) "Had 12 credits when scoring, gained another 12")))

(deftest onr-data-fort-remapping
  ;; Nisei MK II - Remove hosted counter to ETR, check this works in 4.3
  (do-game
    (new-game {:corp {:deck ["ONR Data Fort Remapping"]}})
    (play-and-score state "ONR Data Fort Remapping")
    (let [scored-nisei (get-scored state :corp 0)]
      (is (= 1 (get-counters (refresh scored-nisei) :agenda)) "Scored Nisei has one counter")
      (take-credits state :corp)
      (run-on state "HQ")
      (is (= :movement (:phase (:run @state))) "In Movement phase before Success")
      (card-ability state :corp (refresh scored-nisei) 0)
      (is (not (:run @state)) "Run ended by using Nisei counter")
      (is (zero? (get-counters (refresh scored-nisei) :agenda)) "Scored Nisei has no counters"))))

(deftest onr-marked-accounts
  ;; TGTBT - Give the Runner 1 tag when they access
  ;; OHG still not working...
  (do-game
    (new-game {:corp {:deck [(qty "ONR Marked Accounts" 2) "Old Hollywood Grid"]}})
    (play-from-hand state :corp "ONR Marked Accounts" "New remote")
    (play-from-hand state :corp "Old Hollywood Grid" "Server 1")
    (play-from-hand state :corp "ONR Marked Accounts" "New remote")
    (take-credits state :corp)
    (let [tg1 (get-content state :remote1 0)
          ohg (get-content state :remote1 1)]
      (rez state :corp ohg)
      (run-empty-server state "Server 1")
      (click-card state :runner tg1)
      ;; Accesses TGTBT but can't steal
      (is (= 1 (count-tags state)) "Runner took 1 tag from accessing without stealing")
      (click-prompt state :runner "No action"))
    (click-prompt state :runner "Pay 4 [Credits] to trash") ;; Trashes OHG
    (run-empty-server state "Server 2")
    ;; Accesses TGTBT and can steal
    (click-prompt state :runner "Steal")
    (is (= 2 (count-tags state)) "Runner took 1 tag from accessing and stealing")))

(deftest onr-marine-arcology
  ;; Gila Hands Arcology
  (do-game
    (new-game {:corp {:deck ["ONR Marine Arcology"]}})
    (play-and-score state "ONR Marine Arcology")
    (is (= 2 (:click (get-corp))) "Should have 2 clicks left")
    (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
    (core/gain state :corp :click 2)
    (let [gha-scored (get-scored state :corp 0)]
      (card-ability state :corp gha-scored 0)
      (is (= 2 (:click (get-corp))) "Should spend 2 clicks on Gila Hands")
      (is (= 8 (:credit (get-corp))) "Should gain 3 credits from 5 to 8")
      (card-ability state :corp gha-scored 0)
      (is (zero? (:click (get-corp))) "Should spend 2 clicks on Gila Hands")
      (is (= 11 (:credit (get-corp))) "Should gain 3 credits from 8 to 11"))))

(deftest onr-ice-transmutation
  (do-game
    (new-game {:corp {:hand ["Hydra" "ONR Ice Transmutation"]
                      :credits 15}})
    (play-from-hand state :corp "Hydra" "HQ")
    (let [hydra (get-ice state :hq 0)]
      (rez state :corp hydra)
      (play-from-hand state :corp "ONR Ice Transmutation" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-card state :corp (refresh hydra))
      (is (= 7 (get-strength (refresh hydra))))
      (is (= 3 (count (:subroutines (refresh hydra)))))
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state :encounter-ice)
      (is (= 6 (count (:subroutines (refresh hydra)))))
      (run-continue state :pass-ice)
      (is (= 3 (count (:subroutines (refresh hydra))))))))

(deftest onr-on-call-solo-team
  ;; Private Security Force
  (do-game
    (new-game {:corp {:deck [(qty "ONR On-Call Solo Team" 10)]}})
    (gain-tags state :runner 1)
    (play-and-score state "ONR On-Call Solo Team")
    (let [psf-scored (get-scored state :corp 0)]
      (card-ability state :corp psf-scored 0)
      (is (= 1 (count (:discard (get-runner)))))
      (take-credits state :runner)
      (dotimes [_ 3]
        (card-ability state :corp psf-scored 0))
      (is (= 3 (count (:discard (get-runner)))))
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest onr-political-overthrow
  ;; Government Takeover
  (do-game
    (new-game {:corp {:deck ["ONR Political Overthrow"]}})
    (play-and-score state "ONR Political Overthrow")
    (is (= 5 (:credit (get-corp))) "Should start with 5 credits")
    (let [gt-scored (get-scored state :corp 0)]
      (card-ability state :corp gt-scored 0)
      (is (= 8 (:credit (get-corp))) "Should gain 3 credits from 5 to 8"))))

(deftest onr-polymer-breakthrough
  ;; Rezeki - gain 1c when turn begins
  (do-game
    (new-game {:corp {:deck ["ONR Polymer Breakthrough"]}})
    (play-and-score state "ONR Polymer Breakthrough")
    (take-credits state :corp)
    (let [credits (:credit (get-corp))]
      (take-credits state :runner)
      (is (= (:credit (get-corp)) (+ credits 1)) "Gain 1 from Polymer Breakthrough"))))

(deftest onr-priority-requisition
  ;; Priority Requisition
  (do-game
    (new-game {:corp {:deck ["ONR Priority Requisition" "Archer"]}})
    (play-from-hand state :corp "Archer" "HQ")
    (let [arc (get-ice state :hq 0)]
      (play-and-score state "ONR Priority Requisition")
      (click-card state :corp arc)
      (is (rezzed? (refresh arc))))))

(deftest onr-subsidiary-branch
    ;; Gain an additional click
    (do-game
      (new-game {:corp {:deck ["ONR Subsidiary Branch"
                               "Melange Mining Corp."]}})
      (play-and-score state "ONR Subsidiary Branch")
      (is (= 1 (:agenda-point (get-corp))))
      (play-from-hand state :corp "Melange Mining Corp." "New remote")
      (let [mmc (get-content state :remote2 0)]
        (rez state :corp mmc)
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 4 (:click (get-corp))))
        (card-ability state :corp mmc 0)
        (is (= 1 (:click (get-corp)))))))

(deftest onr-world-domination
  (do-game
    (new-game {:corp {:deck ["ONR World Domination"]}})
    (play-and-score state "ONR World Domination")
    (is (= 7 (:agenda-point (get-corp))))))
