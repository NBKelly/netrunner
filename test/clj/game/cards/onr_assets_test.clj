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

(deftest onr-corprunners-shattered-remains
  ;; Shattered Remains
  (do-game
    (new-game {:corp {:deck [(qty "ONR Corprunner's Shattered Remains" 2)]}
               :runner {:deck ["Cyberfeeder" "Lemuria Codecracker"]}})
    (play-from-hand state :corp "ONR Corprunner's Shattered Remains" "New remote")
    (play-from-hand state :corp "ONR Corprunner's Shattered Remains" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Cyberfeeder")
    (play-from-hand state :runner "Lemuria Codecracker")
    (take-credits state :runner)
    (let [remains1 (get-content state :remote1 0)
          remains2 (get-content state :remote2 0)
          cyber (get-hardware state 0)]
      (rez state :corp remains1)
      (rez state :corp remains2)
      (advance state remains2 1)
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (is (no-prompt? state :corp) "Corp shouldn't get Shattered Remains ability prompt when no counters")
      (click-prompt state :runner "No action")
      (run-empty-server state :remote2)
      (let [credits (:credit (get-corp))]
        (click-card state :corp cyber)
        (is (count (:discard (get-runner))) "Cyberfeeder should be in discard from Shattered Remains")))))

(deftest onr-cowboy-sysop
  ;; Isabel McGuire
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "ONR Cowboy Sysop"]}})
    (play-from-hand state :corp "ONR Cowboy Sysop" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (zero? (-> (get-corp) :hand count)))
    (let [isabel (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (rez state :corp isabel)
      (card-ability state :corp isabel 0)
      (click-card state :corp (refresh iw))
      (is (= 1 (-> (get-corp) :hand count))))))

(deftest onr-department-of-misinformation
  ;; Zaibatsu Loyalty
  (do-game
    (new-game {:corp {:deck ["ONR Department of Misinformation" "Ice Wall"]}
               :runner {:deck ["Lemuria Codecracker"]}})
    (core/gain state :corp :click 10 :click 10)
    (play-from-hand state :corp "ONR Department of Misinformation" "New remote")
    (play-from-hand state :corp "Ice Wall" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :click 10 :click 10)
    (play-from-hand state :runner "Lemuria Codecracker")
    (let [code (get-hardware state 0)
          iw (get-ice state :remote2 0)
          zai (get-content state :remote1 0)]
      (run-empty-server state "HQ")
      (card-ability state :runner code 0)
      (click-card state :runner (refresh iw))
      (is (some? (prompt-map :corp)) "Corp should get the option to rez Zaibatsu Loyalty before expose")
      (click-prompt state :corp "Yes")
      (is (rezzed? (refresh zai)) "Zaibatsu Loyalty should be rezzed")
      (let [credits (:credit (get-corp))]
        (card-ability state :corp zai 0)
        (is (= (dec credits) (:credit (get-corp))) "Corp should lose 1 credit for stopping the expose")
        (click-prompt state :corp "Done")))))

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

(deftest onr-experimental-ai
  ;; Aggressive Secretary
  (do-game
    (new-game {:corp {:deck ["ONR Experimental AI"]}
               :runner {:deck [(qty "Cache" 3)]}})
    (play-from-hand state :corp "ONR Experimental AI" "New remote")
    (let [as (get-content state :remote1 0)]
      ;; Single advance AggSec
      (click-advance state :corp (refresh as))
      (rez state corp (refresh as))
      (take-credits state :corp)
      ;; Run on AggSec with 3 programs
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (run-empty-server state "Server 1")
      ;; Corp can trash one program
      (click-card state :corp (get-program state 1))
      ;; There should be two Caches left
      ;;(is (= 3 (:credit (get-corp))))
      (is (= 2 (count (get-program state)))))))

(deftest onr-i-got-a-rock
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["ONR I Got a Rock" "City Works Project"]}
               :runner {:hand [(qty "Sure Gamble" 16)]}})
    (play-from-hand state :corp "ONR I Got a Rock" "New remote")
    (let [rock (get-content state :remote1 0)]
      (rez state :corp rock)
      (card-ability state :corp (refresh rock) 0)
      (is (= 16 (count (:hand (get-runner)))) "IGAR should not be played as runner has no tags")
      (gain-tags state :runner 2)
      (card-ability state :corp (refresh rock) 0)
      (is (= 16 (count (:hand (get-runner))))
          "IGAR should not be played as the corp can't pay 3 agenda points")
      (play-and-score state "City Works Project")
      (is (zero? (count (:discard (get-runner)))) "Runner should have 0 cards in discard")
      (card-ability state :corp (refresh rock) 0)
      (is (= 15 (count (:discard (get-runner)))) "Runner should take 15 damage"))))

(deftest onr-information-laundering
  ;; GRNDL Refinery
  (do-game
    (new-game {:corp {:deck ["ONR Information Laundering"]}})
    (core/gain state :corp :click 100 :credit 100)
    (dotimes [i 5]
      (play-from-hand state :corp "ONR Information Laundering" "New remote")
      (let [grndl (get-content state (keyword (str "remote" (inc i))) 0)
            credits (- (:credit (get-corp)) i)]
        (rez state :corp grndl)
        (when (pos? i)
          (advance state (refresh grndl) i)
          (is (= i (get-counters (refresh grndl) :advancement)) (str "ONR Information Laundering should have " i " advancement counters on itself")))
        (card-ability state :corp (refresh grndl) 0)
        (is (= (+ credits (* i 4)) (:credit (get-corp))) (str "Corp should gain " (* i 4) " credits"))
        ;;(is (= 1 (-> (get-corp) :discard count)) "Archives should have 1 card in it")
        (is (= "ONR Information Laundering" (-> (get-corp) :discard first :title)) "Only card in Archives should be GRNDL Refinery")
        (core/move state :corp (find-card "ONR Information Laundering" (:discard (get-corp))) :hand)))))

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

(deftest onr-solo-squad
  ;; Private Security Force
  (do-game
    (new-game {:corp {:deck [(qty "ONR Solo Squad" 10)]}})
    (gain-tags state :runner 1)
    (play-from-hand state :corp "ONR Solo Squad" "New remote")
    (let [psf-scored (get-content state :remote1 0)]
      (rez state :corp psf-scored)
      (card-ability state :corp (refresh psf-scored) 0)
      (is (= 1 (count (:discard (get-runner)))))
      (take-credits state :corp)
      (take-credits state :runner)
      (dotimes [_ 3]
        (card-ability state :corp (refresh psf-scored) 0))
      (is (= 3 (count (:discard (get-runner)))))
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))
