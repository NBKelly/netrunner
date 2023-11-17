(ns game.cards.onr-programs-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.macros :refer [req]]
            [game.utils :as utils]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(deftest onr-baedekers-net-map
  (do-game
    (new-game {:corp {:deck ["ONR Manhunt"]}
               :runner {:hand ["ONR Baedeker's Net Map" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "ONR Baedeker's Net Map")
    (run-empty-server state :rd)
    (take-credits state :runner)
    ;; 7 - 4 means that our max trace is 3
    ;; the runner should also have 2 credits
    (play-from-hand state :corp "ONR Manhunt")
    (click-prompt state :corp "3")
    (click-prompt state :runner "ONR Baedeker's Net Map")
    (changes-val-macro
      -0 (:credit (get-runner))
      "spent 0c to boost link to 1"
      (click-prompt state :runner "0 [Credits]: Base Link 1"))
    (changes-val-macro
      -1 (:credit (get-runner))
      "spent 1c to boost link to 2"
      (click-prompt state :runner "1 [Credits]: +1 Link"))
    (click-prompt state :runner "Done")
    (is (= 1 (count-tags state)) "Runner should have 1 tag")))

(deftest onr-bakdoor
  (do-game
    (new-game {:corp {:deck ["ONR Manhunt"]}
               :runner {:hand ["ONR Bakdoor [TM]" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "ONR Bakdoor [TM]")
    (run-empty-server state :rd)
    (take-credits state :runner)
    ;; 7 - 4 means that our max trace is 3
    ;; the runner should also have 2 credits
    (play-from-hand state :corp "ONR Manhunt")
    (click-prompt state :corp "3")
    (click-prompt state :runner "ONR Bakdoor [TM]")
    (changes-val-macro
      -0 (:credit (get-runner))
      "spent 0c to boost link to 3"
      (click-prompt state :runner "0 [Credits]: Base Link 3"))
    (changes-val-macro
      -2 (:credit (get-runner))
      "spent 2c to boost link to 4"
      (click-prompt state :runner "2 [Credits]: +1 Link"))
    (click-prompt state :runner "Done")
    (is (= 0 (count-tags state)) "Runner should have 0 tag")))

(deftest onr-cloak-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["ONR Cloak" "Refractor"]
                          :credits 15}})
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Cloak")
      (play-from-hand state :runner "Refractor")
      (let [cl (get-program state 0)
            refr (get-program state 1)]
        (run-on state :hq)
        (is (= 3 (get-counters cl :recurring)) "Cloak starts with 3 credits")
        (changes-val-macro 0 (:credit (get-runner))
                           "Used 1 credit from Cloak"
                           (card-ability state :runner refr 1)
                           (click-card state :runner cl)))))

(deftest onr-clown
  ;; Clown - lower ice strength on encounter
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Ganked!"]}
               :runner {:deck ["ONR Clown"]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (play-from-hand state :corp "Ganked!" "Archives")
    (take-credits state :corp 2)
    (let [iwall (get-ice state :archives 0)]
      (play-from-hand state :runner "ONR Clown")
      (run-on state "Archives")
      (rez state :corp iwall)
      (run-continue state)
      (is (zero? (get-strength (refresh iwall))) "Ice Wall strength at 0 for encounter")
      (run-continue state :movement)
      (is (= 1 (get-strength (refresh iwall))) "Ice Wall strength at 1 after encounter")
      (run-continue state)
      (click-prompt state :corp "Yes")
      (click-card state :corp iwall)
      (is (zero? (get-strength (refresh iwall))) "Ice Wall strength at 0 for encounter")
      (run-continue state)
      (is (= 1 (get-strength (refresh iwall))) "Ice Wall strength at 1 after encounter"))))

(deftest invisibility-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["ONR Invisibility" "Refractor"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Invisibility")
      (play-from-hand state :runner "Refractor")
      (let [cl (get-program state 0)
            refr (get-program state 1)]
        (run-on state :hq)
        (is (= 1 (get-counters cl :recurring)) "Invisibility starts with 1 credit1")
        (changes-val-macro 0 (:credit (get-runner))
                           "Used 1 credit from Cloak"
                           (card-ability state :runner refr 1)
                           (click-card state :runner cl)))))

(deftest onr-newsgroup-filter
  ;; Magnum Opus - Gain 2 cr
  (do-game
    (new-game {:runner {:deck ["ONR Newsgroup Filter"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Newsgroup Filter")
    (is (= 2 (core/available-mu state)))
    (is (zero? (:credit (get-runner))))
    (let [mopus (get-program state 0)]
      (card-ability state :runner mopus 0)
      (is (= 2 (:credit (get-runner))) "Gain 2cr"))))

(deftest onr-poltergiest
  ;; Paricia
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["PAD Campaign" "Shell Corporation"]}
               :runner {:hand ["ONR Poltergeist"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Shell Corporation" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Poltergeist")
    (let [pad (get-content state :remote1 0)
          shell (get-content state :remote2 0)]
      (run-empty-server state :remote2)
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (no-prompt? state :runner) "No pay-credit prompt as it's an upgrade")
      (is (nil? (refresh shell)) "Shell Corporation successfully trashed")
      (run-empty-server state :remote1)
      (is (= 2 (:credit (get-runner))) "Runner can't afford to trash PAD Campaign")
      (click-prompt state :runner "Pay 4 [Credits] to trash")
      (dotimes [_ 2]
        (click-card state :runner "ONR Poltergeist"))
      (is (nil? (refresh pad)) "PAD Campaign successfully trashed"))))

(deftest zetatech-software-installer-pay-credits-prompt
  ;; Pay-credits prompt
  (do-game
    (new-game {:runner {:deck ["ONR Zetatech Software Installer" "ONR Hammer"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Zetatech Software Installer")
    (let [rara (get-program state 0)]
        (changes-val-macro 0 (:credit (get-runner))
                           "Used 2 credits from ONR Zetatech Software Installer"
                           (play-from-hand state :runner "ONR Hammer")
                           (click-card state :runner rara)
                           (click-card state :runner rara)))))
