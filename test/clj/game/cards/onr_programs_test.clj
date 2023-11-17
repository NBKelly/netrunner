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

(deftest onr-rent-i-con
  ;; Mayfly
  (do-game
      (new-game {:corp {:deck ["Anansi"]
                        :credits 20}
                 :runner {:hand ["ONR Rent-I-Con"]
                          :credits 20}})
      (play-from-hand state :corp "Anansi" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Rent-I-Con")
      (let [mayfly (get-program state 0)]
        (run-on state "HQ")
        (run-continue state)
        (changes-val-macro
          -6 (:credit (get-runner))
          "Paid 6 to fully break Anansi with ONR Rent-I-Con"
          (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card (refresh mayfly)}))
        (is (= 0 (count (remove :broken (:subroutines (get-ice state :hq 0))))) "Broken all subroutines")
        (core/continue state :corp nil)
        (run-jack-out state)
        (is (no-prompt? state :runner) "ONR Rent-I-Con not prompting to resolve each of its events")
        (is (= 1 (count (:discard (get-runner)))) "ONR Rent-I-Con trashed when run ends"))))

(deftest onr-scattershot
  ;; Paricia
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["PAD Campaign" "Shell Corporation"]}
               :runner {:hand ["ONR Scatter Shot"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Shell Corporation" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Scatter Shot")
    (let [pad (get-content state :remote1 0)
          shell (get-content state :remote2 0)]
      (run-empty-server state :remote2)
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (dotimes [_ 2]
        (click-card state :runner "ONR Scatter Shot"))
      (is (nil? (refresh shell)) "Shell Corporation successfully trashed"))))

(deftest onr-self-modifying-code-trash-pay-2-to-search-deck-for-a-program-and-install-it-shuffle
    ;; Trash & pay 2 to search deck for a program and install it. Shuffle
    (do-game
      (new-game {:runner {:deck [(qty "ONR Self-Modifying Code" 3) "Reaver"]}})
      (starting-hand state :runner ["ONR Self-Modifying Code" "ONR Self-Modifying Code"])
      (core/gain state :runner :credit 5)
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Self-Modifying Code")
      (play-from-hand state :runner "ONR Self-Modifying Code")
      (let [smc1 (get-program state 0)
            smc2 (get-program state 1)]
        (card-ability state :runner smc1 0)
        (click-prompt state :runner (find-card "Reaver" (:deck (get-runner))))
        (is (= 4 (:credit (get-runner))) "Paid 4 for SMC's, 2 for install - 4 credits left")
        (is (= 1 (core/available-mu state)) "SMC MU refunded")
        (take-credits state :runner)
        (take-credits state :corp)
        (card-ability state :runner smc2 0)
        (is (= 1 (count (:hand (get-runner)))) "1 card drawn due to Reaver before SMC program selection")
        (is (zero? (count (:deck (get-runner)))) "Deck empty"))))

(deftest onr-shredder-protocol-uplink
    ;; Crisium grid on HQ should prevent Gabriel gaining credits
    (do-game
      (new-game {:corp {:deck ["Crisium Grid"]}
                 :runner {:id "Gabriel Santiago: Consummate Professional"
                          :deck ["ONR Shredder Uplink Protocol"]}})
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Shredder Uplink Protocol")
      (is (= 1 (:credit (get-runner))) "Sneakdoor cost 4 credits")
      (let [sb (get-program state 0)
            crisium (get-content state :hq 0)]
        (rez state :corp crisium)
        (card-ability state :runner sb 0)
        (run-continue state)
        (is (= 1 (:credit (get-runner))) "Did not gain 2 credits from Gabe's ability")
        (is (not= :hq (-> (get-runner) :register :successful-run first)) "Successful Run on HQ was not recorded"))))



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
