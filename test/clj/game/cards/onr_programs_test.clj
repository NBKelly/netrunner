(ns game.cards.onr-programs-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.macros :refer [req]]
            [game.utils :as utils]
            [game.core.ice :refer [add-sub! pump-ice]]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; test utilities

(defn- dismiss-install-prompt [state]
  (when-not (no-prompt? state :runner)
    (click-prompt state :runner "Done")))

(defn- creds-during-run
  ([card] (creds-during-run card 1))
  ([card x] (creds-during-run card x "Fracter"))
  ([card x type] (creds-during-run card x type nil))
  ([card x type unbreaker]
   (let [breaker (cond
                   (= type "Fracter") "Corroder"
                   (= type "Killer") "Bukhgalter"
                   (= type "Noisy") "ONR Jackhammer"
                   (= type "Decoder") "Unity"
                   :else "Corroder")
         unbreaker (cond
                   (= unbreaker "Fracter") "Unity"
                   (= unbreaker "Killer") "Corroder"
                   (= unbreaker "Noisy") "ONR Jackhammer"
                   (= unbreaker "Decoder") "Bukhgalter"
                   :else nil)]
     (do-game
       (new-game {:runner {:credits 50
                           :hand [card breaker unbreaker]}})
       ;; these need to be defined locally
       ;; I guess we only do it once haha
       (letfn [(spends-to-boost
                 [b ab y] (changes-val-macro
                            (- y) (:credit (get-runner))
                            (str "Spent " y " to boost " (:title b))
                            ;; -
                            (card-ability state :runner (refresh b) ab)
                            (is (no-prompt? state :runner) "No floating prompts")))
               (recurring-to-boost
                 [c b ab y] (changes-val-macro
                              0 (:credit (get-runner))
                              (str "Spent " y " to boost " (:title b))
                              ;; -
                              (card-ability state :runner (refresh b) ab)
                              (dotimes [_ y]
                                (click-card state :runner (refresh c)))
                              (is (no-prompt? state :runner) "No floating prompts")))]
         (take-credits state :corp)
         (play-from-hand state :runner card)
         (play-from-hand state :runner breaker)
         (when unbreaker
           (play-from-hand state :runner unbreaker))
         (let [card (get-program state 0)
               breaker (get-program state 1)
               unbreaker (when unbreaker (get-program state 2))]
           ;; there are x recurring credits
           (is (= x (get-counters (refresh card) :recurring)) (str "There are " x " recurring credits on install"))
           (spends-to-boost breaker 1 1)
           (run-on state :hq)
           (when unbreaker
             (spends-to-boost unbreaker 1 1))
           (dotimes [_ x]
             (recurring-to-boost card breaker 1 1))
           (spends-to-boost breaker 1 1)))))))

(defn- basic-program-test
  "tests a program which has basic boost and break functionality"
  [card base-str boost break]
  (let [type (:type break)
        type (condp = type
               "Sentry"     "ONR Banpei"
               "All"        "Rime"
               "Wall"       "ONR Data Wall"
               "Code Gate"  "Enigma"
               "AP"         "Anansi"
               "Barrier"    "Vanilla"
               ;; todo - whatever other types need to be tested
               "Hellhound"  "ONR Baskerville"
               "Watchdog"   "ONR Canis Major"
               "Pit Bull"   "ONR Fang"
               "Bloodhound" "ONR Fetch 4.0.1"
               type)]
    (do-game
      (new-game {:corp {:credits 100
                        :hand ["Mother Goddess" type]}
                 :runner {:credits 100
                          :hand [card]}})
      (play-from-hand state :corp type "Archives")
      (play-from-hand state :corp "Mother Goddess" "HQ")
      (rez state :corp (get-ice state :archives 0))
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner card)
      (dismiss-install-prompt state)
      (let [card (get-program state 0)
            ice (get-ice state :hq 0)
            base-breaker-str (cond
                               (integer? base-str) base-str
                               (= :rand-zero base-str) 0
                               :else nil)]
        (is (= base-breaker-str (get-strength (refresh card)))
            (str (:title card) "starts at base strength " base-str))
        (run-on state :hq)
        (run-continue state :encounter-ice)

        (if boost
          (do
            (pump-ice state :corp (refresh ice) (+ 7 (rand-int 7)))
            ;; how many times should we need to boost?
            (let [base-str (get-strength (refresh card))
                  need-to-boost (- (get-strength (refresh ice)) base-str)
                  boost-strength (:amount boost)
                  times-to-boost (if-not (pos? need-to-boost)
                                   0 (int (Math/ceil(/ need-to-boost boost-strength))))]
              (dotimes [_ times-to-boost]
                (changes-val-macro
                  boost-strength (get-strength (refresh card))
                  (str (:title card) " was boosted by " boost-strength)
                  (changes-val-macro
                    (- (:cost boost)) (:credit (get-runner))
                    (str (:title card) " spends " (:cost boost) " to boost strength")
                    (card-ability state :runner (refresh card) (:ab boost)))))
              (is (>= (get-strength (refresh card))
                      (get-strength (refresh ice))) "At strength to break MOGO")))
          ;; fixed strength breakers should instead have the ice lowered to them
          (do (let [str-diff (- (get-strength (refresh card)) (get-strength (refresh ice)))]
                (pump-ice state :corp (refresh ice) str-diff)
                (is (= (get-strength (refresh card)) (get-strength (refresh ice))) "MOGO and breaker at same str"))))
        (let [addl-subs (+ 3 (rand-int 10))
              total-subs (inc addl-subs)
              ;; we're going to insert a random number of ETR subs
              etr-sub {:label "End the run"
                       :msg "end the run"
                       ;; don't need to actually do anything!
                       :async true}]
          (dotimes [_ addl-subs]
            (add-sub! state :corp (refresh ice) etr-sub))
          (is (= total-subs (count (:subroutines (refresh ice))))
              (str "gained " addl-subs " ice subroutines"))
          (let [subs-per-break (:amount break)
                num-breaks (int (Math/ceil (/ total-subs subs-per-break)))
                last-break (mod total-subs subs-per-break)
                last-break (if (zero? last-break) subs-per-break last-break)]
            (card-ability state :runner (refresh card) (:ab break))
            (dotimes [n num-breaks]
              (let [breaks-this-time (if (= num-breaks (inc n)) last-break subs-per-break)]
                (changes-val-macro
                  (- (:cost break)) (:credit (get-runner))
                  (str "Spent " (:cost break) " credits to break subroutines with " (:title card))
                  (dotimes [z breaks-this-time]
                    (click-prompt state :runner "End the run")))))
            (is (zero? (count (remove :broken (:subroutines (refresh ice))))) "All subroutines have been broken")))))))

;; todo - game the rng sometimes for tests
;; (def ^:dynamic *rand* clojure.core/rand)

;; (defn rand-1
;;  ([]
;;    (*rand* 1))
;;  ([n]
;;    (*rand* n)))

;; (defmacro with-rand-seed
;;  "Sets seed for calls to random in body. Beware of lazy seqs!"
;;  [seed & body]
;;   `(let [g# (java.util.Random. ~seed)]
;;      (binding [*rand* #(* % (.nextFloat g#))]
;;       (with-redefs [rand rand-1]
;;         ~@body))))


;; test impls

(deftest onr-ai-boon
  (let [card "ONR AI Boon"]
    ;; this is a random breaker, run the test a few times
    (basic-program-test card :rand-zero {:ab 1 :amount 1 :cost 1}
                        {:ab 0 :amount 1 :cost 1 :type "Sentry"})
    (basic-program-test card :rand-zero {:ab 1 :amount 1 :cost 1}
                        {:ab 0 :amount 1 :cost 1 :type "Sentry"})
    (basic-program-test card :rand-zero {:ab 1 :amount 1 :cost 1}
                        {:ab 0 :amount 1 :cost 1 :type "Sentry"})
    (basic-program-test card :rand-zero {:ab 1 :amount 1 :cost 1}
                        {:ab 0 :amount 1 :cost 1 :type "Sentry"})
    (basic-program-test card :rand-zero {:ab 1 :amount 1 :cost 1}
                        {:ab 0 :amount 1 :cost 1 :type "Sentry"})
    (basic-program-test card :rand-zero {:ab 1 :amount 1 :cost 1}
                        {:ab 0 :amount 1 :cost 1 :type "Sentry"})))

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

(deftest onr-bartmoss-memorial-icebreaker
  (let [card "ONR Bartmoss Memorial Icebreaker"]
    (basic-program-test card 0 {:ab 1 :amount 1 :cost 1} {:ab 0 :amount 1 :cost 1 :type "All"})))

(deftest onr-big-frackin-gun
  (let [card "ONR Big Frackin' Gun"]
    (basic-program-test card
                        7
                        {:ab 1 :amount 1 :cost 1}
                        {:ab 0 :amount 5 :cost 6 :type "Sentry"})))

(deftest onr-black-dahlia
  (let [card "ONR Black Dahlia"]
    (basic-program-test card
                        5
                        {:ab 1 :amount 1 :cost 2}
                        {:ab 0 :amount 1 :cost 2 :type "Sentry"})))

(deftest onr-black-widow
  (let [card "ONR Black Widow"]
    (basic-program-test card
                        2
                        {:ab 1 :amount 1 :cost 2} ;; boost
                        {:ab 0 :amount 1 :cost 1 :type "Sentry"})))

(deftest onr-boring-bit
  (let [card "ONR Boring Bit"]
    (basic-program-test card
                        5
                        {:ab 1 :amount 1 :cost 1} ;; boost
                        {:ab 0 :amount 1 :cost 2 :type "Wall"})))

(deftest onr-bulldozer
  (let [card "ONR Bulldozer"]
    (basic-program-test card
                        4
                        {:ab 1 :amount 1 :cost 2} ;; boost
                        {:ab 0 :amount 1 :cost 1 :type "Wall"})))

(deftest onr-cloak
  (let [card "ONR Cloak"]
    (creds-during-run card 3 "Fracter" "Noisy")))

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

(deftest onr-codecracker
  (let [card "ONR Codecracker"]
    (basic-program-test card
                        0
                        {:ab 1 :amount 1 :cost 1} ;; boost
                        {:ab 0 :amount 1 :cost 0 :type "Code Gate"})))

(deftest onr-codeslinger
  (let [card "ONR Codeslinger"]
    (basic-program-test card
                        3
                        nil
                        {:ab 0 :amount 1 :cost 1 :type "Sentry"})))

(deftest onr-corrosion
  (let [card "ONR Corrosion"]
    (basic-program-test card
                        0
                        {:ab 1 :amount 1 :cost 1} ;; boost
                        {:ab 0 :amount 1 :cost 0 :type "Wall"})))

(deftest onr-cyfermaster
  (let [card "ONR Cyfermaster"]
    (basic-program-test card
                        5
                        {:ab 1 :amount 1 :cost 1} ;; boost
                        {:ab 0 :amount 1 :cost 2 :type "Code Gate"})))

(deftest onr-dogcatcher
  (let [card "ONR Dogcatcher"]
    (basic-program-test card 3 {:ab 4 :amount 1 :cost 1} {:ab 0 :amount 1 :cost 1 :type "Hellhound"})
    (basic-program-test card 3 {:ab 4 :amount 1 :cost 1}{:ab 1 :amount 1 :cost 1 :type "Bloodhound"})
    (basic-program-test card 3 {:ab 4 :amount 1 :cost 1}{:ab 2 :amount 1 :cost 1 :type "Watchdog"})
    (basic-program-test card 3 {:ab 4 :amount 1 :cost 1}{:ab 3 :amount 1 :cost 1 :type "Pit Bull"})))

(deftest onr-dupre
  (let [card "ONR Dupré"]
    (basic-program-test card
                        0
                        {:ab 1 :amount 1 :cost 2} ;; boost
                        {:ab 0 :amount 1 :cost 1 :type "Code Gate"})))


(deftest onr-flak
  (let [card "ONR Early Worm"]
    (basic-program-test card
                        2
                        {:ab 1 :amount 2 :cost 3}
                        {:ab 0 :amount 1 :cost 1 :type "Wall"})))

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

(deftest onr-flak
  (let [card "ONR Flak"]
    (basic-program-test card
                        2
                        {:ab 1 :amount 1 :cost 1}
                        {:ab 0 :amount 1 :cost 1 :type "AP"})))

(deftest onr-japanese-water-torture
    (let [card "ONR Japanese Water Torture"]
    (basic-program-test card
                        2
                        {:ab 1 :amount 1 :cost 1} ;; boost
                        {:ab 0 :amount 1 :cost 0 :type "Wall"})))

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

(deftest onr-superglue
  ;; Crescentus should only work on rezzed ice
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["ONR Superglue" "Corroder"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "ONR Superglue")
    (run-on state "HQ")
    (let [cor (get-program state 0)
          cres (get-program state 1)
          iw (get-ice state :hq 0)]
      (rez state :corp iw)
      (run-continue state)
      (is (rezzed? (refresh iw)) "Ice Wall is now rezzed")
      (card-ability state :runner cor 0)
      (click-prompt state :runner "End the run")
      (card-ability state :runner cres 0)
      (is (nil? (get-program state 1)) "ONR Superglue could be used because the piece of ice is rezzed")
      (is (not (rezzed? (refresh iw))) "Ice Wall is no longer rezzed"))))

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
