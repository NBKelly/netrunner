(ns game.cards.onr-hardware-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(defn- provides-memory
  [card x]
  (do-game
    (new-game {:corp {:hand ["City Works Project"]}
               :runner {;;:scored ["Hostile Takeover"] - maybe some day
                        :hand [card]
                        :credits 50}})
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "Steal")
    (core/gain state :runner :click 1)
    (play-from-hand state :runner card)
    (is (= (+ 4 x) (core/available-mu state)) (str "Gain " x " memory"))))

(defn- provides-hand-size
  [card x]
  (do-game
    (new-game {:corp {:hand ["City Works Project"]}
               :runner {;;:scored ["Hostile Takeover"]
                        :credits 50
                        :hand [card]}})
    (take-credits state :corp)
    (run-empty-server state :hq)
    (core/gain state :runner :click 1)
    (click-prompt state :runner "Steal")
    (play-from-hand state :runner card)
    (is (= (+ 5 x) (hand-size :runner)) (str card " provides " x " hand size"))))

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
       (new-game {:corp {:hand ["City Works Project"]}
                  :runner {;;:scored ["Hostile Takeover"]
                           :credits 50
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
         (run-empty-server state :hq)
         (click-prompt state :runner "Steal")
         (core/gain state :runner :click 1)
         (play-from-hand state :runner card)
         (play-from-hand state :runner breaker)
         (when unbreaker
           (play-from-hand state :runner unbreaker))
         (let [card (get-hardware state 0)
               breaker (get-program state 0)
               unbreaker (when unbreaker (get-program state 1))]
           ;; there are x recurring credits
           (is (= x (get-counters (refresh card) :recurring)) (str "There are " x " recurring credits on install"))
           (spends-to-boost breaker 1 1)
           (run-on state :hq)
           (when unbreaker
             (spends-to-boost unbreaker 1 1))
           (dotimes [_ x]
             (recurring-to-boost card breaker 1 1))
           (spends-to-boost breaker 1 1)))))))

(defn- creds-to-remove-tags
  [card x]
  (do-game
    (new-game {:corp {:hand ["City Works Project"]}
               :runner {;;:scored ["Hostile Takeover"]
                        :credits 50
                        :deck [card]}})
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "Steal")
    (core/gain state :runner :click 1)
    (play-from-hand state :runner card)
    (gain-tags state :corp x)
    (let [card (get-hardware state 0)]
      (is (= x (get-counters (refresh card) :recurring)) (str "There are " x " recurring credits on install"))
      (dotimes [_ (quot x 2)]
        (changes-val-macro
          0 (:credit (get-runner))
          (str "Used 2 credits from " (:title card) " to remove a tag")
          (remove-tag state :runner)
          (click-card state :runner card)
          (click-card state :runner card)
          (is (no-prompt? state :runner) "no lingering prompt")))
      (dotimes [_ (mod x 2)]
        (changes-val-macro
          -1 (:credit (get-runner))
          (str "Used 1 credits from " (:title card) " to remove a tag")
          (remove-tag state :runner)
          (click-card state :runner card)
          (is (no-prompt? state :runner) "no lingering prompt")))
      (is (= 0 (get-counters (refresh card) :recurring))
          (str "There are 0 recurring credits left")))))

(defn- trash-prevent-damage
  [card type x ab]
  (do-game
    (new-game {:corp {:hand ["City Works Project"]}
               :runner {;;:scored ["Hostile Takeover"]
                        :credits 50
                        :hand [card (qty "Sure Gamble" (inc x))]}})
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "Steal")
    (core/gain state :runner :click 1)
    (play-from-hand state :runner card)
    (let [card (get-hardware state 0)]
      (damage state :corp type (inc x))
      (card-ability state :runner (refresh card) ab)
      (click-prompt state :runner "Done")
      (is (= [(:title card) "Sure Gamble"] (map :title (:discard (get-runner))))
          "Only 1 damage left"))))

(defn- generic-prevent-damage
  ([card type ab] (generic-prevent-damage card type ab 1))
  ([card type ab x] (generic-prevent-damage card type ab x 1))
  ([card type ab x times]
   (do-game
     (new-game {:corp {:hand ["City Works Project"]}
                :runner {;;:scored ["Hostile Takeover"]
                         :hand [card (qty "Sure Gamble" (* 2 x times))]
                         :credits 50}})
     (take-credits state :corp)
     (run-empty-server state :hq)
     (click-prompt state :runner "Steal")
     (core/gain state :runner :click 1)
     (play-from-hand state :runner card)
     (let [card (get-hardware state 0)]
       (dotimes [z times]
         (damage state :corp type (inc x))
         (card-ability state :runner (refresh card) ab)
         (click-prompt state :runner "Done")
         (is (= (map :title (:discard (get-runner))) (vec (repeat (inc z) "Sure Gamble")))))
       ;; we shouldn't be able to get a prevent prompt anymore
       (damage state :corp type (inc x))
       (is (no-prompt? state :runner) "no prompt because no more prevents!")))))

(defn- costs-agenda-points
  [card x]
  (do-game
    (new-game {:corp {:hand ["City Works Project"]}
               :runner {;;:scored [(qty "Hostile Takeover" (inc x))]
                        :hand [card]
                        :credits 50}})
    (take-credits state :corp)
     (run-empty-server state :hq)
     (click-prompt state :runner "Steal")
     (core/gain state :runner :click 1)
     (changes-val-macro
       (- x) (:agenda-point (get-runner))
       (str "spent " x " agenda points to install " card)
       (play-from-hand state :runner card))))

(defn- avoid-tag
  [card cost ab]
  (do-game
    (new-game {:corp {:hand ["City Works Project"]}
               :runner {;;:scored [(qty "Hostile Takeover" (inc x))]
                        :hand [card]
                        :credits 50}})
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "Steal")
    (core/gain state :runner :click 1)
    (play-from-hand state :runner card)
    (let [card (get-hardware state 0)]
      (dotimes [_ (inc (rand-int 10))]
        (changes-val-macro
          (- cost) (:credit (get-runner))
          (str "spent " cost " to avoid a tag")
          (gain-tags state :corp 1)
          (card-ability state :runner (refresh card) ab)
          (is (= 0 (count-tags state)) "Runner has 0 tags"))))))

(defn- credits-for-trace
  [card x]
  ;; I should be testing these with an onr trace
  ;; but it works for both anyway (haha)
  ;;  -- t. clueless about the problems to come
  (do-game
    (new-game {:corp {:hand ["SEA Source"]
                      :discard ["City Works Project"]}
               :runner {:credits 50 :hand [card]}})
    (take-credits state :corp)
    (play-from-hand state :runner card)
    (run-empty-server state :archives)
    (click-prompt state :runner "Steal")
    (let [card (get-hardware state 0)]
      (is (= x (get-counters (refresh card) :recurring)) (str "There are " x " recurring credits on install"))
      (take-credits state :runner)
      (play-from-hand state :corp "SEA Source")
      (is (= :trace (prompt-type :corp)) "Corp should initiate a trace")
      (click-prompt state :corp "0")
      (click-prompt state :runner (str (inc x)))
      (changes-val-macro
          -1 (:credit (get-runner))
          "spent 1 real on trace"
          (dotimes [_ x]
            (click-card state :runner (refresh card))))
      (is (= 0 (get-counters (refresh card) :recurring)) (str "There are " 0 " recurring credits leftover")))))

;; creds-during-run       | card x type untype
;; provides-memory        | card x
;; provides-hand-size     | card x
;; trash-prevent-damage   | card type x ability
;; creds-to-remove-tags   | card x
;; generic-prevent-damage | card type ab x times
;; costs-agenda-points  | card x
;;; tests below here

(deftest onr-artemis-2020
  (let [card "ONR Artemis 2020"]
    (provides-memory card 2)
    (creds-during-run card 2)))

(deftest onr-armadillo-armored-road-home
  (let [card "ONR \"Armadillo\" Armored Road Home"]
    (creds-to-remove-tags card 2)
    (trash-prevent-damage card :meat 3 0)))

(deftest onr-drifter-mobile-environment
  (let [card "ONR \"Drifter\" Mobile Environment"]
    (creds-to-remove-tags card 2)))

(deftest onr-green-knight-surge-buffers
  (let [card "ONR \"Green Knight\" Surge Buffers"]
    (generic-prevent-damage card :net 0)))

(deftest onr-arasaka-portable-prototype
  (let [card "ONR Arasaka Portable Prototype"]
    (provides-memory card 3)
    (creds-during-run card 3)
    (costs-agenda-points card 1)))

(deftest onr-armored-fridge
  ;; Plascrete Carapace - Prevent meat damage
  (do-game
    (new-game {:corp {:deck ["Scorched Earth"]}
               :runner {:deck ["ONR Armored Fridge" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Armored Fridge")
    (let [plas (get-hardware state 0)]
      (is (= 7 (get-counters (refresh plas) :power)) "7 counters on install")
      (take-credits state :runner)
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Scorched Earth")
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (is (= 3 (get-counters (refresh plas) :power)) "3 counters left!"))))

(deftest bodyweight-data-creche
  ;; Doppelgänger - run again when successful
  (do-game
    (new-game {:runner {:deck ["ONR Bodyweight [TM] Data Crèche"]}})
    (core/gain state :corp :bad-publicity 1)
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Bodyweight [TM] Data Crèche")
    (run-empty-server state :hq)
    (click-prompt state :runner "No action")
    (is (zero? (:run-credit (get-runner))) "Runner lost BP credits")
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "R&D")
    (is (:run @state) "New run started")
    (is (= [:rd] (:server (:run @state))) "Running on R&D")
    (is (= 1 (:run-credit (get-runner))) "Runner has 1 BP credit")))

(deftest onr-corolla-speed-chip
  ;; creds-during-run       | card x type untype
  (let [card "ONR Corolla Speed Chip"]
    (creds-during-run card 1 "Killer")))

(deftest onr-cortical-cybermodem
  (let [card "ONR Cortical Cybermodem"]
    (provides-memory card 2)
    (provides-hand-size card 2)
    (creds-during-run card 2)))

(deftest onr-cortical-stimulators
  (let [card "ONR Cortical Stimulators"]
    (generic-prevent-damage card :net 0)
    (generic-prevent-damage card :brain 0)))

(deftest onr-dermatech-bodyplating
  (let [card "ONR Dermatech Bodyplating"]
    (generic-prevent-damage card :meat 0)))

(deftest onr-eurocorpse-spin-chip-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:credits 10
                          :hand ["ONR Eurocorpse [TM] Spin Chip" "Inti"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Eurocorpse [TM] Spin Chip")
      (let [omni (get-hardware state 0)]
          (card-ability state :runner omni 0)
          (click-card state :runner (find-card "Inti" (:hand (get-runner))))
          (let [inti (first (:hosted (refresh omni)))]
            (changes-val-macro 0 (:credit (get-runner))
                               "Used 2 credits from Omni-drive"
                               (card-ability state :runner inti 1)
                               (click-card state :runner omni)
                               (click-card state :runner omni))))))

;; todo - lifesaver nanosurgeons

(deftest onr-little-black-box
  (let [card "ONR Little Black Box"]
    (generic-prevent-damage card :net 0)
    (generic-prevent-damage card :brain 0)
    (provides-memory card 1)
    (provides-hand-size card 1)
    (credits-for-trace card 1)))

(deftest lucidrine-drip-feed
  ;; Stim Dealer - Take 1 brain damage when it accumulates 2 power counters
  (do-game
    (new-game {:runner {:credits 10
                        :deck ["ONR Lucidrine [TM] Drip Feed" "Sure Gamble" "Feedback Filter"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Feedback Filter")
    (play-from-hand state :runner "ONR Lucidrine [TM] Drip Feed")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [sd (get-hardware state 1)]
      (is (= 1 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-runner))) "Gained 1 click")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-runner))) "Gained 1 click")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (zero? (get-counters (refresh sd) :power)) "Lost all counters")
      (is (no-prompt? state :runner) "No Feedback Filter brain dmg prevention possible")
      (is (= 1 (:brain-damage (get-runner))) "Took 1 core damage")
      (is (= 4 (:click (get-runner))) "Didn't gain extra click"))))

(deftest onr-mram-chip
  (provides-hand-size "ONR MRAM Chip" 2))

;; todo - microtech trode set

;; todo - microtech backup drive

(deftest onr-militech-mram-chip
  (provides-hand-size "ONR Militech MRAM Chip" 3))

(deftest onr-nasuko-cycle
  (let [card "ONR Nasuko Cycle"]
    (avoid-tag card 3 0)))

;; todo - omnitech spinal tap, once I figure out how to fix rng

;; todo - omnitech wet drive

(deftest onr-pk-6089a
  (let [card "ONR PK-6089a"]
    (provides-memory card 1)
    (credits-for-trace card 3)))

(deftest onr-pandoras-deck
  (let [card "ONR Pandora's Deck"]
    (provides-memory card 2)
    (credits-for-trace card 3)))

(deftest onr-parraline-5750
  (let [card "ONR Parraline 5750"]
    (provides-memory card 1)
    (creds-during-run card 1)))

(deftest onr-raven-microcyb-eagle
  (let [card "ONR Raven Microcyb Eagle"]
    (provides-memory card 1)
    (creds-during-run card 1)
    (generic-prevent-damage card :net 0)))

(deftest onr-raven-microcyb-owl
  (let [card "ONR Raven Microcyb Owl"]
    (provides-memory card 1)
    (creds-during-run card 3 "Fracter" "Noisy")))

;; todo - record reconstructor

(deftest onr-sunburst-cranial-interface
  (let [card "ONR Sunburst Cranial Interface"]
    (provides-memory card 1)
    (provides-hand-size card 1)
    (creds-during-run card 1 "Fracter" "Noisy")))

(deftest onr-techtronica-tm-utility-suit
  (let [card "ONR Techtronica [TM] Utility Suit"]
    (provides-memory card 1)
    (generic-prevent-damage card :meat 0)
    (credits-for-trace card 5)))

(deftest onr-the-deck
  ;; Trace 6 - Give the runner 1 tag for each point your trace exceeded their link
  (provides-memory "ONR The Deck" 1)
  (do-game
    (new-game {:corp {:deck ["ONR Manhunt"]}
               :runner {:hand ["ONR The Deck" (qty "Sure Gamble" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "ONR The Deck")
    (run-empty-server state :rd)
    (take-credits state :runner)
    ;; 7 - 4 means that our max trace is 3
    ;; the runner should also have 6 credits
    (play-from-hand state :corp "ONR Manhunt")
    (click-prompt state :corp "3")
    (click-prompt state :runner "ONR The Deck")
    (changes-val-macro
      -0 (:credit (get-runner))
      "spent 0c to boost link to 5"
      (click-prompt state :runner "0 [Credits]: Base Link 5"))
    (changes-val-macro
      -1 (:credit (get-runner))
      "spent 1c to boost link to 6"
      (click-prompt state :runner "1 [Credits]: +1 Link"))
    (click-prompt state :runner "Done")
    (is (= 0 (count-tags state)) "Runner should have 0 tags")))

(deftest onr-tycho-mem-chip
  (provides-memory "ONR Tycho Mem Chip" 3))

(deftest onr-vintage-camaro
    (let [card "ONR Vintage Camaro"]
      (avoid-tag card 1 0)))

(deftest onr-wutech-mem-chip
  (provides-memory "ONR WuTech Mem Chip" 1))

(deftest onr-zz22-speed-checp
  ;; creds-during-run       | card x type untype
  (let [card "ONR ZZ22 Speed Chip"]
    (creds-during-run card 2 "Killer")))

(deftest onr-zetatech-mem-chip
  (provides-memory "ONR Zetatech Mem Chip" 2))

(deftest zetatech-portastation-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["ONR Zetatech Portastation" "Dirty Laundry"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "ONR Zetatech Portastation")
      (let [ppvp (get-hardware state 0)]
        (changes-val-macro -1 (:credit (get-runner))
                           "Used 1 credit from "
                           (play-from-hand state :runner "Dirty Laundry")
                           (click-card state :runner ppvp)))))
