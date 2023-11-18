(ns game.cards.onr-ice-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(defn dismiss-rez-prompt [state]
  (when-not (no-prompt? state :corp)
    (click-prompt state :corp "No")))

(defn dismiss-rez [state side card]
  (rez state side card)
  (dismiss-rez-prompt state))

(defn- trivial-etr
  ([name] (trivial-etr name 0))
  ([name sub]
   (do-game
     (new-game {:corp {:credits 20
                       :hand [name]}})
     (play-from-hand state :corp name "New remote")
     (let [card (get-ice state :remote1 0)]
       (dismiss-rez state :corp card)
       (take-credits state :corp)
       (run-on state :remote1)
       (run-continue state)
       (card-subroutine state :corp card sub)
       (is (nil? (:run @state)))))))

(defn- trivial-trash-program
  ([name] (trivial-trash-program name 0))
  ([name sub]
   (do-game
     (new-game {:corp {:credits 20
                       :hand [name]}
                :runner {:hand ["ONR Jackhammer"]}})
     (play-from-hand state :corp name "New remote")
     (take-credits state :corp)
     (play-from-hand state :runner "ONR Jackhammer")
     (let [card (get-ice state :remote1 0)
           jh (get-program state 0)]
       (dismiss-rez state :corp card)
       (run-on state :remote1)
       (run-continue state)
       (card-subroutine state :corp card sub)
       (click-card state :corp jh)
       (is (= 1 (count (:discard (get-runner)))) "program trashed")))))

(defn- trivial-brain-damage
  ([x name] (trivial-brain-damage x name 0))
  ([x name sub]
   (do-game
     (new-game {:corp {:credits 20
                       :hand [name]}
                :runner {:hand [(qty "ONR Networking" (inc x))]}})
     (play-from-hand state :corp name "New remote")
     (take-credits state :corp)
     (let [card (get-ice state :remote1 0)]
       (dismiss-rez state :corp card)
       (run-on state :remote1)
       (run-continue state)
       (card-subroutine state :corp card sub)
       (is (= x (:brain-damage (get-runner))) (str "Runner took " x " brain damage"))))))

(defn- trivial-damage
  ([x name] (trivial-damage x name 0))
  ([x name sub]
   (do-game
     (new-game {:corp {:credits 20
                       :hand [name]}
                :runner {:hand [(qty "ONR Networking" (inc x))]}})
     (play-from-hand state :corp name "New remote")
     (take-credits state :corp)
     (let [card (get-ice state :remote1 0)]
       (dismiss-rez state :corp card)
       (run-on state :remote1)
       (run-continue state)
       (card-subroutine state :corp card sub)
       (is (= x (count (:discard (get-runner)))))))))

(defn- gain-x-on-rez
  ([x name]
   (do-game
     (new-game {:corp {:credits 20 :hand [name]}})
     (play-from-hand state :corp name "New remote")
     (let [card (get-ice state :remote1 0)
           expected-change (- x (:cost card))]
       (changes-val-macro
         expected-change (:credit (get-corp))
         (str "Expected to end gain " x " credits on rez (net change " expected-change ")")
         (dismiss-rez state :corp card))))))

(defn- pay-x-or-etr-sub
  ([x name] (pay-x-or-etr-sub x name 0))
  ([x name sub]
   (do-game
     (new-game {:corp {:credits 20 :hand [name]}
                :runner {:credits (* 3 x)}})
     (play-from-hand state :corp name "New remote")
     (take-credits state :corp)
     (let [card (get-ice state :remote1 0)]
       (dismiss-rez state :corp card)
       (run-on state :remote1)
       (run-continue state)
       (card-subroutine state :corp card sub)
       (changes-val-macro
         (- x) (:credit (get-runner))
         (str "Paid " x "c for subroutine")
         (click-prompt state :runner (str "Pay " x " [Credits]")))
       (card-subroutine state :corp card sub)
       (click-prompt state :runner "End the run")
       (is (nil? (:run @state)))))))

(defn- pay-x-or-bounce-on-pass
  ([x name]
   (do-game
     (new-game {:corp {:credits (+ 20 (* 3 x)) :hand [name]}})
     (play-from-hand state :corp name "New remote")
     (take-credits state :corp)
     (let [card (get-ice state :remote1 0)]
       (dismiss-rez state :corp card)
       (run-on state :remote1)
       (run-continue state)
       (run-continue state :pass-ice)
       (changes-val-macro
         (- x) (:credit (get-corp))
         (str "paid x to keep " (:title name) " on the field")
         (click-prompt state :corp "Yes")
         (is (= 0 (count (:hand (get-corp)))) "card not added to hand"))
       (run-jack-out state)
       (run-on state :remote1)
       (run-continue state)
       (run-continue state :pass-ice)
       (changes-val-macro
         0 (:credit (get-corp))
         (str "paid 0, bounced card to hand ")
         (click-prompt state :corp "No")
         (is (= 1 (count (:hand (get-corp)))) "card added to hand")))
     (take-credits state :runner)
     (play-from-hand state :corp name "New remote")
     (take-credits state :corp)
     ;; test it when we cannot afford to pay
     (let [card (get-ice state :remote2 0)]
       (dismiss-rez state :corp card)
       (run-on state :remote2)
       (run-continue state)
       (core/gain state :corp :credit (- (:credit (get-corp))))
       (is (= 0 (:credit (get-corp))) "corp is poor")
       (changes-val-macro
         0 (:credit (get-corp))
         (str "paid 0, bounced card to hand ")
       (run-continue state :pass-ice)
         (click-prompt state :corp "No")
         (is (= 1 (count (:hand (get-corp)))) "card added to hand"))))))

(defn- gain-x-and-bounce-on-pass
  ([x name]
   (do-game
     (new-game {:corp {:credits (+ 20 (* 3 x)) :hand [name]}})
     (play-from-hand state :corp name "New remote")
     (take-credits state :corp)
     (let [card (get-ice state :remote1 0)]
       (dismiss-rez state :corp card)
       (run-on state :remote1)
       (run-continue state)
       (run-continue state :pass-ice)
       (changes-val-macro
         0 (:credit (get-corp)) "No change"
         (click-prompt state :corp "No")
         (is (= 0 (count (:hand (get-corp)))) "card not added to hand"))
       (run-jack-out state)
       (run-on state :remote1)
       (run-continue state)
       (run-continue state :pass-ice)
       (changes-val-macro
         x (:credit (get-corp))
         (str "gain " x " to bounce paid card to hand")
         (click-prompt state :corp "Yes")
         (is (= 1 (count (:hand (get-corp)))) "card added to hand"))))))

(defn- noisy-breaker-used-discount
  [x name]
  (do-game
    (new-game {:corp {:hand [name]
                      :credits 40}
               :runner {:hand ["ONR Jackhammer"]}})
    (play-from-hand state :corp name "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "ONR Jackhammer")
    (run-on state :hq)
    (let [card (get-ice state :hq 0)
          rez-cost (:cost (refresh card))
          jack (get-program state 0)
          discounted-cost (- rez-cost x)]
      (changes-val-macro
        (- rez-cost) (:credit (get-corp))
        "Normal rez cost"
        (dismiss-rez state :corp (refresh card)))
      (derez state :corp (refresh card))
      ;; boost by 1 - we've now used a noisy breaker
      (card-ability state :runner (refresh jack) 1)
      (changes-val-macro
        (- discounted-cost) (:credit (get-corp))
        "Normal rez cost"
        (dismiss-rez state :corp (refresh card))))))

(defn- cannot-break-next-ice
  ([name] (cannot-break-next-ice name 0))
  ([name sub]
   ;; Cannot break subroutines of next piece of ice
   (do-game
     (new-game {:corp {:hand [name (qty "ONR Data Wall" 2)]
                       :credits 30}
                :runner {:hand ["ONR Jackhammer"]
                         :credits 20}})
     (play-from-hand state :corp "ONR Data Wall" "HQ")
     (play-from-hand state :corp "ONR Data Wall" "HQ")
     (play-from-hand state :corp name "HQ")
     (take-credits state :corp)
     (play-from-hand state :runner "ONR Jackhammer")
     (let [card (get-ice state :hq 2)
           wall1 (get-ice state :hq 1)
           wall2 (get-ice state :hq 0)
           breaker  (get-program state 0)]
       (run-on state "HQ")
       (dismiss-rez state :corp card)
       (run-continue state)
       (card-subroutine state :corp card sub)
       (run-continue-until state :approach-ice wall1)
       (dismiss-rez state :corp wall1)
       (run-continue state)
       ;; Inazuma subs prevented break on next piece of ice
       (card-ability state :runner breaker "Break 1 Wall subroutine")
       (is (no-prompt? state :runner) " can't break so no prompt")
       ;; Next piece of ice is fine to break again
       (run-continue-until state :approach-ice wall2)
       (dismiss-rez state :corp wall2)
       (run-continue state)
       (card-ability state :runner breaker "Break 1 Wall subroutine")
       (click-prompt state :runner "End the run")
       (is (empty? (remove :broken (:subroutines (refresh wall2)))) "All subroutines broken"))))
  )

(defn- changes-subtype-rez
  [x from to name]
  (do-game
    (new-game {:corp {:hand [name]
                      :credits 50}})
    (play-from-hand state :corp name "HQ")
    (let [card (get-ice state :hq 0)]
      (rez state :corp card)
      (is (has-subtype? (refresh card) from) "no change")
      (is (not (has-subtype? (refresh card) to)) "no change")
      (changes-val-macro
        (- x) (:credit (get-corp))
        "spent x to swap types"
        (click-prompt state :corp "Yes"))
      (is (has-subtype? (refresh card) to) (str "swapped to " to))
      (is (not (has-subtype? (refresh card) from)) (str "swapped from" from))
      (is (no-prompt? state :corp))
      (derez state :corp (refresh card))
      (rez state :corp (refresh card))
      (is (has-subtype? (refresh card) from) "no change")
      (is (not (has-subtype? (refresh card) to)) "no change")
      (changes-val-macro
        0 (:credit (get-corp))
        "did not swap"
        (click-prompt state :corp "No"))
      (is (has-subtype? (refresh card) from) "no change")
      (is (not (has-subtype? (refresh card) to)) "no change"))))

(defn- trace-tag
  ([x name] (trace-tag x name 0))
  ([x name sub]
   (do-game
     (new-game {:corp {:credits 20
                       :hand [name]}
                :runner {:hand [(qty "ONR Networking" (inc x))]}})
     (play-from-hand state :corp name "New remote")
     (take-credits state :corp)
     (let [card (get-ice state :remote1 0)]
       (dismiss-rez state :corp card)
       (run-on state :remote1)
       (run-continue state)
       (card-subroutine state :corp card sub)
       (click-prompt state :corp "0")
       (is (= x (count-tags state)) (str "runner gained " x " tags"))))))

(defn- trace-net
  ([x name] (trace-net x name 0))
  ([x name sub]
   (do-game
     (new-game {:corp {:credits 20
                       :hand [name]}
                :runner {:hand [(qty "ONR Networking" (inc x))]}})
     (play-from-hand state :corp name "New remote")
     (take-credits state :corp)
     (let [card (get-ice state :remote1 0)]
       (dismiss-rez state :corp card)
       (run-on state :remote1)
       (run-continue state)
       (card-subroutine state :corp card sub)
       (click-prompt state :corp "0")
       (is (= x (count (:discard (get-runner)))) (str "runner gained " x " tags"))))))

(defn- canis-effect
  ([x name] (canis-effect x name 0))
  ([x name sub]
   (do-game
     (new-game {:corp {:credits 30
                       :hand [name "Vanilla"]}})
     (play-from-hand state :corp "Vanilla" "HQ")
     (play-from-hand state :corp name "HQ")
     (take-credits state :corp)
     (run-on state "HQ")
     (let [canis (get-ice state :hq 1)
           vanil (get-ice state :hq 0)]
       (rez state :corp canis)
       (rez state :corp vanil)
       (run-continue state)
       (card-subroutine state :corp (refresh canis) sub)
       (is (= 0 (:strength (refresh vanil))))
       (run-continue state)
       (run-continue state)
       (run-continue state :encounter-ice)
       (is (= x (core/get-strength (refresh vanil))))))))

(defn- purchase-subroutine-on-rez
  [name sub cost]
  (do-game
    (new-game {:corp {:credits 50
                      :hand [name]}})
    (play-from-hand state :corp name "HQ")
    (let [ice (get-ice state :hq 0)]
      (rez state :corp ice)
      (dotimes [n (inc (rand-int 5))]
        (changes-val-macro
          (- cost) (:credit (get-corp))
          (str "spent " cost " to buy a '" sub "' subroutine")
          (click-prompt state :corp "Yes")
          (is (= sub (:label (last (:subroutines (refresh ice))))) "Sub correct")
          (is (= (inc n) (count (:subroutines (refresh ice)))) "right number of subs"))))))


;; trivial-etr
;; trivial-trash-program
;; trivial-brain-damage
;; trivial-damage
;; gain-x-on-rez
;; pay-x-or-etr-sub
;; pay-x-or-bounce-on-pass
;; noisy-breaker-used-discount
;; cannot-break-next-ice
;; changes-subtype-rez (x from to name)
;; gain-x-and-bounce-on-pass
;; trace-tag
;; trace-net
;; canis-effect

;; tests here

(deftest ^:kaocha/pending onr-asp
  )

(deftest ^:kaocha/pending onr-ball-and-chain
  )

(deftest onr-banpei-etr
  (trivial-trash-program "ONR Banpei" 0)
  (trivial-etr "ONR Banpei" 1))

(deftest ^:kaocha/pending onr-baskerville
  (trivial-damage 2 "ONR Baskerville")
  ;; trace
  (trivial-etr "ONR Baskerville" 2)
  (noisy-breaker-used-discount 5 "ONR Baskerville"))

(deftest onr-brain-wash
  (trivial-brain-damage 1 "ONR Brain Wash"))

(deftest ^:kaocha/pending onr-brain-drain
  ;; control rng for this one somehow
  )

(deftest onr-bolter-cluster
  (trivial-damage 4 "ONR Bolter Cluster")
  (cannot-break-next-ice "ONR Bolter Cluster" 1))

(deftest onr-bolter-swarm
  (noisy-breaker-used-discount 5 "ONR Bolter Swarm")
  (trivial-damage 4 "ONR Bolter Swarm")
  (cannot-break-next-ice "ONR Bolter Swarm" 1))

(deftest ^:kaocha/pending onr-bug-zapper
  ;; 2 net for each ice outside this one
  (trivial-etr "ONR Bug Zapper" 1))

(deftest onr-canis-major
  (canis-effect 2 "ONR Canis Major"))

(deftest onr-canis-minor
  (canis-effect 1 "ONR Canis Minor"))

(deftest onr-caryatid
  (trivial-etr "ONR Caryatid")
  (changes-subtype-rez 1 "Wall" "Code Gate" "ONR Caryatid"))

(deftest ^:kaocha/pending onr-cerberus
  (trivial-damage 3 "ONR Cerberus")
  ;; cerberus counter
  (trivial-etr "ONR Cerberus" 2))

(deftest onr-chihuahua
  (gain-x-on-rez 2 "ONR Chihuahua")
  (trace-net 1 "ONR Chihuahua"))

(deftest ^:kaocha/pending onr-cinderella
  ;; trace: etr, hardware, x meat
  )

(deftest onr-code-corpse
  (trivial-brain-damage 1 "ONR Code Corpse" 0)
  (trivial-brain-damage 1 "ONR Code Corpse" 1)
  (trivial-etr "ONR Code Corpse" 2))

(deftest onr-colonel-failure
  (trivial-trash-program "ONR Colonel Failure" 0)
  (trivial-trash-program "ONR Colonel Failure" 1)
  (trivial-trash-program "ONR Colonel Failure" 2)
  (trivial-etr "ONR Colonel Failure" 3)
  (trivial-etr "ONR Colonel Failure" 4))

(deftest onr-cortical-scanner
  (trivial-etr "ONR Cortical Scanner" 0)
  (trivial-etr "ONR Cortical Scanner" 1)
  (trivial-etr "ONR Cortical Scanner" 2))

(deftest onr-cortical-scrub
  (trivial-brain-damage 1 "ONR Cortical Scrub" 0)
  (trivial-etr "ONR Cortical Scrub" 1))

(deftest ^:kaocha/pending onr-coyote
  ;; pay x on pass or canis 1 (sub)
  (gain-x-on-rez 3 "ONR Chihuahua"))

(deftest onr-credit-blocks
  (trivial-etr "ONR Credit Blocks")
  (changes-subtype-rez 1 "Sentry" "Wall" "ONR Credit Blocks"))

(deftest onr-crystal-wall
  (trivial-etr "ONR Crystal Wall"))

(deftest onr-datacomb
  (trivial-etr "ONR Datacomb")
  (pay-x-or-bounce-on-pass 1 "ONR Datacomb"))

(deftest onr-darc-knight
  (trivial-trash-program "ONR D'Arc Knight" 0)
  (trivial-etr "ONR D'Arc Knight" 1))

(deftest onr-data-naga
  (trivial-trash-program "ONR Data Naga" 0)
  (trivial-etr "ONR Data Naga" 1))

(deftest onr-data-darts
  (trivial-damage 3 "ONR Data Darts")
  (cannot-break-next-ice "ONR Data Darts" 1))

(deftest ^:kaocha/pending onr-data-raven
  ;; trace 5 - data raven token
  )

(deftest onr-data-wall (trivial-etr "ONR Data Wall"))

(deftest onr-data-wall-2.0 (trivial-etr "ONR Data Wall 2.0"))

(deftest onr-deadeye
  (noisy-breaker-used-discount 5 "ONR Deadeye")
  (trivial-trash-program "ONR Deadeye" 0)
  (trivial-etr "ONR Deadeye" 1))

(deftest onr-death-yo-yo
  (gain-x-and-bounce-on-pass 1 "ONR Death Yo-Yo")
  (trivial-brain-damage 1 "ONR Death Yo-Yo")
  (trivial-etr "ONR Death Yo-Yo" 1))

(deftest ^:kaocha/pending onr-digiconda
  (trivial-damage 2 "ONR Digiconda")
  (trivial-etr "ONR Digiconda" 1)
  ;; gain x strength on rez (x creds)
  )

(deftest ^:kaocha/pending onr-dogpile
  ;; do 1 net for each rezzed ice outside dogpile
  (trivial-etr "ONR Dogpile" 1)
  ;; +1 str for each rezzed ice outside dogpile
  )

(deftest ^:kaocha/pending onr-dumpster
  ;; can't install on archives
  ;; bounce runner to archives (straight to encounter)
  )

(deftest onr-endless-corridor
  (trivial-etr "ONR Endless Corridor")
  (trivial-etr "ONR Endless Corridor" 1))

(deftest ^:kaocha/pending onr-entrapment
  ;; sub: pay 2 to aginfusion the runner
  )

(deftest ^:kaocha/pending onr-fang
  ;; trace: etr, can't run unless pay 2
  )

(deftest ^:kaocha/pending onr-fang-2.0
  ;; trace: etr, can't run unless pay 2
  )

(deftest ^:kaocha/pending onr-fatal-attractor
  ;; chum
  )

(deftest onr-fetch-4.0.1
  ;; Subroutine is trace 3 give a tag
  (trace-tag 1 "ONR Fetch 4.0.1"))

(deftest onr-filter
  (trivial-etr "ONR Filter"))

(deftest onr-fire-wall
  (trivial-etr "ONR Fire Wall"))

(deftest onr-food-fight
  ;; spend 2x credits, gain x etr subs (pay on rez)
  (purchase-subroutine-on-rez "ONR Food Fight" "End the run" 2))

(deftest ^:kaocha/pending onr-fragmentation-storm
  ;; trace: trash program, etr, cannot run unless pay
  )

(deftest onr-galatea
  (trivial-etr "ONR Galatea")
  (changes-subtype-rez 1 "Wall" "Code Gate" "ONR Galatea"))

(deftest ^:kaocha/pending onr-gatekeeper
  ;; has x etr subs (pay 2x on rez)
  )

(deftest ^:kaocha/pending onr-glacier
  ;; costs 1 ap to rez
  (trivial-etr "ONR Glacier")
  (trivial-etr "ONR Glacier" 1)
  ;; can anti-formicary it to the outermost for 1 credit
  )

(deftest ^:kaocha/pending onr-haunting-inquisition
  ;; runner cannot run until they have spent 6 actions!!!
  (trivial-etr "ONR Glacier" 1)
  )

(deftest ^:kaocha/pending onr-homewrecker
  ;; trace: etr, hardware, meat
  )

(deftest ^:kaocha/pending onr-homing-missing
  ;; trace: etr, cannot run unless pay
  ;; rez: pay x to make strength x
  )

(deftest onr-hunter
  (trace-tag 1 "ONR Hunter"))

(deftest ^:kaocha/pending onr-hunting-pack
  ;; 1x 'trace 5 - tag' per outside ice?
  )

(deftest onr-ice-pick-willie
  (trivial-trash-program "ONR Ice Pick Willie" 0)
  (trivial-etr "ONR Ice Pick Willie" 1))

(deftest ^:kaocha/pending onr-iceberg
  (trivial-damage 1 "ONR Iceberg")
  ;; 2c each: purchase etr subs on encounter
  )

(deftest onr-imperial-guard
  (noisy-breaker-used-discount 5 "ONR Imperial Guard")
  (trivial-trash-program "ONR Imperial Guard" 0)
  (trivial-etr "ONR Imperial Guard" 1))

(deftest onr-jack-attack
  ;; TODO - write test for 'can't jack out'
  (trace-tag 1 "ONR Jack Attack" 1))

(deftest onr-keeper
  (trivial-etr "ONR Keeper"))

(deftest onr-laser-wire
  (trivial-damage 1 "ONR Laser Wire")
  (trivial-etr "ONR Laser Wire" 1))

(deftest onr-lesser-arcana
  (trivial-etr "ONR Lesser Arcana")
  (changes-subtype-rez 1 "Sentry" "Wall" "ONR Lesser Arcana"))

(deftest onr-liche
  (trivial-brain-damage 1 "ONR Liche")
  (trivial-brain-damage 1 "ONR Liche" 1)
  (trivial-brain-damage 1 "ONR Liche" 2)
  (trivial-etr "ONR Liche" 3))

(deftest onr-marionette
  (trivial-trash-program "ONR Marionette" 0)
  (trivial-etr "ONR Marionette" 1)
  (pay-x-or-bounce-on-pass 1 "ONR Marionette"))

(deftest ^:kaocha/pending onr-mastermind
  ;;1 brain for each ice outside of it
  (trivial-etr "ONR Mastermind" 1)
  ;;+1 strength for each ice outside of it
  )

(deftest ^:kaocha/pending onr-mastiff
  (trivial-brain-damage 1 "ONR Mastiff")
  (trivial-damage 1 "ONR Mastiff" 1)
  (canis-effect 1 "ONR Mastiff" 2)
  ;; mastiff counter...
  (trivial-etr "ONR Mastiff" 4)
  )

(deftest onr-mazer
  (trivial-etr "ONR Mazer"))

(deftest ^:kaocha/pending onr-minotaur
  ;; etr for each rezzed code gate or wall outside of it
  )

(deftest onr-misleading-access-menus
  (gain-x-on-rez 3 "ONR Misleading Access Menus")
  (pay-x-or-etr-sub 1 "ONR Misleading Access Menus"))

(deftest ^:kaocha/pending onr-mobile-barricade
  (trivial-damage 1 "ONR Mobile Barricade")
  (trivial-etr "ONR Mobile Barricade" 1)
  ;;1c: swap with ice in same server at start of run
  )

(deftest onr-nerve-labyrinth
  (trivial-damage 2 "ONR Nerve Labyrinth")
  (trivial-etr "ONR Nerve Labyrinth" 1))

(deftest onr-neural-blade
  (trivial-damage 1 "ONR Neural Blade")
  (cannot-break-next-ice "ONR Neural Blade" 1))

(deftest onr-pocket-virtual-reality
  (trace-tag 1 "ONR Pocket Virtual Reality")
  (trace-tag 1 "ONR Pocket Virtual Reality" 1)
  ;; TODO - test this bit!
  ;; 4 recurring credits for traces, only during encounters with this ice
  )

(deftest ^:kaocha/pending onr-puzzle
  ;; etr, trash end of run
  ;; etr, trash end of run
  )

(deftest onr-pi-in-the-face
  (trivial-etr "ONR π in the 'Face"))

(deftest onr-quandary (trivial-etr "ONR Quandary"))

(deftest onr-razor-wire
  (trivial-damage 2 "ONR Razor Wire")
  (trivial-etr "ONR Razor Wire" 1))

(deftest onr-reinforced-wall
  (trivial-etr "ONR Reinforced Wall" 0)
  (trivial-etr "ONR Reinforced Wall" 1))

(deftest ^:kaocha/pending onr-rex
  ;; trace - etr, cannot run again
  )

(deftest ^:kaocha/pending onr-riddler

  ;; purchase subroutines on encounter (etr) for 2 each
  )

(deftest ^:kaocha/pending onr-roadblock
  ;; trivial etr
  ;; 1d6 on rez - on a 6, derez, otherwise gain that much str for encounter
  )

(deftest onr-rock-is-strong
  (trivial-etr "ONR Rock is Strong"))

(deftest onr-sandstorm
  (purchase-subroutine-on-rez "ONR Sandstorm" "End the run" 2))

(deftest onr-scaffolding
  (gain-x-and-bounce-on-pass 1 "ONR Scaffolding")
  (trivial-etr "ONR Scaffolding"))

(deftest onr-scramble
  (trivial-etr "ONR Scramble"))

(deftest onr-sentinels-prime
  (trivial-trash-program "ONR Sentinels Prime" 0)
  (trivial-etr "ONR Sentinels Prime" 1))

(deftest ^:kaocha/pending onr-shock.r
  ;; sub: both halves of inazuma
  )

(deftest onr-shotgun-wire
  (trivial-damage 2 "ONR Shotgun Wire")
  (trivial-etr "ONR Shotgun Wire" 1))

(deftest onr-sleeper
  (trivial-etr "ONR Sleeper"))

(deftest onr-snowbank
  (gain-x-on-rez 3 "ONR Snowbank")
  (pay-x-or-etr-sub 1 "ONR Snowbank"))

(deftest onr-sphinx-2006
  (trivial-etr "ONR Sphinx 2006")
  (changes-subtype-rez 4 "Code Gate" "Sentry" "ONR Sphinx 2006"))

(deftest onr-sumo-2008
  (trivial-etr "ONR Sumo 2008")
  (changes-subtype-rez 1 "Sentry" "Wall" "ONR Sumo 2008"))

(deftest ^:kaocha/pending onr-tko-2.0
  ;; etr, forgo next click
  )

(deftest onr-too-many-doors
  ;; Snowflake - Win a psi game to end the run
  (do-game
    (new-game {:corp {:deck ["ONR Too Many Doors"]}})
    (play-from-hand state :corp "ONR Too Many Doors" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (let [sf (get-ice state :hq 0)]
      (rez state :corp sf)
      (run-continue state)
      (card-subroutine state :corp sf 0)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (:run @state) "Runner won psi, run continues")
      (card-subroutine state :corp sf 0)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (not (:run @state)) "Run ended"))))

(deftest onr-toughonium-wall
  (trivial-etr "ONR Toughonium [TM] Wall")
  (trivial-etr "ONR Toughonium [TM] Wall" 1)
  (trivial-etr "ONR Toughonium [TM] Wall" 2)
  (trivial-etr "ONR Toughonium [TM] Wall" 3))

(deftest ^:kaocha/pending onr-trapdoor
  ;; only on R&D or HQ
  ;; aginfusion runner to a remote
  ;; automatically break sub on encounter if there are no remotes?
  )

(deftest onr-triggerman
  (trivial-trash-program "ONR Triggerman" 0)
  (trivial-etr "ONR Triggerman" 1))

(deftest onr-tumblers
  (gain-x-and-bounce-on-pass 1 "ONR Tumblers")
  (trivial-etr "ONR Tumblers"))

(deftest ^:kaocha/pending onr-tutor
  ;; rest of ice gets etr for each encounter for rest of run
  )

(deftest onr-twisty-passages
  (trivial-etr "ONR Twisty Passages")
  (pay-x-or-bounce-on-pass 1 "ONR Twisty Passages"))

(deftest ^:kaocha/pending onr-vacuum-link
  ;; roll a 1d6. on 1/2/3, runner moves that many positions back (or may jack out), or to the outside ice if out of space
  )

(deftest ^:kaocha/pending onr-viral-15
  ;; runner must pay 1 to jack out
  ;; when runner passes ice, if they don't jack out, they trash a program (if able)
  )

(deftest ^:kaocha/pending onr-virizz
  ;; midway station grid for rest of run
  )

(deftest ^:kaocha/pending onr-vortex
  ;; pay 2 to aginfusion
  )

(deftest ^:kaocha/pending onr-walking-wall
  (trivial-etr "ONR Walking Wall")
  ;; anti-formicary for 1 credit
  )

(deftest onr-wall-of-ice
  (trivial-damage 2 "ONR Wall of Ice")
  (trivial-damage 2 "ONR Wall of Ice" 1)
  (trivial-etr "ONR Wall of Ice" 2)
  (trivial-etr "ONR Wall of Ice" 3))

(deftest onr-wall-of-static
  (trivial-etr "Wall of Static"))

(deftest ^:kaocha/pending onr-washed-up-solo-construct
  ;; trash program unless runner pays 1
  (gain-x-on-rez 3 "ONR Washed-Up Solo Construct")
  )

(deftest onr-zombie
  (trivial-brain-damage 1 "ONR Zombie" 0)
  (trivial-brain-damage 1 "ONR Zombie" 1)
  (trivial-etr "ONR Zombie" 2))
