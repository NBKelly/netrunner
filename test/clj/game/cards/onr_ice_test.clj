(ns game.cards.onr-ice-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(defn- trivial-etr
  ([name] (trivial-etr name 0))
  ([name sub]
   (do-game
     (new-game {:corp {:credits 20
                       :hand [name]}})
     (play-from-hand state :corp name "New remote")
     (let [card (get-ice state :remote1 0)]
       (rez state :corp card)
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
       (rez state :corp card)
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
       (rez state :corp card)
       (run-on state :remote1)
       (run-continue state)
       (card-subroutine state :corp card sub)
       (is (= x (:brain-damage (get-runner))) "Runner took 2 brain damage")))))

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
       (rez state :corp card)
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
         (rez state :corp card))))))

(defn- pay-x-or-etr-sub
  ([x name] (pay-x-or-etr-sub x name 0))
  ([x name sub]
   (do-game
     (new-game {:corp {:credits 20 :hand [name]}
                :runner {:credits (* 3 x)}})
     (play-from-hand state :corp name "New remote")
     (take-credits state :corp)
     (let [card (get-ice state :remote1 0)]
       (rez state :corp card)
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

;; tests here

(deftest onr-banpei-etr
  (trivial-trash-program "ONR Banpei" 0)
  (trivial-etr "ONR Banpei" 1))

(deftest onr-brain-wash
  (trivial-brain-damage 1 "ONR Brain Wash"))

(deftest onr-canis-major
  (do-game
    (new-game {:corp {:hand ["ONR Canis Major" "Vanilla"]}})
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "ONR Canis Major" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [canis (get-ice state :hq 1)
          vanil (get-ice state :hq 0)]
      (rez state :corp canis)
      (rez state :corp vanil)
      (run-continue state)
      (card-subroutine state :corp (refresh canis) 0)
      (is (= 0 (:strength (refresh vanil))))
      (run-continue state)
      (run-continue state)
      (run-continue state :encounter-ice)
      (is (= 2 (core/get-strength (refresh vanil)))))))

(deftest onr-canis-minor
  (do-game
    (new-game {:corp {:hand ["ONR Canis Minor" "Vanilla"]}})
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "ONR Canis Minor" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [canis (get-ice state :hq 1)
          vanil (get-ice state :hq 0)]
      (rez state :corp canis)
      (rez state :corp vanil)
      (run-continue state)
      (card-subroutine state :corp (refresh canis) 0)
      (is (= 0 (:strength (refresh vanil))))
      (run-continue state)
      (run-continue state)
      (run-continue state :encounter-ice)
      (is (= 1 (core/get-strength (refresh vanil)))))))

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

(deftest onr-crystal-wall
  (trivial-etr "ONR Crystal Wall"))

(deftest onr-darc-knight
  (trivial-trash-program "ONR D'Arc Knight" 0)
  (trivial-etr "ONR D'Arc Knight" 1))

(deftest onr-darc-knight
  (trivial-trash-program "ONR Data Naga" 0)
  (trivial-etr "ONR Data Naga" 1))

(deftest onr-data-wall (trivial-etr "ONR Data Wall"))

(deftest onr-data-wall-2.0 (trivial-etr "ONR Data Wall 2.0"))

(deftest onr-endless-corridor
  (trivial-etr "ONR Endless Corridor")
  (trivial-etr "ONR Endless Corridor" 1))

(deftest onr-filter
  (trivial-etr "ONR Filter"))

(deftest onr-filter
  (trivial-etr "ONR Fire Wall"))

(deftest onr-fetch-4.0.1
  ;; Subroutine is trace 3 give a tag
  (do-game
    (new-game {:corp {:deck ["ONR Fetch 4.0.1"]}})
    (play-from-hand state :corp "ONR Fetch 4.0.1" "HQ")
    (let [resistor (get-ice state :hq 0)]
      (rez state :corp resistor)
      (take-credits state :corp)
      (run-on state "HQ")
      (run-continue state)
      (fire-subs state resistor)
      (click-prompt state :corp "0")
      (is (= 1 (count-tags state)) "Runner has gained 1 tag"))))

(deftest onr-ice-pick-willie
  (trivial-trash-program "ONR Ice Pick Willie" 0)
  (trivial-etr "ONR Ice Pick Willie" 1))

(deftest onr-keeper
  (trivial-etr "ONR Keeper"))

(deftest onr-laser-wire
  (trivial-damage 1 "ONR Laser Wire")
  (trivial-etr "ONR Laser Wire" 1))

(deftest onr-liche
  (trivial-brain-damage 1 "ONR Liche")
  (trivial-brain-damage 1 "ONR Liche" 1)
  (trivial-brain-damage 1 "ONR Liche" 2)
  (trivial-etr "ONR Liche" 3))

(deftest onr-mazer
  (trivial-etr "ONR Mazer"))

(deftest onr-misleading-access-menus
  (gain-x-on-rez 3 "ONR Misleading Access Menus")
  (pay-x-or-etr-sub 1 "ONR Misleading Access Menus"))

(deftest onr-nerve-labyrinth
  (trivial-damage 2 "ONR Nerve Labyrinth")
  (trivial-etr "ONR Nerve Labyrinth" 1))

(deftest onr-pi-in-the-face
  (trivial-etr "ONR π in the 'Face"))

(deftest onr-razor-wire
  (trivial-damage 2 "ONR Razor Wire")
  (trivial-etr "ONR Razor Wire" 1))

(deftest onr-reinforced-wall
  (trivial-etr "ONR Reinforced Wall" 0)
  (trivial-etr "ONR Reinforced Wall" 1))

(deftest onr-rock-is-strong
  (trivial-etr "ONR Rock is Strong"))

(deftest onr-scramble
  (trivial-etr "ONR Scramble"))

(deftest onr-sentinels-prime
  (trivial-trash-program "ONR Sentinels Prime" 0)
  (trivial-etr "ONR Sentinels Prime" 1))

(deftest onr-shotgun-wire
  (trivial-damage 2 "ONR Shotgun Wire")
  (trivial-etr "ONR Shotgun Wire" 1))

(deftest onr-sleeper
  (trivial-etr "ONR Sleeper"))

(deftest onr-snowbank
  (gain-x-on-rez 3 "ONR Snowbank")
  (pay-x-or-etr-sub 1 "ONR Snowbank"))

(deftest onr-toughonium-wall
  (trivial-etr "ONR Toughonium [TM] Wall")
  (trivial-etr "ONR Toughonium [TM] Wall" 1)
  (trivial-etr "ONR Toughonium [TM] Wall" 2)
  (trivial-etr "ONR Toughonium [TM] Wall" 3))

(deftest onr-triggerman
  (trivial-trash-program "ONR Triggerman" 0)
  (trivial-etr "ONR Triggerman" 1))

(deftest onr-quandary (trivial-etr "ONR Quandary"))

(deftest onr-wall-of-ice
  (trivial-damage 2 "ONR Wall of Ice")
  (trivial-damage 2 "ONR Wall of Ice" 1)
  (trivial-etr "ONR Wall of Ice" 2)
  (trivial-etr "ONR Wall of Ice" 3))

(deftest onr-wall-of-static
  (trivial-etr "Wall of Static"))

(deftest onr-zombie
  (trivial-brain-damage 1 "ONR Zombie" 0)
  (trivial-brain-damage 1 "ONR Zombie" 1)
  (trivial-etr "ONR Zombie" 2))
