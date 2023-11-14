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
     (let [iw (get-ice state :remote1 0)]
       (rez state :corp iw)
       (take-credits state :corp)
       (run-on state :remote1)
       (run-continue state)
       (card-subroutine state :corp iw sub)
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
     (let [iw (get-ice state :remote1 0)
           jh (get-program state 0)]
       (rez state :corp iw)
       (run-on state :remote1)
       (run-continue state)
       (card-subroutine state :corp iw sub)
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
     (let [iw (get-ice state :remote1 0)]
       (rez state :corp iw)
       (run-on state :remote1)
       (run-continue state)
       (card-subroutine state :corp iw sub)
       (is (= x (:brain-damage (get-runner))) "Runner took 2 brain damage")))))

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

(deftest onr-data-wall (trivial-etr "ONR Data Wall"))

(deftest onr-data-wall-2.0 (trivial-etr "ONR Data Wall 2.0"))

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

(deftest onr-quandary (trivial-etr "ONR Quandary"))

(deftest onr-reinforced-wall
  (trivial-etr "ONR Reinforced Wall" 0)
  (trivial-etr "ONR Reinforced Wall" 1))
