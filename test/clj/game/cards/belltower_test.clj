(ns game.cards.belltower-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

#_{:clj-kondo/ignore [:unused-binding]}
(defmacro changed? [bindings & body]
  `(throw (ex-info "changed? should only be used in `is` asserts." {})))

(defmethod clojure.test/assert-expr 'changed?
  [msg [_changed bindings & body]]
  (let [exprs (take-nth 2 bindings)
        amts (take-nth 2 (drop 1 bindings))
        init-binds (repeatedly gensym)
        end-binds (repeatedly gensym)
        pairs (mapv vector
                    amts
                    (map #(list `quote %) exprs)
                    init-binds
                    end-binds)]
    `(let [~@(interleave init-binds exprs)
           _# (do ~@body)
           ~@(interleave end-binds exprs)]
       (doseq [[amt# expr# init# end#] ~pairs
               :let [expected# (+ init# amt#)
                     actual-change# (- end# init#)]]
         (clojure.test/do-report
           {:type (if (= actual-change# amt#) :pass :fail)
            :expected amt#
            :actual actual-change#
            :message (format "%s\n%s => (%s to %s)" ~msg expr# init# end#)})))))

;; ANARCH CARDS

;;(deftest sebastio  ) - no test here, I can't figure out the accents

(deftest ^:kaocha/pending achaean-crew-basic-functionality
  (do-game
    (new-game {:corp {:hand ["Vanilla" "City Works Project"]}
               :runner {:hand ["Achaean Crew" (qty "Sketchpad" 3)] :credits 10}})
    (play-from-hand state :corp "Vanilla" "R&D")
    (play-and-score state "City Works Project")
    (take-credits state :corp)
    (core/gain-clicks state :runner 10)
    (let [iwall (get-ice state :hq 0)
          vanil (get-ice state :rd 0)]
      (rez state :corp vanil)
      (is (= 0 (get-strength (refresh vanil))) "Vanilla starts at 0 strength")
      (play-from-hand state :runner "Sketchpad")
      (click-card state :runner (refresh vanil))
      (is (= 0 (get-strength (refresh vanil))) "Vanilla still at 0 strength")
      (play-from-hand state :runner "Achaean Crew")
      (run-on state "R&D")
      (run-continue state :encounter-ice)
      (is (= -1 (get-strength (refresh vanil))) "Vanilla at -1")
      (fire-subs state vanil)
      (play-from-hand state :runner "Sketchpad")
      (click-card state :runner (refresh vanil))
      (run-on state "R&D")
      (run-continue state :encounter-ice)
      (is (= -2 (get-strength (refresh vanil))) "Vanilla at -2")
      (fire-subs state vanil)
      (run-on state "R&D")
      (run-continue state :encounter-ice)
      (card-ability state :runner (get-resource state 0) 0)
      (is (= 1 (count (:discard (get-corp)))) "Vanilla trashed")
      (is (= 3 (count (:discard (get-runner)))) "Hopes and dreams trashed"))))

;; asset

(deftest charlotte
  (do-game
    (new-game {:corp {:hand ["[Carlos Izquierdo]"]
                      :deck [(qty "Hedge Fund" 20)]}})
    (play-from-hand state :corp "[Carlos Izquierdo]" "New remote")
    (let [char (get-content state :remote1 0)]
      (rez state :corp char)
      (click-advance state :corp (refresh char))
      (click-advance state :corp (refresh char))
      (take-credits state :corp)
      (take-credits state :runner)
      (is (changed? [(get-counters (refresh char) :advancement) -1
                     (:credit (get-corp)) 4
                     (count (:hand (get-corp))) 2]
                    (click-prompt state :corp "Yes"))
          "Power counter to gain 4 credit and draw 1 (plus mandatory)")
      (is (changed? [(:credit (get-corp)) 2
                     (count (:hand (get-corp))) 1]
                    (card-ability state :corp (refresh char) 1))
          "Trash to gain 2 credit and draw 1"))
    (is (find-card "[Carlos Izquierdo]" (:discard (get-corp))) "Charlotte trashed")))

(deftest ^:kaocha/pending will-to-win
  (do-game
    (new-game {:corp {:hand ["[Will to Win]" "Hedge Fund" "Hostile Takeover" "Ice Wall"]}})
    (play-from-hand state :corp "[Will to Win]" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-content state :remote1 0))
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (take-credits state :runner)
    (is (:corp-phase-12 @state) "Corp is in Step 1.2")
    (end-phase-12 state :corp)
    (click-card state :corp (find-card "Hostile Takeover" (:hand (get-corp))))
    (click-prompt state :corp "New remote")
    ;; PENDING here: can't seem to select Ice Wall for some reason
    (click-card state :corp (get-ice state :hq 0))
    (is (not (rezzed? (get-content state :remote1 0))) "[Will to Win] derezzed")
    (is (not (rezzed? (get-ice state :hq 0))) "Ice Wall derezzed")
    (click-advance state :corp (get-content state :remote2 0))
    (click-advance state :corp (get-content state :remote2 0))
    (score state :corp (get-content state :remote2 0))
    (is (= 0 (count (get-scored state :corp))) "Hostile Takeover can not be scored")))

;; event

(deftest ar-infinite-access
  (do-game
    (new-game {:runner {:hand ["AR Infinite Access" (qty "Sure Gamble" 5)]
                        :deck [(qty "Sure Gamble" 20)]
                        :discard [(qty "Sure Gamble" 20)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "AR Infinite Access")
    (is (= 0 (count (:discard (get-runner)))) "Heap is empty")
    (is (= 5 (count (:hand (get-runner)))) "Grip is full")
    (is (= 35 (count (:deck (get-runner)))) "Stack is back")
    (is (= 6 (count (:rfg (get-runner)))))))

(deftest eye-for-an-eye
  (do-game
    (new-game {:corp {:hand ["Ice Wall" "Enigma"]}
               :runner {:hand ["Eye for an Eye" (qty "Sure Gamble" 2)]}})
    (take-credits state :corp)
    (gain-tags state :runner 1)
    (play-from-hand state :runner "Eye for an Eye")
    (is (not (:run @state)) "Cannot play Eye for an Eye when tagged")
    (remove-tag state :runner)
    (play-run-event state "Eye for an Eye" :hq)
    (click-prompt state :runner "[Eye for an Eye] Trash 1 card from your hand: Trash card")
    (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
    (click-prompt state :runner "[Eye for an Eye] Trash 1 card from your hand: Trash card")
    (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
    (is (not (:run @state)) "Run ended")
    (is (= 1 (count-tags state)))
    (is (= 2 (count (:discard (get-corp)))))
    (is (= 3 (count (:discard (get-runner)))) "Whole world is blind")))

(deftest true-colors
  (do-game
    (new-game {:corp {:hand [(qty "Hedge Fund" 4)]}
               :runner {:hand ["True Colors"]}})
    (take-credits state :corp)
    (play-run-event state "True Colors" :hq)
    (is (last-log-contains? state "reveals Hedge Fund, Hedge Fund, Hedge Fund from HQ"))
    (changes-val-macro 2 (count (:deck (get-corp)))
                       "2 cards added to R&D"
                       (click-prompt state :runner "Hedge Fund")
                       (click-prompt state :runner "Top of R&D")
                       (click-prompt state :runner "Hedge Fund")
                       (click-prompt state :runner "Bottom of R&D"))))

;; hardware

(deftest alarm-clock
  (do-game
    (new-game {:corp {:hand ["Ice Wall"]}
               :runner {:hand ["Alarm Clock"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Alarm Clock")
    (take-credits state :runner)
    (take-credits state :corp)
    (end-phase-12 state :runner)
    (click-prompt state :runner "Yes")
    (is (:run @state) "Run has started")
    (run-continue state)
    (changes-val-macro -2 (:click (get-runner))
                       "Costs 2 clicks"
                       (click-prompt state :runner "Yes"))
    (is (= :movement (:phase (get-run))) "Run has bypassed Ice Wall")))

(deftest the-wizards-chest
  (do-game
    (new-game {:runner {:hand ["The Wizard's Chest"]
                        :discard ["Legwork" "Corroder" "Ice Carver" "Prepaid VoicePAD" "Femme Fatale" "Egret" "Earthrise Hotel"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "The Wizard's Chest")
    (let [chest (get-hardware state 0)]
      ;; TODO: make this a helper or something for consistently ordered starting deck
      (doseq [card-name ["Legwork" "Corroder" "Ice Carver" "Prepaid VoicePAD" "Femme Fatale" "Egret" "Earthrise Hotel"]]
        (core/move state :runner (find-card card-name (get-in @state [:runner :discard])) :deck))
      (card-ability state :runner chest 0)
      (is (no-prompt? state :runner) "Cannot trigger The Wizard's Chest until all centrals ran")
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (card-ability state :runner (refresh chest) 0)
      (is (= ["Hardware" "Program" "Resource" "Cancel"] (prompt-buttons :runner)))
      (click-prompt state :runner "Program")
      (is (= ["Install Corroder" "Install Femme Fatale" "No thanks"] (prompt-buttons :runner)))
      (changes-val-macro 0 (:credit (get-runner))
                         "Install at no cost"
                         (click-prompt state :runner "Install Femme Fatale"))
      (is (= "Femme Fatale" (:title (get-program state 0))) "Femme Fatale is installed")
      (is (second-last-log-contains? state (str "Runner uses The Wizard's Chest"
                                                " to reveal Legwork, Corroder, Ice Carver, Prepaid VoicePAD, Femme Fatale from the top of the stack"
                                                " and install Femme Fatale, ignoring all costs."))))))

;; ice

(deftest domino
  (do-game
    (new-game {:corp {:hand ["Domino" "Hedge Fund"]}
               :runner {:hand [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Domino" "HQ")
    (take-credits state :corp)
    (run-on state :hq)
    (rez state :corp (get-ice state :hq 0))
    (run-continue state :encounter-ice)
    (is (changed? [(count (:hand (get-runner))) -2]
                  (fire-subs state (get-ice state :hq 0)))
        "Runner takes 2 damage")
    (is (changed? [(count (:hand (get-corp))) 0]
                  (click-prompt state :corp "No"))
        "Corp doesn't trash first time")
    (is (changed? [(count (:hand (get-corp))) -1]
                  (click-prompt state :corp "Yes")
                  (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp)))))
        "Corp does trash second time")
    (is (not (:run @state)) "Run ended")))

(deftest domino-threat
  (do-game
    (new-game {:corp {:hand ["City Works Project" "Hostile Takeover" "Domino"]}})
    (core/gain state :corp :click 10)
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Domino" "HQ")
    (let [domino (get-ice state :hq 0)]
      (rez state :corp domino)
      (play-and-score state "City Works Project")
      (is (changed? [(get-strength (refresh domino)) 2]
                    (play-and-score state "Hostile Takeover"))
          "Domino gains 2 strength at Threat 4"))))

;; operation

(deftest directors-visit
  (do-game
    (new-game {:corp {:hand [(qty "Director’s Visit" 2) (qty "Clearinghouse" 2)]}
               :runner {:hand ["Imp" "Cache"]}})
    (play-from-hand state :corp "Clearinghouse" "New remote")
    (play-from-hand state :corp "Clearinghouse" "New remote")
    (play-from-hand state :corp "Director’s Visit")
    (is (changed? [(get-counters (get-content state :remote1 0) :advancement) 1
                   (get-counters (get-content state :remote2 0) :advancement) 1]
                  (click-prompt state :corp "Place 1 advancement on up to two cards")
                  (click-card state :corp (get-content state :remote1 0))
                  (click-card state :corp (get-content state :remote2 0)))
        "Place an advancement counter on two cards that can be advanced")
    (take-credits state :corp)
    (play-from-hand state :runner "Imp")
    (play-from-hand state :runner "Cache")
    (take-credits state :runner)
    (play-from-hand state :corp "Director’s Visit")
    (is (changed? [(get-counters (find-card "Imp" (get-program state)) :virus) -2
                   (get-counters (find-card "Cache" (get-program state)) :virus) 0]
                  (click-prompt state :corp "Remove virus counters from a card")
                  (click-card state :corp (find-card "Imp" (get-program state))))
        "Remove all virus counters from an installed card")))

(deftest strategic-planning
  (do-game
    (new-game {:corp {:hand ["[Strategic Planning]"]
                      :deck [(qty "Hedge Fund" 10)]
                      :discard ["Ping" "Sprint"]}})
    (is (changed? [(:click (get-corp)) -2
                   (:credit (get-corp)) 2
                   (count (:hand (get-corp))) 2]
                  (play-from-hand state :corp "[Strategic Planning]")
                  (click-card state :corp (find-card "Sprint" (:discard (get-corp)))))
        "Double to gain 2 and draw 2")
    (is (find-card "Ping" (:discard (get-corp))) "Ping still in Archives")
    (is (find-card "Sprint" (:hand (get-corp))) "Sprint now in HQ")))

;; program

(deftest powercache
  (do-game
    (new-game {:runner {:hand ["[Powercache]"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "[Powercache]")
    (is (changed? [(:credit (get-runner)) 2
                   (get-counters (get-program state 0) :power) -1]
                  (card-ability state :runner (get-program state 0) 0))
        "1 power counter for 2 credits")
    (is (= 1 (get-counters (get-program state 0) :power)))
    (take-credits state :runner)
    (changes-val-macro 0 (:credit (get-runner)) "Cannot use [Powercache] to gain credits on Corp turn"
                       (card-ability state :runner (get-program state 0) 0))))

;; resources

;; wrote this test not realizing Arsène was just a prompt, this should hopefully match card intention
(deftest ^:kaocha/pending arsene
  (do-game
    (new-game {:corp {:hand ["Hedge Fund"] :deck [(qty "Hedge Fund" 10)]}
               :runner {:hand ["Arsène" "Jailbreak"] :deck ["Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Arsène")
    (run-empty-server state "R&D")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")
    (play-from-hand state :runner "Jailbreak")
    (click-prompt state :runner "R&D")
    (run-continue state)
    (click-prompt state :runner "No action")
    (click-prompt state :runner "No action")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))

(deftest friend-of-a-friend
  (do-game
    (new-game {:runner {:hand [(qty "[Friend of a Friend]" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "[Friend of a Friend]")
    (is (changed? [(:credit (get-runner)) 10
                   (count-tags state) 1]
                  (card-ability state :runner (get-resource state 0) 1))
        "Runner gains 10 credits a 1 tag")
    (play-from-hand state :runner "[Friend of a Friend]")
    (is (changed? [(:credit (get-runner)) 5
                   (count-tags state) -1]
                  (card-ability state :runner (get-resource state 0) 0))
        "Runner gains 5 credits and loses 1 tag")
    (is (= 2 (count (:discard (get-runner)))) "Both trashed")))
