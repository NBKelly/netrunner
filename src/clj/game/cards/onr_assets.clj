(ns game.cards.onr-assets
  (:require
   [clojure.pprint :as pprint]
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.access :refer [access-card installed-access-trigger]]
   [game.core.actions :refer [score]]
   [game.core.agendas :refer [update-all-advancement-requirements
                              update-all-agenda-points]]
   [game.core.bad-publicity :refer [bad-publicity-prevent gain-bad-publicity
                                    lose-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed get-remotes
                            installable-servers]]
   [game.core.card :refer [agenda? asset? can-be-advanced? corp? event? corp-installable-type?
                           faceup? fake-identity? get-advancement-requirement
                           get-agenda-points get-card get-counters get-title get-zone hardware? has-subtype? ice?
                           identity? in-deck? in-discard? in-hand? in-server? installed? is-type?
                           operation? program? resource? rezzed? runner? upgrade?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.damage :refer [damage damage-prevent damage-bonus]]
   [game.core.def-helpers :refer [corp-recur corp-rez-toast defcard
                                  reorder-choice trash-on-empty get-x-fn]]
   [game.core.drawing :refer [draw first-time-draw-bonus max-draw draw-bonus
                              remaining-draws]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [complete-with-result effect-completed is-basic-advance-action? make-eid]]
   [game.core.engine :refer [pay register-events resolve-ability]]
   [game.core.events :refer [first-event? no-event? last-turn? turn-events]]
   [game.core.expose :refer [expose-prevent]]
   [game.core.flags :refer [lock-zone prevent-current
                            prevent-draw
                            register-turn-flag! release-zone]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-clicks safe-inc-n
                              lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+ runner-hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [add-extra-sub! remove-extra-subs! update-all-ice
                          update-ice-strength]]
   [game.core.identities :refer [disable-card enable-card]]
   [game.core.initializing :refer [card-init]]
   [game.core.installing :refer [corp-install corp-install-msg]]
   [game.core.moving :refer [as-agenda mill move remove-from-currently-drawing
                             swap-cards swap-installed trash trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay? cost-value]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable]]
   [game.core.props :refer [add-counter add-icon add-prop remove-icon set-prop]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [end-run gain-corp-run-credits]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [is-central? is-remote? target-server zone->name]]
   [game.core.set-aside :refer [get-set-aside set-aside-for-me swap-set-aside-cards]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck
                                shuffle-into-rd-effect]]
   [game.core.tags :refer [gain-tags]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.core.winning :refer [check-win-by-agenda win]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]
   [game.core.link :refer [get-link]]
   [game.cards.assets :refer [campaign]]
   [game.core.onr-utils :refer [handle-if-unique onr-trace-tag register-effect-once dice-roll]]
   [game.cards.ice :refer [end-the-run-unless-runner-pays end-the-run]]
   ))

;; (def end-the-run
;;   "Basic ETR subroutine"
;;   {:label "End the run"
;;    :msg "end the run"
;;    :async true
;;    :effect (effect (end-run :corp eid card))})

(defn- onr-advancable
  ([ab] (onr-advancable 0 ab))
  ([cost ab] (onr-advancable 0 true ab))
  ([cost optional ability]
   (let [cost (if (number? cost) [:credit cost] cost)
         ask (if optional
               {:optional {:req (req (and (rezzed? card)
                                            (installed? card)
                                            (can-pay? state :corp eid card nil cost)))
                             :waiting-prompt (:waiting-prompt ability)
                           :prompt (msg "Use " (:title card) " ability?")
                           :yes-ability (dissoc ability :waiting-prompt)}}
               {:req (req (and (rezzed? card)
                               (installed? card)
                               (can-pay? state :corp eid card nil cost)))
                :async true
                :effect (req (continue-ability
                               state side
                               (dissoc ability :waiting-prompt)
                               card nil))})]
     {:advanceable :always
      :implementation "(classic) Installed ambushes must be rezzed to take effect, unless otherwise noted"
      :on-access ask})))

(defn- onr-ambush-impl [impl]
  (merge {:implementation "(classic) Installed ambushes must be rezzed to take effect, unless otherwise noted"} impl))

;; card impls

(defcard "ONR ACME Savings and Loan"
  (letfn [(acme-event []
            {:event :corp-turn-ends
             :unregister-once-resolved true
             :ability-name "ACME debt collection"
             :async true
             :effect (req (let [triggers (or (get-in @state [:corp :acme-loans]) 0)
                                to-spend triggers]
                            (when-not (zero? triggers)
                              (register-events state side (:identity corp) [(acme-event)]))
                            (if-not (can-pay? state side eid card nil [:credit to-spend])
                              (continue-ability
                                state side
                                {:prompt "You gotta pay..."
                                 :choices ["I understand"]
                                 :effect (req (do
                                                (system-msg state side "loses the game after being unable to repay ONR ACME Savings and Loan")
                                                (win state :runner "\"default\"")))}
                                card nil)
                              (wait-for (pay state side (make-eid state eid) card [:credit to-spend])
                                        (system-msg state side (str "pays " to-spend " [Credits] due to ONR ACME Saings and Loan"))
                                        (effect-completed state side eid)))))})]
    {:additional-cost [:agenda-point 1]
     :implementation "All payments are a single instance - if this matters, send me an @ and I will change it"
     :on-rez {:msg (msg "gain 12 [Credits]")
              :cost [:trash-can]
              :asyc true
              :effect (req (wait-for (gain-credits state side (make-eid state eid) 12)
                                     (swap! state update-in [side :acme-loans] (safe-inc-n 1))
                                     ;;(wait-for (trash state side (make-eid state side) (get-card state card))
                                               ;; register the debt collection event too
                                     (handle-if-unique
                                       state side (:identity corp) (acme-event))
                                     (effect-completed state side eid)))}}))

(defcard "ONR BBS Whispering Campaign"
  {:data {:counter {:credit 16}}
   :events [(trash-on-empty :credit)]
   :abilities [{:label "Take 2 [Credits] from this asset"
                :cost [:click 1]
                :keep-menu-open :while-clicks-left
                :msg (msg "gain " (min 2 (get-counters card :credit)) " [Credits]")
                :async true
                :effect (req (let [credits (min 2 (get-counters card :credit))]
                               (wait-for (gain-credits state :corp (make-eid state eid) credits)
                                         (add-counter state side card :credit (- credits) {:placed true})
                                         (effect-completed state side eid))))}]})

(defcard "ONR Bel-Digmo Antibody"
  {:flags {:rd-reveal (req true)}
   :on-rez {:msg (msg "shuffle itself into R&D")
            :async true
            :effect (req (move state side card :deck)
                         (shuffle! state side :deck)
                         (effect-completed state side eid))}
   :on-access {:msg "do 1 net damage"
               :req (req (in-deck? card))
               :async true
               :effect (effect (damage eid :net 1 {:card card}))}})

(defcard "ONR Blood Cat"
  {:abilities [(assoc (onr-trace-tag 5) :cost [:click 1])]})

(defcard "ONR Braindance Campaign"
  (campaign 12 2))

(defcard "ONR Chicago Branch"
  {:abilities [{:cost [:click 1 :credit 3]
                :label "place 2 advancement counters"
                :choices {:card #(and (corp? %)
                                      (can-be-advanced? %)
                                      (installed? %))}
                :msg (msg "place 2 advancement tokens on " (card-str state target))
                :effect (effect (add-prop target :advance-counter 2 {:placed true}))}]})

(defcard "ONR City Surveillance"
  (letfn [(pay-or-tag [ct]
            {:optional
             {:player :runner
              :prompt (msg "Pay 1[Credit] to avoid taking a tag?"
                           (when (> ct 1) (str " (" (dec ct) " remaining)")))
              :yes-ability {:cost [:credit 1]
                            :async true
                            :player :runner
                            ;;:msg (msg "avoid receiving a tag")
                            :effect (req
                                      (system-msg state :runner "pays 1 [Credit] to avoid receiving a tag")
                                      (if (pos? (dec ct))
                                           (continue-ability
                                             state side
                                             (pay-or-tag (dec ct))
                                             card nil)
                                           (effect-completed state side eid)))}
              :no-ability {:player :corp
                           :async true
                           :msg (msg "give the runner a tag")
                           :effect (req (wait-for (gain-tags state :corp (make-eid state eid) 1)
                                                  (if (pos? (dec ct))
                                                    (continue-ability
                                                      state side
                                                      (pay-or-tag (dec ct))
                                                      card nil)
                                                    (effect-completed state side eid))))}}})]
  {:derezzed-events [{:event :pre-runner-draw
                      :async true
                      :req (req (pos? target))
                      :effect (req (let [qty target]
                                     (continue-ability
                                       state side
                                       {:optional
                                        {:req (req (not (rezzed? card)))
                                         :player :corp
                                         :prompt (msg "The Runner is about to draw " qty "cards. Rez " (:title card) "?")
                                         :yes-ability {:async true
                                                       :effect (effect (rez eid card))}}}
                                       card nil)))}]
   :events [{:event :runner-draw
             :async true
             :effect (req (let [num-cards (count runner-currently-drawing)]
                            (if (pos? num-cards)
                              (continue-ability
                                state side
                                (pay-or-tag num-cards)
                                card nil)
                              (effect-completed state side eid))))}]}))

(defcard "ONR Corporate Negotiating Center"
  (let [ability {:req (req (seq (:hand corp)))
                 :async true
                 :label "Reveal agendas"
                 :once :per-turn
                 :interactive (req true)
                 :effect (req (let [gendies (filter agenda? (:hand corp))]
                                 (continue-ability
                                   state side
                                   (if (seq gendies)
                                     {:prompt "reveal any number of agendas from HQ"
                                      :choices {:card #(and (agenda? %)
                                                            (in-hand? %))
                                                :max (count gendies)}
                                      :msg (msg "reveal " (str/join ", " (map :title targets)) " from HQ, and gain " (count targets) " [Credits]")
                                      :async true
                                      :effect (req (wait-for (reveal state side targets)
                                                             (gain-credits state side eid (count targets))))}
                                     {:prompt "You have no agendas"
                                      :choices ["I understand"]})
                                   card nil)))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "ONR Corprunner's Shattered Remains"
  (onr-advancable 0 false {:async true
                           :waiting-prompt true
                           :req (req (and (rezzed? card)
                                          (pos? (get-counters (get-card state card) :advancement))))
                           :prompt (msg "Choose " (quantify (get-counters (get-card state card) :advancement) "piece") " of hardware to trash")
                           :msg (msg "trash " (enumerate-str (map :title targets)))
                           :choices {:max (req (get-counters (get-card state card) :advancement))
                                     :card #(and (installed? %)
                                                 (hardware? %))}
                           :effect (effect (trash-cards eid targets {:cause-card card}))}))

(defcard "ONR Cowboy Sysop"
  {:abilities [{:label "Add an installed card to HQ"
                :cost [:click 1]
                :keep-menu-open :while-clicks-left
                :choices {:card installed?}
                :msg (msg "move " (card-str state target) " to HQ")
                :effect (effect (move target :hand))}]})

(defcard "ONR Cybertech Think Tank"
  (letfn [(boost-abi [used remain]
            {:optional {:prompt (msg "Spend an advancement to boost meat damage?"
                                     (when (> used 0) (str " (" used " used)"))
                                     (when (> remain 1) (str " (" (dec remain) " remain)")))
                        :player :corp
                        :yes-ability {:cost [:advancement 1]
                                      :msg "do 1 additional meat damage"
                                      :async true
                                      :effect (req (damage-bonus state side :meat 1)
                                                   (if (> remain 1)
                                                     (continue-ability
                                                       state side
                                                       (boost-abi (inc used) (dec remain))
                                                       card nil)
                                                     (effect-completed state side eid)))}}})]
    {:implementation "implemented as a pre-damage event"
     :advanceable :always
     :events [{:event :pre-damage
               :req (req (and (= target :meat)
                              (pos? (get-counters (get-card state card) :advancement))))
               :async true
               :effect (req (continue-ability
                              state side
                              (boost-abi 0 (get-counters (get-card state card) :advancement))
                              card nil))}]}))

(defcard "ONR Data Masons"
  {:static-abilities [{:type :rez-cost
                       :req (req (and (ice? target)
                                      (has-subtype? target "Wall")))
                       :value -2}
                      {:type :ice-strength
                       :req (req (and (ice? target)
                                      (has-subtype? target "Wall")))
                       :value 1}]})

(defcard "ONR Department of Misinformation"
  {:interactions {:prevent [{:type #{:expose}
                             :req (req true)}]}
   :derezzed-events [{:event :pre-expose
                      :async true
                      :effect (req (let [etarget target]
                                     (continue-ability
                                       state side
                                       {:optional
                                        {:req (req (not (rezzed? card)))
                                         :player :corp
                                         :prompt (msg "The Runner is about to expose " (:title etarget) ". Rez " (:title card) "?")
                                         :yes-ability {:async true
                                                       :effect (effect (rez eid card))}}}
                                       card nil)))}]
   :abilities [{:msg "prevent 1 card from being exposed"
                :cost [:credit 1]
                :effect (effect (expose-prevent 1))}]})

(defcard "ONR Department of Truth Enhancement"
  {:abilities [{:cost [:click 1]
                :msg "place 3 [Credits]"
                :label "place 3 [Credits]"
                :async true
                :effect (req (add-counter state side eid card :credit 3 nil))}
               {:cost [:click 1]
                :req (req (pos? (get-counters card :credit)))
                :label "take all credits"
                :msg (msg "take " (get-counters card :credit) " credits")
                :async true
                :effect (req (let [creds (get-counters card :credit)]
                               (add-counter state side card :credit (- creds))
                               (gain-credits state :corp eid creds)))}]})

(defcard "ONR ESA Contract"
  {:abilities [{:cost [:click 1]
                :msg "draw 2 cards"
                :async true
                :effect (effect (draw eid 2))}]})

(defcard "ONR Encoder, Inc."
  (let [new-sub (assoc end-the-run :label "[Encoder, Inc.] End the run")]
    (letfn [(all-rezzed-bios [state]
              (filter #(and (ice? %)
                            (has-subtype? % "Code Gate")
                            (rezzed? %))
                      (all-installed state :corp)))
            (remove-one [cid state ice]
              (remove-extra-subs! state :corp ice cid))
            (add-one [cid state ice]
              (add-extra-sub! state :corp ice new-sub cid))
            (update-all [state func]
              (doseq [i (all-rezzed-bios state)]
                (func state i)))]
      {:static-abilities [{:type :rez-cost
                           :req (req (and (ice? target)
                                          (has-subtype? target "Code Gate")))
                           :value -1}]
       :on-rez {:msg "add \"[Subroutine] End the run\" after all other subroutines"
                :effect (req (update-all state (partial add-one (:cid card))))}
       :leave-play (req (system-msg state :corp (str "loses " (:title card) " additional subroutines")
                                    (update-all state (partial remove-one (:cid card)))))
       :events [{:event :rez
                 :req (req (and (ice? (:card context))
                                (has-subtype? (:card context) "Code Gate")))
                 :effect (req (add-one (:cid card) state (get-card state (:card context))))}]})))

(defcard "ONR Euromarket Consortium"
  {:static-abilities [(corp-hand-size+ 2)]
   :abilities [{:cost [:click 1 :credit 1]
                :msg "draw 2 cards"
                :async true
                :effect (effect (draw eid 2))}]})

(defcard "ONR Executive Boot Camp"
  {:abilities [{:label "Gain 2 credits for this run"
                :req (req run)
                :cost [:randomly-trash-from-hand 1]
                :effect (req (gain-corp-run-credits state side eid 2))}]})

(defcard "ONR Experimental AI"
  (onr-advancable 0 false {:req (req (pos? (get-counters (get-card state card) :advancement)))
                           :waiting-prompt true
                           :prompt (msg "Choose " (quantify (get-counters (get-card state card) :advancement) "program") " to trash")
                           :choices {:max (req (get-counters (get-card state card) :advancement))
                                     :card #(and (installed? %)
                                                 (program? %))}
                           :msg (msg "trash " (enumerate-str (map :title targets)))
                           :async true
                           :effect (effect (trash-cards eid targets {:cause-card card}))}))

(defcard "ONR Fortress Architects"
  {:static-abilities [{:type :install-cost
                       :req (req (ice? target))
                       :value -1}]})

(defcard "ONR Hacker Tracker Central"
  (let [abi {:effect (req (add-counter state side card :credit 1))
             :silent (req true)}]
    {:implementation "each credit spent during a trace increases the maximum trace limit for the corp (I think)"
     :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                  :type :credit}}
     :events [(assoc abi :event :successful-trace)
              (assoc abi :event :unsuccessful-trace)]}))

(defcard "ONR Holovid Campaign"
  (campaign 12 1))

(defcard "ONR I Got a Rock"
  {:abilities [{:cost [:click 1 :agenda-point 3]
                :label "Do 15 meat damage"
                :msg "do 15 meat damage"
                :req (req (<= 2 (count-tags state)))
                :async true
                :effect (effect (damage eid :meat 15 {:card card}))}]})

(defcard "ONR Indiscriminate Response Team"
  {:implementation "effect is when the run ends - card needs to live for it to work"
   :events [{:event :run-ends
             :req (req (and (:successful target)
                            (seq (:hand runner))))
             :optional {:prompt "Have the runner draw a new hand?"
                        :waiting-prompt true
                        :yes-ability {:msg (msg "force the runner to shuffle their Grip into the Stack and draw " (count (:hand runner)) " cards")
                                      :async true
                                      :effect (req (let [cards (:hand runner)]
                                                     (doseq [c cards]
                                                       (move state :runner c :deck))
                                                     (shuffle! state :runner :deck)
                                                     (draw state :runner eid (count cards))))}}}]})

(defcard "ONR Information Laundering"
  {:advanceable :always
   :abilities [{:label "Gain 4 [Credits] for each advancement token"
                :cost [:click 1 :trash-can]
                :msg (msg "gain " (* 4 (get-counters card :advancement)) " [Credits]")
                :async true
                :effect (effect (gain-credits eid (* 4 (get-counters card :advancement))))}]})

(defcard "ONR Investment Firm"
  (let [ability {:msg "take 1 [Credits]"
                 :label "Take 1 [Credits] (start of turn)"
                 :once :per-turn
                 :req (req (pos? (get-counters card :credit)))
                 :async true
                 :effect (effect (add-counter card :credit -1)
                                 (gain-credits eid 1))}]
    {:implementation "Manual - click ONR Investment Firm to place credits counters on itself"
     :abilities [ability
                 {:label "Place 2 credits"
                  :cost [:credit 1]
                  :msg "place 2 credits on itself"
                  :effect (effect (add-counter card :credit 2))}]
     :events [(assoc ability :event :corp-turn-begins)]}))

(defcard "ONR Krumz"
  {:recurring 1
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(defcard "ONR LDL Traffic Analyzers"
  (let [remove-abi {:silent (req true)
                    :effect (req (let [c (get-counters (get-card state card) :credit)]
                                   (when (pos? c)
                                     (add-counter state side card :credit (- c)))))}]
    {:advanceable :always
     :implementation "ability automatically resolves after both players have bid"
     :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                  :type :credit}}
     :events [(assoc remove-abi :event :successful-trace)
              (assoc remove-abi :event :unsuccessful-trace)]}))

(defcard "ONR Nevinyrral"
  {:in-play [:click-per-turn 1]
   :on-rez {:effect (req (system-msg state side (str "uses " (:title card) " to gain 1 additional [Click] per turn"))
                         (when (= :corp (:active-player @state))
                           (gain-clicks state :corp 1))
                         (gain state :corp :click-per-turn 1))}
   :leave-play (req (when (rezzed? card)
                      (win state :runner "\"Executive Termination\"")))})

(defcard "ONR Newsgroup Taunting"
  {:events [{:event :run
             :interactive (req true)
             :effect (req (continue-ability
                            state side
                            (end-the-run-unless-runner-pays [:credit 1] "")
                            card nil))}]})

(defcard "ONR Omniscience Foundation"
  (let [abi {:msg "give the runner a tag"
             :effect (req (gain-tags state side eid 1))
             :async true
             :req (req (seq (turn-events state side :runner-gain-tag)))}]
    {:events [(assoc abi :event :corp-turn-ends)
              (assoc abi :event :runner-turn-ends)]}))

(defcard "ONR Pacifica Regional AI"
  {:advanceable :always
   :abilities [{:label "Gain [Click]"
                :msg "gain [Click]"
                :cost [:advancement 1]
                :req (req (= (:active-player @state) :corp))
                :effect (effect (gain-clicks 1))}]})

(defcard "ONR Pattel Antibody"
  (let [pattel-effect {:req (req (and (some #(has-subtype? % "Icebreaker") (all-installed state :runner))
                                      (or (rezzed? card)
                                          (and (not (installed? card))
                                               (not (in-discard? card))))))
                       :msg "Place a Pattel counter on each installed Icebreaker"
                       :effect (req (doseq [icebreaker (filter #(has-subtype? % "Icebreaker") (all-installed state :runner))]
                                      (add-counter state side icebreaker :corp-pattel 1))
                                    (register-effect-once
                                      state side card
                                      {:type :breaker-strength
                                       :ability-name "Pattel Antibodies"
                                       :req (req true)
                                       :value (req (- (get-counters (get-card state target) :corp-pattel)))}))}]
    {:implementation "These are different than the other pattel counters, and do not get removed on a purge"
     :flags {:rd-reveal (req true)}
     :on-access pattel-effect}))

(defcard "ONR Remote Facility"
  {:implementation "Click on the card to gain your click"
   :abilities [{:label "Gain [Click]"
                :msg "gain [Click]"
                :once :per-turn
                :req (req (= (:active-player @state) :corp))
                :effect (effect (gain-clicks 1))}]})

(defcard "ONR Rescheduler"
  {:abilities [{:cost [:click 1]
                :label  "shuffle all cards in HQ into R&D and draw that many cards"
                :async true
                :msg (msg "shuffle HQ into R&D and draw " (count (:hand corp)) " cards")
                :effect (req (let [cards (count (:hand corp))]
                               (shuffle-into-deck state side :hand)
                               (draw state side eid cards)))}]})

(defcard "ONR Rockerboy Promotion"
  {:data {:counter {:credit 15}}
   :events [(trash-on-empty :credit)]
   :abilities [{:label "Take 3 [Credits] from this asset"
                :cost [:click 1]
                :keep-menu-open :while-clicks-left
                :msg (msg "gain " (min 3 (get-counters card :credit)) " [Credits]")
                :async true
                :effect (req (let [credits (min 3 (get-counters card :credit))]
                               (wait-for (gain-credits state :corp (make-eid state eid) credits)
                                         (add-counter state side card :credit (- credits) {:placed true})
                                         (effect-completed state side eid))))}]})

(defcard "ONR Rustbelt HQ Branch"
  {:static-abilities [(corp-hand-size+ 2)]})

(defcard "ONR Satellite Monitors"
  (let [abi {:once :per-turn
             :async true
             :label "Roll the dice"
             :interactive (req true)
             :req (req (pos? (count (last-turn? state :runner :made-run))))
             :effect (req (let [di (dice-roll (count (last-turn? state :runner :made-run)))
                                tags (count (filter #(= 1 %) di))]
                            (continue-ability
                              state side
                              {:optional
                               {:prompt "Roll the dice?"
                                :autoresolve (get-autoresolve :auto-fire)
                                :yes-ability {:msg (msg "roll " (seq di) " and give the runner " tags " tags")
                                              :async true
                                              :effect (req (gain-tags state side eid tags))}}}
                              card nil)))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req (pos? (count (last-turn? state :runner :made-run))))}
     :events [(assoc abi :event :corp-turn-begins)]
     :abilities [abi (set-autoresolve :auto-fire "Satellite Monitors")]}))

(defcard "ONR Schlaghund"
  {:abilities [{:label "Roll for 10 meat"
                :cost [:click 1]
                :effect (req (let [di (dice-roll)
                                   tags (count-tags state)]
                               (continue-ability
                                 state side
                                 (if (<= di tags)
                                   {:msg (msg "roll " di ", do 10 meat damage, and trash itself")
                                    :effect (req (wait-for (damage state side (make-eid state eid) :meat 10)
                                                           (trash state side eid card)))
                                    :async true}
                                   {:msg (msg "roll " di)})
                                 card nil)))}]})

(defcard "ONR Solo Squad"
  {:abilities [{:req (req tagged)
                :cost [:click 1]
                :keep-menu-open :while-clicks-left
                :effect (effect (damage eid :meat 1 {:card card}))
                :msg "do 1 meat damage"}]})

(defcard "ONR Stereogram Antibody"
  {:on-access {:req (req (in-discard? card))
               :msg "do 1 net damage and shuffle itself into R&D"
               :async true
               :effect (req (wait-for (damage state side (make-eid state eid) :net 1 {:card card})
                                      (move state :corp card :deck nil)
                                      (shuffle! state :corp :deck)
                                      (effect-completed state side eid)))}})

(defcard "ONR Strategic Planning Group"
  {:derezzed-events [corp-rez-toast]
   :events [{:event :pre-corp-draw
             :msg "draw 1 additional card"
             ;; The req catches draw events that happened before the card was installed
             :effect (req (draw-bonus state side 1))}
            {:event :corp-draw
             :interactive (req true)
             :async true
             :effect (req (let [drawn corp-currently-drawing]
                            (continue-ability
                              state side
                              (when (seq drawn)
                                {:waiting-prompt true
                                 :prompt (str "Choose a card to add to the bottom of R&D")
                                 :choices {:max 1
                                           :card #(some (fn [c] (same-card? c %)) drawn)
                                           :all true}
                                 :effect (req (doseq [c (reverse targets)]
                                                (system-msg state side
                                                            (str "uses " (:title card) " to add the "
                                                                 (pprint/cl-format nil "~:R" (inc (first (keep-indexed #(when (same-card? c %2) %1) drawn))))
                                                                 " card drawn to the bottom of R&D"))
                                                (move state side c :deck)
                                                (remove-from-currently-drawing state side c)))})
                              card nil)))}]})
