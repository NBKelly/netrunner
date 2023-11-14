(ns game.cards.onr-ice
  (:require
   [clojure.string :as str]
   [cond-plus.core :refer [cond+]]
   [game.core.access :refer [access-bonus access-card breach-server max-access]]
   [game.core.bad-publicity :refer [gain-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed all-installed-runner-type card->server
                            get-all-cards get-all-installed server->zone]]
   [game.core.card :refer [active? agenda? asset? can-be-advanced? card-index
                           corp? corp-installable-type? faceup?
                           get-card get-counters get-zone
                           hardware? has-subtype? ice? in-discard? in-hand? installed? is-type? operation?
                           program? protecting-a-central? protecting-archives? protecting-hq? protecting-rd?
                           resource? rezzed? runner?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage]]
   [game.core.def-helpers :refer [combine-abilities corp-recur defcard
                                  do-brain-damage do-net-damage offer-jack-out
                                  reorder-choice get-x-fn]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [get-effects register-lingering-effect unregister-static-abilities]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid]]
   [game.core.engine :refer [gather-events pay register-events resolve-ability
                             trigger-event trigger-event-simult unregister-events]]
   [game.core.events :refer [run-events]]
   [game.core.finding :refer [find-cid]]
   [game.core.flags :refer [can-rez? card-flag? prevent-draw prevent-jack-out
                            register-run-flag! register-turn-flag! run-flag? zone-locked?]]
   [game.core.gaining :refer [gain-credits lose-clicks lose-credits]]
   [game.core.hand-size :refer [hand-size]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [add-sub add-sub! any-subs-broken? break-sub get-current-ice ice-strength-bonus
                          remove-sub! remove-subs! resolve-subroutine
                          set-current-ice unbroken-subroutines-choice update-all-ice update-all-icebreakers
                          update-ice-strength]]
   [game.core.identities :refer [disable-card enable-card]]
   [game.core.initializing :refer [card-init]]
   [game.core.installing :refer [corp-install corp-install-list
                                 corp-install-msg]]
   [game.core.memory :refer [available-mu init-mu-cost]]
   [game.core.moving :refer [as-agenda mill move swap-cards swap-cards-async
                             swap-ice swap-installed trash
                             trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay? cost->string build-cost-label]]
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-icon add-prop remove-icon]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez get-rez-cost rez]]
   [game.core.runs :refer [bypass-ice encounter-ends end-run
                           force-ice-encounter get-current-encounter prevent-access
                           redirect-run set-next-phase]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name protecting-same-server?
                              target-server zone->name]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.subtypes :refer [update-all-subtypes]]
   [game.core.tags :refer [gain-tags lose-tags sum-tag-effects]]
   [game.core.threat :refer [threat threat-level]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]
   ;; imported from ice
   [game.cards.ice :refer [end-the-run end-the-run-unless-runner-pays gain-credits-sub give-tags trash-program-sub]]
   ))

(defn onr-trace-ability
  "Run a trace with specified max strength.
  If successful trigger specified ability"
  ([max {:keys [label] :as ability} only-tags]
   {:label (str "Trace " max " - " label)
    :onr-trace {:max-strength max
                :label label
                :only-tags only-tags
                :successful ability}})
  ([max ability un-ability only-tags]
   (let [label (str (:label ability) " / " (:label un-ability))]
     {:label (str "Trace " max " - " label)
      :onr-trace {:max-strength max
                  :label label
                  :only-tags only-tags
                  :successful ability
                  :unsuccessful un-ability}})))

(defn- bounce-unless-corp-pays
  ([cost]
   (let [cost (if (integer? cost) [:credit cost] cost)]
     {:event :pass-ice
      :req (req (same-card? (:ice context) card))
      :optional {:prompt (msg "Pay " cost "[Credits] to keep " (:title card) " on the field?")
                 :yes-ability {:cost cost
                               :msg (msg "to keep " (card-str state card) " on the field")}
                 :no-ability {:msg (msg "add " (card-str state card) " to HQ")
                              :effect (effect (move :corp card :hand))}}})))

(defn- used-noisy-discount
  ([state cost]
   (letfn [(find-breaker-in-run [cid state]
             (let [evs (filter #(and (= :breaker-strength-changed (first %)) (= cid (:cid (first (second %))))) (:events (:run @state)))]
               (first (map #(first (second %)) evs))))
           ;; this finds the cids of every icebreaker used to break a subroutine during this run
           (find-breaker-cids [state]
             (let [broken-sub-events (filter #(= :subroutines-broken (first %)) (:events (:run @state)))
                   ;; this should just explicitly be the ices that were broken this run
                   broken-sub-events (map #(:subroutines (first (second %))) broken-sub-events)
                   broken-subs (filter :broken (flatten broken-sub-events))]
               (map :breaker broken-subs)))
           (noisy-breaker-broke-subs-this-run [state]
             (let [breakers (map #(find-breaker-in-run % state) (find-breaker-cids state))]
               (some #(= "Noisy" %) (first (map :subtypes breakers)))))
           (pump-breaker-events [state]
             (map second (filter #(or (= :pump-breaker (first %)) (= :subroutines-broken (first %))) (:events (:run @state)))))
           (noisy-was-pumped [state]
             (some #(has-subtype? % "Noisy") (map first (pump-breaker-events state))))]
     ;; wow! what a nightmare
     (if (or (noisy-breaker-broke-subs-this-run state)
             (noisy-was-pumped state))
       (- cost) 0))))
;; card implementations

(defcard "ONR Banpei"
  {:subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Brain Wash"
  {:subroutines [(do-brain-damage 1)]})

(defcard "ONR Canis Major"
  {:subroutines [{:label (str "All further ice is encountered at +2 Strength")
                  :msg "Make all further ice be encountered at +2 strength this run."
                  :effect (effect (register-lingering-effect
                                    card
                                    {:type :ice-strength
                                     :duration :end-of-run
                                     :req (req (and (get-current-encounter state)
                                                    (same-card? current-ice target)))
                                     :value +2}))}]})

(defcard "ONR Canis Minor"
  {:subroutines [{:label (str "All further ice is encountered at +1 Strength")
                  :msg "Make all further ice be encountered at +1 strength this run."
                  :effect (effect (register-lingering-effect
                                    card
                                    {:type :ice-strength
                                     :duration :end-of-run
                                     :req (req (and (get-current-encounter state)
                                                    (same-card? current-ice target)))
                                     :value +1}))}]})

(defcard "ONR Code Corpse"
  {:subroutines [(do-brain-damage 1)
                 (do-brain-damage 1)
                 end-the-run]})

(defcard "ONR Colonel Failure"
  {:subroutines [trash-program-sub
                 trash-program-sub
                 trash-program-sub
                 end-the-run
                 end-the-run]})

(defcard "ONR Cortical Scanner"
  {:subroutines[end-the-run
                end-the-run
                end-the-run]})

(defcard "ONR Cortical Scrub"
  {:subroutines [(do-brain-damage 1)
                 end-the-run]})

(defcard "ONR Crystal Wall"
  {:subroutines[end-the-run]})

(defcard "ONR D'Arc Knight"
  {:subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Datacomb"
  {:subroutines [end-the-run]
   :events [(bounce-unless-corp-pays 1)]})

(defcard "ONR Data Naga"
  {:subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Data Wall"
  {:subroutines [end-the-run]})

(defcard "ONR Data Wall 2.0"
  {:subroutines [end-the-run]})

(defcard "ONR Deadeye"
  {:rez-cost-bonus (req (used-noisy-discount state 5))
   :subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Endless Corridor"
  {:subroutines [end-the-run
                 end-the-run]})

(defcard "ONR Fetch 4.0.1"
  {:subroutines [(onr-trace-ability 3 (give-tags 1) true)]})

(defcard "ONR Filter"
  {:subroutines [end-the-run]})

(defcard "ONR Fire Wall"
  {:subroutines [end-the-run]})

(defcard "ONR Ice Pick Willie"
  {:subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Keeper"
  {:subroutines [end-the-run]})

(defcard "ONR Laser Wire"
  {:subroutines [(do-net-damage 1)
                 end-the-run]})

(defcard "ONR Liche"
  {:subroutines [(do-brain-damage 1)
                 (do-brain-damage 1)
                 (do-brain-damage 1)
                 end-the-run]})

(defcard "ONR Mazer"
  {:subroutines [end-the-run]})

(defcard "ONR Nerve Labyrinth"
  {:subroutines [(do-net-damage 2)
                 end-the-run]})

(defcard "ONR Misleading Access Menus"
  {:on-rez (gain-credits-sub 3)
   :subroutines [(end-the-run-unless-runner-pays [:credit 1])]})

(defcard "ONR π in the 'Face"
  {:subroutines [end-the-run]})

(defcard "ONR Razor Wire"
  {:subroutines [(do-net-damage 2)
                 end-the-run]})

(defcard "ONR Reinforced Wall"
  {:subroutines [end-the-run
                 end-the-run]})

(defcard "ONR Rock is Strong"
  {:subroutines [end-the-run]})

(defcard "ONR Scramble"
  {:subroutines [end-the-run]})

(defcard "ONR Sentinels Prime"
  {:subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Shotgun Wire"
  {:subroutines [(do-net-damage 2)
                 end-the-run]})

(defcard "ONR Sleeper"
  {:subroutines [end-the-run]})

(defcard "ONR Snowbank"
  {:on-rez (gain-credits-sub 3)
   :subroutines [(end-the-run-unless-runner-pays [:credit 1])]})

(defcard "ONR Toughonium [TM] Wall"
  {:subroutines [end-the-run
                 end-the-run
                 end-the-run
                 end-the-run]})

(defcard "ONR Triggerman"
  {:subroutines [trash-program-sub
                 end-the-run]})

(defcard "ONR Quandary"
  {:subroutines [end-the-run]})

(defcard "ONR Wall of Ice"
  {:subroutines [(do-net-damage 2)
                 (do-net-damage 2)
                 end-the-run
                 end-the-run]})

(defcard "ONR Wall of Static"
  {:subroutines [end-the-run]})

(defcard "ONR Zombie"
  {:subroutines [(do-brain-damage 1)
                 (do-brain-damage 1)
                 end-the-run]})
