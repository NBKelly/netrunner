(ns game.core.onr-trace
  (:require
   [game.core.board :refer [all-active-installed all-installed]]
   [game.core.card :refer [has-subtype? get-card rezzed? get-counters]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.effects :refer [any-effects sum-effects get-effects]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [can-trigger? checkpoint pay register-ability-type resolve-ability trigger-event-simult trigger-event-sync queue-event]]
   [game.core.link :refer [get-link]]
   [game.core.payment :refer [can-pay?]]
   [game.core.prompt-state :refer [add-to-prompt-queue remove-from-prompt-queue]]
   [game.core.prompts :refer [clear-wait-prompt show-wait-prompt]]
   [game.core.props :refer [add-counter]]
   [game.core.say :refer [system-msg system-say]]
   [game.macros :refer [continue-ability effect wait-for msg req]]))

;; onr trace protocol
;; the corp and the runner each secretly note down their bids
;; the runner chooses a link card to use
;; both players reveal credits
;; resolve trace
;;  Note that a tie means the corp wins

;; How I'm doing it:
;;  The corp secretly chooses a bid/trace strength
;;  The runner selects a base link to use
;;  The runner can use the abilities on the base-link to get to a link strength
;;  The runner clicks "Done", then the bids are compared
;;  Note that a tie means the corp wins
;; finally do everything else needed


(defn- update-link
  [state val]
  (swap! state assoc :onr-trace (merge (:onr-trace @state) {:link-changed val})))

(defn boost-link
  [state x]
  "Boosts link by X"
  (update-link state true)
  (swap! state assoc :onr-trace
         (merge (:onr-trace @state) {:boost (+ (or (get-in @state [:onr-trace :boost]) 0) x)})))

(defn- runner-spent-trace
  [state x]
  "Runner spends X during a trace"
  (swap! state assoc :onr-trace
         (merge (:onr-trace @state) {:runner-spent (+ (or (get-in @state [:onr-trace :runner-spent]) 0) x)})))

(defn set-base-link
  [state x]
  "Sets the base link to be X"
  (update-link state true)
  (swap! state assoc :onr-trace
         (merge (:onr-trace @state) {:base x})))

(defn- was-link-changed [state]
  (get-in @state [:onr-trace :link-changed]))

(defn cancel-successful-trace
  [state]
  (swap! state assoc :onr-trace
         (merge (:onr-trace @state) {:success-effect-cancelled true})))

(defn- successful-trace-cancelled
  [state]
  (get-in @state [:onr-trace :success-effect-cancelled]))

(defn- runner-spent-total
  [state]
  "how much the runner spent"
  (or (get-in @state [:onr-trace :runner-spent]) 0))

(defn- pretty-link
  [state]
  "displays link in a pretty way"
  (str " (base: " (or (get-in @state [:onr-trace :base]) 0)
       ", boost: " (or (get-in @state [:onr-trace :boost]) 0) ")"))

(defn- current-link
  [state]
  "Gives the raw link number"
  (+ (or (get-in @state [:onr-trace :boost]) 0)
     (or (get-in @state [:onr-trace :base]) 0)))

(defn- lose-x-hacker-tracker-creds
  [amt]
  {:req (req (pos? amt))
   :prompt (msg "Lose a hacker tracker credit" (when (> amt 1)
                                                 (str " (" amt " remaining)")))
   :async true
   :choices {:card #(and (= (:title %) "ONR Hacker Tracker Central")
                         (rezzed? %)
                         (pos? (get-counters % :credit)))}
   :effect (req (if (pos? (get-counters target :recurring))
                  (add-counter state side target :recurring -1)
                  (add-counter state side target :credit -1))
                (continue-ability
                  state side
                  (lose-x-hacker-tracker-creds (dec amt))
                  card nil))})

;; quantity should be guaranteed!
(defn- lose-from-hacker-tracker
  [amt]
  (if (integer? amt)
    {;;:req (req (pos? (count-stealth-creds state)))
     :waiting-prompt "Corp to resolve Hacker Tracker Central"
     :async true
     :effect (req ;;(let [max (min amt (count-stealth-creds state))]
               (continue-ability state side
                                 (lose-x-hacker-tracker-creds amt)
                                 card nil))}
    nil))

(defn- onr-resolve-trace
  [state side eid card {:keys [link-card max-strength label corp-credits corp-bid hacker-tracker
                               corp-strength] :as trace}]
  "The runner has already paid, now the corp pays. and then we resolve the effects"
  (let [runner-link (current-link state)
        success (>= corp-strength runner-link)]
    (clear-wait-prompt state :corp)
    ;; todo - we need to specially resolve hacker tracker expenditures!
    ;; so we count the strength above the max trace strength, and then that's how much MUST
    ;; be spend on hacker tracker central
    (let [hacker-tracker-overboost
          (if (pos? hacker-tracker)
            (- corp-strength max-strength)
            0)]
      (wait-for
        (resolve-ability
          state :corp
          (lose-from-hacker-tracker hacker-tracker-overboost)
          card nil)
        (when (pos? hacker-tracker-overboost)
          (system-msg state :corp (str "spends " hacker-tracker-overboost " [credits] from ONR Hacker Tracker Central(s) to increase the value and maximum strength of the trace")))
        (continue-ability
          state :corp
          {:cost [:credit (- corp-bid hacker-tracker-overboost)]
           :msg (msg "attempt to trace the Runner with Link Strength " corp-strength " - "
                     (if success
                       "beating" "losing to")
                     " the Runner's " runner-link " Link")
           :async true
           :effect (req
                     ;; now we interrupt, and potentially redefine
                     ;;(trigger-event-simult
                     ;; set this to false so we can see if it changes again!
                     (swap! state assoc :onr-trace (merge (:onr-trace @state) {:link-changed false}))
                     (queue-event state :trace-revealed {:card (get-card state card)})
                     (wait-for (checkpoint state nil (make-eid state eid))
                               (let [runner-link (current-link state)
                                     cheating (any-effects state side :trace-automatic-success)
                                     success (or (>= corp-strength runner-link) cheating)]
                                 (when (was-link-changed state)
                                   (system-msg state side (str "has had their trace attempt adjusted to Strength " corp-strength " - "
                                                               (if success
                                                                 (if cheating
                                                                   "automatically beating"
                                                                   "beating")
                                                                 "losing to")
                                                               " the Runner's " runner-link " Link")))
                                 (let [which-ability (assoc (if success
                                                              (:successful trace)
                                                              (:unsuccessful trace))
                                                            :eid (make-eid state))]
                                   (system-say state side (str "The trace was " (when-not success "un") "successful."))
                                   (wait-for (trigger-event-simult state :corp (if success :successful-trace :unsuccessful-trace)
                                                                   nil ;; No special functions
                                                                   {:corp-strength corp-strength
                                                                    :runner-strength runner-link
                                                                    :only-tags (:only-tags trace)
                                                                    :successful success
                                                                    :corp-spent corp-bid
                                                                    :runner-spent (runner-spent-total state)
                                                                    :ability (:ability trace)
                                                                    :base-link-card link-card})
                                             ;; it's possible for the effects of the trace to be cancelled by cards
                                             (if (successful-trace-cancelled state)
                                               (effect-completed state side eid)
                                               (wait-for (resolve-ability state :corp (:eid which-ability) which-ability
                                                                          card [corp-strength runner-link])
                                                         ;; there's no kicker functionality as far as I know
                                                         (effect-completed state side eid))))))))}
          card nil)))))

;; recursively handle runner link choices until the runner says "Done!"
(defn runner-link-repeat
  [state _ eid card {:keys [link-card trace-card] :as trace} abilities]
  (let [abis (filter #(can-pay? state :runner
                                (make-eid state (assoc eid :source-type :trace))
                                nil nil (and true (:cost %))) abilities)
        ;; todo - figure out why this doesn't work? ^^ - should be able to use the sourced credits
        ;;   -- actually I got it to work - but it doesn't without the and true there
        ;;      idk what the issue is?
        ;; you must establish base link before you can use the other abilities
        ;; and you may only establish base link once!
        abis (filter #(or (and (nil? (get-in @state [:onr-trace :base]))
                               (:onr-base-link %))
                          (and (get-in @state [:onr-trace :base])
                               (not (:onr-base-link %)))) abis)
        abis (filter #(can-trigger? state :runner (assoc eid :source link-card :source-type :trace) % link-card nil) abis)
        build-label (fn [abi] (str (:cost-label abi) ": " (:label abi)))
        labels (into [] (concat (map build-label abis) ["Done"]))
        zipped (map vector abis labels)
        ;; this is a little ghetto but whatever
        chosen-abi (fn [str] (first (first (filter #(= (second %) str) zipped))))
        current-link (str (current-link state) (pretty-link state))]
    {:prompt (str "Current link: " current-link)
     :choices labels
     :player :runner
     :async true
     :effect (req
               (if (= target "Done")
                 (onr-resolve-trace state corp eid trace-card trace)
                 (let [abi (chosen-abi target)]
                   (wait-for (resolve-ability state :runner (make-eid state (assoc eid :source link-card :source-type :trace)) abi card nil)
                             (runner-spent-trace state (second (:cost abi))) ;; this wont work if there are non credit costs!
                             (continue-ability state side
                                               (runner-link-repeat
                                                 state side eid card trace abilities)
                                               card nil)))))}))

(defn runner-link-abi
  [state _ eid card {:keys [link-card] :as trace}]
  ;; create a runner link interaction prompt from the card ability
  (let [abilities (filter #(or (:onr-base-link %) (:onr-boost-link %)) (:abilities link-card))]
    (runner-link-repeat state _ eid card trace abilities)))

(defn- onr-runner-trace-response
  [state side eid card {:keys [label runner-credits runner-links runner-bonus-link] :as trace}]
  ;; if there are no links, then the runner can't really do anything
  (boost-link state runner-bonus-link)
  (if-not (seq runner-links)
    ;; there's no links possible
    (onr-resolve-trace state side eid card trace)
    (continue-ability
      state side
      {:prompt "Choose a Base Link card to use"
       :waiting-prompt "Runner to resolve Link"
       :player :runner
       :async true
       :choices runner-links
       :effect (req (continue-ability
                      state :runner
                      (runner-link-abi state :runner eid card (assoc trace :link-card target))
                      target nil))}
      card nil)))

(defn- onr-trace-start
  "Starts the onr trace process by having the corp secretly decide how many credits to spend"
  [state side eid card {:keys [runner-bonus-link max-strength hacker-tracker label corp-credits corp-extra-bid-cost] :as trace}]
  (system-msg state side (str "uses " (:title card) " to initiate a trace with max strength "
                              max-strength
                              (when label
                                (str " (" label ")"))))
  (show-wait-prompt state :runner
                    (str "Corp to secretly bid on the trace"))
  (let [corp-bid-cost (+ 1 (sum-effects state side :corp-trace-bid-additional-cost)
                         (or corp-extra-bid-cost 0))
        max-bid (min (+ max-strength hacker-tracker) (quot corp-credits corp-bid-cost))
        runner-link (filter #(has-subtype? % "Base Link") (all-active-installed state :runner))]
  ;  ;; if there is no link card, then the trace automatically succeeds?
    ;; The corp still needs to bid though
    (continue-ability
      state :corp
      {:prompt (msg "secretly boost the trace up to "
                    max-bid
                    (when (pos? hacker-tracker)
                      (str "(" max-strength " base, " hacker-tracker " possible Hacker Trackers )"))
                    (when (pos? runner-bonus-link)
                      (str "(runner has " runner-bonus-link " bonus link)"))
                    (when (> corp-bid-cost 1)
                      (str "(each point costs " corp-bid-cost " [Credits])")))
       :choices {:number (req max-bid)}
       :async true
       :effect (req
                 (clear-wait-prompt state :runner)
                 (onr-runner-trace-response state side eid card (assoc trace
                                                                       :corp-strength target
                                                                       :corp-bid (* target corp-bid-cost)
                                                                       :runner-links runner-link
                                                                       :trace-card card)))}
      card nil)))

(defn- reset-onr-trace-modifications
  [state]
  (swap! state assoc :onr-trace nil))

(defn onr-init-trace
  ([state side card] (onr-init-trace state side (make-eid state {:source-type :trace}) card {:max-strength 0}))
  ([state side card trace] (onr-init-trace state side (make-eid state {:source-type :trace}) card trace))
  ([state side eid card {:keys [max-strength] :as trace}]
   (reset-onr-trace-modifications state)
   (wait-for (trigger-event-sync state :corp :initialize-onr-trace card eid)
             (let [eid (assoc eid :source-type :trace)
                   corp-credits (total-available-credits state :corp eid card)
                   runner-credits (total-available-credits state :runner eid card)
                   hacker-trackers (filter #(and (rezzed? %)
                                                 (= (:title %) "ONR Hacker Tracker Central"))
                                           (all-installed state :corp))
                   hacker-tracker-credits (map #(get-counters % :credit) hacker-trackers)
                   hacker-tracker-credits (reduce + hacker-tracker-credits)
                   runner-bonus-link (+ (or (sum-effects state side :link-for-run) 0) (:baselink (:identity (:runner @state))))
                   max-str-adjust (or (sum-effects state side :max-strength card) 0)
                   trace (merge trace {:player :corp
                                       :max-strength (max (+ max-strength max-str-adjust) 0)
                                       :hacker-tracker (or hacker-tracker-credits 0)
                                       :runner-bonus-link runner-bonus-link
                                       :corp-credits corp-credits
                                       :runner-credits runner-credits})]
               (reset-onr-trace-modifications state)
               (onr-trace-start state side eid card trace)))))

(defn- onr-check-trace
  "Checks if there is a trace to resolve"
  [state side {:keys [eid onr-trace] :as ability} card targets]
  (if (can-trigger? state side eid ability card targets)
    (resolve-ability
      state side
      (-> ability
          (dissoc :onr-trace :req)
          (assoc :async true
                 :effect (effect (onr-init-trace eid card onr-trace))))
      card targets)
    (effect-completed state side eid)))

(register-ability-type :onr-trace #'onr-check-trace)
