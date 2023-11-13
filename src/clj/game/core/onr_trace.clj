(ns game.core.onr-trace
  (:require
   [game.core.board :refer [all-active-installed]]
   [game.core.card :refer [has-subtype?]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.effects :refer [any-effects sum-effects get-effects]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [can-trigger? pay register-ability-type resolve-ability trigger-event-simult trigger-event-sync]]
   [game.core.link :refer [get-link]]
   [game.core.payment :refer [can-pay?]]
   [game.core.prompt-state :refer [add-to-prompt-queue remove-from-prompt-queue]]
   [game.core.prompts :refer [clear-wait-prompt show-wait-prompt]]
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


(defn boost-link
  [state x]
  "Boosts link by X"
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
  (swap! state assoc :onr-trace
         (merge (:onr-trace @state) {:base x})))

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

(defn- onr-resolve-trace
  [state side eid card {:keys [link-card max-strength label corp-credits corp-bid] :as trace}]
  "The runner has already paid, now the corp pays. and then we resolve the effects"
  (let [runner-link (current-link state)
        success (>= corp-bid runner-link)
        corp-strength corp-bid]
    (clear-wait-prompt state :corp)
    ;;(show-wait-prompt state :corp
    ;;                (str "Runner to resolve Link"))
    ;; TODO - there's an interrupt to insert here at some point
    ;; the runner can manipulate a trace result after the trace succeeds
    ;; maybe even the corp too?
    (continue-ability
      state :corp
      {:cost [:credit corp-bid]
       :msg (msg "attempt to trace the Runner - "
                 (if success
                   "beating" "losing to")
                 " the Runner's " runner-link " Link")
       :async true
       :effect (req
                 (let [which-ability (assoc (if success
                                              (:successful trace)
                                              (:unsuccessful trace))
                                            :eid (make-eid state))]
                   (system-say state side (str "The trace was " (when-not success "un") "successful."))
                   (wait-for (trigger-event-simult state :corp (if success :successful-trace :unsuccessful-trace)
                                                   nil ;; No special functions
                                                   {:corp-strength corp-bid ;;TODO - this might be wrong
                                                    :runner-strength runner-link
                                                    :successful success
                                                    :corp-spent corp-bid
                                                    :runner-spent (runner-spent-total state)
                                                    :ability (:ability trace)
                                                    :base-link-card link-card})
                             (wait-for (resolve-ability state :corp (:eid which-ability) which-ability
                                                card [corp-strength runner-link])
                                       ;; there's no kicker functionality as far as I know
                                       (effect-completed state side eid)))))}
      card nil)))

;; recursively handle runner link choices until the runner says "Done!"
(defn runner-link-repeat
  [state _ eid card {:keys [link-card trace-card] :as trace} abilities]
  (let [abis (filter #(can-pay? state :runner nil
                               (assoc eid :source link-card :source-type :resource)
                               nil nil (:cost %)) abilities)
        ;; you must establish base link before you can use the other abilities
        ;; and you may only establish base link once!
        abis (filter #(or (and (nil? (get-in @state [:onr-trace :base]))
                               (:onr-base-link %))
                          (and (get-in @state [:onr-trace :base])
                               (not (:onr-base-link %)))) abis)
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
                   (wait-for (resolve-ability state :runner abi card nil)
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
  [state side eid card {:keys [label runner-credits corp-bid runner-links] :as trace}]
  ;; if there are no links, then the runner can't really do anything
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
  [state side eid card {:keys [max-strength label corp-credits] :as trace}]
  (system-msg state side (str "uses " (:title card) " to initiate a trace with max strength "
                              max-strength
                              (when label
                                (str " (" label ")"))))
  (show-wait-prompt state :runner
                    (str "Corp to secretly bid on the trace"))
  (let [max-bid (min max-strength corp-credits)
        runner-link (filter #(has-subtype? % "Base Link") (all-active-installed state :runner))]
  ;  ;; if there is no link card, then the trace automatically succeeds?
    ;; The corp still needs to bid though
    (continue-ability
      state :corp
      {:prompt (msg "secretly bid up to " max-bid " credits")
       :choices {:number (req max-bid)}
       :async true
       :effect (req
                 (clear-wait-prompt state :runner)
                 (onr-runner-trace-response state side eid card (assoc trace :corp-bid target
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
                   trace (merge trace {:player :corp
                                       :max-strength max-strength
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
