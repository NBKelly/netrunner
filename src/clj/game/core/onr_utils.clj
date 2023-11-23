(ns game.core.onr-utils
  (:require
   [game.core.card :refer [has-subtype? get-card in-discard? installed? rezzed?]]
   [game.core.damage :refer [damage-prevent]]
   [game.core.effects :refer [gather-effects register-lingering-effect]]
   [game.core.eid :refer [complete-with-result make-eid]]
   [game.core.engine :refer [not-used-once? register-events gather-events]]
   [game.core.gaining :refer [lose-click-debt gain-clicks safe-inc-n sub->0]]
   [game.core.onr-trace :refer [boost-link set-base-link cancel-successful-trace]]
   [game.core.props :refer [add-counter]]
   [game.core.say :refer [system-msg]]
   [game.core.tags :refer [gain-tags]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer [quantify]]
   ))

(defn gain-runner-counter
  ([state side type id]
   (add-counter state :runner (get-card state id) type 1)
   (swap! state update-in [:runner type] (safe-inc-n 1))))

(defn lose-runner-counter
  ([state side type id]
   (add-counter state :runner (get-card state id) type -1)
   (swap! state update-in [:runner type] (sub->0 1))))

(defn noisy-breaker-used
  ([state]
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
     (or (noisy-breaker-broke-subs-this-run state)
         (noisy-was-pumped state)))))

(defn gain-or-forgo
  [state side eid]
  (if (pos? (or (get-in @state [side :action-debt]) 0))
    (wait-for (lose-click-debt state side (make-eid state eid) 1)
              (complete-with-result state side eid "Forgo Click"))
    (do (gain-clicks state side 1)
        (complete-with-result state side eid "Gain Click"))))

(defn- give-tags
  "Basic give runner n tags subroutine."
  [n]
  {:label (str "Give the Runner " (quantify n "tag"))
   :msg (str "give the Runner " (quantify n "tag"))
   :async true
   :effect (effect (gain-tags :corp eid n))})

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

(defn onr-trace-tag
  ([max] (onr-trace-tag max 1))
  ([max tags]
   (onr-trace-ability max (give-tags tags) true)))

(defn ambush-outside-archives [card]
  (and (not (in-discard? card))
       (or (not (installed? card))
           (rezzed? card))))

(defn handle-if-unique
  ([state side card handler] (handle-if-unique state side card handler nil))
  ([state side card handler targets] (handle-if-unique state side card handler targets false))
  ([state side card handler targets debug]
   (let [matching-events
         (seq (filter #(= (:ability-name handler) (:ability-name (:ability %)))
                      (gather-events state side (:event handler) targets)))]
     (when debug
       (do
         (system-msg state side (str "event type: " (:event handler)))
         (system-msg state side (str (gather-events state side (:event handler) targets)))
         ))
     (when-not matching-events
       (do (when debug (system-msg state side (str "registered " (:ability-name handler))))
           (register-events state side card [handler]))))))

(defn register-effect-once [state side card effect]
  (let [em (gather-effects state side (:type effect))
        matches (filter #(= (:ability-name %) (:ability-name effect)) em)]
    (when (empty? matches)
      (register-lingering-effect
        state side card
        effect))))

(defn base-link-abi
  [cost val]
  (let [cost (if (integer? cost) [:credit cost] cost)]
    {:onr-base-link true
     :req (req true)
     :cost cost
     :base-link val
     :label (str "Base Link " val)
     :msg (str "set their Base Link to " val)
     :effect (req (set-base-link state val))}))

(defn boost-link-abi
  [cost val]
  (let [cost (if (integer? cost) [:credit cost] cost)]
    {:onr-boost-link true
     :cost cost
     :label (str "+" val " Link")
     :msg (str "gain +" val " Link")
     :effect (req (boost-link state val))}))

(defn dice-roll
  ([] (inc (rand-int 6)))
  ([x] (if-not (pos? x)
         []
         (if (= x 1)
           [(dice-roll)]
           (concat [(dice-roll)] (dice-roll (dec x)))))))

(defn deep-merge [a & maps]
   (if (map? a)
     (apply merge-with deep-merge a maps)
     (apply merge-with deep-merge maps)))

(defn generic-prevent-damage
  ([x type]
   {:interactions {:prevent [{:type #{type}
                              :req (req (not-used-once? state {:once :per-turn} card))}]}
    :abilities [{:cost [:credit 0]
                 :once :per-turn
                 :label (str "Prevent " x " " (name type) " damage")
                 :msg (msg "prevent " x " " (name type) " damage")
                 :effect (effect (damage-prevent type x))}]})
  ([x typea typeb]
   {:interactions {:prevent [{:type #{typea typeb}
                              :req (req (not-used-once? state {:once :per-turn} card))}]}
    :abilities [{:cost [:credit 0]
                 :once :per-turn
                 :label (str "Prevent " x " " (name typea) " or " (name typeb) " damage")
                 :msg (msg "prevent " x " " (name typea) " or " (name typeb) " damage")
                 :effect (effect (damage-prevent typea x)
                                 (damage-prevent typeb x))}]}))
