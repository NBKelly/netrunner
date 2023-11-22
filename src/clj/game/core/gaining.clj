(ns game.core.gaining
  (:require
   [game.core.agendas :refer [update-all-agenda-points]]
   [game.core.say :refer [system-msg]]
   [game.core.eid :refer [make-eid effect-completed]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.engine :refer [trigger-event trigger-event-sync]]
   [game.core.winning :refer [check-win-by-agenda]]
   [game.macros :refer [req wait-for]]
   [game.core.toasts :refer [toast]]))

(defn safe-inc-n
  "Helper function to safely update a value by n. Returns a function to use with `update` / `update-in`"
  [n]
  (partial (fnil + 0 0) n))

(defn sub->0
  "Helper function for use in `update` or `update-in` to subtract for a value, to a minimum of 0."
  [n]
  #(max 0 ((fnil - 0 0) % n)))

(defn deduct
  "Deduct the value from the player's attribute."
  [state side [attr value]]
  (cond
    ;; value is a map, should be :base, :mod, etc.
    (map? value)
    (doseq [[subattr value] value]
      (swap! state update-in [side attr subattr] (if (#{:mod :used} subattr)
                                                   ;; Modifications and mu used may be negative
                                                   ;; mu used is for easier implementation of the 0-mu hosting things
                                                   #(- % value)
                                                   (sub->0 value))))

    ;; values that expect map, if passed a number use default subattr of :mod
    (#{:memory} attr)
    (deduct state side [attr {:mod value}])

    ;; default case for tags and bad-publicity is `:base`
    (#{:tag :bad-publicity} attr)
    (deduct state side [attr {:base value}])

    :else
    (do (swap! state update-in [side attr] (if (= attr :agenda-point)
                                             ;; Agenda points may be negative
                                             #(- % value)
                                             (sub->0 value)))
        (when (and (= attr :credit)
                   (= side :corp)
                   (pos? (get-in @state [:corp :run-credit] 0)))
          (swap! state update-in [:corp :run-credit] (sub->0 value)))
        (when (and (= attr :credit)
                   (= side :runner)
                   (pos? (get-in @state [:runner :run-credit] 0)))
          (swap! state update-in [:runner :run-credit] (sub->0 value))))))

(defn gain [state side & args]
  (doseq [[cost-type amount] (partition 2 args)]
    (cond
      ;; amount is a map, merge-update map
      (map? amount)
      (doseq [[subtype amount] amount]
        (swap! state update-in [side cost-type subtype] (safe-inc-n amount))
        (swap! state update-in [:stats side :gain cost-type subtype] (fnil + 0) amount))

      ;; Default cases for the types that expect a map
      (#{:hand-size :memory} cost-type)
      (gain state side cost-type {:mod amount})

      ;; Default case for tags and bad publicity is `:base`
      (#{:tag :bad-publicity} cost-type)
      (gain state side cost-type {:base amount})

      ;; Else assume amount is a number and try to increment cost-type by it.
      :else
      (do (swap! state update-in [side cost-type] (safe-inc-n amount))
          (swap! state update-in [:stats side :gain cost-type] (fnil + 0 0) amount)))
    (trigger-event state side (if (= side :corp) :corp-gain :runner-gain) [cost-type amount])))

(defn lose [state side & args]
  (doseq [[cost-type amount] (partition 2 args)]
    (if (= amount :all)
      (do (swap! state update-in [:stats side :lose cost-type] (fnil + 0) (get-in @state [side cost-type]))
          (swap! state assoc-in [side cost-type] 0))
      (do (when (number? amount)
            (swap! state update-in [:stats side :lose cost-type] (fnil + 0) amount))
          (deduct state side [cost-type amount])))
    (trigger-event state side (if (= side :corp) :corp-lose :runner-lose) [cost-type amount])))

(defn lose-agenda-point-debt
  "Utility function for losing AP debt"
  ([state side eid amount] (lose-agenda-point-debt state side eid amount nil))
  ([state side eid amount args]
   (if (and amount
            (or (= :all amount)
                (pos? amount))
            (pos? (:agenda-point-debt (side @state))))
     (do (lose state side :agenda-point-debt amount)
         (swap! state update-in [side :register :forfiet-agenda-points] #(+ (or % 0) amount))
         (trigger-event-sync state side eid (if (= :corp side) :corp-agenda-point-debt-loss :runner-agenda-point-debt-loss) amount args))
     (effect-completed state side eid))))

(defn lose-agenda-points
  "Utility for forfieting agenda points. This is different to costs"
  ([state side eid amount] (lose-agenda-points state side eid amount nil))
  ([state side eid amount args]
   (if (and amount
            (pos? amount))
     (let [saved-side side]
       (do (register-lingering-effect
             state side nil
             {:type :user-agenda-points
              ;; `target` is either `:corp` or `:runner`
              :req (req (= saved-side target))
              :value (- amount)})
           (swap! state update-in [side :register :forfiet-agenda-points] #(+ (or % 0) amount))
           (update-all-agenda-points state side)
           (check-win-by-agenda state side)
           (trigger-event-sync state side eid (if (= :corp side) :corp-agenda-point-forfieted :runner-agenda-point-forfieted) amount args)
           (effect-completed state side eid))))))

(defn gain-agenda-points
  "Utility function to pass fake AP through the debt system"
  ([state side eid amount] (gain-agenda-points state side eid amount nil))
  ([state side eid amount args]
   (if (and amount
            (pos? amount))
     ;; do we owe debt?
     (let [ap-debt (or (get-in @state [side :agenda-point-debt]) 0)
           saved-side side]
       (if (zero? ap-debt)
         (do (register-lingering-effect
               state side nil
               {:type :user-agenda-points
                ;; `target` is either `:corp` or `:runner`
                :req (req (= saved-side target))
                :value amount})
             (swap! state update-in [side :register :scored-agenda] #(+ (or % 0) amount))
             (update-all-agenda-points state side)
             (check-win-by-agenda state side)
             (trigger-event-sync state side eid (if (= :corp side) :corp-agenda-point-gain :runner-agenda-point-gain) amount args)
             (effect-completed state side eid))
         (if (>= ap-debt amount) ;; this looks correct to me
           (do
             (system-msg state side (str "forfiets " amount " agenda points"))
             (lose-agenda-point-debt state side eid amount args))
           (do
             (system-msg state side (str "forfiets " ap-debt " agenda points"))
             (wait-for (lose-agenda-point-debt state side eid ap-debt args)
                       (gain-agenda-points state side eid (- amount ap-debt))))))))))

(defn gain-agenda-point-debt
  "Utility function for agenda debt"
  ([state side eid amount] (gain-agenda-point-debt state side eid amount nil))
  ([state side eid amount args]
   (if (and amount
            (pos? amount))
     (do (gain state side :agenda-point-debt amount)
         (trigger-event-sync state side eid (if (= :corp side) :corp-agenda-point-debt-gain :runner-agenda-point-debt-gain) amount args))
     (effect-completed state side eid))))

(defn gain-click-debt
  "Utility function for gaining click debt"
  ([state side eid amount] (gain-click-debt state side eid amount nil))
  ([state side eid amount args]
   (if (and amount
            (pos? amount))
     (do (gain state side :action-debt amount)
         (trigger-event-sync state side eid (if (= :corp side) :corp-action-debt-gain :runner-action-debt-gain) amount args))
     (effect-completed state side eid))))

(defn gain-debt
  "Utility function for gaining debt"
  ([state side eid amount] (gain-debt state side eid amount nil))
  ([state side eid amount args]
   (if (and amount
            (pos? amount))
     (do (gain state side :debt amount)
         (trigger-event-sync state side eid (if (= :debt side) :corp-debt-gain :runner-debt-gain) amount args))
     (effect-completed state side eid))))

(defn lose-click-debt
  "Utility function for triggering events"
  ([state side eid amount] (lose-click-debt state side eid amount nil))
  ([state side eid amount args]
   (if (and amount
            (or (= :all amount)
                (pos? amount))
            (pos? (:action-debt (side @state))))
     (do (lose state side :action-debt amount)
         (trigger-event-sync state side eid (if (= :corp side) :corp-action-debt-loss :runner-action-debt-loss) amount args))
     (effect-completed state side eid))))

(defn lose-debt
  "Utility function for triggering events"
  ([state side eid amount] (lose-debt state side eid amount nil))
  ([state side eid amount args]
   (if (and amount
            (or (= :all amount)
                (pos? amount))
            (pos? (:debt (side @state))))
     (do (lose state side :debt amount)
         (trigger-event-sync state side eid (if (= :corp side) :corp-debt-loss :runner-debt-loss) amount args))
     (effect-completed state side eid))))

(defn gain-credits
  "Utility function for triggering events"
  ([state side eid amount] (gain-credits state side eid amount nil))
  ([state side eid amount args]
   (if (and amount
            (pos? amount))
     (do (gain state side :credit amount)
         (trigger-event-sync state side eid (if (= :corp side) :corp-credit-gain :runner-credit-gain) amount args))
     (effect-completed state side eid))))

(defn lose-credits
  "Utility function for triggering events"
  ([state side eid amount] (lose-credits state side eid amount nil))
  ([state side eid amount args]
   (if (and amount
            (or (= :all amount)
                (pos? amount))
            (pos? (:credit (side @state))))
     (do (lose state side :credit amount)
         (when (and (= side :runner)
                    (= :all amount))
           (lose state :runner :run-credit :all))
         (when (and (= side :corp)
                    (= :all amount))
           (lose state :corp :run-credit :all))
         (trigger-event-sync state side eid (if (= :corp side) :corp-credit-loss :runner-credit-loss) amount args))
     (effect-completed state side eid))))

(defn gain-clicks
  ([state side amount] (gain-clicks state side amount nil))
  ([state side amount args]
    (when (and amount
               (pos? amount))
      (do (gain state side :click amount)
          (trigger-event state side (if (= :corp side) :corp-click-gain :runner-click-gain) amount args)))))

(defn lose-clicks
  ([state side amount] (lose-clicks state side amount nil))
  ([state side amount args]
    (when (and amount
               (or (= :all amount)
                   (pos? amount)))
      (do (lose state side :click amount)
          (trigger-event state side (if (= :corp side) :corp-click-loss :runner-click-loss) amount args)))))

;;; Stuff for handling {:base x :mod y} data structures
(defn base-mod-size
  "Returns the value of properties using the `base` and `mod` system"
  [state side prop]
  (let [base (get-in @state [side prop :base] 0)
        mod (get-in @state [side prop :mod] 0)]
    (+ base mod)))
