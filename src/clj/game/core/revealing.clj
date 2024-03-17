(ns game.core.revealing
  (:require
    [game.core.engine :refer [trigger-event-sync]]))

(defn reveal-hand
  "Reveals a side's hand to opponent and spectators."
  [state side]
  (swap! state assoc-in [side :openhand] true))

(defn conceal-hand
  "Conceals a side's revealed hand from opponent and spectators."
  [state side]
  (swap! state update side dissoc :openhand))

(defn reveal
  "Trigger the event for revealing one or more cards."
  [state side eid & targets]
  (apply trigger-event-sync state side eid (if (= :corp side) :corp-reveal :runner-reveal) (flatten targets)))

(defn reveal-and-move
  "Reveals and moves cards to another location"
  [state side eid cards to move-args]
  (if (seq cards)
    (wait-for (reveal state side eid (first card))
              (when-let [c (get-card state (first card))]
                (move state side c to move-args)
              (reveal-and-move-multiple state side eid (next cards) to move-args)))
    (effect-completed state side eid)))
