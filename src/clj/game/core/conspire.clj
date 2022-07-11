(ns game.core.conspire
  (:require
   [game.core.card :refer [corp? in-hand?]]
   [game.core.eid :refer [effect-completed]]
   [game.core.moving :refer [move]]
   [game.core.say :refer [system-msg]]
   [game.macros :refer [continue-ability req]]))

(defn conspire
  [state side eid card targets]
  (let [ab {:async true
            :effect (req (move state side (first (get-in @state [:corp :conspiracy])) :hand)
                         (system-msg state side "moves the card in their conspiracy to HQ.")
                         (continue-ability
                           state side
                           {:prompt "Choose the card to be added to your conspiracy."
                            :choices {:card #(and (corp? %)
                                                  (in-hand? %))}
                            :async true
                            :effect (req (system-msg state side "adds a card from HQ to their conspiracy")
                                         (move state side target :conspiracy)
                                         (effect-completed state side eid))}
                           nil nil))}
        ask-ab {:optional
                {:async true
                 :prompt "Do you want to swap your card in the conspiracy?"
                 :yes-ability ab}}]
    (if (seq (get-in @state [:corp :conspiracy]))
      (continue-ability state side ab card targets)
      (continue-ability state side ask-ab card targets))))
