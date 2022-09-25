(ns game.core.conspire
  (:require
    [game.core.card :refer [corp? in-hand?]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [resolve-ability]]
    [game.core.moving :refer [move]]
    [game.core.say :refer [system-msg]]
    [game.macros :refer [req]]
    [clojure.string :as string]))

(def conspire-ability
  (let [ab {:effect (req (move state side (first (get-in @state [:corp :conspiracy])) :hand)
                         (system-msg state side "moves the card in their conspiracy to HQ.")
                         (resolve-ability state side eid
                                          {:prompt "Choose the card to be added to your conspiracy."
                                           :choices {:card #(and (corp? %)
                                                                 (in-hand? %))}
                                           :async true
                                           :effect (req (system-msg state side "adds a card from HQ to their conspiracy")
                                                        (move state side target :conspiracy)
                                                        (effect-completed state side eid))}
                                          nil nil)
                         (effect-completed state side eid))}
        ask-ab {:optional
                {:async true
                 :prompt "Do you want to swap your card in the conspiracy?"
                 :yes-ability ab}}]
        {:effect (req
                   (if (not-empty (:conspiracy corp))
                     (resolve-ability state side eid ask-ab nil nil)
                     (resolve-ability state side eid ab nil nil)))}))

(defn conspire
  ([state side] (conspire state side (make-eid state) nil nil))
  ([state side eid targets] (conspire state side eid targets nil))
  ([state side eid targets args]
   (resolve-ability state side eid conspire-ability targets args)))
