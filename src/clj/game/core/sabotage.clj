(ns game.core.sabotage
  (:require
   [game.core.card :refer [corp? in-hand?]]
   [game.core.moving :refer [trash-cards]]
   [game.core.say :refer [system-msg]]
   [game.macros :refer [continue-ability msg req]]
   [game.utils :refer [pluralize]]))

(defn sabotage-ability
  [n]
  (let [trash-req (req
                    (let [targets (filter some? targets)
                          selected-hq (count targets)
                          selected-rd (min (count (:deck corp))
                                           (- n selected-hq))
                          to-trash (concat targets (take selected-rd (:deck corp)))]
                      (system-msg state side
                                  (str
                                    "trashes"
                                    (when (pos? selected-hq)
                                      (str " " selected-hq " " (pluralize "card" selected-hq) " from HQ"))
                                    (when (and (pos? selected-hq) (pos? selected-rd))
                                      " and")
                                    (when (pos? selected-rd)
                                      (str " " selected-rd " " (pluralize "card" selected-rd) " from the top of R&D"))))
                      (trash-cards state side eid to-trash {:unpreventable true})))
        choosing-ab (fn [forced-hq]
                      {:waiting-prompt "Corp to choose an option"
                       :player :corp
                       :prompt (req
                                 (let [cards-rd (count (:deck corp))
                                       forced-hq (- n cards-rd)]
                                   (str "Choose"
                                        (when (pos? forced-hq)
                                          (str " at least " forced-hq " " (pluralize "card" forced-hq) " and"))
                                        " up to " n " " (pluralize "card" n)
                                        " to trash from HQ. Remainder will be trashed from top of R&D.")))
                       :choices {:min forced-hq
                                 :max n
                                 :card #(and (corp? %)
                                             (in-hand? %))}
                       :async true
                       :cancel-effect trash-req
                       :effect trash-req})]
    {:req (req (pos? n))
     :msg (msg "sabotage " n)
     :async true
     :effect (req (let [cards-rd (count (:deck corp))
                        cards-hq (count (:hand corp))
                        forced-hq (- n cards-rd)]
                    (if (> forced-hq cards-hq)
                      (trash-req state :corp eid card (:hand corp))
                      (continue-ability state side
                                        (choosing-ab forced-hq)
                                        card nil))))}))
