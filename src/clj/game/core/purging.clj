(ns game.core.purging
  (:require
    [game.core.board :refer [get-all-installed]]
    [game.core.card :refer [get-counters get-card]]
    [game.core.effects :refer [get-effects]]
    [game.core.engine :refer [trigger-event trigger-event-sync]]
    [game.core.ice :refer [update-all-ice]]
    [game.core.say :refer [system-msg]]
    [game.core.props :refer [add-counter]]))

(defn- purge-virus-type
  "Purges viruses (of exactly one type)"
  [state side type id]
  ;; todo - figure out the viruses that live on servers????
  (let [cards-to-purge (conj (get-all-installed state) id)
        cards-to-purge (map #(get-card state %) cards-to-purge)
        ;;cards-to-purge (filter #(pos? (get-counters % type)) cards-to-purge)
        counters (map (fn [card]
                        (let [qty (get-counters card type)]
                          (add-counter state side card type (- qty))
                          qty)) cards-to-purge)]
    (reduce + 0 counters)));;(mapv :quantity cards-to-purge))))

(defn extended-purge
  "Purges viruses (of many different types)"
  [state side]
  (trigger-event state side :pre-onr-purge) ;; probably don't need this at all
  (let [types [:doom :boardwalk :butcher-boy :cascade :cockroach :crumble :thought
               :fait :garbage :gremlin :highlighter :incubate :pattel :pox :scaldan
               :skivviss :tax :vienna :socket :pipe]]
    (system-msg state side (str (:identity (:corp @state))))
    (let [purged-counters (map #(purge-virus-type state side % (:identity (:corp @state))) types)
          total-counters (reduce + 0 purged-counters)]
      total-counters)))

(defn purge
  "Purges viruses."
  [state side eid]
  (trigger-event state side :pre-purge)
  (let [purge-preventions
        (->> (get-effects state side :prevent-purge-virus-counters)
             (reduce
               (fn [acc cur]
                 (assoc acc (-> cur :card :cid) cur))
               {}))
        cards-to-purge
        (->> (get-all-installed state)
             (keep (fn [card]
                     (let [qty (get-counters card :virus)
                           pp (get purge-preventions (:cid card))
                           qty (if pp
                                 (- qty (:quantity pp 0))
                                 qty)]
                       (when (pos? qty)
                         {:card card :quantity qty}))))
             (vec))]
    (doseq [{:keys [card quantity]} cards-to-purge]
      (add-counter state :runner card :virus (- quantity)))
    (update-all-ice state side)
    (let [total-purged-counters (reduce + 0 (mapv :quantity cards-to-purge))
          total-purged-counters (+ total-purged-counters (extended-purge state side))]
      (trigger-event-sync
        state side eid :purge
        {:total-purged-counters total-purged-counters
         :purges cards-to-purge}))))
