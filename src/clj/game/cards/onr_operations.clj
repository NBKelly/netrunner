(ns game.cards.onr-operations
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.access :refer [access-card steal-cost-bonus]]
   [game.core.actions :refer [advance score]]
   [game.core.bad-publicity :refer [gain-bad-publicity lose-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed
                            get-all-installed get-remote-names get-remotes
                            installable-servers server->zone]]
   [game.core.card :refer [active? agenda? asset? can-be-advanced? card-index corp? corp-installable-type?
                           event? facedown? faceup? get-advancement-requirement
                           get-card get-counters get-title get-zone hardware? has-subtype? ice? identity?
                           in-discard? in-hand? installed? is-type? operation? program? resource?
                           rezzed? runner? upgrade?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.cost-fns :refer [play-cost trash-cost]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage damage-bonus]]
   [game.core.def-helpers :refer [corp-recur defcard do-brain-damage
                                  reorder-choice get-x-fn]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [effect-completed make-eid make-result]]
   [game.core.engine :refer [pay register-events resolve-ability]]
   [game.core.events :refer [first-event? last-turn? no-event? not-last-turn?
                             turn-events]]
   [game.core.flags :refer [can-score? clear-persistent-flag! in-corp-scored?
                            in-runner-scored? is-scored? prevent-jack-out
                            register-persistent-flag! register-turn-flag! when-scored? zone-locked?]]
   [game.core.gaining :refer [gain-clicks gain-credits gain-debt lose-clicks lose-debt
                              lose-credits]]
   [game.core.hand-size :refer [runner-hand-size+]]
   [game.core.ice :refer [add-extra-sub! remove-extra-subs! update-all-ice]]
   [game.core.identities :refer [disable-identity enable-identity]]
   [game.core.initializing :refer [ability-init card-init]]
   [game.core.installing :refer [corp-install corp-install-list
                                 corp-install-msg install-as-condition-counter]]
   [game.core.memory :refer [mu+ update-mu]]
   [game.core.moving :refer [as-agenda mill move swap-agendas swap-ice trash
                             trash-cards]]
   [game.core.payment :refer [can-pay? cost-target]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable clear-wait-prompt show-wait-prompt]]
   [game.core.props :refer [add-counter add-prop]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [end-run make-run]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [is-remote? remote->name zone->name]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck
                                shuffle-into-rd-effect]]
   [game.core.tags :refer [gain-tags]]
   [game.core.threat :refer [threat-level]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [number-of-virus-counters]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

(defn handle-debt-collections
  ([debt]
   (letfn [(debt-abi []
             {:event :corp-gain
              :async true
              :duration :true
              :unregister-once-resolved true
              :req (req (and (= :credit (first target))
                             (pos? (second target)))) ;; only if we actually gain creds!
              :msg (msg
                     (let [creds-owed (:debt corp)
                           creds-gained (second target)
                           siphoned (if (>= creds-owed creds-gained) creds-gained creds-owed)
                           remainder (- creds-owed siphoned)]
                       (str "forfeit " siphoned "[credits] to cover their debts"
                            (when (pos? remainder) (str " (they still owe " remainder "[credits])")))))
              :effect (req (let [creds-owed (:debt corp)
                                 creds-gained (second target)
                                 siphoned (if (>= creds-owed creds-gained) creds-gained creds-owed)
                                 remainder (- creds-owed siphoned)]
                             (wait-for (lose-credits state side (make-eid state eid) siphoned)
                                       (when (pos? remainder)
                                         (register-events
                                           state side card
                                           [(debt-abi)]))
                                       (lose-debt state side eid siphoned))))})]
     {:msg (msg "incur a debt of " debt "[Credits]"(when (pos? (:debt corp)) (str " (they now owe " (+ debt (:debt corp)) "[Credits])")))
      :async true
      :effect (req
                (when (zero? (:debt corp))
                  (register-events
                    state side (get-card state card)
                    [(debt-abi)]))
                (gain-debt state side eid debt))})))

(defcard "ONR Accounts Receivable"
  {:on-play
   {:msg "gain 9 [Credits]"
    :async true
    :effect (effect (gain-credits eid 9))}})

(defcard "ONR Badtimes"
  {:on-play {:req (req tagged)
             :msg "force the Runner to lose 2[mu] until the end of the turn"
             :effect (req (register-lingering-effect
                            state :corp card
                            (assoc (mu+ -2) :duration :end-of-turn))
                          (update-mu state))}})


(defcard "ONR Corporate Guard(R) Temps"
  (letfn [(gain-click-event [x]
            {:event :corp-turn-begins
             :duration true
             :req (req true)
             :unregister-once-resolved true
             :async true
             :msg (msg "gain an action" (when (> x 1) (str " (" (dec x) " remain)")))
             :effect (req (gain-clicks state :corp 1)
                          (when (> x 1)
                            (register-events
                              state side card
                              [(gain-click-event (dec x))]))
                          (effect-completed state side eid))})]
    {:on-play {:req (req (> (:credit corp) 1))
               :prompt (msg "Pay 2X[credit] to gain actions for the next X turns and forfeit the next X[credit] you gain")
               :choices {:number (req (int (/ (:credit corp) 2)))}
               :async true
               :effect (req
                         (let [x target]
                           (continue-ability
                             state side
                             (when (pos? x)
                               {:cost [:credit (* 2 x)]
                                :msg (msg "gain an additional action for the next " x " turns, and forfeit the next " x " credits they gain")
                                :async true
                                :effect (req
                                          (register-events
                                            state side card
                                            [(gain-click-event x)])
                                          (continue-ability
                                            state side
                                            (handle-debt-collections x)
                                            card nil))})
                             card nil)))}}))

;; this is a nearprint of midseasons - a good test for the trace system
(defcard "ONR Manhunt"
  {:on-play
   {:onr-trace
    {:req (req (last-turn? state :runner :made-run))
     :max-strength 6
     :only-tags true
     :label "Trace 6 - Give the Runner X tags"
     :successful {;:msg "give the Runner X tags"
                  :async true
                  :effect (effect (system-msg
                                    (str "gives the Runner " (quantify (- target (second targets)) "tag")))
                                  (gain-tags eid (- target (second targets))))}}}})
