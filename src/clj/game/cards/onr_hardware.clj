(ns game.cards.onr-hardware
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.access :refer [access-bonus access-card breach-server
                             get-only-card-to-access]]
   [game.core.actions :refer [play-ability]]
   [game.core.board :refer [all-active all-active-installed all-installed]]
   [game.core.card :refer [active? corp? event? facedown? get-card get-counters get-title
                           get-zone hardware? has-subtype? ice? in-deck? in-discard?
                           in-hand? in-scored? installed? program? resource? rezzed?
                           runner? virus-program? faceup?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.cost-fns :refer [all-stealth install-cost
                               rez-additional-cost-bonus rez-cost trash-cost]]
   [game.core.damage :refer [chosen-damage damage damage-prevent
                             enable-runner-damage-choice runner-can-choose-damage?]]
   [game.core.def-helpers :refer [breach-access-bonus defcard offer-jack-out
                                  reorder-choice trash-on-empty get-x-fn]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect
                              unregister-effects-for-card unregister-lingering-effects]]
   [game.core.eid :refer [effect-completed make-eid make-result]]
   [game.core.engine :refer [can-trigger? not-used-once? register-events
                             register-once register-suppress resolve-ability trigger-event
                             unregister-floating-events unregister-suppress-by-uuid]]
   [game.core.events :refer [event-count first-event? first-trash? no-event?
                             run-events]]
   [game.core.expose :refer [expose]]
   [game.core.finding :refer [find-card]]
   [game.core.flags :refer [can-trash? card-flag? in-corp-scored? register-run-flag!
                            zone-locked?]]
   [game.core.gaining :refer [gain-clicks gain-credits lose-clicks
                              lose-credits]]
   [game.core.hand-size :refer [hand-size runner-hand-size+]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [all-subs-broken? any-subs-broken? break-sub pump
                          reset-all-ice update-all-ice update-all-icebreakers
                          update-breaker-strength]]
   [game.core.installing :refer [runner-can-install? runner-can-pay-and-install? runner-install]]
   [game.core.link :refer [get-link link+]]
   [game.core.memory :refer [caissa-mu+ mu+ update-mu virus-mu+]]
   [game.core.moving :refer [mill move swap-agendas trash trash-cards]]
   [game.core.optional :refer [get-autoresolve never? set-autoresolve]]
   [game.core.payment :refer [build-cost-string can-pay? cost-value]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-icon remove-icon]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [bypass-ice end-run end-run-prevent
                           get-current-encounter jack-out make-run
                           successful-run-replace-breach total-cards-accessed]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [target-server is-central?]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.tags :refer [gain-tags lose-tags tag-prevent]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [count-virus-programs number-of-virus-counters]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]
   [game.core.onr-trace :refer [boost-link set-base-link cancel-successful-trace]]
   [game.core.set-aside :refer [set-aside get-set-aside]]
   [game.core.sabotage :refer [sabotage-ability]]
   [game.core.mark :refer [identify-mark-ability]]))

(defn- base-link-abi
  [cost val]
  (let [cost (if (integer? cost) [:credit cost] cost)]
    {:onr-base-link true
     :req (req true)
     :cost cost
     :base-link val
     :label (str "Base Link " val)
     :msg (str "set their Base Link to " val)
     :effect (req (set-base-link state val))}))

(defn- boost-link-abi
  [cost val]
  (let [cost (if (integer? cost) [:credit cost] cost)]
    {:onr-boost-link true
     :cost cost
     :label (str "+" val " Link")
     :msg (str "gain +" val " Link")
     :effect (req (boost-link state val))}))

;; Card definitions

(defcard "ONR Arasaka Portable Prototype"
  {:recurring 3
   :additional-cost [:agenda-point 1]
   :static-abilities [(mu+ 3)]
   :interactions {:pay-credits {:req (req (and run
                                               (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "ONR Bodyweight [TM] Data Crèche"
  {:implementation "Occurs after the run ends"
   :static-abilities [(mu+ 1)]
   :events [{:event :runner-install
             :req (req (same-card? card (:card context)))
             :silent (req true)
             :effect (effect (update! (assoc card :dopp-active true)))}
            {:event :runner-turn-begins
             :effect (effect (update! (assoc card :dopp-active true)))}
            {:event :run-ends
             :interactive (req true)
             :optional
             {:req (req (and (:successful target)
                             (:dopp-active (get-card state card))))
              :player :runner
              :prompt "Make another run?"
              :yes-ability {:prompt "Choose a server"
                            :async true
                            :choices (req runnable-servers)
                            :msg (msg "make a run on " target)
                            :makes-run true
                            :effect (effect (update! (dissoc card :dopp-active))
                                            (unregister-lingering-effects :end-of-run)
                                            (unregister-floating-events :end-of-run)
                                            (update-all-icebreakers)
                                            (update-all-ice)
                                            (reset-all-ice)
                                            (clear-wait-prompt :corp)
                                            (make-run eid target (get-card state card)))}}}]})


(defcard "ONR MRAM Chip"
  {:static-abilities [(runner-hand-size+ 2)]})

(defcard "Omnitech Wet Drive"
  (let [update-base-mu (fn [state n] (swap! state assoc-in [:runner :memory :base] n))]
    {:effect (req (update-base-mu state (count (get-in @state [:runner :hand])))
                  (add-watch state :ekomind (fn [_k ref old new]
                                                (let [hand-size (count (get-in new [:runner :hand]))]
                                                  (when (not= (count (get-in old [:runner :hand])) hand-size)
                                                    (update-base-mu ref hand-size))))))
     :leave-play (req (remove-watch state :ekomind))}))

(defcard "ONR Parraline 5750"
  {:recurring 1
   :static-abilities [(mu+ 1)]
   :interactions {:pay-credits {:req (req (and run
                                               (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "ONR Raven Microcyb Owl"
  {:recurring 3
   :static-abilities [(mu+ 1)]
   :interactions {:pay-credits {:req (req (and run
                                               (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")
                                               (not (has-subtype? target "Noisy"))))
                                :type :recurring}}})

(defcard "ONR Raven Microcyb Eagle"
  {:interactions {:prevent [{:type #{:net}
                             :req (req (not-used-once? state (first (:abilities card)) card))}]
                  :pay-credits {:req (req (and run
                                               (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}
  :static-abilities [(mu+ 1)]
  :abilities [{:cost [:credit 0]
               :once :per-turn
               :msg "prevent 1 net damage"
               :effect (effect (damage-prevent :net 1))}]
  :recurring 1})

(defcard "ONR Sunburst Cranial Interface"
  {:recurring 1
   :static-abilities [(mu+ 1)
                      (runner-hand-size+ 1)]
   :interactions {:pay-credits {:req (req (and run
                                               (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")
                                               (not (has-subtype? target "Noisy"))))
                                :type :recurring}}})

(defcard "ONR The Deck"
  {:abilities [(base-link-abi 0 5)
               (boost-link-abi 1 1)]
   :static-abilities [(mu+ 1)]})

(defcard "ONR Zetatech Portastation"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and
                                           (event? target)
                                           (or (= 0 (count (:cost-paid eid)))
                                               (:x-cost eid))
                                           (= :play (:source-type eid))))
                                :type :recurring}}})
