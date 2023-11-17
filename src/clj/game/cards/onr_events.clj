(ns game.cards.onr-events
(:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.access :refer [access-card breach-server get-only-card-to-access
                             num-cards-to-access]]
   [game.core.agendas :refer [update-all-agenda-points]]
   [game.core.bad-publicity :refer [gain-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed server->zone]]
   [game.core.card :refer [agenda? asset? card-index condition-counter? corp?
                           event? facedown? get-card get-counters
                           get-nested-host get-title get-zone hardware? has-subtype? ice? in-discard? in-hand?
                           installed? is-type? operation? program? resource? rezzed? runner? upgrade?]]
   [game.core.charge :refer [can-charge charge-ability charge-card]]
   [game.core.cost-fns :refer [install-cost play-cost rez-cost]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage damage-prevent]]
   [game.core.def-helpers :refer [breach-access-bonus defcard offer-jack-out
                                  reorder-choice]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid
                          make-result]]
   [game.core.engine :refer [not-used-once? pay register-events
                             resolve-ability trigger-event trigger-event-simult
                             unregister-events unregister-floating-events]]
   [game.core.events :refer [first-event? first-run-event? run-events
                             turn-events]]
   [game.core.expose :refer [expose]]
   [game.core.finding :refer [find-cid find-latest]]
   [game.core.flags :refer [any-flag-fn? can-rez? can-trash?
                            clear-all-flags-for-card! clear-run-flag! clear-turn-flag!
                            in-corp-scored? register-run-flag! register-turn-flag! zone-locked?]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-clicks
                              lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+ hand-size]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [all-subs-broken? get-strength pump pump-all-icebreakers
                          update-all-ice update-breaker-strength]]
   [game.core.identities :refer [disable-card disable-identity enable-card
                                 enable-identity]]
   [game.core.initializing :refer [card-init make-card]]
   [game.core.installing :refer [install-as-condition-counter install-locked?
                                 runner-can-install? runner-install]]
   [game.core.link :refer [get-link]]
   [game.core.mark :refer [identify-mark-ability]]
   [game.core.memory :refer [available-mu]]
   [game.core.moving :refer [as-agenda flip-facedown forfeit mill move
                             swap-ice trash trash-cards]]
   [game.core.payment :refer [can-pay?]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-icon add-prop remove-icon]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez get-rez-cost rez]]
   [game.core.runs :refer [bypass-ice can-run-server? gain-next-run-credits get-runnable-zones
                           make-run prevent-access successful-run-replace-breach
                           total-cards-accessed]]
   [game.core.sabotage :refer [sabotage-ability]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name is-central? is-remote? target-server unknown->kw zone->name
                              zones->sorted-names]]
   [game.core.set-aside :refer [get-set-aside set-aside]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck]]
   [game.core.tags :refer [gain-tags lose-tags tag-prevent]]
   [game.core.threat :refer [threat threat-level]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [get-virus-counters]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]
   [jinteki.validator :refer [legal?]]))

(defcard "ONR All-Hands"
  {:makes-run true
   :implementation "Can't use Noisy is manual"
   :on-play {:req (req hq-runnable)
             :async true
             :effect (effect (make-run eid :hq card))}
   :events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :hq (target-server context))
                            this-card-run))
             :effect (effect (register-events
                              card [(breach-access-bonus :hq 3 {:duration :end-of-run})]))}]})

(defcard "ONR Anonymous Tip"
  {:on-play
   {:req (req (some #(and (rezzed? %) (ice? %) (has-subtype? % "Black Ice"))
                    (all-installed state :corp)))
    :msg (msg "derez " (:title target))
    :choices {:card #(and (ice? %)
                          (has-subtype? % "Black Ice")
                          (rezzed? %))}
    :effect (effect (derez target))}})

(defcard "ONR Blackmail"
  {:makes-run true
   :on-play {:req (req hq-runnable)
             :async true
             :effect (effect (make-run eid :hq card))}
   :events [(successful-run-replace-breach
                {:target-server :hq
                 :mandatory true
                 :this-card-run true
                 :ability {:msg "add itself to their score area as an agenda worth 1 agenda point"
                           :effect (req (as-agenda state :runner card 1))}})]})

(defcard "ONR Bodyweight [TM] Synthetic Blood"
  {:on-play
   {:msg "draw 5 cards"
    :async true
    :effect (effect (draw eid 5))}})


(defcard "ONR Ice and Data's Guide to the Net"
  (letfn [(outermost-ice [state server]
            (last (:ices (get-in @state (cons :corp (server->zone state server))))))
          (expose-chain [state side eid xs]
            (if-not (seq xs)
              (effect-completed state side eid)
              (wait-for (expose state side (make-eid state eid) (first xs))
                        (expose-chain state side eid (rest xs)))))]
    {:on-play {:async true
               :msg "expose the outermost piece of ice on each server"
               :req (req (seq (filter #(and (outermost-ice state %)
                                            (not (rezzed? (outermost-ice state %))) servers))))
               :effect (req (let [ices (map #(outermost-ice state %) servers)
                                  unrezzed (filter #(not (rezzed? %)) ices)]
                              (expose-chain state side eid unrezzed)))}}))

(defcard "ONR Ice and Data Special Report"
  (letfn [(valid-server [state server]
            (let [server-zone (second (server->zone state server))]
              (seq (filter #(and (installed? %) (not (rezzed? %))
                                 (= (second (get-zone %)) server-zone))
                           (all-installed state :corp)))))
          (valid-servers [state servers]
            (filter #(valid-server state %) servers))
          (expose-chain [state side eid xs]
            (if-not (seq xs)
              (effect-completed state side eid)
              (wait-for (expose state side (make-eid state eid) (first xs))
                        (expose-chain state side eid (rest xs)))))]
  {:on-play {:async true
             :prompt "Choose a server"
             :choices (req (valid-servers state servers))
             :msg (msg "expose up to 5 cards in or protecting " target)
             :effect (req (let [target-server target]
                            (continue-ability
                              state side
                              {:choices {:max (req (min 5 (count (filter #(and (installed? %) (not (rezzed? %))
                                                                               (= (second (get-zone %)) (second (server->zone state target-server))))
                                                                         (all-installed state :corp)))))
                                         :card #(and (installed? %)
                                                     (not (rezzed? %))
                                                     (= (second (server->zone state target-server))
                                                        (second (get-zone %))))}
                               ;;:msg (msg "exposes " (map :title targets))
                               :async true
                               :effect (req (expose-chain state side eid targets))
                               }
                              card nil)))}}))

(defcard "ONR Livewire's Contacts"
  {:on-play
   {:msg "gain 3 [Credits]"
    :async true
    :effect (effect (gain-credits eid 3))}})

(defcard "ONR Networking"
  {:on-play
   {:msg "gain 9 [Credits]"
    :async true
    :effect (effect (gain-credits eid 9))}})

(defcard "ONR Synchronized Attack on HQ"
  {:on-play
   {:implementation "ERRATUM - For each card stored in HQ, Corp either
    pays[2] or discards that card."
    :req (req (and (some #{:hq} (:successful-run runner-reg))
                   (pos? (count (:hand corp)))))
    :msg "force the corp to (pay 2 or discard) each card in HQ"
    :effect (req (wait-for (resolve-ability
                             state :corp
                             {:prompt "Choose cards in HQ to keep (2[Credits] each)"
                              :waiting-prompt true
                              :player :corp
                              :async true
                              :choices {:max (req (min (count (:hand corp)) (quot (total-available-credits state :corp eid card) 2)))
                                        :card #(and (corp? %)
                                                    (in-hand? %))}
                              :effect (req (wait-for (pay state :corp (make-eid state eid) card :credit (* 2 (count targets)))
                                                     (system-msg
                                                       state :corp
                                                       (str (:msg async-result) " to prevent the trashing of "
                                                            (count targets) " cards from HQ"))
                                                     (effect-completed state side (make-result eid targets))))}
                             card nil)
                           (let [prevented async-result
                                 trashtargets (:hand corp)
                                 cids-to-trash (set/difference (set (map :cid trashtargets)) (set (map :cid prevented)))
                                 cards-to-trash (filter #(cids-to-trash (:cid %)) trashtargets)]
                             (when (not async-result)
                               (system-msg state :corp "chooses to discard ALL cards"))
                             (wait-for (trash-cards state :corp cards-to-trash {:cause-card card})
                                       (system-msg state :corp (str "discards " (quantify (count cards-to-trash) " card") " from HQ"))
                                       (effect-completed state side eid)))))}})
