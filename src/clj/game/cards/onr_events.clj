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
   [game.core.def-helpers :refer [breach-access-bonus defcard offer-jack-out runner-recur
                                  reorder-choice]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect gather-effects]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid
                          make-result]]
   [game.core.engine :refer [not-used-once? pay register-events gather-events pay
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
   [game.core.winning :refer [check-win-by-agenda]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]
   [jinteki.validator :refer [legal?]]))

(defn- handle-if-unique
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

(defn- register-effect-once [state side card effect]
  (let [em (gather-effects state side (:type effect))
        matches (filter #(= (:ability-name %) (:ability-name effect)) em)]
    (when (empty? matches)
      (register-lingering-effect
        state side card
        effect))))

;; card implementations

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

(defcard "ONR Deal with Militech"
  {:on-play
   {:req (req (and (some #(has-subtype? % "Icebreaker") (all-installed state :runner))
                   (some #(has-subtype? (:card (first %)) "Research") (turn-events state side :agenda-stolen))))
    :effect (req (doseq [icebreaker (filter #(has-subtype? % "Icebreaker") (all-installed state :runner))]
                   (add-counter state side icebreaker :militech 1))
                 (register-effect-once
                   state side card
                   {:type :breaker-strength
                    :ability-name "Militech Counters"
                    :req (req true)
                    :value (req (get-counters (get-card state target) :militech))}))}})

(defcard "ONR Demolition Run"
  {:makes-run true
   :on-play {:async true
             :prompt "Choose a server"
             :choices (req runnable-servers)
             :effect (effect (make-run eid target card))}
   :events [(successful-run-replace-breach
              {:this-card-run true
               :mandatory true
               :ability
               {:async true
                :effect (req (let [ice-to-trash (filter #(and (ice? %) (rezzed? %)
                                                              (= (first (:server run))
                                                                 (second (get-zone %))))
                                                        (all-installed state :corp))]
                               (continue-ability
                                 state side
                                 {:msg (msg "trash " (str/join ", " (map :title ice-to-trash)) " and take 3 tags")
                                 :async true
                                 :effect (req (wait-for (trash-cards state side (make-eid state eid) ice-to-trash {:cause-card card})
                                                        (gain-tags state :corp eid 3)))}
                                card nil)))}})]})


(defcard "ONR Edited Shipping Manifests"
  {:makes-run true
   :on-play {:req (req hq-runnable)
             :async true
             :effect (effect (make-run eid :hq card))}
   :events [(successful-run-replace-breach
              {:target-server :hq
               :this-card-run true
               :mandatory (req (pos? (:credit corp)))
               :ability
               {:async true
                :req (req (pos? (:credit corp)))
                :msg (msg "force the Corp to lose 1[Credits], gain 10[Credits] and a tag")
                :effect (req (let [creds-lost (min 1 (:credit corp))]
                               (wait-for
                                (lose-credits state :corp creds-lost)
                                (wait-for (gain-tags state :runner 1)
                                          (gain-credits state :runner eid 10)))))}})]})

(defcard "ONR Gideon's Pawnshop"
  {:on-play (runner-recur)})

(defcard "ONR Gypsy [TM] Schedule Analyzer"
  (letfn [(move-gendie-to-hand [state side eid card revealed-card rev-str]
            (continue-ability
              state side
              {:msg (msg "reveal " rev-str " from the top of R&D and store "
                         (:title revealed-card) " in HQ")
               :effect (req (move state :corp revealed-card :hand)
                            (shuffle! state :corp :deck)
                            (system-msg state :corp "shuffles R&D"))}
              card nil))
          (gypsy-search-fn [state side eid card remainder rev-str]
            (if (not-empty remainder)
              (let [revealed-card (first remainder)
                    rest-of-deck (rest remainder)
                    rev-str (if (= "" rev-str)
                              (:title revealed-card)
                              (str rev-str ", " (:title revealed-card)))]
                (if (agenda? revealed-card)
                  (move-gendie-to-hand state side eid card revealed-card rev-str)
                  (gypsy-search-fn state side eid card rest-of-deck rev-str)))
              (continue-ability
                state side
                {:msg (msg "reveal " rev-str " from the top of the stack")
                 :effect (effect (shuffle! state :corp :deck)
                                 (system-msg :corp "shuffles R&D"))}
                card nil)))]
    {:makes-run true
     :on-play {:req (req rd-runnable)
               :async true
               :effect (effect (make-run eid :rd card))}
     :events [(successful-run-replace-breach
                {:target-server :rd
                 :this-card-run true
                 :mandatory true
                 :ability
                 {:async true
                  :effect (req (gypsy-search-fn state side eid card (:deck corp) ""))}})]}))

(defcard "ONR Hot Tip for WNS"
  {:on-play
   {:req (req (some #(has-subtype? (:card (first %)) "Black Ops") (turn-events state side :agenda-stolen)))
    :msg "gain 1 agenda points";; 👀
    :effect (req (register-lingering-effect
                   state side nil
                   {:type :user-agenda-points
                    ;; `target` is either `:corp` or `:runner`
                    :req (req (= :runner target))
                    :value 1})
                 (update-all-agenda-points state side)
                 (check-win-by-agenda state side))}})


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

(defcard "ONR Identity Donor"
  {:additional-cost [:agenda-point 50] ;; ghetto way of stopping regular cast
   ;; todo - would it be possible to hide what card is triggering I wonder? this works for now though
   :events [{:event :pre-damage
             :async true
             :location :hand
             :optional
             {:prompt "Play identity donor to prevent damage and give 2 bad publicity?"
              :waiting-prompt "Runner to resolve pre-damage events"
              :player :runner
              :async true
              :req (req (and (= :meat (first targets))
                             (can-pay? state side
                                       (assoc eid :source card :source-type :play)
                                       card nil [:credit (play-cost state side (assoc card :additional-cost nil))])))
              :yes-ability {:msg (msg "play itself in response to meat damage")
                            :async true
                            :effect (req (play-instant
                                           state side eid
                                           ;; {:no-additional-cost true}) - prob no need
                                           (assoc card :additional-cost nil) nil))}}}]
   :msg "prevent all meat damage, and give the Corp 2 Bad Publicity points"
   :async true
   :effect (req (damage-prevent state :runner :meat Integer/MAX_VALUE)
                (gain-bad-publicity state :runner eid 2))})

(defcard "ONR Inside Job"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :encounter-ice
             :req (req (first-run-event? state side :encounter-ice))
             :once :per-run
             :msg (msg "bypass " (:title (:ice context)))
             :effect (req (bypass-ice state))}]})

(defcard "ONR Kilroy Was Here"
  {:makes-run true
   :on-play {:req (req rd-runnable)
             :async true
             :effect (effect (make-run eid :rd card))}
   :interactions {:access-ability
                  {:label "Trash card"
                   :req (req (can-trash? state :runner target))
                   :msg (msg "trash " (:title target) " at no cost")
                   :async true
                   :effect (effect (trash eid (assoc target :seen true) {:cause-card card}))}}})

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

(defcard "ONR Prearranged Drop"
  {:events [{:event :access
             :req (req (agenda? target))
             :duration :end-of-turn
             :once :per-turn
             :unregister-once-resolved true
             :msg "gain 6 [Credits]"
             :async true
             :effect (effect (gain-credits eid 6))}]})

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
