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
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-clicks gain-click-debt
                              lose-credits gain-agenda-point-debt gain-agenda-points
                              lose-agenda-points]]
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
                           make-run prevent-access successful-run-replace-breach end-run
                           total-cards-accessed]]
   [game.core.sabotage :refer [sabotage-ability]]
   [game.core.say :refer [system-msg say]]
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
   [game.core.onr-utils :refer [dice-roll noisy-breaker-used]]
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

(defcard "ONR All-Nighter"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :msg (msg "make a run on " target)
             :effect (req (wait-for (make-run state side target card)
                                    (let [card (get-card state card)
                                          run-again (get-in card [:special :run-again])]
                                      (if run-again
                                        (continue-ability
                                          state side
                                          {:prompt "Choose a server"
                                           :choices (req runnable-servers)
                                           :async true
                                           :msg (msg "make a run on " target)
                                           :effect (req (make-run state side eid target card))}
                                          card nil)
                                        (effect-completed state side eid)))))}
   :events [{:event :run-ends
             :optional {:req (req (and (not (get-in card [:special :run-again]))
                                       (:unsuccessful target)))
                        :player :runner
                        :prompt "Make another run?"
                        :yes-ability
                        {:effect (req (let [last-run (get-in @state [:runner :register :last-run])
                                            attacked-server (first (:server last-run))]
                                        (update! state side (update card :special
                                                                    assoc
                                                                    :run-again attacked-server))))}}}]})

(defcard "ONR Arasaka Owns You"
  (let [gendie-debt {:msg (msg "forfiet the next three agenda points they score")
                     :async true
                     :effect (req (gain-agenda-point-debt state side eid 3))}
        forgo-four {:msg (msg "forgo their next four actions")
                    :async true
                    :effect (req (wait-for (gain-click-debt state side (make-eid state eid) 4)
                                           (continue-ability state side gendie-debt card nil)))}
        remove-tags {:msg (msg "remove all tags")
                     :async true
                     :effect (req (wait-for (lose-tags state side (make-eid state eid) :all)
                                            (continue-ability state side forgo-four card nil)))}
        gain-ten {:msg (msg "gain 10 [Credits]")
                  :async true
                  :effect (req (wait-for (gain-credits state side (make-eid state eid) 10)
                                         (continue-ability state side remove-tags card nil)))}
        draw-to-hand-size {:msg (msg "draw up to maximum hand size ("
                                     (quantify (max (- (hand-size state runner) (count (:hand runner))) 0)
                                     " card") ")")
                           :async true
                           :effect (req (let [to-draw (max (- (hand-size state runner) (count (:hand runner))) 0)]
                                          (wait-for (draw state side (make-eid state eid) to-draw)
                                                    (continue-ability
                                                      state side
                                                      gain-ten card nil))))}]
    {;; todo - would it be possible to hide what card is triggering I wonder? this works for now though
     :on-play
     {:additional-cost [:agenda-point 50] ;; ghetto way of stopping regular cast
      :msg (msg "prevent all damage")
      :async true
      :effect (req (damage-prevent state :runner :meat Integer/MAX_VALUE)
                   (damage-prevent state :runner :net Integer/MAX_VALUE)
                   (damage-prevent state :runner :brain Integer/MAX_VALUE)
                   (when (pos? (:brain-damage runner))
                     (system-msg state side (str "uses " (:title card) " to cure all brain damage")))
                   (swap! state update-in [:runner :brain-damage] #(- % %))
                   (continue-ability
                     state side
                     draw-to-hand-size
                     card nil))
      ;;, draw up to hand size, gain 10 [Credits], remove all tags and forgo the next four actions")
      }
     :events [{:event :pre-damage
               :async true
               :location :hand
               :optional
               {:prompt (msg "Play " (:title card) "?")
                :waiting-prompt "Runner to resolve pre-damage events"
                :player :runner
                :req (req (and (> (last targets) (count (:hand runner)));;would this flatline?
                               (not (nth targets 2)) ;; can be prevented
                               (can-pay? state side
                                         (assoc eid :source card :source-type :play)
                                         card nil [:credit (play-cost state side (assoc-in card [:on-play :additional-cost] nil))])))
                :yes-ability {:msg (msg "play itself in response to a flatline")
                              :async true
                              :effect (req (play-instant
                                             state side eid
                                             (assoc-in card [:on-play :additional-cost] nil) {:no-additional-cost true}))}}}]}))

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
                 :ability {:msg "score 1 agenda point"
                           :effect (req (gain-agenda-points state side eid 1))}})]})

(defcard "ONR Bodyweight [TM] Synthetic Blood"
  {:on-play
   {:msg "draw 5 cards"
    :async true
    :effect (effect (draw eid 5))}})

(defcard "ONR Boostergang Connections"
  (letfn [(tutor [x]
            {:prompt (msg "Choose a card (" x " remaining)")
             :msg "add a card from the stack to the grip"
             :choices (req (cancellable (distinct (:deck runner)) :sorted))
             :async true
             :cancel-effect (effect (shuffle! :deck)
                                    (effect-completed eid))
             :effect (req (trigger-event state side :searched-stack nil)
                             (move state side target :hand)
                             (if (> x 1)
                               (continue-ability
                                 state side
                                 (tutor (dec x))
                                 card nil)
                               (do (shuffle! state side :deck)
                                   (effect-completed state side eid))))})]
  {:on-play
   {:msg (msg "trash thier hand and search for up to " (count (:hand runner)) " cards from the stack")
    :async true
    :effect (req (let [to-search (count (:hand runner))]
                   (wait-for (trash-cards state side (make-eid state eid) (:hand runner))
                             (continue-ability
                               state side
                               (tutor to-search)
                               card nil))))}}))

;; TODO - write this better
(defcard "ONR Core Command: Jettison Ice"
  {:on-play
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :prompt "How many credits do you want to spend?"
    :choices :credit
    :effect (effect (continue-ability
                      (let [spent-credits target]
                        {:choices {:card #(and (ice? %)
                                               (rezzed? %)
                                               (<= (:cost %) target))}
                         :async true
                         :effect (effect (trash eid target))
                         :msg (msg "spend " spent-credits "[Credits] and trash " (:title target))})
                      card nil))}})

(defcard "ONR Corruption"
  {:implementation "takes forfieted AP into account. You can't lose the same points more than once"
   :on-play
   {:req (req (pos? (- (:scored-agenda runner-reg 0) (:forfiet-agenda-points runner-reg 0))))
    :msg (msg "lose all agenda points they scored this turn ("
              (- (:scored-agenda runner-reg 0) (:forfiet-agenda-points runner-reg 0))
              "), gain 10 [Credits] for each, and have the corp gain that many agenda points")
    :async true
    :effect (req (let [transfer (- (:scored-agenda runner-reg 0) (:forfiet-agenda-points runner-reg 0))
                       creds (* 10 transfer)]
                   (wait-for (lose-agenda-points state :runner (make-eid state eid) transfer)
                             (wait-for (gain-credits state :runner creds)
                                       (system-msg state side (str "gains " creds " [Credits]"))
                                       (wait-for (gain-agenda-points state :corp transfer)
                                                 (gain-tags state :corp eid 1))))))}})

(defcard "ONR Cruising for Netwatch"
  {:on-play
   {:msg "gain 1 [Credits] and draw 2 cards"
    :async true
    :effect (req (wait-for (gain-credits state side 1)
                           (draw state side eid 2)))}})

(defcard "ONR Custodial Position"
  {:makes-run true
   :on-play {:req (req rd-runnable)
             :async true
             :effect (effect (make-run eid :rd card))}
   :events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :rd (target-server context))
                            this-card-run))
             :effect (effect (register-events
                              card [(breach-access-bonus :rd 2 {:duration :end-of-run})]))}]})

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

(defcard "ONR Decoy Signal"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :req (req (some #(can-run-server? state %) servers))
             :choices (req (filter #(can-run-server? state %) servers))
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:req (req (and ;;(= :approach-ice (:phase run))
                         ;;this-card-run
             ;;            (ice? current-ice)
                         (not (rezzed? current-ice))))
             :event :approach-ice
             :label "expose approached ice"
             :msg "expose the approached ice"
             :async true
             :effect (req (wait-for (expose state side (make-eid state eid) current-ice)
                                    (continue-ability state side (offer-jack-out) card nil)))}]})

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

(defcard "ONR Desperate Competitor"
  {:on-play
   {:req (req (some #(has-subtype? (:card (first %)) "Gray Ops") (turn-events state side :agenda-stolen)))
    :msg "score 1 agenda point" ;; 👀
    :async true
    :effect (req (gain-agenda-points state side eid 1))}})

(defcard "ONR Disgruntled Ice Technician"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :pass-ice
             :req (req (and (rezzed? (:ice context))
                            (not-used-once? state {:once :per-run} card)
                            (all-subs-broken? (:ice context))))
             :async true
             :effect
             (effect
               (continue-ability
                 (let [ice (:ice context)]
                   {:optional
                      {:prompt (str "Derez " (:title ice) " and end the run?")
                       :once :per-run
                       :yes-ability
                       {:async true
                        :msg (msg "derez " (card-str state ice) " and end the run")
                        :effect (req (derez state side ice)
                                     (end-run state :runner eid card))}}})
                 card nil))}]})

(defcard "ONR Do the 'Drine [TM]"
  {:on-play
   {:req (req (and (> (count (:hand runner)) 2) (pos? (hand-size state :runner))))
    :async true
    :prompt "Take how much brain damage?"
    :choices {:number (req (min (count (:hand runner)) (hand-size state :runner)))
              :default (req 1)}
    :msg (msg "take " target " brain damage (cannot be prevented) and gain " (* 4 target) " [Credits]")
    :effect (req (wait-for (damage state :runner (make-eid state eid) :brain target {:card card :unpreventable true})
                           (gain-credits state side eid (* 4 target))
                           (when-not (= 0 target) ;; todo - remove this if anyone complains
                             (say state side {:text "Worth it"}))))}})

(defcard "ONR Drone for a Day"
  {:msg "gain 9 [Credits] and take 1 tag"
   :effect (req (wait-for (gain-tags state :corp 1)
                          (gain-credits state :runner eid 9)))})

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

(defcard "ONR Executive Wiretaps"
  {:makes-run true
   :on-play {:req (req hq-runnable)
             :async true
             :effect (effect (make-run eid :hq card))}
   :events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :hq (target-server context))
                            this-card-run))
             :effect (effect (register-events
                              card [(breach-access-bonus :hq 2 {:duration :end-of-run})]))}]})


(defcard "ONR Faked Hit"
  {:on-play {:msg "Give the Corp 1 bad publicity and suffer 2 brain damage"
             :async true
             :effect (req (wait-for (gain-bad-publicity state :runner 1)
                                    (damage state :runner eid :brain 2 {:unpreventable true :card card})))}})

(defcard "ONR Finders Keepers"
  {:on-play {:async true
             :effect (req (let [di (dice-roll 3)
                                sum (reduce + di)]
                            (continue-ability
                              state side
                              {:msg (msg "Gain " sum " [credits] (3d6 - "
                                         (first di) " "
                                         (second di) " "
                                         (last di) ")")
                               :async true
                               :effect (effect (gain-credits eid sum))}
                              card nil)))}})

(defcard "ONR Forged Activation Orders"
  {:on-play
   {:choices {:card #(and (ice? %)
                          (not (rezzed? %)))}
    :async true
    :effect (req (let [ice target
                       serv (zone->name (second (get-zone ice)))
                       icepos (card-index state ice)]
                   (continue-ability
                     state :corp
                     {:prompt "Choose one"
                      :choices [(when (and (can-rez? state :corp ice)
                                           (can-pay? state :corp eid ice nil (get-rez-cost state :corp ice nil)))
                                  (str "Rez " (card-str state ice)))
                                (str "Trash " (card-str state ice))]
                      :async true
                      :msg (msg "force the Corp to " (decapitalize target))
                      :waiting-prompt true
                      :effect (req (if (str/starts-with? target "Rez")
                                     (rez state :corp eid ice)
                                     (trash state :corp eid ice {:cause-card card
                                                                 :cause :forced-to-trash})))}
                     card nil)))}})

(defcard "ONR Forgotten Backup Chip"
  {:on-play (runner-recur program?)})

(defcard "ONR Fortress Respecification"
  (letfn [(sun [serv]
            {:prompt "Choose 2 pieces of ice to swap"
             :choices {:card #(and (= [:servers serv :ices] (get-zone %))
                                   (ice? %))
                       :max 2}
             :async true
             :effect (req (if (= (count targets) 2)
                            (do (swap-ice state side (first targets) (second targets))
                                (system-msg state side
                                            (str "uses " (:title card) " to swap "
                                                 (card-str state (first targets))
                                                 " with "
                                                 (card-str state (second targets))))
                                (continue-ability state side (sun serv) card nil))
                            (do (system-msg state side "has finished rearranging ice")
                                (effect-completed state side eid))))})]
    {:on-play {:req (req (seq (:successful-run runner-reg)))
               :msg (msg "rearrange ice protecting " (zone->name (last (:successful-run runner-reg))))
               :effect (req
                         (continue-ability
                           state side
                           (sun (first (:successful-run runner-reg)))
                           card nil))}}))

(defcard "ONR Frame-Up"
  {:on-play {:req (req (and (some #{:hq} (:successful-run runner-reg))
                            (some #{:rd} (:successful-run runner-reg))))
             :msg (msg "give the Corp 1 bad publicity")
             :async true
             :effect (req (wait-for (gain-bad-publicity state side 1)
                                    (if (or (some #(has-subtype? (:card (first %)) "Black Ops") (turn-events state side :agenda-stolen))
                                            (some #(has-subtype? (:card (first %)) "Black Ops") (turn-events state side :runner-trash)))
                                      (continue-ability
                                        state side
                                        {:msg "give the Corp 1 additional bad publicity"
                                         :async true
                                         :effect (req (gain-bad-publicity state side eid 1))}
                                        card nil)
                                      (effect-completed state side eid))))}})

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

(defcard "ONR Hijack"
  {:data {:counter {:credit 3}}
   :interactions {:pay-credits {:req (req (and (= :runner-install (:source-type eid))
                                               (or (program? target)
                                                   (hardware? target))))
                                :type :credit}}
   :implementation "credits are placed on the card"
   :on-play {:prompt "Choose a program or piece of hardware to install"
             :choices {:req (req (and (or (hardware? target)
                                          (program? target))
                                      (in-hand? target)
                                      (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                [:credit (install-cost state side target nil)])))}
             :async true
             :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target nil))}})

(defcard "ONR Hot Tip for WNS"
  {:on-play
   {:req (req (some #(has-subtype? (:card (first %)) "Black Ops") (turn-events state side :agenda-stolen)))
    :msg "score 1 agenda point" ;; 👀
    :async true
    :effect (req (gain-agenda-points state side eid 1))}})

(defcard "ONR Hunt Club BBS"
  (letfn [(expose-chain [state side eid xs]
            (if-not (seq xs)
              (effect-completed state side eid)
              (wait-for (expose state side (make-eid state eid) (first xs))
                        (expose-chain state side eid (rest xs)))))]
    {:on-play {:choices {:max (req (min 3 (count (filter #(and (installed? %) (not (rezzed? %)))
                                                         (all-installed state :corp)))))
                         :card #(and (installed? %)
                                     (not (rezzed? %))
                                     (corp? %))}
               :async true
               :effect (req (expose-chain state side eid targets))}}))

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
  {;; todo - would it be possible to hide what card is triggering I wonder? this works for now though
   :events [{:event :pre-damage
             :async true
             :location :hand
             :optional
             {:prompt "Play identity donor to prevent damage and give 2 bad publicity?"
              :waiting-prompt "Runner to resolve pre-damage events"
              :player :runner
              :async true
              :req (req (and (= :meat (first targets))
                             (not (nth 2 targets)) ;; can be prevented
                             (can-pay? state side
                                       (assoc eid :source card :source-type :play)
                                       card nil [:credit (play-cost state side (assoc-in card [:on-play :additional-cost] nil))])))
              :yes-ability {:msg (msg "play itself in response to meat damage")
                            :async true
                            :effect (req (play-instant
                                           state side eid
                                           ;; {:no-additional-cost true}) - prob no need
                                           (assoc-in card [:on-play :additional-cost] nil) {:no-additional-cost true}))}}}]
   :on-play {:additional-cost [:agenda-point 50] ;; ghetto way of stopping regular cast
             :msg "prevent all meat damage, and give the Corp 2 Bad Publicity points"
             :async true
             :effect (req (damage-prevent state :runner :meat Integer/MAX_VALUE)
                          (gain-bad-publicity state :runner eid 2))}})

(defcard "ONR If You Want It Done Right..."
  {:on-play {:req (req (seq (:deck runner)))
             :prompt "Add a card to hand"
             :choices (req (take 5 (:deck runner)))
             :async true
             :msg "add a card from the top 5 to the hand, and rearranges the rest"
             :effect (req (move state side target :hand)
                          (if (seq (:deck runner))
                            (continue-ability
                              state side
                              (reorder-choice :runner :corp (take 4 (:deck runner)) '()
                                              4 (take 4 (:deck runner)))
                              card nil)
                            (effect-completed state side eid)))}})

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

(defcard "ONR Jack 'n' Joe"
  {:on-play
   {:msg "draw 3 cards"
    :async true
    :effect (effect (draw eid 3))}})

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

(defcard "ONR Library Search"
  {:makes-run true
   :on-play {:req (req (or rd-runnable hq-runnable))
             :prompt "Choose a server"
             :choices ["HQ" "R&D"]
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :successful-run
             :silent (req true)
             :req (req (and (#{:hq :rd} (target-server context))
                            this-card-run
                            (empty? (run-events state side :initialize-trace))
                            (empty? (run-events state side :initialize-onr-trace))
                            (not (noisy-breaker-used state))))
             :msg (msg "access 2 additional cards")
             :effect (effect (register-events
                              card [(breach-access-bonus (target-server context) 2 {:duration :end-of-run})]))}]})

(defcard "ONR Live News Feed"
  (letfn [(update-var [state side card var x]
            (let [old (or (get-in (get-card state card) [:special var]) 0)]
              (update! state side (assoc-in (get-card state card) [:special var] (+ x old)))))]
    {:makes-run true
     :on-play {:async true
               :prompt "Choose a server"
               :choices (req runnable-servers)
               :effect (effect
                         (update-var card :black-ice 0)
                         (update-var card :black-op-stolen 0)
                         (update-var card :black-op-rezzed 0)
                         (make-run eid target (get-card state card)))}
     :events [{:event :encounter-ice
               :silent (req true)
               :req (req (has-subtype? (:ice target) "Black Ice"))
               :effect (effect (update-var card :black-ice 1))}
              {:event :rez
               :silent (req true)
               :req (req (has-subtype? (:card target) "Black Ops"))
               :effect (effect (update-var card :black-op-rezzed 1))}
              {:event :agenda-stolen
               :silent (req true)
               :req (req (has-subtype? (:card target) "Black Ops"))
               :effect (effect (update-var card :black-op-stolen 1))}
              {:event :run-ends
               :req (req (and (:successful target)
                              this-card-run))
               :async true
               :effect (req
                         (let [black-ice (get-in (get-card state card) [:special :black-ice])
                               bo-rezzed (get-in (get-card state card) [:special :black-op-rezzed])
                               bo-stolen (get-in (get-card state card) [:special :black-op-stolen])
                               total (+ black-ice bo-rezzed bo-stolen)]
                           (continue-ability
                             state side
                             {:msg (msg "gain 2 tags and give the corp " total " bad publicity")
                              :effect (req (wait-for (gain-tags state :corp (make-eid state eid) 2)
                                                     (gain-bad-publicity state :runner eid total)))
                              :async true}
                             card nil)))}]}))

(defcard "ONR Livewire's Contacts"
  {:on-play
   {:msg "gain 3 [Credits]"
    :async true
    :effect (effect (gain-credits eid 3))}})

(defcard "ONR Lucidrine [TM] Booster Drug"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (gain-next-run-credits 9)
                             (make-run eid target card))}
   :events [{:event :run-ends
             :req (req this-card-run)
             :msg "take 1 brain damage"
             :effect (effect (damage eid :brain 1 {:unpreventable true
                                                   :card card}))}]})

(defcard "ONR MIT West Tier"
  {:on-play
   {:msg (msg (if (not (zone-locked? state :runner :discard))
                "shuffle the grip and heap into the stack and draw 5 cards"
                "shuffle the grip into the stack and draw 5 cards"))
    :rfg-instead-of-trashing true
    :async true
    :effect (effect (shuffle-into-deck :hand :discard)
                    (draw eid 5))}})

(defcard "ONR Mantis, Fixer-at-Large"
  {:on-play {:prompt "Choose a card"
             :msg "add 1 card from the stack to the grip"
             :choices (req (cancellable (distinct (:deck runner)) :sorted))
             :effect (effect (trigger-event :searched-stack nil)
                             (shuffle! :deck)
                             (move target :hand))}})

(defcard "ONR Meat Upgrade"
  {:on-play
   {:msg "remove 2 tags and draw 3 cards"
    :async true
    :effect (req (wait-for (lose-tags state side 2)
                           (draw state side eid 3)))}})

(defcard "ONR Networking"
  {:on-play
   {:msg "gain 9 [Credits]"
    :async true
    :effect (effect (gain-credits eid 9))}})

(defcard "ONR On the Fast Track"
  (letfn [(trashed-transaction [state] ;;onr/1996nr interop
            (seq (filter #(or (has-subtype? % "Transactions") (has-subtype? % "Transaction"))
                         (map :card (map first (turn-events state :runner :runner-trash))))))
          (trashed-advertisement [state]
            (seq (filter #(has-subtype? % "Advertisement")
                         (map :card (map first (turn-events state :runner :runner-trash))))))]
          ;; (map second)
          ;;        (map :card)
          ;;        (filter #(has-subtype? % "Advertisement"))
          ;; seq))]
    {:on-play
     {:req (req (or (trashed-transaction state) (trashed-advertisement state)))
      :msg (msg (if (trashed-advertisement state)
                  "gain 8 [Credits]"
                  "gain 6 [Credits]"))
      :async true
      :effect (req (gain-credits state side eid (if (trashed-advertisement state) 8 6)))
     }}))

(defcard "ONR Open-Ended(R) Mileage Program"
  {:on-play
   {:async true
    :req (req (pos? (count-real-tags state)))
    :msg "remove 1 tag"
    :effect (req (wait-for (lose-tags state side 1)
                           (continue-ability
                             state side
                             {:optional
                              {:prompt (msg "Pay 1 [Credits] to add " (:title card) " to Grip?")
                               :yes-ability
                               {:cost [:credit 1]
                                :msg "add itself to the Grip"
                                :effect (effect (move card :hand))}}}
                             card nil)))}})

(defcard "ONR Organ Donor"
  {:on-play
   {:choices {:max 5
              :card #(and (in-hand? %))}
    :prompt "trash up to 5 cards"
    :msg (msg "trash " (enumerate-str (map :title targets)) " and gain "
              (* 2 (count targets)) " [Credits]")
    :async true
    :effect (req (wait-for (trash-cards state side targets {:unpreventable true :cause-card card})
                           (gain-credits state side eid (* 2 (count targets)))))}})

(defcard "ONR Panzer Run"
  {:on-play
   {:msg "gain 4 [Credits] and draw 2 cards"
    :async true
    :effect (req (wait-for (gain-credits state side 4)
                           (draw state side eid 2)))}})

(defcard "ONR Pirate Broadcast"
  (letfn [(continue-fn
          [pending-servers total]
          {:prompt "Choose a server"
           :choices (req pending-servers)
           :msg (msg "attempt a run on " target)
           :async true
           :effect (req
                     (wait-for (make-run state side target (get-card state card))
                                  (let [new-targets (remove #{target} pending-servers)]
                                    (if (seq new-targets)
                                      (continue-ability
                                        state side
                                        (continue-fn new-targets total)
                                        (get-card state card) nil)
                                      (continue-ability
                                        state side
                                        (if (= total (get-in (get-card state card) [:special :successes]))
                                          {:msg "give the Corp 1 bad publicity"
                                           :async true
                                           :effect (req (gain-bad-publicity state side eid 1))}
                                          {:msg "forgo their next action"
                                           :async true
                                           :effect (req (gain-click-debt state side eid 1))})
                                        (get-card state card) nil)))))})]
    {:makes-run true
     :implementation "doesn't handle redirects"
     :events [{:event :successful-run
               :silent (req true)
               :effect (effect (update! (assoc-in (get-card state card) [:special :successes] (+ 1 (get-in (get-card state card) [:special :successes])))))}]
     :on-play
     {:msg "make a run on each data fort"
      :async true
      :effect (req
                (update! state side (assoc-in card [:special :successes] 0))
                (continue-ability
                  state side
                  (continue-fn servers (count servers))
                  (get-card state card) nil))}}))

(defcard "ONR Playful AI"
    (letfn [(playful [n t r all]
              (if (> n 0)
                {;;:msg (msg (str "play a round (" (dec n) " more rounds remaining)"))
                 :effect (req (let [roll (dice-roll)
                                    all (concat all [roll])]
                                (continue-ability
                                 state :runner
                                 (if (> roll 3)
                                   {;:msg (msg "roll out (" roll ")")
                                    :effect (req (continue-ability
                                                    state side
                                                    (playful (dec n) t (inc r) all)
                                                    card nil))}
                                   ;; choose to set aside or take credits
                                   {;;:msg (msg "roll " roll " and must make choices")
                                    :prompt (str "Set aside how many dice (you rolled " roll ", have " (dec n) " dice left, and " t " credits)")
                                    :waiting-prompt "waiting for runner to decide the stakes"
                                    :choices (req [(when (= roll 3) "3")
                                                   (when (> roll 1) "2")
                                                   "1"
                                                   "0"])
                                    :effect (req (let [setaside (Integer/parseInt target)
                                                       creds (- roll (Integer/parseInt target))]
                                                   (do (when (pos? creds)
                                                         (do (gain-credits state :runner eid creds)
                                                             (system-msg
                                                              state side
                                                              (str "gains " creds
                                                                   " from ONR Playful AI"))))
                                                       ;;(system-msg
                                                       ;; state side
                                                       ;; (str "sets aside " setaside " dice"))
                                                       (continue-ability
                                                        state side
                                                        (playful (+ (dec n) setaside)
                                                                 (+ t creds)
                                                                 (inc r)
                                                                 all)
                                                        card nil))))})
                                 card nil)))}
                {:msg (msg (str "gain a total of "t " [Credits] over " r " rolls " (seq all)))
                 :effect (req (when (> 30 (count all))
                                (say state side {:text "Garfield was truly a madman"})))}))]
      {:on-play
       {:async true
        :effect (req (system-msg state side "decides to play with an AI")
                     (continue-ability state side (playful 1 0 0 []) card nil))}}))

(defcard "ONR Poisoned Water Supply" ;; todo - make this not a cost!
  {:on-play {:additional-cost [:connection 2]
             :msg "give the corp 1 bad publicity"
             :effect (req (gain-bad-publicity state side eid 1))}})

(defcard "ONR Prearranged Drop"
  {:events [{:event :access
             :req (req (agenda? target))
             :duration :end-of-turn
             :once :per-turn
             :unregister-once-resolved true
             :msg "gain 6 [Credits]"
             :async true
             :effect (effect (gain-credits eid 6))}]})

(defcard "ONR Priority Wreck"
  {:makes-run true
   :on-play {:req (req hq-runnable)
             :async true
             :effect (effect (make-run eid :hq card))}
   :events [(successful-run-replace-breach
              {:target-server :hq
               :this-card-run true
               :mandatory true
               :ability
               {:async true
                :prompt "How many [Credits] do you want to spend?"
                :choices :credit
                :msg (msg "make the Corp lose " target " [Credits]")
                :effect (req (lose-credits state :corp eid target))}})]})

(defcard "ONR Private LDL Access"
  {:on-play {:msg "make a run on HQ"
             :makes-run true
             :async true
             :effect (effect (register-events
                               card
                               [{:event :pre-successful-run
                                 :duration :end-of-run
                                 :unregister-once-resolved true
                                 :interactive (req true)
                                 :msg "change the attacked server to R&D"
                                 :req (req (= :hq (-> run :server first)))
                                 :effect (req (swap! state assoc-in [:run :server] [:rd])
                                              (trigger-event state :corp :no-action))}])
                             (make-run eid :hq (get-card state card)))}})

(defcard "ONR Promises, Promises"
  {:on-play {:msg "gain 1 agenda point the next time they access an agenda this turn"
             :effect (req (register-events
                            state side card
                            [{:event :access
                              :duration :end-of-turn
                              :unregister-once-resolved true
                              :req (req (agenda? target))
                              :msg "gain 1 agenda point"
                              :async true
                              :effect (req (gain-agenda-points state side eid 1))}]))}})

(defcard "ONR Reconnaissance"
  {:makes-run true
   :events [{:event :rez
             :async true
             :effect (req (gain-credits state side eid 1))}]
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}})

(defcard "ONR Remote Detonator"
  {:on-play {:req (req (pos? (count (:successful-run runner-reg))))
             :async true
             :effect (req (let [target-servers (distinct (map zone->name (:successful-run runner-reg)))]
                            (continue-ability
                              state side
                              {:prompt "Blow up rezzed ice where?"
                               :choices target-servers
                               :async true
                               :msg (msg "trash all rezzed ice protecting " target " and take 3 tags")
                               :effect (req (let [zone (second (server->zone state target))
                                                  target-ice (filter #(and (rezzed? %) (ice? %)
                                                                          (= [:servers zone :ices] (:zone %)))
                                                                     (all-installed state :corp))]
                                              (if (not (empty? target-ice))
                                                (wait-for (trash-cards state side (make-eid state eid) target-ice)
                                                          (system-msg state side (str "trashes " (str/join (map :title target-ice)) " protecting " target))
                                                          (gain-tags state :corp eid 3))
                                                (gain-tags state :corp eid 3))))}
                              card nil)))}})

(defcard "ONR Romp through HQ"
  {:makes-run true
   :on-play {:req (req hq-runnable)
             :async true
             :effect (effect (make-run eid :hq card))}
   :interactions {:access-ability
                  {:label "Trash card"
                   :req (req (can-trash? state :runner target))
                   :msg (msg "trash " (:title target) " at no cost")
                   :async true
                   :effect (effect (trash eid (assoc target :seen true) {:cause-card card}))}}})

(defcard "ONR Running Interference"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (register-lingering-effect
                               card
                               {:type :rez-additional-cost
                                :duration :end-of-run
                                :req (req (ice? target))
                                :value (req [:credit (:cost target)])})
                             (make-run eid target card))}})

(defcard "ONR Rush Hour"
  {:makes-run true
   :implementation "Noisy restriction not enforced" ;; todo fix this
   :on-play {:req (req rd-runnable)
             :async true
             :effect (effect (make-run eid :rd card))}
   :events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :rd (target-server context))
                            this-card-run))
             :effect (effect (register-events
                              card [(breach-access-bonus :rd 3 {:duration :end-of-run})]))}]})

(defcard "ONR Score!"
  {:on-play
   {:msg "gain 9 [Credits]"
    :async true
    :effect (effect (gain-credits eid 9))}})

(defcard "ONR Security Code WORM Chip"
  {:on-play
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :prompt "Choose an unrezzed piece of ice"
    :choices {:req (req (and (installed? target)
                             (ice? target)
                             (not (rezzed? target))))}
    :async true
    :effect (effect (trash eid target {:cause-card card}))}})

(defcard "ONR Senatorial Field Trip"
  {:implementation "Erratum - Choose a rezzed piece of black ice the Corp rezzed this turn. The Corp either derezzes that piece of ice or receives 2 Bad Publicity points."
   :on-play
   {:req (req (let [relevant-rezzes (filter #(has-subtype? % "Black Ice")
                                            (map :card (map first (turn-events state side :rez))))
                    was-rezzed? (fn [c rr] (seq (filter #(same-card? % c) rr)))]
                (some #(and (was-rezzed? % relevant-rezzes) (rezzed? %) (ice? %))
                      (all-installed state :corp))))
    :choices {:req (req (let [relevant-rezzes
                              (filter #(has-subtype? % "Black Ice")
                                      (map :card (map first (turn-events state side :rez))))
                              was-rezzed? (fn [c rr] (seq (filter #(same-card? % c) rr)))]
                          (and (was-rezzed? target relevant-rezzes) (rezzed? target) (ice? target))))}
    :async true
    :effect (req (let [target-ice target
                       ch1 (str "Derez " (:title target-ice))
                       ch2 (str "Take 2 bad publicity")]
                  (continue-ability
                    state side
                    {:prompt "Choose one"
                     :choices [ch1 ch2]
                     :async true
                     :player :corp
                     :msg (msg (decapitalize target))
                     :effect (req (if (= target ch1)
                                    (do (derez state side target-ice)
                                        (effect-completed state side eid))
                                    (gain-bad-publicity state side eid 2)))}
                    card nil)))}})

(defcard "ONR Sneak Preview"
  {:on-play
   {:prompt (req (if (not (zone-locked? state :runner :discard))
                   "Install a program from the stack or heap?"
                   "Install a program from the stack?"))
    :choices (req ["Stack"
                   (when (not (zone-locked? state :runner :discard)) "Heap")])
    :msg (msg "install a program from the " target)
    :async true
    :effect (effect
              (continue-ability
                (let [where target]
                  {:prompt "Choose a program to install"
                   :choices (req (cancellable
                                   (filter program? ((if (= where "Heap") :discard :deck) runner))))
                   :async true
                   :effect (req (when (= where "Stack")
                                  (trigger-event state side :searched-stack nil)
                                  (shuffle! state side :deck))
                                (wait-for (runner-install state side (make-eid state {:source card :source-type :runner-install})
                                                          target {:ignore-all-cost true})
                                          (if async-result
                                            (let [installed-card (update! state side (assoc-in async-result [:special :test-run] true))]
                                              (register-events
                                                state side installed-card
                                                [{:event :runner-turn-ends
                                                  :duration :end-of-turn
                                                  :req (req (get-in (find-latest state installed-card) [:special :test-run]))
                                                  :msg (msg "move " (:title installed-card) " to the top of the stack")
                                                  :effect (effect (move (find-latest state installed-card) :deck {:front true}))}])
                                              (effect-completed state side eid))
                                            (effect-completed state side eid))))})
                card nil))}})

(defcard "ONR Social Engineering"
  (letfn [(choose-ice []
            {:player :runner
             :waiting-prompt true
             :prompt "Choose a piece of ice to bypass"
             :choices {:card ice?}
             :msg (msg "make a run and bypass " (card-str state target))
             :async true
             :effect (effect (register-events
                               card
                               (let [target-ice target]
                                 [{:event :encounter-ice
                                   :req (req (same-card? target-ice (:ice context)))
                                   :msg (msg "bypass " (:title (:ice context)))
                                   :effect (req (bypass-ice state))}]))
                             (make-run eid (second (get-zone target)) card))})
          (corp-guess [maxc chosen]
            {:prompt "Guess how many credits the runner wagered (at least 2)"
             :choices {:number (req maxc)}
             :player :corp
             :async true
             :effect (req
                       (system-msg state side "check wager")
                       (if (< target 2)
                            (continue-ability state side (corp-guess maxc chosen) card nil)
                            (if (= target chosen)
                              (do (system-msg state :corp (str "guesses the runner wagered " target " - and they're correct!"))
                                  (continue-ability
                                    state :runner
                                    {:msg (msg "lose " chosen " [Credits]")
                                     :player :runner
                                     :async true
                                     :effect (req (lose-credits state :runner eid chosen))}
                                    card nil))
                              (do (system-msg state :corp (str "guesses the runner wagered " target " - but they actually wagered " chosen "!"))
                                  (continue-ability
                                    state :runner
                                    (choose-ice)
                                    card nil)))))})
            (choose-abi [maxc]
              {:prompt "Secretly choose a number (at least 2)"
               :choices {:number (req maxc)}
               :async true
               :effect (req (if (< target 2)
                              (continue-ability state side (choose-abi maxc) card nil)
                              (continue-ability
                                state side
                                (corp-guess maxc target)
                                card nil)))})]
    {:on-play {:req (req (<= 3 (:credit runner)))
               :async true
               :effect (req (if (<= 2 (:credit runner))
                              (continue-ability
                                state side
                                (choose-abi (:credit runner))
                                card nil)
                              (effect-completed state side eid)))}}))

(defcard "ONR Stakeout"
  {:on-play
   {:msg "gain 2 [Credits] and draw 1 card"
    :async true
    :effect (req (wait-for (gain-credits state side 2)
                           (draw state side eid 1)))}})

(defcard "ONR Stumble through Wilderspace"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :static-abilities [{:type :link-for-run
                       :req (req true)
                       :value 9}]})

(defcard "ONR Subliminal Corruption"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :runner-trash
             :req (req (has-subtype? (:card target) "Advertisement"))
             :msg (msg "give the Corp 1 bad publicity")
             :async true
             :effect (req (gain-bad-publicity state :runner eid 1))}]})

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

(defcard "ONR Temple Microcode Outlet"
  {:on-play
   {:prompt "Choose a program"
    :choices (req (cancellable (filter #(program? %) (:deck runner)) :sorted))
    :msg (msg "add " (:title target) " from the stack to the grip and shuffle the stack")
    :effect (effect (trigger-event :searched-stack nil)
                    (shuffle! :deck)
                    (move target :hand))}})

(defcard "ONR Terrorist Reprisal"
  {:on-play {:req (req (:scored-black-ops corp-reg-last))
             :msg (msg "force the Corp to discard 5 cards from HQ at random")
             :async true
             :effect (req (trash-cards state :corp eid (take 5 (shuffle (:hand corp))) {:cause-card card :cause :force-to-discard}))}})

(defcard "ONR Test Spin"
  {:makes-run true
   :on-play {:prompt "Choose a program to install"
             :msg (msg "install a program from the stack")
             :choices (req (cancellable
                             (filter program? (:deck runner))))
             :async true
             :effect (req (trigger-event state side :searched-stack nil)
                          (shuffle! state side :deck)
                          (wait-for (runner-install state side (make-eid state {:source card :source-type :runner-install})
                                                    target {:ignore-all-cost true})
                                    (let [installed-card (update! state side (assoc-in async-result [:special :test-run] true))]
                                      (if (and installed-card (installed? installed-card))
                                        (continue-ability
                                          state side
                                          {:prompt "Choose a server"
                                           :choices (req runnable-servers)
                                           :async true
                                           :effect (req (wait-for (make-run state side (make-eid state eid) target card)
                                                                  (let [nch (find-latest state installed-card)
                                                                        is-same (get-in nch [:special :test-run])]
                                                                    (if is-same
                                                                      (continue-ability
                                                                        state side
                                                                        {:msg (msg "shuffles " (:title nch) " into the Stack")
                                                                         :effect (effect (move (find-latest state nch) :deck)
                                                                                         (shuffle! :deck))}
                                                                        card nil)
                                                                      (let [repayment (+ 4 (:cost card))
                                                                            can-repay (:credit runner)
                                                                            meat (- repayment can-repay)]
                                                                        (do (system-msg state side (str "loses " can-repay " [Credits]"
                                                                                                        (when (pos? meat)
                                                                                                          (str " and suffers " meat " meat damage"))
                                                                                                        " to make up for losing track of " (:title installed-card)))
                                                                            (wait-for (lose-credits state side (make-eid state eid) can-repay)
                                                                                      (damage state side eid :meat meat))))))))}
                                          card nil)
                                        (effect-completed state side eid)))))}})

(defcard "ONR The Personal Touch"
  {:implementation "Places a militech counter instead. Send me an @ if there are issues"
   :on-play {:req (req (and (some #(has-subtype? % "Icebreaker") (all-installed state :runner))))
             :choices {:card #(and (installed? %)
                                   (has-subtype? % "Icebreaker"))}
             :effect (req (add-counter state side target :militech 1)
                          (register-effect-once
                            state side card
                            {:type :breaker-strength
                             :ability-name "Militech Counters"
                             :req (req true)
                             :value (req (get-counters (get-card state target) :militech))}))}})

(defcard "ONR Total Genetic Retrofit"
  {:on-play {:msg (msg "remove all tags, and avoid the next tag")
             :async true
             :effect (req (wait-for (lose-tags state side (make-eid state eid) :all)
                                    (register-events
                                      state side (:identity runner)
                                      [{:event :pre-tag
                                        :persistent true
                                        :ability-name "Genetic Retrofit"
                                        :unregister-once-resolved true
                                        :async true
                                        :msg "avoid the first tag received"
                                        :effect (effect (tag-prevent :runner eid 1))}])
                                    (effect-completed state side eid)))}})

(defcard "ONR Valu-Pak Software Bundle"
  {:implementation "Gain (1 credit and 5 actions) for installing programs. You may forgo these clicks. Manually remove clicks and credits you don't use."
   :msg (msg "gain 1 credit and 5 consecutive actions for installing cards")
   :effect (req (wait-for (gain-credits state side (make-eid state eid) 1)
                          (gain-clicks state side 5)
                          (effect-completed state side eid)))
   :async true})

(defcard "ONR Weather-to-Finance Pipe"
    {:makes-run true
     :on-play {:req (req hq-runnable)
               :async true
               :effect (effect (make-run eid :hq card))}
     :events [(successful-run-replace-breach
                {:target-server :hq
                 :mandatory true
                 :this-card-run true
                 :ability
                 {:async true
                  :msg (msg "force the Corp to lose " (min 4 (:credit corp))
                            " [Credits]")
                  :effect (req (let [creds-lost (min 4 (:credit corp))]
                                 (lose-credits state :corp eid creds-lost)))}})]})

(defcard "ONR Weefle Initiation"
  {:interactions {:prevent [{:type #{:net :brain :meat}
                             :req (req true)}]}
   :makes-run true
   :implementation "prevent ability on the card uses power counters"
   :data {:counter {:power 7}}
   :abilities [{:label "prevent 1 damage"
                :msg "prevent 1 damage"
                :cost [:power 1]
                :effect (effect (damage-prevent :net 1)
                                (damage-prevent :brain 1)
                                (damage-prevent :meat 1))}]
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}})

(defcard "ONR misc.for-sale"
  {:on-play
   {:req (req (some installed?
                    (all-installed state :runner)))
    :prompt "Choose any number of installed cards to trash"
    :choices {:max (req (count (all-installed state :runner)))
              :card #(and (installed? %)
                          (runner? %))}
    :msg (msg "trash " (enumerate-str (map :title targets))
              " and gain " (* (count targets) 3) " [Credits]")
    :async true
    :effect (req (wait-for (trash-cards state side targets {:cause-card card})
                           (gain-credits state side eid (* (count targets) 3))))}})
