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
                          update-breaker-strength update-current-encounter]]
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

(defn- dice-roll [] (inc (rand-int 6)))

;; Card definitions

(defcard "ONR \"Armadillo\" Armored Road Home"
  {:interactions {:prevent [{:type #{:meat}
                             :req (req true)}]
                  :pay-credits {:req (req (or (= :remove-tag (:source-type eid))
                                              (and (same-card? (:source eid) (:basic-action-card runner))
                                                   (= 5 (:ability-idx (:source-info eid))))))
                                :type :recurring}}
   :recurring 2
   :abilities [{:label "Trash to prevent up to 3 meat damage"
                :msg "prevent up to 3 meat damage"
                :cost [:trash-can]
                :effect (effect (damage-prevent :meat 3))}]})

(defcard "ONR \"Drifter\" Mobile Environment"
  {:interactions {:pay-credits {:req (req (or (= :remove-tag (:source-type eid))
                                              (and (same-card? (:source eid) (:basic-action-card runner))
                                                   (= 5 (:ability-idx (:source-info eid))))))
                                :type :recurring}}
   :recurring 2})

(defcard "ONR \"Green Knight\" Surge Buffers"
  {:interactions {:prevent [{:type #{:net}
                             :req (req true)}]}
   :abilities [{:cost [:credit 0]
                :once :per-turn
                :msg "prevent 1 net damage"
                :effect (effect (damage-prevent :net 1))}]})

(defcard "ONR Arasaka Portable Prototype"
  {:recurring 3
   :additional-cost [:agenda-point 1]
   :static-abilities [(mu+ 3)]
   :interactions {:pay-credits {:req (req (and run
                                               (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "ONR Armored Fridge"
  {:data {:counter {:power 7}}
   ;; todo - ablative counters
   :implementation "just uses power counters for now"
   :interactions {:prevent [{:type #{:meat}
                             :req (req true)}]}
   :events [(trash-on-empty :power)]
   :abilities [{:cost [:power 1]
                :msg "prevent 1 meat damage"
                :effect (req (damage-prevent state side :meat 1))}]})

(defcard "ONR Artemis 2020"
  {:recurring 2
   :static-abilities [(mu+ 2)]
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

(defcard "ONR Corolla Speed Chip"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Killer")
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "ONR Cortical Cybermodem"
  {:recurring 2
   :static-abilities [(runner-hand-size+ 2)
                      (mu+ 2)]
   :interactions {:pay-credits {:req (req (and run
                                               (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "ONR Cortical Stimulators"
  {:interactions {:prevent [{:type #{:net :brain}
                             :req (req true)}]}
   :abilities [{:cost [:credit 0]
                :once :per-turn
                :msg "prevent 1 net or brain damage"
                :effect (effect
                          (damage-prevent :brain 1)
                          (damage-prevent :net 1))}]})

(defcard "ONR Dermatech Bodyplating"
  {:interactions {:prevent [{:type #{:meat}
                             :req (req true)}]}
   :abilities [{:cost [:credit 0]
                :once :per-turn
                :msg "prevent 1 meat damage"
                :effect (effect
                          (damage-prevent :meat 1))}]})

(defcard "ONR Eurocorpse [TM] Spin Chip"
  {:recurring 2
   :abilities [{:async true
                :label "Install and host a program of 1[mu] or less"
                :req (req (empty? (:hosted card)))
                :cost [:click 1]
                :prompt "Choose a program of 1[mu] or less in the grip"
                :choices {:card #(and (program? %)
                                      (<= (:memoryunits %) 1)
                                      (in-hand? %))}
                :msg (msg "install and host " (:title target))
                :effect (effect (runner-install eid target {:host-card card :no-mu true}))}
               {:label "Host an installed program of 1[mu] or less (manual)"
                :prompt "Choose an installed program of 1[mu] or less"
                :choices {:card #(and (program? %)
                                      (<= (:memoryunits %) 1)
                                      (installed? %))}
                :msg (msg "host " (:title target))
                :effect (effect (host card target)
                                (unregister-effects-for-card target #(= :used-mu (:type %)))
                                (update-mu))}]
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (program? target)
                                               (same-card? card (:host target))))
                                :type :recurring}}})

(defcard "ONR Full Body Conversion" ;;TODO - come back here
  {:implementation "unimplemented"})

(defcard "ONR HQ Interface"
  {:events [(breach-access-bonus :hq 1)]})

(defcard "ONR Lifesaver [TM] Nanosurgeons"
  {:implementation "damage requirement not enforced. Damage BETWEEN actions does not count!"
   :interactions {:prevent [{:type #{:brain}
                             :req (req true)}]}
   :abilities [{:cost [:trash-can]
                :once :per-turn
                :msg "prevent 1 brain damage"
                :effect (effect
                          (damage-prevent :brain 1))}
               {:msg "draw 2 cards"
                :keep-menu-open :while-clicks-left
                :cost [:click 1]
                :async true
                :effect (req (draw state :runner eid 2))}]})

(defcard "ONR Little Black Box"
  {:recurring 1
   :static-abilities [(runner-hand-size+ 1)
                      (mu+ 1)]
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}
                  :prevent [{:type #{:net :brain}
                             :req (req true)}]}
   :abilities [{:cost [:credit 0]
                :once :per-turn
                :msg "prevent 1 net or brain damage"
                :effect (effect
                          (damage-prevent :brain 1)
                          (damage-prevent :net 1))}]})

(defcard "ONR Lucidrine [TM] Drip Feed"
  {:implementation "uses power counters instead of Drip counters"
   :events [{:event :runner-turn-begins
             :effect (req (if (>= (get-counters card :power) 2)
                            (do (add-counter state side card :power (- (get-counters card :power)))
                                (damage state side eid :brain 1 {:unpreventable true :card card})
                                (system-msg state side "takes 1 brain damage from " (:title card)))
                            (do (add-counter state side card :power 1)
                                (gain-clicks state side 1)
                                (system-msg state side (str "uses " (:title card) " to gain [Click]")))))}]})

(defcard "ONR MRAM Chip"
  {:static-abilities [(runner-hand-size+ 2)]})

(defcard "ONR Microtech 'Trode Set"
  {:static-abilities [{:type :break-sub-additional-cost
                       :req (req true)
                       :value [:credit 1]}]
   :implementation "if this doesn't work, send me a message"
   :interactions {:prevent [{:type #{:net}
                             :req (req (if (and current-ice (has-subtype? current-ice "AP"))
                                         true
                                         false))}]}
   :abilities [{:msg (msg "prevent " (dec (:dmg-amount card)) " net damage")
                :label "prevent all but 1 net damage from AP sub"
                :req (req (and current-ice (has-subtype? current-ice "AP")
                               (pos? (or (:dmg-amount card) 0))))
                :cost [:credit 0]
                :effect (effect
                          (update! (assoc card :dmg-amount 0))
                          (damage-prevent :net (dec (:dmg-amount card))))}]
   :events [{:event :pre-damage
             :req (req (and (= target :net)
                            current-ice (has-subtype? current-ice "AP")))
             :effect (effect (update! (assoc card :dmg-amount (nth targets 2))))}
            {:event :pre-resolve-subroutine
             :req (req (let [sub (first targets)
                             ice (second targets)]
                         (when (and (has-subtype? ice "AP")
                                    (not (or (str/includes? (:label sub) "trace")
                                             (str/includes? (:label sub) "Trace")
                                             (str/includes? (:label sub) "Net Damage")
                                             (str/includes? (:label sub) "Net damage")
                                             (str/includes? (:label sub) "net damage"))))
                           (system-msg state side (str "ignores the effect of '" (:label sub) "' subroutine"))
                           (update-current-encounter state :prevent-subroutine true))))}]})

(defcard "ONR Microtech Backup Drive"
  {:implementation "Manually host programs with the card ability"
   :abilities [{:label "Manually host a resource or piece of hardware"
                :cost [:credit 0]
                :keep-menu-open :while-clicks-left
                :prompt "Choose a program"
                :show-discard true
                :choices {:card #(and (program? %)
                                      (in-discard? %))}
                :effect (effect (host card target))
                :msg (msg "host " (:title target))}
               {:label "Add the last program to hand"
                :req (req (seq (:hosted card)))
                :cost [:click 1]
                :msg (msg "move " (:title (first (:hosted card))) " to the Grip")
                :effect (req (move state side (first (:hosted card)) :hand))}]})

(defcard "ONR Militech MRAM Chip"
  {:static-abilities [(runner-hand-size+ 3)]})

(defcard "ONR Nasuko Cycle"
  {:interactions {:prevent [{:type #{:tag}
                             :req (req true)}]}
   :abilities [{:async true
                :cost [:credit 3]
                :msg "avoid 1 tag"
                :effect (effect (tag-prevent :runner eid 1))}]})

(defcard "ONR Omnitech Wet Drive"
  (let [update-base-mu (fn [state n] (swap! state assoc-in [:runner :memory :base] n))]
    {:effect (req (update-base-mu state (count (get-in @state [:runner :hand])))
                  (add-watch state :ekomind (fn [_k ref old new]
                                                (let [hand-size (count (get-in new [:runner :hand]))]
                                                  (when (not= (count (get-in old [:runner :hand])) hand-size)
                                                    (update-base-mu ref hand-size))))))
     :leave-play (req (remove-watch state :ekomind))}))

(defcard "ONR Omnitech \"Spinal Tap\" Cybermodem"
  {:static-abilities [(mu+ 1)]
   :recurring 2
   :interactions {:pay-credits {:req (req (or (and run
                                                   (= :ability (:source-type eid))
                                                   (has-subtype? target "Icebreaker"))
                                              (= :trace (:source-type eid))))
                                :type :recurring}}
   :events [{:event :runner-turn-begins
             :interactive (req true)
             :ability-label "Roll for 2 brain damage"
             :async true
             :effect (req (let [di (dice-roll)]
                            (continue-ability
                              state side
                              (if (= 1 di)
                                {:msg (msg "rolls a 1(1d6), and suffers 2 brain damage (cannot be prevented)")
                                 :async true
                                 :effect (req (damage state side eid :brain 2 {:unboostable true
                                                                               :unpreventable true
                                                                               :card card}))}
                                {:msg (msg "rolls a " di "(1d6)")})
                              card nil)))}]
   :leave-play (req (system-msg state side "suffers 2 brain damage")
                    (damage state side eid :brain 2 {:unboostable true :card card}))})

(defcard "ONR PK-6089a"
  {:recurring 3
   :static-abilities [(mu+ 1)]
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(defcard "ONR Pandora's Deck"
  {:recurring 3
   :static-abilities [(mu+ 2)]
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(defcard "ONR Parraline 5750"
  {:recurring 1
   :static-abilities [(mu+ 1)]
   :interactions {:pay-credits {:req (req (and run
                                               (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "ONR R&D Interface"
  {:events [(breach-access-bonus :rd 1)]})

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

(defcard "ONR Record Reconstructor"
  (let [ability (successful-run-replace-breach
                  {:target-server :archives
                   :duration :end-of-run
                   :ability
                   {:async true
                    :effect (req
                              (let [chosen (take 2 (shuffle (filter faceup? (:discard corp))))]
                                (continue-ability
                                  state side
                                  {:msg (msg "place " (str/join ", " (map :title chosen)) " on the top of R&D (first card is ontop)")
                                   :effect (req (doseq [c (reverse chosen)]
                                                  (move state :corp c :deck {:front true})))}
                                  card nil)))}})]
    {:abilities [{:cost [:click 1]
                  :msg "make a run on Archives"
                  :makes-run true
                  :async true
                  :effect (effect (register-events card [ability])
                                  (make-run eid :archives card))}]}))

(defcard "ONR Sunburst Cranial Interface"
  {:recurring 1
   :static-abilities [(mu+ 1)
                      (runner-hand-size+ 1)]
   :interactions {:pay-credits {:req (req (and run
                                               (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")
                                               (not (has-subtype? target "Noisy"))))
                                :type :recurring}}})

(defcard "ONR Techtronica [TM] Utility Suit"
  {:recurring 5
   :static-abilities [(mu+ 1)]
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}
                  :prevent [{:type #{:meat}
                             :req (req true)}]}
   :abilities [{:cost [:credit 0]
                :once :per-turn
                :msg "prevent 1 meat damage"
                :effect (effect
                          (damage-prevent :meat 1))}]})

(defcard "ONR The Deck"
  {:abilities [(base-link-abi 0 5)
               (boost-link-abi 1 1)]
   :static-abilities [(mu+ 1)]})

(defcard "ONR Tycho Mem Chip"
  {:static-abilities [(mu+ 3)]})

(defcard "ONR Vintage Camaro"
  {:interactions {:prevent [{:type #{:tag}
                             :req (req true)}]}
   :abilities [{:async true
                :cost [:credit 1 :forgo-next-click 1]
                :msg "avoid 1 tag"
                :effect (effect (tag-prevent :runner eid 1))}]})

(defcard "ONR WuTech Mem Chip"
  {:static-abilities [(mu+ 1)]})

(defcard "ONR ZZ22 Speed Chip"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Killer")
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})


(defcard "ONR Zetatech Mem Chip"
  {:static-abilities [(mu+ 2)]})

(defcard "ONR Zetatech Portastation"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and
                                           (event? target)
                                           (or (= 0 (count (:cost-paid eid)))
                                               (:x-cost eid))
                                           (= :play (:source-type eid))))
                                :type :recurring}}})
