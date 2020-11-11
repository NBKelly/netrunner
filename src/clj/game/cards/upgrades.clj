(ns game.cards.upgrades
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [jinteki.utils :refer :all]
            [clojure.string :as string]))

;; Card definitions

(defcard "Akitaro Watanabe"
  {:constant-effects [{:type :rez-cost
                       :req (req (and (ice? target)
                                      (= (card->server state card) (card->server state target))))
                       :value -2}]})

(defcard "Amazon Industrial Zone"
  {:events [{:event :corp-install
             :optional
             {:req (req (and (ice? target)
                             (protecting-same-server? card target)
                             (can-pay? state side (assoc eid :source card :source-type :rez) target nil
                                       [:credit (rez-cost state side target {:cost-bonus -3})])))
              :prompt "Rez ICE with rez cost lowered by 3?"
              :yes-ability {:msg (msg "lower the rez cost of " (:title target) " by 3 [Credits]")
                            :effect (effect (rez eid target {:cost-bonus -3}))}}}]})

(defcard "Arella Salvatore"
  (let [select-ability
        {:prompt "Select a card to install with Arella Salvatore"
         :choices {:card #(and (corp-installable-type? %)
                               (in-hand? %)
                               (corp? %))}
         :async true
         :cancel-effect (req (effect-completed state side eid))
         :effect (req (wait-for (corp-install state :corp target nil {:ignore-all-cost true :display-message false})
                                (let [inst-target (find-latest state target)]
                                  (add-prop state :corp inst-target :advance-counter 1 {:placed true})
                                  (system-msg state :corp
                                              (str "uses Arella Salvatore to install and place a counter on "
                                                   (card-str state inst-target) ", ignoring all costs"))
                                  (effect-completed state side eid))))}]
    {:events [{:event :agenda-scored
               :req (req (= (:previous-zone target) (get-zone card)))
               :interactive (req (some corp-installable-type? (:hand corp)))
               :silent (req (not-any? corp-installable-type? (:hand corp)))
               :async true
               :effect (req (if (some corp-installable-type? (:hand corp))
                              (continue-ability state side select-ability card nil)
                              (effect-completed state side eid)))}]}))

(defcard "Ash 2X3ZB9CY"
  {:events [{:event :successful-run
             :interactive (req true)
             :req (req this-server)
             :trace {:base 4
                     :successful
                     {:msg "prevent the Runner from accessing cards other than Ash 2X3ZB9CY"
                      :effect (effect (set-only-card-to-access card)
                                      (effect-completed eid))}}}]})

(defcard "Awakening Center"
  {:can-host (req (ice? target))
   :abilities [{:label "Host a piece of Bioroid ICE"
                :cost [:click 1]
                :prompt "Select a piece of Bioroid ICE to host on Awakening Center"
                :choices {:card #(and (ice? %)
                                      (has-subtype? % "Bioroid")
                                      (in-hand? %))}
                :msg "host a piece of Bioroid ICE"
                :async true
                :effect (req (corp-install state side eid target card {:ignore-all-cost true}))}
               {:req (req (and this-server
                               (zero? (get-in @state [:run :position]))
                               (some #(can-pay? state side (assoc eid :source card :source-type :rez) % nil
                                                [:credit (rez-cost state side % {:cost-bonus -7})])
                                     (:hosted card))))
                :label "Rez a hosted piece of Bioroid ICE"
                :prompt "Choose a piece of Bioroid ICE to rez"
                :choices (req (:hosted card))
                :msg (msg "lower the rez cost of " (:title target) " by 7 [Credits] and force the Runner to encounter it")
                :effect (effect (rez eid target {:cost-bonus -7})
                                (update! (dissoc (get-card state target) :facedown))
                                (register-events
                                  card
                                  (let [ice target]
                                    [{:event :run-ends
                                      :duration :end-of-run
                                      :async true
                                      :req (req (get-card state ice))
                                      :effect (effect (trash eid (get-card state ice) nil))}])))}]})

(defcard "Bamboo Dome"
  (letfn [(reorder-cards []
            {:async true
             :effect (req (let [from (take 2 (:deck corp))]
                            (if (pos? (count from))
                              (continue-ability state side (reorder-choice :corp :runner from '()
                                                                           (count from) from)
                                                card nil)
                              (do (clear-wait-prompt state :runner)
                                  (effect-completed state side eid)))))})]
    {:install-req (req (filter #{"R&D"} targets))
     :abilities [{:cost [:click 1]
                  :req (req (pos? (count (:deck corp))))
                  :async true
                  :msg (msg (str "reveal " (string/join ", " (map :title (take 3 (:deck corp)))) " from R&D"))
                  :label "Reveal the top 3 cards of R&D. Secretly choose 1 to add to HQ. Return the others to the top of R&D, in any order."
                  :effect (req
                            (wait-for
                              (reveal state side (take 3 (:deck corp)))
                              (show-wait-prompt state :runner "Corp to use Bamboo Dome")
                              (continue-ability
                                state side
                                {:prompt "Select a card to add to HQ"
                                 :async true
                                 :choices (take 3 (:deck corp))
                                 :not-distinct true
                                 :msg "secretly add card to HQ"
                                 :effect (req (move state side target :hand)
                                              (continue-ability state side (reorder-cards) card nil))}
                                card nil)))}]}))

(defcard "Ben Musashi"
  {:on-trash
   {:req (req (and (= :runner side)
                   (:run @state)))
    :effect (effect (register-events
                      card
                      [{:event :pre-steal-cost
                        :duration :end-of-run
                        :req (req (or (= (get-zone target) (:previous-zone card))
                                      (= (central->zone (get-zone target))
                                         (butlast (:previous-zone card)))))
                        :effect (effect (steal-cost-bonus [:net 2]))}]))}
   :events [{:event :pre-steal-cost
             :req (req (or (in-same-server? card target)
                           (from-same-server? card target)))
             :effect (effect (steal-cost-bonus [:net 2]))}]})

(defcard "Bernice Mai"
  {:events [{:event :successful-run
             :interactive (req true)
             :req (req this-server)
             :trace {:base 5
                     :successful {:msg "give the Runner 1 tag"
                                  :async true
                                  :effect (effect (gain-tags :corp eid 1))}
                     :unsuccessful
                     {:async true
                      :effect (effect (system-msg "trashes Bernice Mai from the unsuccessful trace")
                                      (trash eid card nil))}}}]})

(defcard "Bio Vault"
  {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
   :advanceable :always
   :abilities [{:label "End the run"
                :req (req (:run @state))
                :msg "end the run"
                :async true
                :cost [:advancement 2 :trash]
                :effect (effect (end-run eid card))}]})

(defcard "Black Level Clearance"
  {:events [{:event :successful-run
             :async true
             :interactive (req true)
             :player :runner
             :req (req this-server)
             :prompt "Take 1 brain damage or jack out?"
             :choices ["Take 1 brain damage" "Jack out"]
             :effect (req (if (= target "Take 1 brain damage")
                            (damage state :runner eid :brain 1 {:card card})
                            (wait-for
                              (jack-out state :runner (make-eid state))
                              (wait-for
                                (gain-credits state :corp 5)
                                (wait-for
                                  (draw state :corp 1 nil)
                                  (system-msg state :corp
                                              (str "gains 5 [Credits] and draws 1 card. "
                                                   "Black Level Clearance is trashed"))
                                  (trash state :corp eid card nil))))))}]})

(defcard "Breaker Bay Grid"
  {:constant-effects [{:type :rez-cost
                       :req (req (in-same-server? card target))
                       :value -5}]})

(defcard "Bryan Stinson"
  {:abilities [{:cost [:click 1]
                :req (req (and (< (:credit runner) 6)
                               (pos? (count (filter #(and (operation? %)
                                                          (has-subtype? % "Transaction")) (:discard corp))))))
                :label "Play a transaction operation from Archives, ignoring all costs, and remove it from the game"
                :prompt "Choose a transaction operation to play"
                :msg (msg "play " (:title target) " from Archives, ignoring all costs, and removes it from the game")
                :choices (req (cancellable (filter #(and (operation? %)
                                                         (has-subtype? % "Transaction")) (:discard corp)) :sorted))
                :async true
                :effect (effect (play-instant eid (-> target
                                                      (assoc :rfg-instead-of-trashing true)
                                                      (assoc-in [:special :rfg-when-trashed] true))
                                              {:ignore-cost true}))}]})

(defcard "Calibration Testing"
  {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
   :abilities [{:label "Place 1 advancement token on a card in this server"
                :async true
                :trash-icon true
                :effect (effect (continue-ability
                                  {:prompt "Select a card in this server"
                                   :choices {:card #(in-same-server? % card)}
                                   :async true
                                   :msg (msg "place an advancement token on " (card-str state target))
                                   :cost [:trash]
                                   :effect (effect (add-prop target :advance-counter 1 {:placed true}))}
                                  card nil))}]})

(defcard "Caprice Nisei"
  {:events [{:event :pass-all-ice
             :req (req this-server)
             :psi {:not-equal {:msg "end the run"
                               :effect (effect (end-run eid card))}}}]})

(defcard "Cayambe Grid"
  (let [ability {:interactive (req (->> (all-installed state :corp)
                                        (filter #(and (ice? %)
                                                      (same-server? card %)))
                                        count
                                        pos?))
                 :label "place an advancement counter"
                 :async true
                 :effect
                 (effect
                   (continue-ability
                     (when (->> (all-installed state :corp)
                                (filter #(and (ice? %)
                                              (same-server? card %)))
                                count
                                pos?)
                       {:prompt (str "Place 1 advancement token on an ice protecting " (zone->name (second (get-zone card))))
                        :choices {:card #(and (ice? %)
                                              (same-server? % card))}
                        :msg (msg "place an advancement token on " (card-str state target))
                        :effect (effect (add-prop target :advance-counter 1 {:placed true}))})
                     card nil))}]
    {:events [(assoc ability :event :corp-turn-begins)
              {:event :approach-server
               :interactive (req true)
               :req (req this-server)
               :async true
               :effect
               (effect
                 (show-wait-prompt :corp "Runner to choose for Cayambe Grid")
                 (continue-ability
                   (let [cost (->> (get-run-ices state)
                                   (filter #(pos? (get-counters % :advancement)))
                                   count
                                   (* 2))]
                     {:async true
                      :player :runner
                      :prompt (str "Pay " cost " [Credits] or end the run?")
                      :choices [(when (can-pay? state :runner (assoc eid :source card :source-type :ability) card nil [:credit cost])
                                  (str "Pay " cost " [Credits]"))
                                "End the run"]
                      :msg (msg (if (= target "End the run")
                                  "end the run"
                                  (str "force the runner to pay " cost " [Credits]")))
                      :effect (req (clear-wait-prompt state :corp)
                                   (if (= target "End the run")
                                     (end-run state side eid card)
                                     (wait-for (pay state :runner card :credit cost)
                                               (system-msg state :runner (:msg async-result))
                                               (effect-completed state side eid))))})
                   card nil))}]
     :abilities [ability]}))

(defcard "ChiLo City Grid"
  {:events [{:event :successful-trace
             :req (req this-server)
             :async true
             :effect (effect (gain-tags :corp eid 1))
             :msg "give the Runner 1 tag"}]})

(defcard "Code Replicator"
  {:abilities [{:label "Force the runner to approach the passed piece of ice again"
                :req (req (and this-server
                               (< run-position (count (get-run-ices state)))
                               (rezzed? (get-in (:ices (card->server state card)) [(:position run)]))))
                :async true
                :effect (req (wait-for
                               (trash state :corp card nil)
                               (swap! state update-in [:run :position] inc)
                               (set-next-phase state :approach-ice)
                               (update-all-ice state side)
                               (update-all-icebreakers state side)
                               (system-msg state :corp (str "trashes Code Replicator to make the runner approach "
                                                            (:title (get-in (:ices (card->server state card)) [(:position run)]))
                                                            " again"))
                               (effect-completed state side eid)
                               (start-next-phase state side nil)))}]})

(defcard "Cold Site Server"
  {:constant-effects [{:type :run-additional-cost
                       :req (req (= (:server (second targets)) (unknown->kw (get-zone card))))
                       :value (req (repeat (get-counters card :power) [:credit 1 :click 1]))}]
   :events [{:event :corp-turn-begins
             :req (req (pos? (get-counters card :power)))
             :msg "remove all hosted power counters"
             :effect (effect (add-counter card :power (- (get-counters card :power))))}]
   :abilities [{:cost [:click 1]
                :msg "place 1 power counter on Cold Site Server"
                :effect (effect (add-counter card :power 1))}]})

(defcard "Corporate Troubleshooter"
  {:abilities [{:async true
                :label "Add strength to a rezzed ICE protecting this server"
                :cost [:trash :x-credits]
                :choices {:all true
                          :req (req (and (ice? target)
                                         (rezzed? target)
                                         (protecting-same-server? card target)))}
                :msg (msg "add " (cost-value eid :x-credits)
                          " strength to " (:title target))
                :effect (effect (pump-ice target (cost-value eid :x-credits) :end-of-turn))}]})

(defcard "Crenellation"
  {:events [{:event :agenda-scored
             :optional {:prompt "Search R&D for non-agenda card?"
                        :req (req (= (:previous-zone target) (get-zone card)))
                        :yes-ability {:prompt "Select card"
                                      :choices (req (cancellable (filter #(not (agenda? %)) (:deck corp))
                                                                :sorted))
                                      :msg (msg "reveal " (:title target) " and add it to HQ")
                                      :async true
                                      :effect (req (wait-for
                                                    (reveal state side target)
                                                    (shuffle! state side :deck)
                                                    (move state side target :hand)
                                                    (effect-completed state side eid)))}}}]})
           
(defcard "Crisium Grid"
  {:constant-effects [{:type :block-successful-run
                       :req (req this-server)
                       :value true}]})

(defcard "Cyberdex Virus Suite"
  {:flags {:rd-reveal (req true)}
   :access {:async true
            :effect (effect (show-wait-prompt :runner "Corp to use Cyberdex Virus Suite")
                            (continue-ability
                              {:optional {:prompt "Purge virus counters with Cyberdex Virus Suite?"
                                          :yes-ability {:msg (msg "purge virus counters")
                                                        :effect (effect (clear-wait-prompt :runner)
                                                                        (purge))}
                                          :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                              card nil))}
   :abilities [{:label "Purge virus counters"
                :msg "purge virus counters"
                :cost [:trash]
                :effect (effect (purge))}]})

(defcard "Daruma"
  (let [choose-swap
        (fn [to-swap]
          {:prompt (str "Select a card to swap with " (:title to-swap))
           :choices {:not-self true
                     :card #(and (corp? %)
                                 (not (operation? %))
                                 (or (in-hand? %) ; agenda, asset or upgrade from HQ
                                     (and (installed? %) ; card installed in a server
                                          (not (in-root? %)))))} ; central upgrades are not in a server
           :cost [:trash]
           :msg (msg "swap " (card-str state to-swap)
                     " with " (card-str state target))
           :effect (effect (clear-wait-prompt :runner)
                           (swap-cards to-swap target))
           :cancel-effect (effect (clear-wait-prompt :runner))})
        ability
        {:optional
         {:prompt "Trash Daruma to swap a card in this server?"
          :yes-ability
          {:async true
           :prompt "Select a card in this server to swap"
           :choices {:req (req (and (installed? target)
                                    (in-same-server? card target)))
                     :not-self true}
           :effect (effect (continue-ability (choose-swap target) card nil))}
          :no-ability {:effect (effect (clear-wait-prompt :runner))}}}]
    {:events [{:event :approach-server
               :async true
               :effect (effect (show-wait-prompt :runner "Corp to use Daruma")
                               (continue-ability :corp ability card nil))}]}))

(defcard "Dedicated Technician Team"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :corp-install (:source-type eid))
                                               (= (second (get-zone card))
                                                  (second (server->zone state (:source eid))))))
                                :type :recurring}}})

(defcard "Defense Construct"
  {:advanceable :always
   :abilities [{:label "Add cards from Archives to HQ"
                :req (req (and run
                               (= (:server run) [:archives])
                               (pos? (get-counters card :advancement))))
                :cost [:trash]
                :show-discard true
                :choices {:max (req (get-counters card :advancement))
                          :card #(and (corp? %)
                                      (not (:seen %))
                                      (in-discard? %))}
                :msg (msg "add " (count targets) " facedown cards in Archives to HQ")
                :effect (req (doseq [c targets]
                               (move state side c :hand)))}]})

(defcard "Directory Wipe"
  {:events [{:event :successful-run
             :optional {:prompt "Pay 2 [Credits] and trash 2 cards from HQ to end the run?"
                        :req (req (and (can-pay? state side eid card nil [:credit 2 :trash-from-hand 2])
                                       this-server))
                        :async true
                        :yes-ability {:async true
                                      :msg "pay 2 [Credits] and trash 2 cards from HQ to end the run"
                                      :effect (req (wait-for (pay state :corp card [:credit 2 :trash-from-hand 2])
                                                             (end-run state side eid card)))}}}]})                                     
                                                         
(defcard "Disposable HQ"
  (letfn [(dhq [i n]
            {:req (req (pos? n))
             :prompt "Select a card in HQ to add to the bottom of R&D"
             :choices {:card #(and (corp? %)
                                   (in-hand? %))}
             :async true
             :msg "add a card to the bottom of R&D"
             :effect (req (move state side target :deck)
                          (if (< i n)
                            (continue-ability state side (dhq (inc i) n) card nil)
                            (do
                              (clear-wait-prompt state :runner)
                              (effect-completed state side eid))))
             :cancel-effect (effect (clear-wait-prompt :runner)
                                    (effect-completed eid))})]
    {:flags {:rd-reveal (req true)}
     :access {:async true
              :effect (req (let [n (count (:hand corp))]
                             (show-wait-prompt state :runner "Corp to finish using Disposable HQ")
                             (continue-ability
                               state side
                               {:optional
                                {:prompt "Use Disposable HQ to add cards to the bottom of R&D?"
                                 :yes-ability {:async true
                                               :msg "add cards in HQ to the bottom of R&D"
                                               :effect (effect (continue-ability (dhq 1 n) card nil))}
                                 :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                               card nil)))}}))

(defcard "Drone Screen"
  {:events [{:event :run
             :req (req (and this-server tagged))
             :async true
             :trace {:base 3
                     :successful
                     {:msg "do 1 meat damage"
                      :effect (effect (damage eid :meat 1 {:card card
                                                           :unpreventable true}))}}}]})

(defcard "Embolus"
  (let [maybe-gain-counter {:once :per-turn
                            :async true
                            :label "Place a power counter on Embolus"
                            :effect (effect
                                      (continue-ability
                                        {:optional
                                         {:prompt "Pay 1 [Credit] to place a power counter on Embolus?"
                                          :yes-ability {:effect (effect (add-counter card :power 1))
                                                        :cost [:credit 1]
                                                        :msg "pay 1 [Credit] to place a power counter on Embolus"}}}
                                        card nil))}
        etr {:req (req this-server)
             :cost [:power 1]
             :msg "end the run"
             :effect (effect (end-run eid card))}]
    {:derezzed-events [(assoc corp-rez-toast :event :runner-turn-ends)]
     :events [(assoc maybe-gain-counter :event :corp-turn-begins)
              {:event :successful-run
               :req (req (pos? (get-counters card :power)))
               :msg "remove 1 power counter from Embolus"
               :effect (effect (add-counter card :power -1))}]
     :abilities [maybe-gain-counter
                 etr]}))

(defcard "Equivalent Exchange"
  {:on-trash
   {:req (req (and (= :runner side)
                   (:run @state)))
    :effect (effect (register-events
                     card
                     [{:event :run-ends
                       :duration :end-of-run
                       :req (req (or (= (first (:server target)) (second (:previous-zone card)))
                                     (= (central->zone (first (:server target)))
                                        (butlast (:previous-zone card)))))
                       :async true
                       :effect (req (when (:did-steal target) (gain-tags state :corp eid 2)))}]))}
   :events [{:event :run-ends
             :req (req   
                   (= (second (get-zone card)) (first (:server target))))
             :async true
             :effect (req (when (:did-steal target) (gain-tags state :corp eid 2)))}]})

(defcard "Experiential Data"
  {:constant-effects [{:type :ice-strength
                       :req (req (protecting-same-server? card target))
                       :value 1}]})

(defcard "Expo Grid"
  (let [ability {:req (req (some #(and (asset? %)
                                       (rezzed? %))
                                 (get-in corp (get-zone card))))
                 :msg "gain 1 [Credits]"
                 :once :per-turn
                 :label "Gain 1 [Credits] (start of turn)"
                 :async true
                 :effect (effect (gain-credits eid 1))}]
    {:derezzed-events [(assoc corp-rez-toast :event :runner-turn-ends)]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Forced Connection"
  {:flags {:rd-reveal (req true)}
   :access {:req (req (not (in-discard? card)))
            :interactive (req true)
            :trace {:base 3
                    :successful {:msg "give the Runner 2 tags"
                                 :async true
                                 :effect (effect (gain-tags :corp eid 2))}}}})

(defcard "Fractal Threat Matrix"
  {:implementation "Manual trigger each time all subs are broken"
   :abilities [{:async true
                :label "Trash the top 2 cards from the Stack"
                :msg (msg (let [deck (:deck runner)]
                            (if (pos? (count deck))
                              (str "trash " (string/join ", " (map :title (take 2 deck))) " from the Stack")
                              "trash the top 2 cards from their Stack - but the Stack is empty")))
                :effect (effect (mill :corp eid :runner 2))}]})

(defcard "Ganked!"
  {:implementation "Forced encounter is completely manual. All breaking and costs must be done manually"
   :flags {:rd-reveal (req true)}
   :access
   {:async true
    :req (req (and (not (in-discard? card))
                   (some ice? (all-active-installed state :corp))))
    :effect (effect (show-wait-prompt :runner (str "Corp to decide to trigger " (:title card)))
                    (continue-ability
                      {:optional
                       {:prompt "Trash Ganked! to force the Runner to encounter a piece of ice?"
                        :yes-ability
                        {:async true
                         :choices {:card #(and (ice? %)
                                               (installed? %)
                                               (rezzed? %))}
                         :msg (msg "to encounter " (:title target))
                         :effect (req (clear-wait-prompt state :runner)
                                      (wait-for (trash state :corp card {:unpreventable true})
                                                (show-wait-prompt state :corp (str "Runner to decide about encountering " (:title target)))
                                                (continue-ability
                                                  state :runner
                                                  {:optional
                                                   {:player :runner
                                                    :prompt (str "You are encountering " (:title target)". Allow its subroutine to fire?")
                                                    :yes-ability
                                                    {:async true
                                                     :effect (effect (clear-wait-prompt :corp)
                                                                     (resolve-unbroken-subs! :corp eid target))}
                                                    :no-ability
                                                    {:effect (effect (clear-wait-prompt :corp)
                                                                     (effect-completed eid))}}}
                                                  card targets)))}
                        :no-ability {:effect (effect (system-msg :corp (str "declines to force the Runner to encounter " (:title target)))
                                                     (clear-wait-prompt :runner))}}}
                      card nil))}})

(defcard "Georgia Emelyov"
  {:events [{:event :unsuccessful-run
             :req (req (= (target-server target)
                          (second (get-zone card))))
             :async true
             :msg "do 1 net damage"
             :effect (effect (damage eid :net 1 {:card card}))}]
   :abilities [{:cost [:credit 2]
                :label "Move to another server"
                :async true
                :effect (effect (continue-ability
                                  {:prompt "Choose a server"
                                   :choices (server-list state)
                                   :msg (msg "move to " target)
                                   :effect (req (let [c (move state side card
                                                              (conj (server->zone state target) :content))]
                                                  (unregister-events state side card)
                                                  (register-events state side c)))}
                                  card nil))}]})

(defcard "Giordano Memorial Field"
  {:events [{:event :successful-run
             :interactive (req true)
             :async true
             :req (req this-server)
             :msg "force the Runner to pay or end the run"
             :effect (req (let [credits (:credit runner)
                                cost (* 2 (count (:scored runner)))
                                pay-str (str "pay " cost " [Credits]")
                                c-pay-str (capitalize pay-str)]
                            (show-wait-prompt state :corp (str "Runner to " pay-str " or end the run"))
                            (continue-ability
                              state :runner
                              {:player :runner
                               :async true
                               :prompt (msg "You must " pay-str " or end the run")
                               :choices [(when (>= credits cost)
                                           c-pay-str)
                                         "End the run"]
                               :effect (req (clear-wait-prompt state :corp)
                                            (if (= c-pay-str target)
                                              (wait-for (pay state :runner card :credit cost)
                                                        (system-msg state :runner (:msg async-result))
                                                        (effect-completed state side eid))
                                              (do (system-msg state :corp "ends the run")
                                                  (end-run state :corp eid card))))}
                              card nil)))}]})

(defcard "Glial-Map Encryption"
  {:events [{:event :successful-run
             :player :runner
             :prompt "Choose one"
             :choices ["Spend [Click][Click]" "Pay 5 [Credits]" "End the run"]
             :async true
             :effect (req (if (and (= target "Spend [Click][Click]")
                                   (can-pay? state :runner (assoc eid :source card :source-type :subroutine) card nil [:click 2]))
                            (do (wait-for (pay state side card :click 2)
                                          (system-msg state side (:msg async-result))
                                          (effect-completed state :runner eid)))
                            (if (and (= target "Pay 5 [Credits]")
                                     (can-pay? state :runner (assoc eid :source card :source-type :subroutine) card nil [:credit 5]))
                              (do (wait-for (pay state side card :credit 5)
                                            (system-msg state side (:msg async-result))
                                            (effect-completed state :runner eid)))
                              (do (system-msg state :corp "ends the run")
                                  (end-run state :corp eid card)))))}]})       

(defcard "Heinlein Grid"
  {:abilities [{:req (req this-server)
                :label "Force the Runner to lose all [Credits] from spending or losing a [Click]"
                :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]")
                :once :per-run
                :async true
                :effect (effect (lose-credits :runner eid :all))}]})

(defcard "Helheim Servers"
  {:abilities [{:label "All ice protecting this server has +2 strength until the end of the run"
                :req (req (and this-server
                               (pos? (count run-ices))
                               (pos? (count (:hand corp)))))
                :async true
                :cost [:trash-from-hand 1]
                :effect (effect (register-floating-effect
                                  card
                                  {:type :ice-strength
                                   :duration :end-of-run
                                   :req (req (protecting-same-server? card target))
                                   :value 2})
                                (update-all-ice))}]})

(defcard "Henry Phillips"
  (letfn [(hp-gain-credits [state side eid n]
            (if (pos? n)
              (wait-for (gain-credits state :corp 2)
                        (hp-gain-credits state side eid (dec n)))
              (effect-completed state side eid)))]
    {:events [{:event :subroutines-broken
               :req (req (and this-server tagged))
               :msg (msg "gain " (count (second targets)) " [Credits]")
               :async true
               :effect (effect (hp-gain-credits :corp eid (count (second targets))))}]}))

(defcard "Hired Help"
  (let [prompt-to-trash-agenda-or-etr
        {:prompt "Choose one"
         :player :runner
         :choices ["Trash 1 scored agenda" "End the run"]
         :async true
         :effect (req (if (= target "End the run")
                        (do (system-msg state :runner (str "declines to pay the additional cost from Hired Help"))
                            (end-run state side eid card))
                        (if (seq (:scored runner))
                          (continue-ability state :runner
                                            {:prompt "Choose an Agenda to trash"
                                             :async true
                                             :choices {:max 1
                                                       :card #(is-scored? state side %)}
                                             :effect (req (wait-for (trash state side target {:unpreventable true})
                                                                    (system-msg state :runner (str "trashes " (:title target)
                                                                                                   " as an additional cost to initiate a run"))
                                                                    (effect-completed state side eid)))}
                                            card nil)
                          (do (system-msg state :runner (str "wants to pay the additional cost from Hired Help but has no scored agenda to trash"))
                              (end-run state side eid card)))))}]
    {:events [{:event :run
               :async true
               :req (req (and this-server
                              (empty? (filter #{:hq} (:successful-run runner-reg)))))
               :effect (req (continue-ability state :runner prompt-to-trash-agenda-or-etr card nil))}]}))

(defcard "Hokusai Grid"
  {:events [{:event :successful-run
             :req (req this-server)
             :msg "do 1 net damage"
             :async true
             :effect (effect (damage eid :net 1 {:card card}))}]})

(defcard "Increased Drop Rates"
  {:flags {:rd-reveal (req true)}
   :access {:interactive (req true)
            :async true
            :effect (effect (show-wait-prompt :corp "Runner to decide if they will take 1 tag")
                            (continue-ability
                              {:optional
                               {:player :runner
                                :prompt "Take 1 tag to prevent Corp from removing 1 bad publicity?"
                                :yes-ability {:async true
                                              :effect (effect (system-msg "takes 1 tag to prevent Corp from removing 1 bad publicity")
                                                              (gain-tags eid 1 {:unpreventable true}))}
                                :no-ability {:msg "remove 1 bad publicity"
                                             :effect (effect (lose-bad-publicity :corp 1)
                                                             (effect-completed eid))}
                                :end-effect (effect (clear-wait-prompt :corp))}}
                              card nil))}})

(defcard "Intake"
  {:flags {:rd-reveal (req true)}
   :access {:req (req (not (in-discard? card)))
            :interactive (req true)
            :trace {:base 4
                    :label "add an installed program or virtual resource to the Grip"
                    :successful
                    {:async true
                     :effect (effect (show-wait-prompt :runner "Corp to resolve Intake")
                                     (continue-ability
                                       {:prompt "Select a program or virtual resource"
                                        :player :corp
                                        :choices {:card #(and (installed? %)
                                                              (or (program? %)
                                                                  (and (resource? %)
                                                                       (has-subtype? % "Virtual"))))}
                                        :msg (msg "move " (:title target) " to the Grip")
                                        :effect (req (move state :runner target :hand)
                                                     (clear-wait-prompt state :runner)
                                                     (effect-completed state side eid))
                                        :cancel-effect (req (clear-wait-prompt state :runner)
                                                            (effect-completed state side eid))}
                                       card nil))}}}})

(defcard "Jinja City Grid"
  (letfn [(install-ice [ice ices grids server]
            (let [remaining (remove-once #(same-card? % ice) ices)]
              {:async true
               :effect (req (if (= "None" server)
                              (continue-ability state side (choose-ice remaining grids) card nil)
                              (wait-for
                                (reveal state side ice)
                                (system-msg state side (str "reveals that they drew " (:title ice)))
                                (wait-for (corp-install state side ice server {:cost-bonus -4})
                                          (if (= 1 (count ices))
                                            (effect-completed state side eid)
                                            (continue-ability state side (choose-ice remaining grids)
                                                              card nil))))))}))
          (choose-grid [ice ices grids]
            (if (= 1 (count grids))
              (install-ice ice ices grids (-> (first grids) :zone second zone->name))
              {:async true
               :prompt (str "Choose a server to install " (:title ice))
               :choices (conj (mapv #(-> % :zone second zone->name) grids) "None")
               :effect (effect (continue-ability (install-ice ice ices grids target) card nil))}))
          (choose-ice [ices grids]
            (when (seq ices)
              {:async true
               :prompt "Choose an ice to reveal and install, or None to decline"
               :choices (conj (mapv :title ices) "None")
               :effect (req (if (= "None" target)
                              (effect-completed state side eid)
                              (continue-ability state side
                                                (choose-grid (some #(when (= target (:title %)) %) ices)
                                                             ices grids)
                                                card nil)))}))]
    {:events [{:event :corp-draw
               ;; This prevents multiple Jinja from showing the "choose a server to install into" sequence
               :once :per-turn
               :once-key :jinja-city-grid-draw
               :async true
               :req (req (not (find-cid (:cid card) (flatten (vals (get-in @state [:trash :trash-list]))))))
               :effect (req (cond
                              ;; if ice were drawn, do the full routine
                              (some ice? (:most-recent-drawn corp-reg))
                              (let [ices (filter #(and (ice? %)
                                                       (get-card state %))
                                                 (:most-recent-drawn corp-reg))
                                    grids (filterv #(= "Jinja City Grid" (:title %))
                                                   (all-active-installed state :corp))]
                                (when (= :runner (:active-player @state))
                                  (show-wait-prompt state :runner "Corp to resolve Jinja City Grid"))
                                (if (not-empty ices)
                                  (continue-ability state side (choose-ice ices grids) card nil)
                                  (effect-completed state side eid)))
                              ;; else, if it's the runner's turn, show a fake prompt so the runner can't infer that ice weren't drawn
                              (= :runner (:active-player @state))
                              (do (show-wait-prompt state :runner "Corp to resolve Jinja City Grid")
                                  (continue-ability
                                    state :corp
                                    {:prompt "You did not draw any ice to use with Jinja City Grid"
                                     :choices ["Carry on!"]
                                     :prompt-type :bogus
                                     :effect (effect (clear-wait-prompt :runner))}
                                    card nil))
                              ;; otherwise, we done
                              :else
                              (effect-completed state side eid)))}
              {:event :post-corp-draw
               :effect (req (swap! state dissoc-in [:per-turn :jinja-city-grid-draw])
                         (when (= :runner (:active-player @state))
                           (clear-wait-prompt state :runner)))}]}))

(defcard "K. P. Lynn"
  {:events [{:event :pass-all-ice
             :req (req this-server)
             :player :runner
             :prompt "Choose one"
             :choices ["Take 1 tag" "End the run"]
             :async true
             :effect (req (if (= target "Take 1 tag")
                            (do (system-msg state :corp (str "uses K. P. Lynn. Runner chooses to take 1 tag"))
                                (gain-tags state :runner eid 1))
                            (do (system-msg state :corp (str "uses K. P. Lynn. Runner chooses to end the run"))
                                (end-run state side eid card))))}]})

(defcard "La Costa Grid"
  (let [ability {:prompt (msg "Select a card in " (zone->name (second (get-zone card))))
                 :label "place an advancement counter"
                 :msg (msg "place an advancement token on " (card-str state target))
                 :choices {:req (req (and (installed? target)
                                          (in-same-server? card target)))}
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]
    {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
     :derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Letheia Nisei"
  {:events [{:event :approach-server
             :once :per-run
             :req (req this-server)
             :psi {:not-equal
                   {:async true
                    :effect
                    (effect
                      (show-wait-prompt :runner "Corp to use Letheia Nisei")
                      (continue-ability
                        :corp
                        {:optional
                         {:prompt "Trash to force re-approach outer ice?"
                          :autoresolve (get-autoresolve :auto-fire)
                          :yes-ability
                          {:async true
                           :msg "force the Runner to approach outermost piece of ice"
                           :effect (req (wait-for (trash state side card {:unpreventable true})
                                                  (redirect-run state side (zone->name (second (get-zone card))) :approach-ice)
                                                  (effect-completed state side eid)
                                                  (start-next-phase state side nil)))}
                          :end-effect (effect (clear-wait-prompt :runner))}}
                        card nil))}}}]
   :abilities [(set-autoresolve :auto-fire "Fire Letheia Nisei?")]})

(defcard "Keegan Lane"
  {:abilities [{:req (req (and this-server
                               (some? (first (filter program? (all-active-installed state :runner))))))
                :prompt "Select a program to trash"
                :label "Trash a program"
                :msg (msg "trash " (:title target))
                :choices {:card #(and (installed? %)
                                      (program? %))}
                :cost [:tag 1 :trash]
                :async true
                :effect (effect (trash eid target nil))}]})

(defcard "Khondi Plaza"
  {:recurring (effect (set-prop card :rec-counter (count (get-remotes state))))
   :effect (effect (set-prop card :rec-counter (count (get-remotes state))))
   :interactions {:pay-credits {:req (req (and (= :rez (:source-type eid))
                                               (ice? target)
                                               (= (card->server state card) (card->server state target))))
                                :type :recurring}}})

(defcard "Manta Grid"
  {:events [{:event :run-ends
             :msg "gain a [Click] next turn"
             :req (req (and (:successful target)
                            (= (target-server target) (second (get-zone card)))
                            (or (< (:credit runner) 6) (zero? (:click runner)))))
             :effect (req (swap! state update-in [:corp :extra-click-temp] (fnil inc 0)))}]})

(defcard "Marcus Batty"
  {:abilities [{:req (req this-server)
                :label "Start a Psi game to resolve a subroutine"
                :cost [:trash]
                :psi {:not-equal
                      {:prompt "Select the ice"
                       :choices {:card #(and (ice? %)
                                             (rezzed? %))
                                 :all true}
                       :effect (effect
                                 (continue-ability
                                   (let [ice target]
                                     {:prompt "Select the subroutine"
                                      :choices (req (unbroken-subroutines-choice ice))
                                      :msg (msg "resolve the subroutine (\"[subroutine] "
                                                                                        target "\") from " (:title ice))
                                      :effect (req (let [sub (first (filter #(= target (make-label (:sub-effect %))) (:subroutines ice)))]
                                                     (continue-ability state side (:sub-effect sub) ice nil)))})
                                   card nil))}}}]})

(defcard "Mason Bellamy"
  {:events [{:event :end-of-encounter
             :req (req (and this-server
                            (seq (filter :broken (:subroutines (:ice context))))))
             :msg "force the Runner to lose [Click]"
             :effect (effect (lose :runner :click 1))}]})

(defcard "Midori"
  {:events [{:event :approach-ice
             :optional
             {:req (req this-server)
              :once :per-run
              :prompt "Swap the ICE being approached with a piece of ICE from HQ?"
              :yes-ability
              {:prompt "Select a piece of ICE"
               :choices {:card #(and (ice? %)
                                     (in-hand? %))}
               :once :per-run
               :msg (msg "swap " (card-str state current-ice)
                         " with a piece of ICE from HQ")
               :effect (effect (swap-cards :corp current-ice target))}}}]})

(defcard "Midway Station Grid"
  {:constant-effects [{:type :break-sub-additional-cost
                       :req (req (and ; The card is an icebreaker
                                      (has-subtype? target "Icebreaker")
                                      ; and is using a break ability
                                      (contains? (second targets) :break)
                                      (pos? (:break (second targets) 0))
                                      ; during a run on this server
                                      this-server))
                       :value [:credit 1]}]})

(defcard "Mumbad City Grid"
  {:events [{:event :pass-ice
             :req (req this-server (<= 2 (count run-ices)))
             :async true
             :effect
             (effect
               (continue-ability
                 (let [passed-ice (:ice context)]
                   {:prompt (msg "Select a piece of ICE to swap with " (:title target))
                    :choices {:req (req (and (installed? target)
                                             (ice? target)
                                             (= (target-server run) (second (get-zone target)))
                                             (not (same-card? target passed-ice))))}
                    :effect (effect (swap-ice target passed-ice))})
                 card nil))}]})

(defcard "Mumbad Virtual Tour"
  {:flags {:must-trash (req (when installed true))}})

(defcard "Mwanza City Grid"
  (let [gain-creds-and-clear {:req (req (= (:from-server target)
                                           (second (get-zone card))))
                              :silent (req true)
                              :async true
                              :effect (req (let [cnt (total-cards-accessed run)
                                                 total (* 2 cnt)]
                                             (if cnt
                                               (do (system-msg state :corp
                                                               (str "gains " total " [Credits] from Mwanza City Grid"))
                                                   (gain-credits state :corp eid total))
                                               (effect-completed state side eid))))}
        boost-access-by-3 {:req (req (= target (second (get-zone card))))
                           :msg "force the Runner to access 3 additional cards"
                           :effect (req (access-bonus state :runner (-> card :zone second) 3))}]
    {:install-req (req (filter #{"HQ" "R&D"} targets))
     :events [(assoc boost-access-by-3 :event :pre-access)
              (assoc gain-creds-and-clear :event :end-access-phase)]
     ;; TODO: as written, this may fail if mwanza is trashed outside of a run on its server
     ;; (e.g. mwanza on R&D, run HQ, use polop to trash mwanza mid-run, shiro fires to cause RD
              :on-trash ; if there is a run, mark mwanza effects to remain active until the end of the run
              {:req (req (and (= :runner side)
                              (:run @state)))
               :effect (effect (register-events
                                 card
                                 [(assoc boost-access-by-3
                                         :event :pre-access
                                         :duration :end-of-run
                                         :req (req (= target (second (:previous-zone card)))))
                                  (assoc gain-creds-and-clear
                                         :event :end-access-phase
                                         :duration :end-of-run
                                         :req (req (= (:from-server target) (second (:previous-zone card)))))]))}}))

(defcard "Navi Mumbai City Grid"
  {:constant-effects [{:type :prevent-ability
                       :req (req (let [target-card (first targets)]
                                   (and run
                                        (= (:side target-card) "Runner")
                                        (= (target-server run) (second (get-zone card)))
                                        (not (has-subtype? target-card "Icebreaker")))))
                       :value true}]})

(defcard "NeoTokyo Grid"
  (let [ng {:req (req (in-same-server? card target))
            :once :per-turn
            :msg "gain 1 [Credits]"
            :async true
            :effect (effect (gain-credits eid 1))}]
    {:events [(assoc ng :event :advance)
              (assoc ng :event :advancement-placed)]}))

(defcard "Nihongai Grid"
  {:events [{:event :successful-run
             :interactive (req true)
             :optional
             {:req (req (and this-server
                             (or (< (total-available-credits state :runner eid card) 6)
                                 (< (count (:hand runner)) 2))
                             (not-empty (:hand corp))
                             (pos? (count (take 5 (:deck corp))))))
              :prompt (msg "Look at the top "
                           (quantify (count (take 5 (:deck corp))) "card")
                           " of R&D and swap one with a card from HQ?")
              :yes-ability
              {:async true
               :msg (msg "look at the top " (quantify (count (take 5 (:deck corp))) "card") " of R&D")
               :effect
               (effect
                 (continue-ability
                   {:async true
                    :prompt "Choose a card in R&D"
                    :choices (take 5 (:deck corp))
                    :effect (effect
                              (continue-ability
                                (when-let [rdc target]
                                  {:prompt "Choose a card in HQ"
                                   :choices {:card in-hand?}
                                   :msg "swap a card from the top 5 of R&D with a card in HQ"
                                   :effect (req (move state side rdc :hand)
                                                (move state side target :deck {:index (:index rdc)}))})
                                card nil))}
                   card nil))}}}]})

(defcard "Oaktown Grid"
  {:constant-effects [{:type :trash-cost
                       :req (req (in-same-server? card target))
                       :value 3}]})

(defcard "Oberth Protocol"
  {:additional-cost [:forfeit]
   :events [{:event :advance
             :req (req (and (same-server? card target)
                            (= 1 (count (filter #(= (second (get-zone %)) (second (get-zone card)))
                                                (map first (turn-events state side :advance)))))))
             :msg (msg "place an additional advancement token on " (card-str state target))
             :effect (effect (add-prop :corp target :advance-counter 1 {:placed true}))}]})

(defcard "Off the Grid"
  {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
   :effect (req (prevent-run-on-server state card (second (get-zone card))))
   :events [{:event :runner-turn-begins
             :effect (req (prevent-run-on-server state card (second (get-zone card))))}
            {:event :successful-run
             :req (req (= :hq (target-server context)))
             :async true
             :effect (req (enable-run-on-server state card (second (get-zone card)))
                          (system-msg state :corp (str "trashes Off the Grid"))
                          (trash state :corp eid card nil))}]
   :leave-play (req (enable-run-on-server state card (second (get-zone card))))})

(defcard "Old Hollywood Grid"
  (let [ohg {:effect (effect
                       (register-persistent-flag!
                         card :can-steal
                         (fn [state _ card]
                           (if-not (some #(= (:title %) (:title card)) (:scored runner))
                             ((constantly false)
                              (toast state :runner "Cannot steal due to Old Hollywood Grid." "warning"))
                             true))))}]
    {:on-trash
     {:req (req (and (= :runner side)
                     (:run @state)))
      :effect (effect (register-events
                        card
                        [(assoc ohg
                                :event :pre-steal-cost
                                :duration :end-of-run
                                :req (req (or (= (get-zone target)
                                                 (:previous-zone card))
                                              (= (central->zone (get-zone target))
                                                 (butlast (:previous-zone card))))))
                         {:event :run-ends
                          :duration :end-of-run
                          :effect (req (clear-persistent-flag! state side card :can-steal))}]))}
     :events [(assoc ohg
                     :event :pre-steal-cost
                     :req (req (or (in-same-server? card target)
                                   (from-same-server? card target))))
              {:event :access
               :effect (req (clear-persistent-flag! state side card :can-steal))}]}))

(defcard "Overseer Matrix"
  (let [ability {:event :runner-trash
                 :once-per-instance true
                 :async true
                 :interactive (req true)
                 :req (req (some #(and (corp? (:card %))
                                       (or (in-same-server? card (:card %))
                                           (from-same-server? card (:card %))))
                                 targets))
                 :effect (effect (show-wait-prompt :runner "Corp to use Overseer Matrix")
                                 (continue-ability
                                   (let [num-trashed-cards
                                         (->> targets
                                              (filter #(or (in-same-server? card (:card %))
                                                           (from-same-server? card (:card %))
                                                           (in-same-server? (assoc card :zone (:previous-zone card)) (:card %))))
                                              count)]
                                     {:async true
                                      :prompt "Pay how much to use Overseer Matrix's ability?"
                                      :player :corp
                                      :choices {:number (req (min num-trashed-cards
                                                                  (total-available-credits state :corp eid card)))}
                                      :effect
                                      (effect
                                        (clear-wait-prompt :runner)
                                        (continue-ability
                                          (let [n target]
                                            {:async true
                                             :cost [:credit n]
                                             :msg (str "give the Runner " (quantify n "tag"))
                                             :effect (effect (gain-tags :corp eid n))})
                                          card nil))})
                                   card nil))}]
    {:on-trash
     {:async true
      :interactive (req true)
      :req (req (= :runner side))
      :effect (req (when (:run @state)
                     (register-events
                       state side card
                       [(assoc ability :duration :end-of-run)]))
                   (continue-ability state side ability card targets))}
     :events [ability]}))

(defcard "Panic Button"
  {:install-req (req (filter #{"HQ"} targets))
   :abilities [{:cost [:credit 1]
                :msg "draw 1 card"
                :req (req (and run (= (target-server run) :hq)))
                :effect (effect (draw))}]})

(defcard "Port Anson Grid"
  {:msg "prevent the Runner from jacking out unless they trash an installed program"
   :constant-effects [{:type :jack-out-additional-cost
                       :duration :end-of-run
                       :req (req this-server)
                       :value [:program 1]}]
   :events [{:event :run
             :req (req this-server)
             :msg "prevent the Runner from jacking out unless they trash an installed program"}]})

(defcard "Prisec"
  {:access {:req (req (installed? card))
            :async true
            :effect (effect (show-wait-prompt :runner "Corp to use Prisec")
                            (continue-ability
                              {:optional
                               {:prompt "Pay 2 [Credits] to use Prisec ability?"
                                :end-effect (effect (clear-wait-prompt :runner))
                                :yes-ability {:cost [:credit 2]
                                              :msg "do 1 meat damage and give the Runner 1 tag"
                                              :async true
                                              :effect (req (wait-for (damage state side :meat 1 {:card card})
                                                                     (gain-tags state :corp eid 1)))}}}
                              card nil))}})

(defcard "Product Placement"
  {:flags {:rd-reveal (req true)}
   :access {:req (req (not (in-discard? card)))
            :msg "gain 2 [Credits]"
            :async true
            :effect (effect (gain-credits :corp eid 2))}})

(defcard "Red Herrings"
  {:on-trash
   {:req (req (and (= :runner side)
                   (:run @state)))
    :effect (effect (register-events
                      card
                      [{:event :pre-steal-cost
                        :duration :end-of-run
                        :req (req (or (= (get-zone target) (:previous-zone card))
                                      (= (central->zone (get-zone target))
                                         (butlast (:previous-zone card)))))
                        :effect (effect (steal-cost-bonus [:credit 5]))}]))}
   :events [{:event :pre-steal-cost
             :req (req (or (in-same-server? card target)
                           (from-same-server? card target)))
             :effect (effect (steal-cost-bonus [:credit 5]))}]})

(defcard "Reduced Service"
  {:constant-effects [{:type :run-additional-cost
                       :req (req (= (:server (second targets)) (unknown->kw (get-zone card))))
                       :value (req (repeat (get-counters card :power) [:credit 2]))}]
   :events [{:event :successful-run
             :req (req (and (pos? (get-counters card :power))
                            (is-central? (:server context))))
             :msg "remove a hosted power counter"
             :effect (effect (add-counter card :power -1))}]
   :effect (effect (show-wait-prompt :runner "Corp to place credits on Reduced Service")
                   (continue-ability
                     {:choices (req (map str (range (inc (min 4 (get-in @state [:corp :credit]))))))
                      :prompt "How many credits to spend?"
                      :async true
                      :effect (req (clear-wait-prompt state :runner)
                                   (let [spent (str->int target)]
                                     (add-counter state :corp card :power spent)
                                     (system-msg state :corp (str "places " (quantify spent "power counter") " on Reduced Service"))
                                     (lose-credits state :corp eid spent)))}
                     card nil))})

(defcard "Research Station"
  {:install-req (req (filter #{"HQ"} targets))
   :constant-effects [{:type :hand-size
                       :req (req (= :corp value))
                       :value 2}]})

(defcard "Ruhr Valley"
  {:constant-effects [{:type :run-additional-cost
                       :req (req (= (:server (second targets)) (unknown->kw (get-zone card))))
                       :value [:click 1]}]})

(defcard "Rutherford Grid"
  {:events [{:event :pre-init-trace
             :req (req this-server)
             :effect (effect (init-trace-bonus 2))}]})

(defcard "Ryon Knight"
  {:abilities [{:label "Do 1 brain damage"
                :req (req (and this-server (zero? (:click runner))))
                :cost [:trash]
                :msg "do 1 brain damage"
                :async true
                :effect (effect (damage eid :brain 1 {:card card}))}]})

(defcard "SanSan City Grid"
  {:effect (req (when-let [agenda (some #(when (agenda? %) %)
                                        (:content (card->server state card)))]
                  (update-advancement-cost state side agenda)))
   :events [{:event :corp-install
             :req (req (and (agenda? target)
                            (in-same-server? card target)))
             :effect (effect (update-advancement-cost target))}
            {:event :pre-advancement-cost
             :req (req (in-same-server? card target))
             :effect (effect (advancement-cost-bonus -1))}]})

(defcard "Satellite Grid"
  {:effect (req (doseq [c (:ices (card->server state card))]
                  (set-prop state side c :extra-advance-counter 1))
                (update-all-ice state side))
   :events [{:event :corp-install
             :req (req (and (ice? target)
                            (protecting-same-server? card target)))
             :effect (effect (set-prop target :extra-advance-counter 1))}]
   :leave-play (req (doseq [c (:ices (card->server state card))]
                      (update! state side (dissoc c :extra-advance-counter)))
                    (update-all-ice state side))})

(defcard "Self-destruct"
  {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
   :abilities [{:async true
                :req (req this-server)
                :cost [:trash]
                :label "Trace X - Do 3 net damage"
                :effect (req (let [serv (card->server state card)
                                   cards (concat (:ices serv) (:content serv))]
                               (wait-for (trash-cards state side cards nil)
                                         (continue-ability
                                           state side
                                           {:trace
                                            {:base (count cards)
                                             :successful
                                             {:async true
                                              :msg "do 3 net damage"
                                              :effect (effect (damage eid :net 3 {:card card}))}}}
                                           card nil))))}]})

(defcard "Shell Corporation"
  {:abilities
   [{:cost [:click 1]
     :msg "store 3 [Credits]"
     :once :per-turn
     :effect (effect (add-counter card :credit 3))}
    {:cost [:click 1]
     :msg (msg "gain " (get-counters card :credit) " [Credits]")
     :once :per-turn
     :label "Take all credits"
     :async true
     :effect (effect (set-prop card :counter {:credit 0})
                     (gain-credits eid (get-counters card :credit)))}]})

(defcard "Signal Jamming"
  {:abilities [{:label "Cards cannot be installed until the end of the run"
                :msg (msg "prevent cards being installed until the end of the run")
                :req (req this-server)
                :cost [:trash]
                :effect (effect (register-run-flag! card :corp-lock-install (constantly true))
                                (register-run-flag! card :runner-lock-install (constantly true))
                                (toast :runner "Cannot install until the end of the run")
                                (toast :corp "Cannot install until the end of the run"))}]})

(defcard "Simone Diego"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :advance (:source-type eid))
                                               (same-server? card target)))
                                :type :recurring}}})

(defcard "Strongbox"
  {:on-trash
   {:req (req (and (= :runner side)
                   (:run @state)))
    :effect (effect (register-events
                      card
                      [{:event :pre-steal-cost
                        :duration :end-of-run
                        :req (req (or (= (get-zone target) (:previous-zone card))
                                      (= (central->zone (get-zone target))
                                         (butlast (:previous-zone card)))))
                        :effect (effect (steal-cost-bonus [:click 1]))}]))}
   :events [{:event :pre-steal-cost
             :req (req (or (in-same-server? card target)
                           (from-same-server? card target)))
             :effect (effect (steal-cost-bonus [:click 1]))}]})

(defcard "Surat City Grid"
  {:events [{:event :rez
             :req (req (and (same-server? card target)
                            (not (and (upgrade? target)
                                      (is-central? (second (get-zone target)))))
                            (not (same-card? target card))
                            (some #(and (not (rezzed? %))
                                        (not (agenda? %))
                                        (can-pay? state side (assoc eid :source card :source-type :rez) % nil
                                                  [:credit (install-cost state side % {:cost-bonus -2})]))
                                  (all-installed state :corp))))
             :effect (effect (continue-ability
                               {:optional
                                {:prompt (msg "Rez another card with Surat City Grid?")
                                 :yes-ability {:prompt "Select a card to rez"
                                               :choices {:card #(and (not (rezzed? %))
                                                                     (not (agenda? %))
                                                                     (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                                               [:credit (rez-cost state side % {:cost-bonus -2})]))}
                                               :msg (msg "rez " (:title target) ", lowering the rez cost by 2 [Credits]")
                                               :effect (effect (rez eid target {:cost-bonus -2}))}}}
                               card nil))}]})

(defcard "Tempus"
  {:flags {:rd-reveal (req true)}
   :access {:req (req (not (in-discard? card)))
            :interactive (req true)
            :effect (req (when (= (first (get-zone card)) :deck)
                           (system-msg state :runner (str "accesses Tempus"))))
            :trace {:base 3
                    :successful
                    {:msg "make the Runner choose between losing [Click][Click] or suffering 1 brain damage"
                     :async true
                     :effect (req (let [tempus card]
                                    (if (< (:click runner) 2)
                                      (do
                                        (system-msg state side "suffers 1 brain damage")
                                        (damage state side eid :brain 1 {:card tempus}))
                                      (do
                                        (show-wait-prompt state :corp "Runner to resolve Tempus")
                                        (continue-ability
                                          state :runner
                                          {:prompt "Lose [Click][Click] or take 1 brain damage?"
                                           :player :runner
                                           :choices ["Lose [Click][Click]" "Take 1 brain damage"]
                                           :async true
                                           :effect
                                           (req (clear-wait-prompt state :corp)
                                                (if (.startsWith target "Take")
                                                  (do
                                                    (system-msg state side (str "chooses to take 1 brain damage"))
                                                    (damage state side eid :brain 1 {:card tempus}))
                                                  (do
                                                    (system-msg state side "chooses to lose [Click][Click]")
                                                    (lose state :runner :click 2)
                                                    (effect-completed state side eid))))}
                                          card nil)))))}}}})

(defcard "The Twins"
  {:events [{:event :pass-ice
             :optional
             {:req (req (and this-server
                             (rezzed? (:ice context))
                             (seq (filter #(same-card? :title % (:ice context)) (:hand corp)))))
              :prompt (msg "Force the runner to encounter "
                           (:title current-ice) " again?")
              :yes-ability
              {:async true
               :prompt "Select a copy of the ICE just passed"
               :choices {:req (req (and (in-hand? target)
                                        (ice? target)
                                        (same-card? :title current-ice target)))}
               :msg (msg "trash a copy of " (:title target) " from HQ and force the Runner to encounter it again")
               :effect (req (wait-for
                              (reveal state side target)
                              (swap! state update-in [:run :position] inc)
                              (set-next-phase state :encounter-ice)
                              (update-all-ice state side)
                              (update-all-icebreakers state side)
                              (trash state side eid (assoc target :seen true) nil)
                              (start-next-phase state side nil)))}}}]})

(defcard "Tori Hanzō"
  {:events [{:event :pre-resolve-damage
             :once :per-run
             :async true
             :req (req (and this-server
                            (= target :net)
                            (= :corp (second targets))
                            (pos? (last targets))
                            (can-pay? state :corp (assoc eid :source card :source-type :ability) card nil [:credit 2])))
             :effect (req (swap! state assoc-in [:damage :damage-replace] true)
                          (show-wait-prompt state :runner "Corp to use Tori Hanzō")
                          (continue-ability
                            state side
                            {:optional
                             {:prompt (str "Pay 2 [Credits] to do 1 brain damage with Tori Hanzō?")
                              :player :corp
                              :yes-ability
                              {:async true
                               :msg "do 1 brain damage instead of net damage"
                               :effect (req (swap! state update-in [:damage] dissoc :damage-replace :defer-damage)
                                            (clear-wait-prompt state :runner)
                                            (wait-for (pay state :corp card :credit 2)
                                                      (system-msg state side (:msg async-result))
                                                      (wait-for (damage state side :brain 1 {:card card})
                                                                (swap! state assoc-in [:damage :damage-replace] true)
                                                                (effect-completed state side eid))))}
                              :no-ability
                              {:effect (req (swap! state update-in [:damage] dissoc :damage-replace)
                                            (clear-wait-prompt state :runner))}}}
                            card nil))}
            {:event :prevented-damage
             :req (req (and this-server
                            (= target :net)
                            (pos? (last targets))))
             :effect (req (swap! state assoc-in [:per-run (:cid card)] true))}]})

(defcard "Traffic Analyzer"
  {:events [{:event :rez
             :req (req (and (protecting-same-server? card target)
                            (ice? target)))
             :interactive (req true)
             :trace {:base 2
                     :successful {:msg "gain 1 [Credits]"
                                  :async true
                                  :effect (effect (gain-credits eid 1))}}}]})

(defcard "Tranquility Home Grid"
  {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
   :events [{:event :corp-install
             :req (req (and (or (asset? target)
                                (agenda? target)
                                (upgrade? target))
                            (in-same-server? card target)
                            (first-event? state :corp :corp-install #(in-same-server? card (first %)))))
             :prompt (msg "Use " (:title card) " to gain 2 [Credits] or draw 1 card?")
             :choices ["Gain 2 [Credits]" "Draw 1 card"]
             :msg (msg (decapitalize target))
             :async true
             :effect (req (if (= target "Gain 2 [Credits]")
                            (gain-credits state side eid 2)
                            (draw state side eid 1 nil)))}]})

(defcard "Tyr's Hand"
  {:abilities [{:label "Prevent a subroutine on a piece of Bioroid ICE from being broken"
                :req (req (and (= (butlast (get-zone current-ice)) (butlast (get-zone card)))
                               (has-subtype? current-ice "Bioroid")))
                :cost [:trash]
                :msg (msg "prevent a subroutine on " (:title current-ice) " from being broken")}]})

(defcard "Underway Grid"
  {:events [{:event :pre-expose
             :req (req (same-server? card target))
             :msg "prevent 1 card from being exposed"
             :effect (effect (expose-prevent 1))}]
   :constant-effects [{:type :bypass-ice
                       :req (req (same-server? card target))
                       :value false}]})

(defcard "Valley Grid"
  {:events [{:event :subroutines-broken
             :req (req (and this-server (all-subs-broken? target)))
             :msg "reduce the Runner's maximum hand size by 1 until the start of the next Corp turn"
             :effect (effect (register-floating-effect
                               card
                               {:type :hand-size
                                :duration :until-corp-turn-begins
                                :req (req (= :runner side))
                                :value -1}))}]})

(defcard "Warroid Tracker"
  (letfn [(wt [n]
            {:prompt "Choose an installed card to trash due to Warroid Tracker"
             :async true
             :interactive (req true)
             :player :runner
             :choices {:all true
                       :max n
                       :card #(and (runner? %)
                                   (installed? %))}
             :msg (msg "force the Runner to trash " (string/join ", " (map :title targets)))
             :effect (req (wait-for (trash-cards state :runner targets {:unpreventable true})
                                    (clear-wait-prompt state :corp)
                                    (effect-completed state side eid)
                                    ;; this ends-the-run if WT is the only card and is trashed, and trashes at least one runner card
                                    (when (not (get-only-card-to-access state))
                                      (handle-end-run state side))))})
          (ability []
            {:trace {:base 4
                     :successful
                     {:async true
                      :msg (msg (let [n (min 2 (count (all-installed state :runner)))]
                                  (str "to force the runner to trash "
                                       (quantify n "installed card")
                                       (when (not (pos? n))
                                         "but there are no installed cards to trash"))))
                      :effect (req (let [n (min 2 (count (all-installed state :runner)))]
                                     (if (pos? n)
                                       (do (show-wait-prompt state :corp "Runner to choose cards to trash")
                                           (continue-ability state side (wt n) card nil))
                                       (effect-completed state side eid))))}}})]
    {:on-trash {:async true
                :once-per-instance true
                :req (req (and (= side :runner)
                               (not (in-root? card))))
                :effect (effect (continue-ability (ability) card nil))}
     :events [{:event :runner-trash
               :async true
               :once-per-instance true
               :req (req (some (fn [target]
                                 (and (corp? (:card target))
                                      (let [target-zone (get-zone (:card target))
                                            target-zone (or (central->zone target-zone) target-zone)
                                            warroid-zone (get-zone card)]
                                        (and (not (is-root? target-zone))
                                             (= (second warroid-zone)
                                                (second target-zone))))))
                               targets))
               :effect (effect (continue-ability (ability) card nil))}]}))

(defcard "Will-o'-the-Wisp"
  {:implementation "Doesn't restrict icebreaker selection"
   :events [{:event :successful-run
             :interactive (req true)
             :async true
             :req (req (and this-server
                            (some #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
             :effect (effect (show-wait-prompt :runner "Corp to use Will-o'-the-Wisp")
                             (continue-ability
                               {:optional
                                {:prompt "Trash Will-o'-the-Wisp?"
                                 :yes-ability {:async true
                                               :prompt "Choose an icebreaker used to break at least 1 subroutine during this run"
                                               :choices {:card #(has-subtype? % "Icebreaker")}
                                               :msg (msg "add " (:title target) " to the bottom of the Runner's Stack")
                                               :effect (req (wait-for (trash state side card nil)
                                                                      (move state :runner target :deck)
                                                                      (clear-wait-prompt state :runner)
                                                                      (effect-completed state side eid)))}
                                 :no-ability {:effect (req (clear-wait-prompt state :runner)
                                                           (effect-completed state side eid))}}}
                               card nil))}]})
