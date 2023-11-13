(ns game.cards.onr-programs
  (:require
   [clojure.string :as str]
   [game.core.access :refer [access-bonus max-access]]
   [game.core.board :refer [all-active all-active-installed all-installed all-installed-runner-type
                            card->server server->zone]]
   [game.core.card :refer [active? agenda? asset? card-index corp? facedown? faceup?
                           get-advancement-requirement get-card get-counters
                           get-nested-host get-title get-zone
                           hardware? has-subtype? in-hand? in-discard? ice? installed?
                           is-type? program? resource? rezzed? runner?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.charge :refer [charge-ability]]
   [game.core.cost-fns :refer [all-stealth install-cost min-stealth rez-cost]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage damage-prevent]]
   [game.core.def-helpers :refer [breach-access-bonus defcard offer-jack-out trash-on-empty get-x-fn]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [any-effects register-lingering-effect
                              unregister-effects-for-card]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [ability-as-handler dissoc-req not-used-once? pay
                             print-msg register-events register-once
                             trigger-event trigger-event-simult unregister-events]]
   [game.core.events :refer [run-events first-event? first-installed-trash? run-events
                             first-successful-run-on-server? turn-events]]
   [game.core.expose :refer [expose]]
   [game.core.finding :refer [find-cid]]
   [game.core.flags :refer [can-host? can-trash? card-flag? lock-zone release-zone zone-locked?]]
   [game.core.gaining :refer [gain-clicks gain-credits lose-credits]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [all-subs-broken-by-card? all-subs-broken?
                          any-subs-broken-by-card? auto-icebreaker break-sub
                          break-subroutine! break-subroutines-msg breaker-strength-bonus dont-resolve-subroutine!
                          get-strength ice-strength pump pump-ice set-current-ice strength-pump
                          unbroken-subroutines-choice update-all-icebreakers update-breaker-strength]]
   [game.core.initializing :refer [ability-init card-init]]
   [game.core.installing :refer [install-locked? runner-can-install? runner-can-pay-and-install?
                                 runner-install]]
   [game.core.link :refer [get-link]]
   [game.core.mark :refer [identify-mark-ability]]
   [game.core.memory :refer [available-mu update-mu]]
   [game.core.moving :refer [flip-facedown mill move swap-cards swap-ice trash
                             trash-prevent]]
   [game.core.optional :refer [get-autoresolve set-autoresolve never?]]
   [game.core.payment :refer [build-cost-label can-pay? cost-target cost-value]]
   [game.core.prompts :refer [cancellable]]
   [game.core.props :refer [add-counter add-icon remove-icon]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez get-rez-cost rez]]
   [game.core.runs :refer [active-encounter? bypass-ice continue end-run-prevent
                           get-current-encounter make-run successful-run-replace-breach
                           update-current-encounter]]
   [game.core.sabotage :refer [sabotage-ability]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name is-central? is-remote? protecting-same-server?
                              remote->name target-server unknown->kw zone->name]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.tags :refer [gain-tags lose-tags]]
   [game.core.to-string :refer [card-str]]
   [game.core.threat :refer [threat threat-level]]
   [game.core.trace :refer [force-base]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [get-virus-counters]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

(defcard "ONR Blink"
  (letfn [(attempt
            [state side card sub]
            (let [c (get-card state card)]
              (update!
                state side
                (assoc-in c [:special :attempted-breaks]
                          (concat (get-in c [:special :attempted-breaks]) [sub])))))
          (viable-sub
            [sub card]
            (not (seq (filter #(or (:broken %) (= % sub)) (get-in card [:special :attempted-breaks])))))
          (viable-subs
            [subs card]
            (filter #(viable-sub % card) subs))]
    ;; we're only allowed to attempt each subroutine once!
    {:implementation "always breaks identical subs from the top down (I'll fix this later, send me an @ if it's an issue)"
     :on-install {:silent (req true)
                  :effect (req (update! state :runner (assoc-in (get-card state card) [:special :attempted-breaks] [])))}
     :events [{:event :encounter-ice
               :silent (req true)
               :effect (req (update! state :runner (assoc-in (get-card state card) [:special :attempted-breaks] [])))}]
     :abilities [{:label "Maybe break a subroutine"
                  :req (req (and
                              current-ice
                              (<= (get-strength current-ice) (get-strength card))
                              (seq (viable-subs (remove :broken (:subroutines current-ice)) card))))
                  :cost [:credit 0]
                  :prompt "Choose a subroutine to try to break"
                  :choices (req (cancellable (map #(make-label (:sub-effect %)) (viable-subs (remove :broken (:subroutines current-ice)) card))))
                  :async true
                  :effect (req
                            (let [chosen-sub (first (filter #(and (= target (make-label (:sub-effect %)))
                                                                  (not (:broken %))
                                                                  (viable-sub % card)) (:subroutines current-ice)))
                                  die-result (+ 1 (rand-int 6))
                                  success (<= 4 die-result)]
                              (system-msg state side (str "rolls a " die-result " (1d6)"))
                              (if success
                                (do
                                  (system-msg state side (str (break-subroutines-msg current-ice [chosen-sub] card)))
                                  (break-subroutine! state (get-card state current-ice) chosen-sub)
                                  (effect-completed state side eid))
                                (do
                                  (system-msg state side (str "takes " die-result " net damage"))
                                  (damage state side eid :net die-result {:card card})))))}]}))

(defcard "ONR Cyfermaster"
  (auto-icebreaker {:abilities [(break-sub 2 1 "Code Gate")
                                (strength-pump 1 1)]}))
