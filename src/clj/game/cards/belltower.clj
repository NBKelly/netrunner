(ns game.cards.belltower
  (:require
   [game.core.card :refer [has-subtype? in-hand? installed? corp? resource?]]
   [game.core.def-helpers :refer [breach-access-bonus corp-recur corp-rez-toast defcard do-brain-damage do-net-damage trash-on-empty]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [checkpoint not-used-once? pay register-default-events register-events register-once register-suppress resolve-ability trigger-event unregister-events unregister-suppress-by-uuid]]
   [game.core.events :refer [first-event? first-run-event? no-event? run-events last-turn?]]
   [game.core.installing :refer [install-locked? runner-install]]
   [game.core.moving :refer [as-agenda flip-facedown flip-faceup mill move swap-cards trash trash-cards]]
   [game.core.runs :refer [bypass-ice continue force-ice-encounter get-current-encounter end-run make-run set-next-phase successful-run-replace-breach start-next-phase]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name in-same-server? is-central? is-remote? from-same-server? same-server? target-server remote->name unknown->kw zone->name zones->sorted-names]]
   [game.core.tags :refer [gain-tags lose-tags]]
   [game.core.threat :refer [threat-level]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

;; helpers
(defn not-tagged-req
  [state]
  (= 0 (count-tags state)))

;; Anarch cards
(defcard "Sebastião Pessoa: Activist Organiser"
  ;; Whenever you take 1 or more tags during your turn, if you were not tagged,
  ;; you may install 1 connection resource from your grip, paying 2[Credits] less.
  ;; As an additional cost to trash connection resources, the Corp must trash 1 card
  ;; from HQ. (I'm assuming a multi-trash requires a multi-cost)
  {:implementation "2v2. Cost to trash connections not implemented"
   ;; todo - cost to trash connection resources by the corp - look at hacktivist?
   :events [{:event :runner-gain-tag
             :async true
             :req (req (and (not (install-locked? state side))
                            (= :runner (:active-player @state))
                            (= target (count-tags state)))) ;; every tag is one that was just gained
             :effect (req (continue-ability
                            state :runner
                            {:prompt "Choose a connection to install (for 2[Credits] less)"
                             :player :runner
                             :choices {:card #(and (has-subtype? % "Connection")
                                                   (resource? %)
                                                   (in-hand? %))}
                             :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -2}))}
                            card nil))}]})

(defcard "[Eye for an Eye]"
  ;; Play only if you are not tagged.
  ;; Run archives. If successful, instead of breaching Archives,
  ;; take 1 tag and the corp trashes 1 of their install cards.
  ;; Threat 5 - if you took a tag this way, the Corp trashes another 1 of their installed cards.
  ;; Remove this event from the game
  (letfn [(trash-x-installed [x]
            {:player :corp
             :prompt (str "Choose " (quantify x "card") " to trash")
             :waiting-prompt "Corp to trash a card"
             :msg (msg "trash " (:title target))
             :choices {:max x
                       :card #(and (installed? %)
                                   (corp? %))}
             :async true
             :effect (req (trash-cards state side eid targets {:cause :forced-to-trash}))})]
    {:makes-run true
     :implementation "2v2"
     :rfg-instead-of-trashing true
     :on-play {:req (req (and archives-runnable
                              (not-tagged-req state)))
               :async true
               :effect (effect (make-run eid :archives card))}
     :events [(successful-run-replace-breach
                {:target-server :archives
                 :this-card-run true
                 :ability
                 {:async true
                  :effect
                  (req (let [old-tags (count-tags state)]
                         (wait-for (gain-tags state :runner 1)
                                   (let [new-tags (count-tags state)]
                                     (wait-for (resolve-ability
                                                 state side
                                                 (trash-x-installed 1)
                                                 card nil)
                                               (if (and (= new-tags (+ 1 old-tags))
                                                        (threat-level 5 state))
                                                 (continue-ability
                                                   state side
                                                   (trash-x-installed 1)
                                                   card nil)
                                                 (effect-completed state side eid)))))))}})]}))

(defcard "[HQ Occupation]"
  ;; Play only if you are not tagged.
  ;; Run HQ. If successful, take 1 tag and access 2 additional cards when you breach HQ.
  {:implementation "2v2"
   :makes-run true
   :on-play {:req (req (and hq-runnable
                            (not-tagged-req state)))
             :async true
             :effect (effect (make-run eid :hq card))}
   :events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :hq (target-server context))
                            this-card-run))
             :async true
             :msg "take 1 tag and access 2 additional cards"
             :effect (req
                       (wait-for (gain-tags state :runner 1 {:unpreventable true})
                                 (register-events
                                   state side
                                   card [(breach-access-bonus :hq 2 {:duration :end-of-run})])
                                 (effect-completed state side eid)))}]})
