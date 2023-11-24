(ns game.core.player)

(defrecord HandSize
  [base total])

(defrecord Corp
  [aid
   acme-loans
   user
   identity
   options
   basic-action-card
   deck
   deck-id
   hand
   discard
   scored
   rfg
   play-area
   current
   set-aside
   set-aside-tracking
   servers
   click
   click-per-turn
   credit
   run-credit
   debt
   action-debt
   bad-publicity
   toast
   hand-size
   agenda-point
   agenda-point-req
   agenda-point-debt
   keep
   quote])

(defrecord Servers
  [hq rd archives])

(defrecord BadPublicity
  [base additional])

(defn new-corp
  [user c-identity options deck deck-id c-quote]
  (map->Corp
    {:aid 0
     :acme-loans 0
     :user user
     :identity c-identity
     :options options
     :basic-action-card nil
     :deck deck
     :deck-id deck-id
     :hand []
     :discard [] :scored [] :rfg [] :play-area [] :current [] :set-aside [] :set-aside-tracking {}
     :servers (map->Servers {:hq {:content [] :ices []}
                             :rd {:content [] :ices []}
                             :archives {:content [] :ices []}})
     :click 0 :click-per-turn 3
     :credit 5 :run-credit 0
     :debt 0 :action-debt 0
     :bad-publicity (map->BadPublicity {:base 0 :additional 0})
     :toast []
     :hand-size (map->HandSize {:base 5 :total 5})
     :agenda-point 0 :agenda-point-req 7 :agenda-point-debt 0
     :keep false
     :quote c-quote}))

(defrecord Runner
  [aid
   user
   identity
   options
   basic-action-card
   deck
   deck-id
   debt
   hand
   discard
   scored
   rfg
   play-area
   current
   set-aside
   set-aside-tracking
   rig
   toast
   click
   click-per-turn
   credit
   action-debt
   run-credit
   link
   tag
   memory
   hand-size
   agenda-point
   agenda-point-req
   agenda-point-debt
   hq-access
   rd-access
   rd-access-fn
   brain-damage
   keep
   quote
   ;; you can thank garfield for this nonsense
   flatline-counter     ;; cannot run unless you pay 1+click to remove
   stun-counter         ;; cannot run unless you pay 2+click to remove
   doppelganger-counter ;; lose 1 at the start of your turn. Click+4c to remove.
   baskerville-counter  ;; 2 net at the start of each run. Click+3c to remove.
   cerberus-counter     ;; 2 net at the start of each run. Click+4c to remove.
   data-raven-counter   ;; tag at the start of each run. Click+1c to remove.
   mastiff-counter      ;; brain at the start of each run. Click+4c to remove.
   crying-counter       ;; -2 link. Tag+2c to remove.
   haunting-inquisition-counter
   ])

(defrecord Rig
  [facedown hardware program resource])

(defrecord Tags
  [base total is-tagged])

(defn new-runner
  [user r-identity options deck deck-id r-quote]
  (map->Runner
    {:aid 0
     :user user
     :identity r-identity
     :options options
     :basic-action-card nil
     :deck deck
     :deck-id deck-id
     :hand []
     :discard [] :scored [] :rfg [] :play-area [] :current [] :set-aside [] :set-aside-tracking {}
     :rig (map->Rig {:facedown [] :hardware [] :program [] :resource []})
     :toast []
     :click 0 :click-per-turn 4
     :credit 5 :run-credit 0
     :debt 0 :action-debt 0
     :link 0
     :tag (map->Tags {:base 0 :total 0 :is-tagged false})
     :memory {:base 4
              :available 0
              :used 0
              :only-for {}}
     :hand-size (map->HandSize {:base 5 :total 5})
     :agenda-point 0 :agenda-point-req 7 :agenda-point-debt 0
     :rd-access-fn seq
     :hq-access-fn shuffle
     :brain-damage 0
     :keep false
     :quote r-quote
     ;; thanks garfield
     :flatline-counter 0     ;; cannot run unless you pay 1+click to remove
     :stun-counter 0         ;; cannot run unless you pay 2+click to remove
     :doppelganger-counter 0 ;; lose 1 at the start of your turn. Click+4c to remove.
     :baskerville-counter 0  ;; 2 net at the start of each run. Click+3c to remove.
     :cerberus-counter 0     ;; 2 net at the start of each run. Click+4c to remove.
     :data-raven-counter 0   ;; tag at the start of each run. Click+1c to remove.
     :mastiff-counter 0      ;; brain at the start of each run. Click+4c to remove.
     :crying-counter 0       ;; -2 link. Click+2c to remove.
     :haunting-inquisition-counter 0 ;; cannot make run for next 6 actions
     }))
