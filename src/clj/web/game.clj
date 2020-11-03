(ns web.game
  (:require [web.ws :as ws]
            [web.lobby :refer [all-games old-states already-in-game? spectator?] :as lobby]
            [web.utils :refer [response]]
            [web.stats :as stats]
            [game.main :as main]
            [game.diffs :refer [public-diffs public-states]]
            [game.core :as core]
            [web.db :refer [db object-id]]
            [monger.collection :as mc]
            [jinteki.utils :refer [side-from-str]]
            [cheshire.core :as json]
            [crypto.password.bcrypt :as bcrypt]
            [clj-time.core :as t]))


(defn send-state-diffs!
  "Sends diffs generated by public-diffs to all connected clients."
  [{:keys [gameid players spectators] :as game}
   {:keys [type runner-diff corp-diff spect-diff] :as diffs}]
  (doseq [{:keys [ws-id side] :as pl} players]
    (ws/broadcast-to! [ws-id] :netrunner/diff (json/generate-string {:gameid gameid
                                                                     :diff (if (= side "Corp")
                                                                              corp-diff
                                                                              runner-diff)})))
  (ws/broadcast-to! (map #(:ws-id %) spectators)
                    :netrunner/diff
                    (json/generate-string {:gameid gameid
                                           :diff spect-diff})))

(defn send-state!
  "Sends full states generated by public-states to either the client specified or all connected clients."
  ([event
    {:keys [gameid players spectators] :as game}
    {:keys [type runner-state corp-state spect-state] :as states}
    ws-id]
   (let [player (some #(= (:ws-id %) ws-id) players)]
     (ws/broadcast-to! [ws-id] event (json/generate-string {:gameid gameid
                                                            :state (if player
                                                                      (if (= (:side player) "Corp")
                                                                        corp-state
                                                                        runner-state)
                                                                      spect-state)}))))

  ([event
    {:keys [gameid players spectators] :as game}
    {:keys [type runner-state corp-state spect-state] :as states}]
   (doseq [{:keys [ws-id side] :as pl} players]
     (ws/broadcast-to! [ws-id] event (json/generate-string {:gameid gameid
                                                            :state (if (= side "Corp")
                                                                      corp-state
                                                                      runner-state)})))
   (ws/broadcast-to! (map #(:ws-id %) spectators) event (json/generate-string {:gameid gameid
                                                                               :state spect-state}))))

(defn swap-and-send-state!
  "Updates the old-states atom with the new game state, then sends a :netrunner/state
  message to game clients."
  [{:keys [gameid state] :as game}]
  (swap! old-states assoc gameid @state)
  (send-state! :netrunner/state game (public-states state)))

(defn swap-and-send-diffs!
  "Updates the old-states atom with the new game state, then sends a :netrunner/diff
  message to game clients."
  [{:keys [gameid state] :as game}]
  (let [old-state (get @old-states gameid)]
    (when (and state @state)
      (swap! old-states assoc gameid @state)
      (send-state-diffs! game (public-diffs old-state state)))))

(defn- active-game?
  [gameid-str client-id]
  (if (nil? gameid-str)
    false
    (let [gameid (java.util.UUID/fromString gameid-str)
          game-from-gameid (lobby/game-for-id gameid)
          game-from-clientid (lobby/game-for-client client-id)]
      (and game-from-clientid
           game-from-gameid
           (= (:gameid game-from-clientid) (:gameid game-from-gameid))))))

(defn handle-game-start
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id}]
  (when-let [{:keys [players gameid started messages] :as game} (lobby/game-for-client client-id)]
    (when (and (lobby/first-player? client-id gameid)
               (not started))
      (let [strip-deck (fn [player] (-> player
                                        (update-in [:deck] #(select-keys % [:_id :identity :name]))
                                        (update-in [:deck :identity] #(select-keys % [:title :faction]))))
            stripped-players (mapv strip-deck players)
            start-date (t/now)
            game (as-> game g
                   (assoc g :started true
                          :original-players stripped-players
                          :ending-players stripped-players
                          :start-date (java.util.Date.)
                          :last-update start-date
                          :state (core/init-game g))
                   (update-in g [:players] #(mapv strip-deck %)))]
        (stats/game-started game)
        (lobby/refresh-lobby gameid game)
        (swap! old-states assoc gameid @(:state game))
        (send-state! :netrunner/start game (public-states (:state game)))))))

(defn handle-game-leave
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    {:keys [gameid-str] :as msg}        :?data}]
  (let [{:keys [started players gameid state] :as game} (lobby/game-for-client client-id)]
    (when (and started state)
      (lobby/remove-user client-id gameid)
      (when-let [game (lobby/game-for-id gameid)]
        ; The game will not exist if this is the last player to leave.
        (main/handle-notification state (str username " has left the game."))
        (swap-and-send-diffs! (lobby/game-for-id gameid))))))

(defn handle-game-rejoin
  [{{{:keys [username _id] :as user} :user} :ring-req
    client-id                           :client-id
    {:keys [gameid]}   :?data
    reply-fn                            :?reply-fn}]
  (let [{:keys [original-players started players state] :as game} (lobby/game-for-id gameid)]
    (if (and started
             (< (count (filter #(not= _id (get-in % [:user :_id])) players)) 2)
             (some #(= _id (:_id %)) (map :user original-players)))
      (let [player (lobby/join-game user client-id gameid)
            side (keyword (str (.toLowerCase (:side player)) "-state"))]
        (main/handle-rejoin state (:user player))
        (ws/broadcast-to! [client-id] :lobby/select {:gameid gameid
                                                     :started started
                                                     :state (json/generate-string (side (public-states (:state game))))})
        (send-state! :netrunner/state (lobby/game-for-id gameid) (public-states (:state game)) client-id)
        (swap-and-send-diffs! (lobby/game-for-id gameid))))))

(defn handle-game-concede
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    {:keys [gameid-str] :as msg}        :?data}]
  (when (active-game? gameid-str client-id)
    (let [gameid (java.util.UUID/fromString gameid-str)
          {:keys [started players state] :as game} (lobby/game-for-id gameid)
          side (some #(when (= client-id (:ws-id %)) (:side %)) players)]
      (when (lobby/player? client-id gameid)
        (main/handle-concede state (side-from-str side))
        (swap-and-send-diffs! game)))))

(defn handle-mute-spectators
  [{{{:keys [username] :as user} :user}          :ring-req
    client-id                                    :client-id
    {:keys [gameid-str mute-state] :as msg}      :?data}]
  (when (active-game? gameid-str client-id)
    (let [gameid (java.util.UUID/fromString gameid-str)
          {:keys [started state] :as game} (lobby/game-for-id gameid)
          message (if mute-state "muted" "unmuted")]
      (when (lobby/player? client-id gameid)
        (lobby/refresh-lobby-assoc-in gameid [:mute-spectators] mute-state)
        (main/handle-notification state (str username " " message " specatators."))
        (swap-and-send-diffs! game)))))

(defn handle-game-action
  [{{{:keys [username] :as user} :user}       :ring-req
    client-id                                 :client-id
    {:keys [gameid-str command args] :as msg} :?data}]
  (when (active-game? gameid-str client-id)
    (let [gameid (java.util.UUID/fromString gameid-str)
          {:keys [players state] :as game} (lobby/game-for-id gameid)
          side (some #(when (= client-id (:ws-id %)) (:side %)) players)
          spectator (spectator? client-id gameid)]
      (if (and state side)
        (do
          (main/handle-action user command state (side-from-str side) args)
          (lobby/refresh-lobby-assoc-in gameid [:last-update] (t/now))
          (swap-and-send-diffs! game))
        (when-not spectator
          (println "handle-game-action unknown state or side")
          (println "\tGameID:" gameid)
          (println "\tGameID by ClientID:" (:gameid (lobby/game-for-client client-id)))
          (println "\tClientID:" client-id)
          (println "\tSide:" side)
          (println "\tPlayers:" (map #(select-keys % [:ws-id :side]) players))
          (println "\tSpectators" (map #(select-keys % [:ws-id]) (:spectators game)))
          (println "\tCommand:" command)
          (println "\tArgs:" args "\n"))))))

(defn handle-game-watch
  "Handles a watch command when a game has started."
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    {:keys [gameid password options]}   :?data
    reply-fn                            :?reply-fn}]
  (if-let [{game-password :password state :state started :started :as game}
           (lobby/game-for-id gameid)]
    (when (and user game (lobby/allowed-in-game game user) state @state)
      (if-not started
        false ; don't handle this message, let lobby/handle-game-watch.
        (if (and (not (already-in-game? user game))
                 (or (empty? game-password)
                     (bcrypt/check password game-password)))
          (let [{:keys [spect-state]} (public-states state)]
            ;; Add as a spectator, inform the client that this is the active game,
            ;; Send existing state to spectator
            ;; add a chat message, then send diff state to all players.
            (lobby/spectate-game user client-id gameid)
            (main/handle-notification state (str username " joined the game as a spectator."))
            (ws/broadcast-to! [client-id] :lobby/select {:gameid gameid
                                                         :started started})
            (send-state! :netrunner/state (lobby/game-for-id gameid) (public-states (:state game)) client-id)
            (swap-and-send-diffs! (lobby/game-for-id gameid))
            (when reply-fn (reply-fn 200))
            true)
          (when reply-fn
            (reply-fn 403)
            false))))
    (when reply-fn
      (reply-fn 404)
      false)))

(defn handle-game-say
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    {:keys [gameid-str msg]}                :?data}]
  (when (active-game? gameid-str client-id)
    (let [gameid (java.util.UUID/fromString gameid-str)
          {:keys [state mute-spectators] :as game} (lobby/game-for-id gameid)
          {:keys [side user]} (lobby/player? client-id gameid)]
      (if (and state side user)
        (do (main/handle-say state (jinteki.utils/side-from-str side) user msg)
            (swap-and-send-diffs! game))
        (let [{:keys [user]} (lobby/spectator? client-id gameid)]
          (when (and user (not mute-spectators))
            (main/handle-say state :spectator user msg)
            (lobby/refresh-lobby-assoc-in gameid [:last-update] (t/now))
            (try
              (swap-and-send-diffs! game)
              (catch Exception ex
                (println (str "handle-game-say exception:" (.getMessage ex) "\n"))))))))))

(defn handle-game-typing
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    {:keys [gameid-str typing]}             :?data}]
  (when (active-game? gameid-str client-id)
    (let [gameid (java.util.UUID/fromString gameid-str)
          {:keys [state] :as game} (lobby/game-for-id gameid)
          {:keys [side user]} (lobby/player? client-id gameid)]
      (when (and state side user)
        (main/handle-typing state (jinteki.utils/side-from-str side) user typing)
        (try
          (swap-and-send-diffs! game)
          (catch Exception ex
            (println (str "handle-game-typing exception:" (.getMessage ex) "\n"))))))))

(defn handle-ws-close [{{{:keys [username] :as user} :user} :ring-req
                        client-id                           :client-id}]
  (when-let [{:keys [gameid state] :as game} (lobby/game-for-client client-id)]
    (lobby/remove-user client-id (:gameid game))
    (when-let [game (lobby/game-for-id gameid)]
      ; The game will not exist if this is the last player to leave.
      (main/handle-notification state (str username " has disconnected."))
      (swap-and-send-diffs! game))))

(ws/register-ws-handlers!
  :netrunner/start #'handle-game-start
  :netrunner/action #'handle-game-action
  :netrunner/leave #'handle-game-leave
  :netrunner/rejoin #'handle-game-rejoin
  :netrunner/concede #'handle-game-concede
  :netrunner/mute-spectators #'handle-mute-spectators
  :netrunner/say #'handle-game-say
  :netrunner/typing #'handle-game-typing
  :lobby/watch #'handle-game-watch
  :chsk/uidport-close #'handle-ws-close)
