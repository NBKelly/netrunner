(ns nr.gameboard.subversion
  (:require
   [nr.gameboard.actions :refer [send-command]]
   [nr.gameboard.state :refer [game-state]]
   [nr.translations :refer [tr]]
   [nr.utils :refer [render-message]]))

(defn subversion-pane []
  (fn []
    [:div.subversions
     [:section
      [:h4 (tr [:game.subversion "Subversion"])]
      (doall (for [key [:subversion-a :subversion-b :subversion-c :subversion-d]]
               ^{:key key}
               [:div.subversion
                [:div.stat-controls
                 (case key
                   :subversion-a "Repress: "
                   :subversion-b "Isolate: "
                   :subversion-c "Starve: "
                   :subversion-d "Tax: "
                   "????: ")
                 (get-in @game-state [:special key] 0)
                 [:div.sub-controls
                  [:button.small {:on-click #(send-command "change" {:key key :delta -1}) :type "button"} "-"]
                  [:button.small {:on-click #(send-command "change" {:key key :delta 1}) :type "button"} "+"]]]
                [:div.explanation
                 (render-message
                   (case key
                     :subversion-a "The runner discards a card from their grip for each hosted power counter."
                     :subversion-b "Trash the top card of the stack for each hosted power counter."
                     :subversion-c "The runner loses 1[credit] for each hosted power counter."
                     :subversion-d "The corp gains 1[credit] for each hosted power counter."
                     "????"))]]))]]))
