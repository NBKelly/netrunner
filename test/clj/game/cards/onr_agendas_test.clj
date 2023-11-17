(ns game.cards.onr-agendas-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core.eid :refer [make-eid]]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest onr-ice-transmutation
  (do-game
    (new-game {:corp {:hand ["Hydra" "ONR Ice Transmutation"]
                      :credits 15}})
    (play-from-hand state :corp "Hydra" "HQ")
    (let [hydra (get-ice state :hq 0)]
      (rez state :corp hydra)
      (play-from-hand state :corp "ONR Ice Transmutation" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-card state :corp (refresh hydra))
      (is (= 7 (get-strength (refresh hydra))))
      (is (= 3 (count (:subroutines (refresh hydra)))))
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state :encounter-ice)
      (is (= 6 (count (:subroutines (refresh hydra)))))
      (run-continue state :pass-ice)
      (is (= 3 (count (:subroutines (refresh hydra))))))))

(deftest onr-subsidiary-branch
    ;; Gain an additional click
    (do-game
      (new-game {:corp {:deck ["ONR Subsidiary Branch"
                               "Melange Mining Corp."]}})
      (play-and-score state "ONR Subsidiary Branch")
      (is (= 1 (:agenda-point (get-corp))))
      (play-from-hand state :corp "Melange Mining Corp." "New remote")
      (let [mmc (get-content state :remote2 0)]
        (rez state :corp mmc)
        (take-credits state :corp)
        (take-credits state :runner)
        (is (= 4 (:click (get-corp))))
        (card-ability state :corp mmc 0)
        (is (= 1 (:click (get-corp)))))))

(deftest onr-world-domination
  (do-game
    (new-game {:corp {:deck ["ONR World Domination"]}})
    (play-and-score state "ONR World Domination")
    (is (= 7 (:agenda-point (get-corp))))))
