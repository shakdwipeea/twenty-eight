(ns shakdwipeea.twenty-eight.core-test
  (:require [shakdwipeea.twenty-eight.play :as p]
            [shakdwipeea.twenty-eight.core :as c]
            [snow.async :as a]
            [clojure.test :as t :refer [is deftest]]
            [clojure.core.async :refer [go >!! <!! <! >!] :as async]))



#_(deftest game-sim 
    (def game-1 (atom {}))
    ;; run the game
    (<!! (go (p/play-game game-1 :simply-pass)))

    ;; query bid value
    (is (= 16 (-> @game-1 ::c/bid-value))))

(def test-msg {::c/control ::c/state-change
               ::c/game-state ::c/choose-trump})

(deftest game-control-sub []
  (let [sample-chan (c/chan-of ::c/game-control 2)
        dest-chan (a/subscribe (async/mult sample-chan) ::c/control ::c/state-change)]
    (is (true? (>!! sample-chan test-msg)))
    (is (= (<!! dest-chan) test-msg))))

(def game (atom {}))

#_(-> @game ::c/players)

(def players (-> @game ::c/players))

(deftest game-sim-test 
  (reset! game (c/initial-draw))

  ;; setup pub channels
  (let [p' (-> @game ::c/players p/make-mult)]
    (swap! game assoc ::c/players p'))

  ;; setup state atom

  (async/thread (p/start-players @game))

  (let [g (<!! (async/thread (c/run-game @game)))]
    (is (= 28 (->> g ::c/players (map ::c/points-collected) (filter some?) (apply +))))))

#_(go (hi))

#_(c/change-game-state! ::c/started)

#_(-> @g ::c/game-state)
