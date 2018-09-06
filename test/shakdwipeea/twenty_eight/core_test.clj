(ns shakdwipeea.twenty-eight.core-test
  (:require [clojure.test :as t :refer [is deftest]]
            [shakdwipeea.twenty-eight.core :as c]
            [clojure.spec.alpha :as s]
            [clojure.core.async :refer [go >!! <!! <! >!] :as async]))

(deftest spec-async
  (let [test-ch (c/chan-of ::c/suit 1)]
    (is (thrown? Exception (>!! test-ch :bla)))
    (is (true? (>!! test-ch :diamond)))))

(defn run-player [{:keys [::c/player-chan ::c/game-chan]}]
  (go (while true
        (case (<! game-chan)
          :want-redeal (>! player-chan false)
          :bid (>! player-chan {::c/bid-value 17})))))

(defn state-listener []
  (async/go-loop []
    (let [new-state (<! (c/notify-game-state-change))]
      (println "State now is " new-state)
      (is (s/valid? ::c/game-state new-state)))
    (recur)))

(defn run-players
  [{players ::c/players}]
  (doseq [p players]
    (run-player p)))

(deftest play-game
  (println "We're gonna play games")
  (state-listener)
  (c/deal-hand)
  (go (c/run-game (c/cur-game)))
  (run-players (c/cur-game))
  (is (= 1 1)))

(defn hi []
  (println "Hi!!"))

#_(go (hi))

#_(c/change-game-state! ::c/started)

#_(-> (c/cur-game) ::c/game-state)
