(ns shakdwipeea.twenty-eight.core-test
  (:require [clojure.test :as t :refer [is deftest]]
            [shakdwipeea.twenty-eight.core :as c]
            [clojure.core.async :refer [go >!! <!! <! >!] :as async]))

(deftest spec-async
  (let [test-ch (c/chan-of ::c/suit 1)]
    (is (thrown? Exception (>!! test-ch :bla)))
    (is (true? (>!! test-ch :diamond)))))

(defn run-player1 [{:keys [::c/player-chan ::c/game-chan]}]
  (go (while true
        (case (<! game-chan)
          :want-redeal (>! player-chan false)))))

(defn state-listener []
  (->> (c/notify-game-state-change)
     (map #(println "State now is " %))))

(deftest play-game
  (println "We're gonna play games")
  (c/deal-hand)
  (go (c/run-game (c/cur-game)))
  (run-player1 (-> (c/cur-game) ::c/players first))
  (state-listener)
  (is (= 1 1)))
