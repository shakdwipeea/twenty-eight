(ns shakdwipeea.twenty-eight.core-test
  (:require [clojure.test :as t :refer [is deftest]]
            [shakdwipeea.twenty-eight.core :as c]
            [clojure.core.async :refer [go >!! <!! <! >!] :as async]))

(deftest spec-async
  (let [test-ch (c/chan-of ::c/suit 1)]
    (is (thrown? Exception (>!! test-ch :bla)))
    (is (true? (>!! test-ch :diamond)))))

(defn run-player1 [{:keys [::c/player-chan ::c/game-chan]}]
  (async/go-loop [a true]
    (case (<! game-chan)
      :want-redeal (>! player-chan false))))

(deftest play-game
  (println "We're gonna play games")
  (c/deal-hand)
  (go (c/run-game))
  (go (run-player1 (c/cur-game))) 
  (is (= 1 1)))
