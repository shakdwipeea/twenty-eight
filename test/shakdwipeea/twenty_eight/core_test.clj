(ns shakdwipeea.twenty-eight.core-test
  (:require [shakdwipeea.twenty-eight.play :as p]
            [shakdwipeea.twenty-eight.core :as c]
            [clojure.test :as t :refer [is deftest]]
            [clojure.core.async :refer [go >!! <!! <! >!] :as async]))



#_(deftest game-sim 
    (def game-1 (atom {}))
    ;; run the game
    (<!! (go (p/play-game game-1 :simply-pass)))

    ;; query bid value
    (is (= 16 (-> @game-1 ::c/bid-value))))

(deftest game-sim-2
  (def game-2 (atom {}))

  (<!! (go (p/play-game game-2 :bid-higher)))
  (is (= 28 (-> @game-2 ::c/bid-value))))

#_(go (hi))

#_(c/change-game-state! ::c/started)

#_(-> @g ::c/game-state)
