(ns shakdwipeea.twenty-eight.core-test
  (:require [clojure.test :as t :refer [is deftest]]
            [shakdwipeea.twenty-eight.core :as c]
            [clojure.spec.alpha :as s]
            [clojure.core.async :refer [go >!! <!! <! >!] :as async]))

(deftest spec-async
  (let [test-ch (c/chan-of ::c/suit 1)]
    (is (thrown? Exception (>!! test-ch :bla)))
    (is (true? (>!! test-ch :diamond)))))

(def bid-funcs {:types '(:bid-higher :simply-pass)
                :bid-higher (fn [b] (cond-> b
                                     (< b 28) (+ 1)))
                :simply-pass (fn [_] :pass)})

#_(def bidf (get bid-funcs :simply-pass))

(defn run-player [<game> {:keys [::c/player-chan ::c/game-chan]} bid-func-type]
  (let [bidf (get bid-funcs bid-func-type)
        game-msg (<!! game-chan)]
    (println "Got message from game-channel " game-msg)
    (case game-msg
      :want-redeal (>!! player-chan false)
      :bid (do (println "Trying to bid")
               (>!! player-chan {::c/bid-value (or (some-> @<game> ::c/bid-value bidf)
                                                  16)}))
      (println "What gibberish!!"))))

(defn state-listener [game]
  (async/go-loop []
    (let [new-state (<! (c/<notify-game-state-change game))]
      (println "State now is " new-state)
      (is (s/valid? ::c/game-state new-state)))
    (recur)))

(defn run-players
  [<g>]
  (println "Run players " (keys @<g>))
  (doseq [p (-> @<g> ::c/players)]
    (println "Running player")
    (async/thread (while true (run-player <g> p :simply-pass)))))

(def game (atom {}))

(deftest play-game
  (println "We're gonna play games")
  (let [game (atom {})
        a (async/chan 1)]
    (def <game> game)
    (state-listener game)
    (c/init-game game)
    (async/thread (run-players <game>))
    (async/thread (c/run-game <game>))
    (println "Game is " (keys @game))        
    (is (= 16 (-> @game ::c/bid-value)))))

(defn hi []
  (println "Hi!!"))

(deftest s
  (def a (async/chan 1))
  (go (>!! a :hello))
  (is (= (<!! (go (<! a))) :hello)))

#_(go (hi))

#_(c/change-game-state! ::c/started)

#_(-> @g ::c/game-state)
