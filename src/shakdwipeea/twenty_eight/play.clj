(ns shakdwipeea.twenty-eight.play
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as test]
            [shakdwipeea.twenty-eight.core :as c]
            [defn-spec.core :as ds]
            [clojure.core.async :refer [go >!! <!! <! >!] :as async]))


(def bid-funcs {:types '(:bid-higher :simply-pass)
                :bid-higher (fn [b] (cond-> b
                                     (< b 28) (+ 1)))
                :simply-pass (fn [_] :pass)})

(defn run-player [{:keys [::c/player-chan ::c/game-chan]} bid-func-type] 
  (let [bidf (get bid-funcs bid-func-type)
        game-msg (<!! game-chan)
        [msg-type value] (s/conform ::c/game-ctrl-msg game-msg)] 
    (case msg-type
      :redeal-msg (>!! player-chan false)
      :bid-msg (do (println "Trying to bid using strategy:-: " bid-func-type (-> value second second bidf))
                   (>!! player-chan {::c/bid-value (or (-> value second second bidf)
                                                      16)}))
      (println "What gibberish!!"))))

#_(-> (s/conform ::c/game-ctrl-msg [:bid 17]))

(defn echo-game [game]
  (->> game
     ::c/players
     (map (fn [{game-chan ::c/game-chan}]
            (go (println "Message for game-chan" (<! game-chan)))))
     doall))

(defn state-listener [game]
  (async/go-loop []
    (let [new-state (<! (c/<notify-game-state-change game))]
      (println "State now is " new-state)
      (assert (s/valid? ::c/game-state new-state)))
    (recur)))

(defn run-players
  [g bid-func-type]
  (println "Run players " (keys g))
  (doseq [p (-> g ::c/players)]
    (println "Running player")
    (async/thread (while true (run-player p bid-func-type)))))

(defn play-game [game]
  (state-listener game)
  (c/init-game game)
  (echo-game @game)
  (async/thread (c/run-game game)))

(def game (atom {}))

#_(play-game game)

#_(go (echo-game game))

(def first-player (-> @game ::c/players first))

#_(c/reply-for-redeal! first-player false)

;; perform bid for all
#_(let [players (-> @game ::c/players)]
    (for [p players]
      (c/perform-bid! p :pass)))

;; choose trump now
#_(go (c/choose-trump! game))

#_(c/player-choose-trump! first-player :diamond)

#_(-> @game ::c/game-state)

#_(-> @game ::c/last-bidder)

#_(-> @game ::c/trump)

#_(c/get-player-by-name "player-0" @game)
