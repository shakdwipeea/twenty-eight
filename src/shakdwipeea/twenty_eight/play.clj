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


;; execute these statements to play the game .. for emacs C-c C-c

;; defining a container to store the game state
(def game (atom {}))

;; Start playing the game

#_(play-game game)

;;[optional] execute this if you want to echo if any commands are being received by the players
#_(go (echo-game game))

;; get hold of the first player .. so that we can answer any commands the game asks us
;; eg. Do i want a redeal ?
#_(def first-player (-> @game ::c/players first))

;; If game asks for :want-redeal (monitor game state in repl) ..execute this to say no
#_(c/reply-for-redeal! first-player false)

;; perform bid for all
#_(let [players (-> @game ::c/players)]
    (for [p players]
      (c/perform-bid! p :pass)))

;; choose a trump when asked about it
#_(c/player-choose-trump! first-player :diamond)

;; View game state
#_(-> @game ::c/game-state)

;; View who the last bidder was
#_(-> @game ::c/last-bidder)

;; View the current trump
#_(-> @game ::c/trump)

#_(c/get-player-by-name "player-0" @game)
