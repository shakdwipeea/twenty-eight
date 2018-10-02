(ns shakdwipeea.twenty-eight.play
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as test]
            [taoensso.timbre :as timbre]
            [shakdwipeea.twenty-eight.core :as c]
            [defn-spec.core :as ds]
            [clojure.core.async :refer [go >!! <!! <! >!] :as async]))

(timbre/refer-timbre)

(def bid-funcs {:types '(:bid-higher :simply-pass)
                :bid-higher (fn [b] (cond-> b
                                     (< b 28) (+ 1)))
                :simply-pass (fn [_] :pass)})

(defn conform-game-msg! [player]
  (info "waiting for msg on " (-> player ::c/name))
  (s/conform ::c/game-ctrl-msg (c/receive-message player)))

#_(-> (s/conform ::c/game-ctrl-msg [:bid 17]))

(defn player-msg [{ch ::c/player-chan name ::c/name} msg]
  (info name "Player is replying with " msg)
  (>!! ch msg))

(def find-first (comp first filter))

#_(find-first #{:abc}  #{:abc :def})

(defn choose-card [suit-led hand]
  (info "Choosing card suit-led: " suit-led " hand suits " (doall (map ::c/suit hand)))
  (or (first (filter (partial c/legal-card? suit-led hand) hand))
     (rand-nth hand)))

(defn play-card [{hand ::c/hand :as p}]
  (let [[tag [_ p1]] (conform-game-msg! p)]
    (info "p1 is " p1)
    (case tag
      :play-trick (player-msg p (choose-card (::c/suit-led p1) hand))
      (error "This fn cannot handle this"))))

#_(some #(= % (->> first-player ::c/hand rand-nth)) (::c/hand first-player))

(defn echo-game [game]
  (->> game
     ::c/players
     (map (fn [{game-chan ::c/game-chan name ::c/name}]
            (async/thread (while true (info "Message for game-chan " (<!! game-chan) " for player " name)))))
     doall))


(defn play-game [game]
  (async/thread (c/run-game game)))


;; execute these statements to play the game .. for emacs C-c C-c

;; defining a container to store the game state
(def game (atom {}))

;; Start playing the game

#_(empty? (select-keys @game [::c/trump ::c/game-stage ::c/suit-led])
          )
(defn perform-bid-for-all! [players]
  (info "Count of playr (3) = " (count players))
  (doseq [p players]
    (let [[tag value] (conform-game-msg! p)]
      (info "received for " (-> p ::c/name) value)
      (case tag
        :bid-msg (c/perform-bid! p :pass)
        (error "invalid handler " tag)))))

(def name "player-3")

#_(do (def game (c/initial-draw))
      (def th (async/thread (def g' (c/run-game game))))
      (def first-player (-> game ::c/players first))
      ;; to bidding
      (loop [[tag value] (conform-game-msg! first-player)]
        (info "Tag is " tag)
        (case tag
          :redeal-msg (do (c/reply-for-redeal! first-player false)
                          (recur (conform-game-msg! first-player)))
          :bid-msg (let [ps (->> game ::c/players)]
                     (c/perform-bid! (first ps) 16)
                     (perform-bid-for-all! (rest ps)))
          (error "unknown msg from game " value)))

      ;; choose-trump
      (let [[[_ name] _] (->> game
                            ::c/players
                            (map ::c/game-chan)
                            (mapv (fn [src]
                                    (let [dest (async/mult src)]
                                      (async/tap dest src) src)))
                            async/alts!!)]
        (info "player is" name)
        (let [[tag val] (conform-game-msg! (c/get-player-by-name game name))]
          (info "got it")
          (case tag
            :choose-trump (c/player-choose-trump! (c/get-player-by-name game name) :diamond)
            (error "invalid handler fn " tag val))))
      
      ;; play cards
      (doseq [p (-> game ::c/players)]
        (play-card p))

      (<!! th))



#_(-> g' keys)

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

#_(play-card first-player)

#_(play-card (-> @game ::c/players (nth 2)))
;; View game state
#_(-> @game ::c/game-state)

;; View who the last bidder was
#_(-> @game ::c/last-bidder)

;; View the current trump
#_(-> @game ::c/trump)

#_(c/get-player-by-name "player-0" @game)

;; testing legal card
#_(c/legal-card? :diamond (-> first-player ::c/hand first) (-> first-player ::c/hand))


;; Core async experiment

;; (def hi (async/chan 10))

;; (def chans (for [n (range 4)]
;;              (async/chan 2)))

;; (async/thread  (let [[val p] (async/alts!!  (mapv (fn [ch]
;;                                                     (let [dest (async/mult ch)]
;;                                                       (async/tap dest ch)
;;                                                       )) chans))]
;;                  (info "val is " val)))

;; (map (fn [ch] (>!! ch :stuff)) chans)

;; (<!! (nth chans 2))

;; (>!! hi :hi)

;; (<!! hi)
