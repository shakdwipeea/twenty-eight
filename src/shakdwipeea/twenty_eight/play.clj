(ns shakdwipeea.twenty-eight.play
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as test]
            [taoensso.timbre :as timbre]
            [shakdwipeea.twenty-eight.core :as c]
            [snow.async :as a]
            [defn-spec.core :as ds]
            [clojure.string :as string]
            [clojure.core.async :refer [go >!! <!! <! >!] :as async]))

(def sz 14)

(timbre/refer-timbre)


(defn receive-message [{mult ::game->player-mult}]
  (let [game-dup-chan (async/chan sz)]
    (async/tap mult game-dup-chan)
    (<!! game-dup-chan)))


(defn conform-game-msg! [player]
  (info "waiting for msg on " (-> player ::c/name))
  (s/conform ::c/game->player-msg (receive-message player)))


#_(-> (s/conform ::c/game->player-msg [:bid 17]))


(defn handle-msg! [player tag f]
  (let [[t value] (conform-game-msg! player)]
    (info "received for " (-> player ::c/name) value)
    (if (= t tag)
      (f value)
      (error "invalid handler " tag value))))


(defn player-msg [{ch ::c/player-chan name ::c/name} msg]
  (info name "Player is replying with " msg)
  (>!! ch msg))


(def find-first (comp first filter))


#_(find-first #{:abc}  #{:abc :def})


(defn choose-card [suit-led hand]
  (info "Choosing card suit-led: " suit-led " hand suits " (->> hand
                                                              (map ::c/suit)
                                                              (string/join ",")))
  (or (first (filter (partial c/legal-card? suit-led hand) hand))
     (rand-nth hand)))


(defn play-card [{hand ::c/hand :as p}]
  (handle-msg! p
               :play-trick
               (fn [_ {suit-led ::c/suit-led}]
                 (player-msg p (choose-card suit-led hand)))))



;; execute these statements to play the game .. for emacs C-c C-c

;; defining a container to store the game state
(def game (atom {}))

;; Start playing the game

#_(empty? (select-keys @game [::c/trump ::c/game-stage ::c/suit-led])
          )

(def n "player-3")

(defn make-mult 
  [players]
  (map (fn [{ch ::c/game->player-chan :as p}]
         (assoc p ::game->player-mult (async/mult ch)))  players))


(defn subscribe-game-state
  "returns a chan which will info about game stage changes"
  [{ch ::c/game-chan}]
  (a/subscribe (async/mult ch) ::control ::state-change))



#_(<!! hand-update-chan)

;; (def first-player (-> @game ::c/players first))

;; (def chan (async/chan 2 ))

;; (-> first-player ::c/game->player-chan (>!! [:new-hand [(-> @game ::c/deck first)]]))

;; (def pub (async/pub (-> first-player ::c/game->player-chan) #(->> % (s/conform ::c/game->player-msg) first)))

;; (async/sub pub :new-hand chan)

;; (<!! chan)


(def game (atom {}))

(defn play-card! [player suit-led]
  (->> player
     ::c/hand
     (choose-card suit-led)
     (player-msg player)))


(defn player-actions [player msg]
  (info "Trying to conform now")
  (let [[tag _] (s/conform ::c/game->player-msg msg)]
    (info "Msg is " msg " tag is " tag)
    (case tag
      
      :redeal-msg (do (c/reply-for-redeal! player false)
                      player)
      
      :bid-msg (do (c/perform-bid! player 16)
                   player)
      
      :new-hand (assoc player ::c/hand (second msg))
      
      :choose-trump (do (c/player-choose-trump! player :diamond)
                        player)
      
      :play-trick (do (play-card! player (-> msg second ::c/suit-led))
                      player)
      
      (do (error "unknown msg from game " msg ".. waiting for next msg")
          player))))


#_(partial reduce player-actions player)


(defn start-player [{mult ::game->player-mult name ::c/name :as player}]
  (let [game-dup-chan (async/chan sz )]
    (async/tap mult game-dup-chan)
    (info "Starting player " name)
    (async/reduce player-actions player game-dup-chan)))


(defn start-players [game]
  (doseq [p (-> game ::c/players)]
    (async/thread (start-player p))))


#_(defn complete-bidding [game]
    (let [first-player (-> game ::c/players first)]
      (loop [[tag value] (conform-game-msg! first-player)]
        (info "Tag is " tag)
        (case tag
          :redeal-msg (do (c/reply-for-redeal! first-player false)
                          (recur (conform-game-msg! first-player)))
          :bid-msg (let [ps (->> game ::c/players)]
                     (c/perform-bid! (first ps) 16)
                     (perform-bid-for-all! (rest ps)))
          (error "unknown msg from game " value)))))

#_(->> @game ::c/players (map (comp count ::c/hand)))

#_(do (reset! game (c/initial-draw))

      (let [p' (-> @game ::c/players get-pubs)]
        (swap! game assoc ::c/players p'))

      ;; update hand continuosly
      (async/thread (hand-update-subscriber game)) 

      ;; run the game
      (def th (async/thread (def g' (c/run-game @game))))

      ;; first player
      (def first-player (-> game ::c/players first))
      
      (def  (update-hands game))
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
      #_(let [[[_ name] _] (->> game
                                ::c/players
                                (map ::c/game->player-chanxs)
                                (mapv (fn [src]
                                        (let [dest (async/mult src)]
                                          (async/tap dest src))))
                                async/alts!!)]
          (info "player is" name))
      (handle-msg! (c/get-player-by-name game n)
                   :choose-trump
                   (fn [_]
                     (info "got it")
                     (c/player-choose-trump! (c/get-player-by-name game n) :diamond)))

      (def game (update-hands game))
      
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

;; let's do some pub sub

(def ch (async/chan 10))

(def pub (async/pub ch :hello))

(def out-ch (async/chan 10))

(async/sub pub :first out-ch)

(>!! ch {:hello :first
         :not :any})

(<!! out-ch)

;; Perfect! this was using map key as topic-fn

;; Now let's try conforming to spec

;; first we define a simple spec

(def element-chan (async/chan 10))

(def woah (s/def ::good-element (s/or :water #{:water}
                                      :air #{:air}
                                      :number int?)))

;; conform gives us [tag value]
(s/conform ::good-element :water)

(defn tag-topic [val]
  (println "var is " val)
  (first (s/conform ::good-element val)))

(tag-topic :water)

(def pub2 (async/pub element-chan tag-topic))

(def air-chan (async/chan 10 (map inc)))

(async/sub pub2 :air air-chan)

(async/sub pub2 :number air-chan)

(>!! element-chan 19)

(<!! air-chan)

#_(<!! element-chan)

;; Great!

;; (def c (a/subscribe element-chan :type :water))

;; (>!! element-chan {:type :water
;;                    :element :air})

;; (<!! c)

;; (def go-away (a/subscribe-val element-chan :type :world))
