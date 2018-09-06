(ns shakdwipeea.twenty-eight.core
  (:require [clojure.spec.alpha :as s]
            [clojure.core.async :refer [chan <!! >!! go <!] :as a]
            [defn-spec.core :as ds]
            [clojure.core.async.impl.protocols :as ap] 
            [clojure.spec.gen.alpha :as gen])
  (:import [clojure.core.async.impl.channels ManyToManyChannel]))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; core.async with spec ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro chan-of [spec & chan-args]
  `(let [ch# (a/chan ~@chan-args)]
     (reify
       ap/ReadPort
       (take! [_ fn1-handler#]
         (ap/take! ch# fn1-handler#))
       ap/WritePort
       (put! [_ val# fn1-handler#]
         (if (s/valid? ~spec val#)
           (ap/put! ch# val# fn1-handler#)
           (throw (ex-info (str "Spec failed to validate: "
                                (s/explain-str ~spec val#))
                           {:validation-err (s/explain-data ~spec val#)
                            :val val#})))))))

(defn pipe-trans
  [ci xf]
  (let [co (chan 1 xf)]
    (a/pipe ci co)
    co))

;; (def c1 (chan))
;; (def c2 (pipe-trans c1 (filter even?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Domain data generator functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def suit? #{:club :diamond :heart :spade})

(def rank? (into #{:jack :queen :king :ace} (range 0 11)))

(def points {:jack 3
             9 2
             :ace 1
             10 1})

(def all-cards (for [suit suit? rank rank?]
                 {::rank rank
                  ::suit suit
                  ::points (get points rank 0)}))

(def playable-cards (filter (fn [{:keys [::rank]}]
                              (or (keyword? rank) (> rank 7)))  all-cards))

(def deck (for [suit suit? rank rank?]
            {::rank rank
             ::suit suit
             ::points (get points rank 0)}))

;;;;;;;;;;;
;; Specs ;;
;;;;;;;;;;;
(def chan? #(and (satisfies? ap/ReadPort %)
               (satisfies? ap/WritePort %)))

(s/def ::rank rank?)
(s/def ::suit suit?)
(s/def ::points #{3 2 1 0})
(s/def ::card (s/keys :req [::rank ::suit ::points]))
(s/def ::hand (s/* ::card))
(s/def ::deck (s/* ::card))

(s/def ::name string?)
(s/def ::score int?)

;; possible msgs sent from game
(s/def ::game-ctrl-msg #{:want-redeal :bid})

;; possible replies sent by the player
(s/def ::player-reply-msg (s/or :redeal-reply boolean?
                                :bid-reply (s/keys :req [::bid-value])))

;; Channels for communication
(s/def ::game-chan chan?)
(s/def ::player-chan chan?)

(s/def ::player (s/keys :req [::name ::score ::hand ::game-chan ::player-chan]))

(s/def ::players (s/* ::player))
(s/def ::game-state #{::started ::asking-for-redeal ::bidding})

;; bid starts from 16 and game has a max of 28 points
(s/def ::bid-value (s/and int?
                          #(<= 16 % 28)))

(s/def ::game (s/keys :req [::players ::deck ::game-state]
                      :opt [::bid-value]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game state mgmt functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def players (for [p (range 0 4)]
               {::name (str "player-" p)
                ::score 0
                ::player-chan (chan-of ::player-reply-msg 1)
                ::game-chan (chan-of ::game-ctrl-msg 1)
                ::hand []}))

(def *game* (atom {}))

(ds/defn-spec set-game!
  {::s/args (s/cat :game ::game)}
  [game]
  (reset! *game* game))

(defn swap-game!
  [f]
  (swap! *game* f))

(ds/defn-spec cur-game {:s/ret ::game} [] @*game*)

(ds/defn-spec change-game-state!
  {::s/args (s/cat :new-state ::game-state)
   :s/ret ::game}
  [new-state]
  (swap-game! #(assoc % ::game-state new-state)))

(def game-updates (chan-of ::game 10))

(ds/defn-spec <notify-game-change
  {:s/ret chan?}
  []
  (let [ch (chan 10)]
    (add-watch *game* :watcher (fn [key atom old-state new-state]
                                 (println "State changed")
                                 (>!! ch new-state)))
    ch))

(ds/defn-spec notify-game-state-change
  {:s/ret chan?}
  []
  (pipe-trans (<notify-game-change) (map ::game-state)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game transition functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ds/defn-spec deal-cards
  {::s/args (s/cat :game ::game)
   ::s/ret ::game}
  [{:keys [::players ::deck] :as g}]
  (assoc g
         ::players (map (fn [{:keys [::name ::score ::hand] :as p} card]
                          (update p ::hand conj card))
                        players
                        deck)         
         ::deck (nthrest deck (count players))))

(ds/defn-spec initial-draw
  {::s/ret ::game}
  []
  (reduce (fn [g i]
            (deal-cards g))
          {::players players
           ::game-state ::started
           ::deck (shuffle deck)} (range 0 4)))

;;;;;;;;;;;;;;;;;;;;
;; initial redeal ;;
;;;;;;;;;;;;;;;;;;;;

(defn no-points-card? [deck]
  (not-any? (fn [{:keys [::points]}]
              (> points 0)) deck))

(ds/defn-spec redeal-possible?
  {::s/args (s/cat :game ::game)
   ::s/ret boolean?}
  [{:keys [::players]}]
  (-> players first ::hand no-points-card?))

(ds/defn-spec ask-for-redeal?
  {::s/args (s/cat :game ::game)
   ::s/ret boolean?}
  [{:keys [::players]}]
  (let [{:keys [::game-chan ::player-chan]} (first players)]
    (>!! game-chan :want-redeal)
    (<!! player-chan)))

(ds/defn-spec reply-for-redeal
  {::s/args (s/cat :player ::players
                   :action boolean?)}
  [{player-chan ::player-chan} action]
  (>!! player-chan action))

;;;;;;;;;;;;;;;;;;;;;
;; bidding process ;;
;;;;;;;;;;;;;;;;;;;;;

(ds/defn-spec receive-bid
  "Ask for bid to player and receive the value"
  {::s/args (s/cat :player ::player)
   :s/ret ::bid-value}
  [{:keys [::game-chan ::player-chan]}]
  (>!! game-chan :bid)
  (-> player-chan <!! ::bid-value))

(ds/defn-spec perform-bid
  "When asked about bid it will perform-bid with given value"
  {::s/args (s/cat :bid-value ::bid-value
                   :player ::player)}
  [bid-value {:keys [::game-chan ::player-chan]}]
  (case (<!! game-chan)
    :bid (>!! player-chan bid-value)))

(ds/defn-spec start-bidding
  {::s/args (s/cat :game ::game)
   ::s/ret ::game} 
  [{players ::players :as g}]
  (->> players
     (map receive-bid)
     (map #(println "Bid values are " %))
     doall)
  g)

(defn deal-hand []
  (set-game! (initial-draw)))

(ds/defn-spec run-game
  {::s/args (s/cat :intial-game ::game)}
  [initial-game]
  (println "Starting to run game")
  (when (and (redeal-possible? initial-game)
           (do (change-game-state! ::asking-for-redeal)
               (ask-for-redeal? initial-game)))
    (println "Dealing hand")
    (deal-hand))
  (println "Now, let's start bidding")
  (change-game-state! ::bidding)
  (start-bidding (cur-game)))


;; todo encode and verify possible states through spec
#_(-> game ::players first ::action (>!! false))

#_(go (deal-hand))

;; (defn chooser
;;   [[first-choice second-choice & unimportant-choices]]
;;   (println (str "Your first choice is: " first-choice))
;;   (println (str "Your second choice is: " second-choice))
;;   (println (str "We're ignoring the rest of your choices. "
;;                 "Here they are in case you need to cry over them: "
;;                 (clojure.string/join ", " unimportant-choices))))
;; (chooser ["Marmalade", "Handsome Jack", "Pigpen", "Aquaman"])

;; (def test-a (atom 0))

;; (add-watch test-a :test-me (fn [key atom old-state new-state]
;;                              (prn "-- Atom Changed --")
;;                              (prn "key" key)
;;                              (prn "atom" atom)
;;                              (prn "old-state" old-state)
;;                              (prn "new-state" new-state)))

;; (reset! test-a {:abc "world"
;;                 :def "go-now"})

;; (swap! test-a #(assoc % :def "why"))

#_(go (println "as"))
