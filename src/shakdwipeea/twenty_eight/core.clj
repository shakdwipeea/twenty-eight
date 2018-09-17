(ns shakdwipeea.twenty-eight.core
  (:require [clojure.spec.alpha :as s]
            [clojure.core.async :refer [chan <!! >!! go <! >!] :as a]
            [snow.util :as u]
            [special.core :refer [condition manage]]
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
(s/def ::game-ctrl-msg (s/or :redeal-msg #{:want-redeal}
                             :bid-msg (s/tuple #{:bid} ::bid-value)
                             :choose-trump #{:choose-trump}))

;; possible replies sent by the player
(s/def ::player-reply-msg (s/or :redeal-reply boolean?
                                :bid-reply (s/keys :req [::bid-value])
                                :trump ::suit))

;; Channels for communication
(s/def ::game-chan chan?)
(s/def ::player-chan chan?)

(s/def ::player (s/keys :req [::name ::score ::hand ::game-chan ::player-chan]))

(s/def ::players (s/* ::player))
(s/def ::game-state #{::started ::asking-for-redeal ::bidding ::choose-trump})

;; bid starts from 16 and game has a max of 28 points
;; or you can just pass
(s/def ::bid-value (s/or :bid (s/and int?
                                     #(<= 16 % 28))
                         :pass #{:pass}))

;; to track who bid last we store the name for now
(s/def ::last-bidder ::name)

(s/def ::new-bid (s/keys :req [::bid-value ::last-bidder]))

(s/def ::game  (s/keys :req [::players ::deck ::game-state]
                       :opt [::bid-value ::last-bidder ::trump]))

;;;;;;;;;;;;;
;; Players ;;
;;;;;;;;;;;;;

(defn get-player-by-name [name game]
  (->> game
     ::players
     (filter #(= (-> % ::name) name))
     first))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game state mgmt functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gen-players []
  (for [p (range 0 4)]
    {::name (str "player-" p)
     ::score 0
     ::player-chan (chan-of ::player-reply-msg 1)
     ::game-chan (chan-of ::game-ctrl-msg 1)
     ::hand []}))

(defn swap-game!
  [game f]
  (swap! game f))

(ds/defn-spec change-game-state! 
  [game new-state]
  (swap-game! game #(assoc % ::game-state new-state)))

(ds/defn-spec <notify-game-change
  {::s/ret chan?}
  [game]
  (let [ch (chan 10)]
    (add-watch game :watcher (fn [key atom old-state new-state]
                               (println "State changed")
                               (>!! ch new-state)))
    ch))

(ds/defn-spec <notify-game-state-change
  {::s/ret chan?}
  [game]
  (pipe-trans (<notify-game-change game) (map ::game-state)))


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

(defn distribute-cards [num-cards game]
  (reduce (fn [g i]
            (deal-cards g)) 
          game (range 0 num-cards)))

(ds/defn-spec initial-draw
  {::s/ret ::game}
  []
  (distribute-cards 4 {::players (gen-players)
                       ::game-state ::started
                       ::bid-value 16
                       ::deck (shuffle deck)}))

#_(initial-draw)

#_(->> (initial-draw)
       ::players
       (map #(-> % ::hand count)))
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

(ds/defn-spec reply-for-redeal!
  {::s/args (s/cat :player ::players
                   :action boolean?)}
  [{player-chan ::player-chan} action]
  (>!! player-chan action))

;;;;;;;;;;;;;;;;;;;;;
;; bidding process ;;
;;;;;;;;;;;;;;;;;;;;;

(ds/defn-spec receive-bid
  "Ask for bid to player and receive the value"
  {::s/args (s/cat :bid-value ::bid-value
                   :player ::player)
   :s/ret ::new-bid}
  [bid-value {:keys [::game-chan ::player-chan ::name]}]
  (println "will ask for bid ")
  (>!! game-chan [:bid bid-value])
  (println "Will now wait for bids to come")
  (assoc (<!! player-chan) ::last-bidder name))

(defn bid-valid? [bid {:keys [::bid-value]}]
  (< bid-value bid))

(ds/defn-spec perform-bid!
  {::s/args (s/cat :player ::player
                   :bid-value ::bid-value)}
  [{:keys [::player-chan]} bid-value]
  (>!! player-chan {::bid-value bid-value}))

(defn update-bid
  {::s/args (s/cat :new-bid ::new-bid
                   :game ::game)}
  [{:keys [::bid-value ::last-bidder] :as new-bid} game] 
  (cond-> game
    (not= bid-value :pass) (assoc ::bid-value bid-value
                               ::last-bidder last-bidder)))

(ds/defn-spec update-game-with-bid!
  [<game> new-bid]
  (println "new-bid was made for " new-bid)
  (swap-game! <game> (partial update-bid new-bid)))


(ds/defn-spec default-last-bidder
  "if no one is last bidder then player 0 is"
  {::s/args (s/cat :game ::game)
   ::s/ret ::game}
  [{:keys [::last-bidder] :as g}]
  (cond-> g
    (nil? last-bidder) (assoc ::last-bidder (-> g ::players first ::name))))


(defn start-bidding! [<game>] 
  (let [{players ::players cur-bid ::bid-value :as g} @<game>
        g'  (->> players
               (map (partial receive-bid cur-bid))
               (map (partial update-game-with-bid! <game>))
               last)
        new-bid (-> g' ::bid-value)]
    (println "new bid value for this round " new-bid)
    (if (or (>= new-bid 28)
           (= new-bid cur-bid))
      (->> g' default-last-bidder (reset! <game>))
      (recur <game>))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trump cards secret management ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::trump-id string?)

(s/def ::state (s/map-of ::trump-id ::suit))

(def ^:dynamic *trump-store* (atom {}))

(ds/defn-spec add-trump-suit
  {::s/args (s/cat :suit ::suit)
   ::s/ret ::trump-id}
  [suit]
  (let [uuid (u/uuid)]
    (swap! *trump-store* #(assoc % uuid suit))
    uuid))

(ds/defn-spec get-trump-suit
  {::s/args (s/cat :id ::trump-id)
   ::s/ret ::suit}
  [id]
  (get @*trump-store* id))

#_(assert (let [suit :diamond]
            (= (get-trump-suit (add-trump-suit suit))
               suit)))

;;;;;;;;;;;;;;;;;;
;; choose trump ;;
;;;;;;;;;;;;;;;;;;

(ds/defn-spec ask-for-trump
  {::s/args (s/cat :player ::player)
   ::s/ret ::suit}
  [{:keys [::player-chan ::game-chan ::name]}]
  (println "Asking " name " for trump card.")
  (go (>! game-chan :choose-trump))
  (<!! player-chan))


(ds/defn-spec choose-trump
  {::s/args (s/cat :game ::game)
   ::s/ret ::suit}
  [game]
  (-> game ::last-bidder (get-player-by-name game) ask-for-trump))


(defn choose-trump! [<game>]
  (let [suit (choose-trump @<game>)]
    (swap! <game> assoc ::trump suit)))


(ds/defn-spec player-choose-trump!
  "client fn to choose trump"
  {::s/args (s/cat :player ::player
                   :suit ::suit)}
  [{player-chan ::player-chan} suit]
  (>!! player-chan suit))


(defn deal-hand! [game]
  (reset! game (initial-draw)))


(defn run-game [game]
  (println "Starting to run game")
  (dosync (when (and (redeal-possible? @game)
                   (do (change-game-state! game ::asking-for-redeal)
                       (ask-for-redeal? @game)))
            (println "Dealing hand again")
            (deal-hand! game))
          (println "Now, let's start bidding")
          (change-game-state! game ::bidding)
          (start-bidding! game)
          (println "Final bid is " (-> @game ::bid-value))
          (change-game-state! game ::choose-trump)
          (println "Now the last bidder will choose a trump")
          (choose-trump! game)
          (println "Trump chosen is " (-> @game ::trump))
          (swap! game (partial distribute-cards 4))
          (println "okay " (->> @game ::players (map #(-> % ::hand count))))))

(defn init-game [game]
  (deal-hand! game))

;; todo encode and verify possible states through spec
#_(-> game ::players first ::action (>!! false))

#_(go (deal-hand))

#_(go (println "as"))
