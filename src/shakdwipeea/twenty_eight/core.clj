(ns shakdwipeea.twenty-eight.core
  (:require [clojure.spec.alpha :as s]
            [clojure.core.async :refer [chan <!! >!! go <! >!] :as a]
            [snow.util :as u]
            [snow.async :as async]
            [taoensso.timbre :as timbre]
            [special.core :refer [condition manage]]
            [defn-spec.core :as ds]
            [clojure.core.async.impl.protocols :as ap]
            [taoensso.timbre.appenders.core :refer [println-appender]]
            [failjure.core :as f]
            [clojure.spec.gen.alpha :as gen])
  (:import [clojure.core.async.impl.channels ManyToManyChannel]))

;; logging setup
(timbre/refer-timbre)

(timbre/set-config!   {:level :debug  ; e/o #{:trace :debug :info :warn :error :fatal :report}

                       ;; Control log filtering by namespaces/patterns. Useful for turning off
                       ;; logging in noisy libraries, etc.:
                       :ns-whitelist  [] #_["my-app.foo-ns"]
                       :ns-blacklist  [] #_["taoensso.*"]

                       :middleware [] ; (fns [data]) -> ?data, applied left->right

                       :appenders
                       {;; The standard println appender:
                        :println (println-appender {:stream :auto})

                        }})

(info "Hi!!")
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

(def find-first (comp first filter))

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


(defn is-playable? [{:keys [::rank]}]
  (or (keyword? rank) (> rank 6)))


(def deck (filter is-playable? (for [suit suit? rank rank?]
                                 {::rank rank
                                  ::suit suit
                                  ::points (get points rank 0)})))

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

(s/def ::trump-suit ::suit)

;; if trump is exposed then we can directly contain the trump suit
;; o/w we store a reference id to the trump-suit-store atom defined above
(defmulti trump-exposed ::trump-exposed)
(defmethod trump-exposed true [_]
  (s/keys :req [::trump-exposed ::trump-suit]))
(defmethod trump-exposed false [_]
  (s/keys :req [::trump-exposed]))

(s/def ::trump (s/multi-spec trump-exposed ::trump-exposed))

;; possible msgs sent from game
(s/def ::game->player-msg (s/or :commands #{:invalid-card}
                                :redeal-msg #{:want-redeal}
                                :bid-msg (s/tuple #{:bid} ::bid-value)
                                :choose-trump (s/tuple #{:choose-trump} ::name)
                                :new-hand (s/tuple #{:new-hand} ::hand)
                                :trump-exposed ::trump
                                :play-trick (s/tuple #{:play-trick}
                                                     (s/keys :req [::trump]
                                                             :opt [::game-stage ::suit-led]))))


;; possible replies sent by the player
(s/def ::player-reply-msg (s/or :redeal-reply boolean?
                                :bid-reply (s/keys :req [::bid-value])
                                :trump ::suit
                                :trick ::card
                                :expose-trump #{:expose-trump}
                                :trump-trick (s/tuple #{:trump} ::card)))


(s/def ::control #{::state-change})

(defmulti game-control ::control)
(defmethod game-control ::state-change [_] (s/keys :req [::control ::game-state]))

(s/def ::game-control (s/multi-spec game-control ::control))

;; channel for game to communicate with a particular player
(s/def ::game->player-chan chan?)

;; channel for player to respond on
(s/def ::player-chan chan?)

;; channel for game wide communication
(s/def ::game-chan chan?)

(s/def ::points-collected int?)

(s/def ::player (s/keys :req [::name ::score ::hand ::game->player-chan ::player-chan]
                        :opt [::points-collected]))

(s/def ::players (s/* ::player))

;; current board on which cards are being played
(s/def ::game-stage (s/* (s/keys :req [::name ::card])))

;; suit currently being led
(s/def ::suit-led ::suit)

(s/def ::game-state #{::started ::asking-for-redeal ::bidding ::choose-trump})

;; bid starts from 16 and game has a max of 28 points
;; or you can just pass
(s/def ::bid-value (s/or :bid (s/and int?
                                     #(<= 16 % 28))
                         :pass #{:pass}))

;; to track who bid last we store the name for now
(s/def ::last-bidder ::name)

(s/def ::new-bid (s/keys :req [::bid-value]
                         :opt [::last-bidder]))

(s/def ::game  (s/keys :req [::players ::deck ::game-state ::game-chan]
                       :opt [::bid-value ::last-bidder ::trump ::game-stage ::suit-led]))

;;;;;;;;;;;;;
;; Players ;;
;;;;;;;;;;;;;

(defn get-player-by-name [game name]
  (->> game
     ::players
     (filter #(= (-> % ::name) name))
     first))


(defn update-player [players player-name update-fn]
  (map (fn [p]
         (cond-> p
           (= (::name p) player-name) update-fn)) players))

#_(update-player (-> g ::players) "player-2" #(update % ::name (fn [_] "A")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game state mgmt functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gen-players []
  (for [p (range 0 4)]
    {::name (str "player-" p)
     ::score 0
     ::player-chan (chan-of ::player-reply-msg 14)
     ::game->player-chan (chan-of ::game->player-msg 14)
     ::hand []}))


(defn change-game-state [{ :keys [::game-chan ::game-state] :as game} new-state]
  (info "Changing game state to " new-state)
  (go (>! game-chan {::control ::state-change
                     ::game-state game-state}))
  (assoc game ::game-state new-state))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game transition functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn notify-new-hand
  "Notify the new hand .. this would generally be when cards is being dealt/redealt"
  [{:keys [::game->player-chan ::hand ::name] :as player}]
  (info "Notifying player " name " for new hand " (count hand))
  (>!! game->player-chan [:new-hand hand]))


(defn update-hand
  "update hand of a player"
  [player hand]
  (let [p' (assoc player ::hand hand)]
    (notify-new-hand p')
    p'))


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

(defn distribute-cards [game num-cards]
  (reduce (fn [g i]
            (deal-cards g)) 
          game (range 0 num-cards)))

(defn distribute-and-notify! [game num-cards]
  (let [g' (distribute-cards game num-cards)]
    (doseq [p (::players g')]
      (notify-new-hand p))
    g'))

(ds/defn-spec initial-draw
  {::s/ret ::game}
  []
  {::players (gen-players)
   ::game-state ::started
   ::bid-value 16
   ::game-chan (chan-of ::game-control 4)
   ::deck (shuffle deck)})

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
  (let [{:keys [::game->player-chan ::player-chan]} (first players)]
    (>!! game->player-chan :want-redeal)
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
  {::s/args (s/cat :bid ::new-bid
                   :player ::player)
   :s/ret ::new-bid}
  [{cur-bid ::bid-value} {:keys [::game->player-chan ::player-chan ::name]}]
  (info "will ask for bid to " name)
  (>!! game->player-chan [:bid cur-bid])
  (info "Will now wait for bids to come")
  (let [msg (<!! player-chan)]
    (info "Received " msg " from player " name)    
    (assoc (cond-> msg
             (= :pass (::bid-value msg)) (assoc ::bid-value cur-bid))
           ::last-bidder name)))

(defn bid-valid? [bid {:keys [::bid-value]}]
  (< bid-value bid))

(ds/defn-spec perform-bid!
  {::s/args (s/cat :player ::player
                   :bid-value ::bid-value)}
  [{:keys [::player-chan ::name]} bid-value]
  (info "Performing bid " bid-value " for player " name)
  (>!! player-chan {::bid-value bid-value}))

(defn update-bid
  {::s/args (s/cat :new-bid ::new-bid
                   :game ::game)}
  [game {:keys [::bid-value ::last-bidder] :as new-bid}] 
  (cond-> game
    (not= bid-value :pass) (assoc ::bid-value bid-value
                               ::last-bidder last-bidder)))


(ds/defn-spec default-last-bidder
  "if no one is last bidder then player 0 is"
  {::s/args (s/cat :game ::game)
   ::s/ret ::game}
  [{:keys [::last-bidder] :as g}]
  (cond-> g
    (nil? last-bidder) (assoc ::last-bidder (-> g ::players first ::name))))

(defn start-bidding [{players ::players cur-bid ::bid-value :as game}] 
  (let [g'  (->> players
               (reduce receive-bid {::bid-value cur-bid})
               (update-bid game))
        new-bid (-> g' ::bid-value)]
    (info "new bid value for this round " new-bid)
    (if (or (>= new-bid 28)
           (= new-bid cur-bid))
      (default-last-bidder g')
      (recur g'))))


#_(assert (let [suit :diamond]
            (= (get-trump-suit (add-trump-suit suit))
               suit)))

;;;;;;;;;;;;;;;;;;
;; choose trump ;;
;;;;;;;;;;;;;;;;;;

(ds/defn-spec ask-for-trump
  {::s/args (s/cat :player ::player)
   ::s/ret ::trump}
  [{:keys [::player-chan ::game->player-chan ::name]}]
  (info "Asking " name " for trump card.")
  (>!! game->player-chan [:choose-trump name])
  (info "Now waiting for response " name)
  {::trump-exposed false
   ::trump-suit (<!! player-chan)})


(ds/defn-spec choose-trump
  {::s/args (s/cat :game ::game)
   ::s/ret ::game}
  [game]
  (->> game ::last-bidder (get-player-by-name game) ask-for-trump (assoc game ::trump)))


(ds/defn-spec player-choose-trump!
  "client fn to choose trump"
  {::s/args (s/cat :player ::player
                   :suit ::suit)}
  [{player-chan ::player-chan} suit]
  (>!! player-chan suit))


(defn deal-hand! [game]
  (reset! game (initial-draw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Play the actual game ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn notify-player [{ch ::game->player-chan name ::name} msg]
  (info "Going to notify player " name "with msg " msg)
  (>!! ch msg))

(defn notify-invalid-play [player]
  (error "Invalid card played by player " (-> player ::name))
  (notify-player player :invalid-card))


(defn notify-to-play-trick [player game]
  (notify-player player
                 [:play-trick (select-keys game
                                           [::trump ::game-stage ::suit-led])]))


(defn receive-message [{ch ::game->player-chan}] 
  (<!! ch))


(defn get-msg-from-player [{ch ::player-chan}]
  (<!! ch))


(defn card-in-hand
  "if card is present in hand then get it"
  [card hand]
  (if (some #(= % card) hand) card nil))


(defn legal-card?
  "if there is a suit-led and the player has a card of that suit
   and still plays a card from another suit ..that's illegal"
  [suit-led hand {suit ::suit}]
  (or (= suit suit-led)
     (not-any? #(-> % ::suit (= suit-led)) hand)))


(defn valid-card
  "if card is valid return it"
  [card {hand ::hand} {suit-led ::suit-led}]
  (info "checking card legality " card suit-led)
  (and (legal-card? suit-led hand card)
     (card-in-hand card hand)))


;; (legal-card? :heart (::hand (get-player-by-name g "player-2")) (first gs))

;; (valid-card (::card (first gs)) (get-player-by-name g "player-2") {::suit-led :heart})

(declare play-trick)

(defn notify-trump-exposed [{:keys [::players ::trump]}]
  (doseq [p players]
    (notify-player p trump)))


(defn play-trick [game player f]
  (notify-to-play-trick player game)
  (info (-> player ::name) " Player notified ")
  (let [[tag val] (->> player
                     get-msg-from-player
                     (s/conform ::player-reply-msg))]
    (info "tag is " tag)
    (case tag
      :expose-trump (let [g' (assoc-in game [::trump ::trump-exposed] true)]
                      (notify-trump-exposed g')
                      (recur g' player f))
      
      :trick (if (valid-card val player game)
               (f val)
               (do (notify-invalid-play player)
                   (recur game player f)))

      ;; todo add notify-error for player
      (notify-invalid-play player))))


(defn play-first-trick [game {:keys [::name] :as player}]
  (play-trick game player (fn [card]
                            (assoc game
                                   ::game-stage [{::name name
                                                  ::card card}]
                                   ::suit-led (-> card ::suit)))))


(defn play-normal-trick [game {:keys [::name] :as player}]
  (play-trick game player (fn [card]
                            (update game ::game-stage conj {::name name
                                                            ::card card}))))


(defn collect-trick [{:keys [::suit-led] :as game} player]
  (if (nil? suit-led)
    (play-first-trick game player)
    (play-normal-trick game player)))


(def g {})

(defn get-max-point-card [game-stage]
  (doseq [c (map ::card game-stage)]
    (info "final card " c))
  (->> game-stage
     (sort-by (comp ::points ::card) >)
     first))

(defn find-winner-on-stage [game-stage suit-led {:keys [::trump-exposed ::trump-suit] :as t}]
  (if-let [trumps (when trump-exposed
                    (seq (filter #(= (-> % ::card ::suit)
                                     trump-suit) game-stage)))]
    (get-max-point-card trumps)
    (do (info "no trumps suit-led " suit-led (get-max-point-card game-stage))
        (def gs game-stage)
        (->> game-stage
           (filter (fn [{{s ::suit} ::card}]
                     (= s suit-led)))
           get-max-point-card))))


(defn find-play-winner [{:keys [::game-stage ::suit-led ::trump] :as g}]
  (assoc g
         ::round-winner-name (-> game-stage
                                (find-winner-on-stage suit-led trump)
                                ::name)
         ::points-collected (reduce (fn [points {{card-points ::points} ::card}]
                                      (+ points card-points)) 0 game-stage)))

#_(find-play-winner g)

(def addf (fnil + 0))

#_(addf nil 20 20)

(defn update-player-points [players points-collected winning-player-name]
  (println "Points collected by player" points-collected winning-player-name)
  (update-player players winning-player-name (fn [p]
                                               (update p
                                                       ::points-collected
                                                       #(addf % points-collected)))))


#_(update-player-points (-> g ::players) 20 "player-1")


;; todo represent game-functions by a special macro

#_(::hand (get-player-by-name g "player-2"))

(defmacro game-func
  "this macro is a bit of a gamble .. it allows us to control the value of
   the symbol game and make sure it is always sane..
   let's see how this turns out"
  [fn-name & body]
  `(ds/defn-spec ~(symbol fn-name)
     {::s/args (s/cat :game ::game)
      ::s/ret ::game}
     [~'game]
     ~@body))


(def game {})

#_(->> g ::players (map ::points-collected) (filter some?) (apply +))

(game-func play-round
           (->> game
              ::players
              (reduce collect-trick game)))


(defn arrange-players-for-turn
  "arrange players so that lead-player-name is on top"
  [players lead-player-name]
  (->>  players
      cycle
      (drop-while #(not= (::name %) lead-player-name))
      (take (count players))))

#_(arrange-players-for-turn (-> game ::players) "player-3")

(defn remove-card [hand card]
  (filter #(not= card %) hand))


(defn remove-played-cards [players game-stage]
  (reduce (fn [players {:keys [::name ::card]}]
            (update-player players name (fn [{h ::hand :as player}]
                                          (->> card
                                             (remove-card h)
                                             (update-hand player)))))
          players
          game-stage))


#_(remove-played-cards (-> g ::players) gs)

(defn update-player-for-rounds [{:keys [::points-collected ::round-winner-name ::game-stage]
                                 :as game}]
  (update game ::players #(-> %
                             (update-player-points points-collected round-winner-name)
                             (arrange-players-for-turn round-winner-name)
                             (remove-played-cards game-stage))))


(defn clear-round-state [game]
  (dissoc (assoc game ::game-stage '()) ::suit-led))


(defn update-game-for-rounds [game]
  (-> game
     play-round
     find-play-winner
     update-player-for-rounds
     clear-round-state))


#_(update-player-for-rounds g)

(def game g)

(game-func round-controller
           (let [num-rounds (-> game ::players first ::hand)]
             (reduce (fn [g _] (update-game-for-rounds g)) game num-rounds)))

#_(-> g ::game-stage)

(game-func play-game
           (do (def g (round-controller game))
               (println "Round complete")
               g))


(game-func ask-and-redeal
           (let [game (change-game-state game ::asking-for-redeal)]
             (if (ask-for-redeal? game)
               (do (info "Dealing hand again")
                   (distribute-and-notify! game 4))
               game)))


(game-func check-for-redeal
           (do (info "Checking if redeal is possible")
               (cond-> game
                 (redeal-possible? game) ask-and-redeal)))


(defn print-game-key [game key tag]
  (info tag (-> game key) (-> game ::deck count))
  game)


(defn run-game
  "run a game.. a game can be constructed from initial-draw"
  [game]
  (info "Starting to run game")
  (-> game
     (distribute-and-notify! 4)
     check-for-redeal
     (change-game-state ::bidding)
     start-bidding
     (print-game-key ::bid-value "bid value:")
     (change-game-state ::choose-trump)
     choose-trump
     (print-game-key ::trump "Trump chosen is ")
     (distribute-and-notify! 4)
     play-game))


(defn init-game [game]
  (deal-hand! game))
