(ns twenty-eight.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

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

(s/def ::rank rank?)
(s/def ::suit suit?)
(s/def ::points #{3 2 1 0})
(s/def ::card (s/keys :req [::rank ::suit ::points]))
(s/def ::hand (s/* ::card))
(s/def ::deck (s/* ::card))

(s/def ::name string?)
(s/def ::score int?)
(s/def ::player (s/keys :req [::name ::score ::hand]))

(s/def ::players (s/* ::player))
(s/def ::game (s/keys :req [::players ::deck]))

(gen/generate (s/gen ::player))


(def players (for [p (range 0 4)]
               {::name (str "player-" p)
                ::score 0
                ::hand []}))

(defn deal-cards [{:keys [::players ::deck]}]
  {::players (map (fn [{:keys [::name ::score ::hand] :as p} card]
                    (update p ::hand conj card))
                  players
                  deck)
   ::deck (nthrest deck (count players))})

(defn no-points-card? [deck]
  (not-any? (fn [{:keys [::points]}]
              (> points 0)) deck))

(defn initial-draw []
  (reduce (fn [g i]
            (deal-cards g))
          {::players players
           ::deck (shuffle deck)} (range 0 4)))

(defn redeal-possible? [{:keys [::players]}]
  (-> players first ::hand no-points-card?))
