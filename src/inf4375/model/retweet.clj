(ns inf4375.model.retweet
  (:gen-class)
  (:require [inf4375.model-util :as util]))

(def retweets
  (atom {}))

(defn create! [tweet-id author-id]
  (let [id (util/gen-id)]
    (swap! retweets conj {id {:id id
                              :tweet-id tweet-id
                              :author-id author-id
                              :timestamp (util/stamp)}})
    id))

(defn all []
  (vals @retweets))

(defn fetch [id]
  (get @retweets id))

(defn del! [id]
  (swap! retweets dissoc id))