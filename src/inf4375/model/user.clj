(ns inf4375.model.user
  (:gen-class)
  (:require [inf4375.model-util :as util]
            [inf4375.model.tweet :as tweet]))

(def users
  (atom {}))

(def user-tweets
  (atom {}))

(def user-retweet
  (atom {}))

(defn create [handle]
  (let [id (util/gen-id)]
    (swap! users conj {id {:id id :handle handle}})
    (swap! user-tweets conj {id (set '())})
    id))

(defn fetch [id]
  (get @users id))

(defn tweet-as [id message]
  (let [tweet-id (tweet/create message)
        tweet-list (get @user-tweets id)]
    (swap! user-tweets conj {id (set (concat tweet-list (list tweet-id)))})
    (tweet/fetch tweet-id)))

(defn tweets [id]
  (map (fn [tweet-id] (tweet/fetch tweet-id) )(get @user-tweets id)))