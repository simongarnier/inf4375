(ns inf4375.model.user
  (:gen-class)
  (:require [inf4375.model-util :as util]
            [inf4375.model.tweet :as tweet]
            [inf4375.model.retweet :as retweet]))

(def users
  (atom {}))

(def user-tweets
  (atom {}))

(def user-retweets
  (atom {}))

(defn- associate! [rel user-id tweet-id]
  (swap! rel conj {user-id (set (concat
                                     (get @rel user-id)
                                     (list tweet-id)))}))

(defn- dissociate! [rel user-id tweet-id]
  (let [tweet-list (get @rel user-id)
        tweet-list-removed (remove (fn [t] (= t tweet-id)) tweet-list)]
    (swap! rel conj {user-id tweet-list-removed})))

(defn create! [handle]
  (let [id (util/gen-id)]
    (swap! users conj {id {:id id :handle handle}})
    (swap! user-tweets conj {id (set '())})
    id))

(defn all []
  (vals @users))

(defn fetch [user-id]
  (get @users user-id))

(defn feed [user-id]
  (sort-by (fn [tweet] (* (:timestamp tweet) -1))
           (concat
             (map (fn [tweet-id] (tweet/fetch tweet-id)) (get @user-tweets user-id))
             (map
               (fn [retweet-id] (let [retweet (retweet/fetch retweet-id)]
                                  (conj retweet {:tweet (tweet/fetch (:tweet-id retweet))})))
               (get @user-retweets user-id)))))

(defn tweet-as!
  "tweet the given message as a user."
  [user-id message]
  {:pre [(not (nil? (fetch user-id)))]}
  (let [tweet-id (tweet/create! message user-id)]
    (associate! user-tweets user-id tweet-id)
    tweet-id))

(defn retweet-as!
  "retweet the given tweet as a user."
  [user-id tweet-id]
  {:pre [(not (nil? (fetch user-id)))
         (not (nil? (tweet/fetch tweet-id)))
         (not (contains? (get @user-tweets user-id) tweet-id))]}
  (let [retweet-id (retweet/create! tweet-id user-id)]
    (associate! user-retweets user-id retweet-id)
    retweet-id))

(defn undo-retweet! [user-id, tweet-id]
  "undo all retweet retweeting the specified tweet for the user-id
   and return the list of undone retweet"
  (let [retweet-list (filter
                       (fn [retweet] (= (:tweet-id retweet) tweet-id))
                       (map #(retweet/fetch %) (get @user-retweets user-id)))]
    (map (fn [retweet]
           (let [id (:id retweet)]
             (dissociate! user-retweets user-id id)
             (retweet/del! id)
             id)) retweet-list)))

(defn tweets [user-id]
  (map (fn [tweet-id] (tweet/fetch tweet-id))(get @user-tweets user-id)))

(defn tweet [user-id tweet-id]
  (tweet/fetch (first (filter #(= % tweet-id) (get @user-tweets user-id)))))

(defn retweets [user-id]
  (map (fn [retweet-id] (retweet/fetch retweet-id)) (get @user-retweets user-id)))

(defn retweet [user-id retweet-id]
  (tweet/fetch (first (filter #(= % retweet-id) (get @user-retweets user-id)))))

