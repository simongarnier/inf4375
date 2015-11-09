(ns inf4375.model.user
  (:gen-class)
  (:require [inf4375.model-util :as util]
            [inf4375.model.tweet :as tweet]))

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
    (map (fn [tweet-id] (tweet/fetch tweet-id))
         (concat (get @user-tweets user-id) (get @user-retweets user-id)))))

(defn tweet-as!
  "tweet the given message as a user."
  ([user-id message]
   {:pre [(not (nil? (fetch user-id)))]}
   (let [tweet-id (tweet/create! message)]
     (associate! user-tweets user-id tweet-id)
     tweet-id))
  ([user-id message id-other-tweet]
   {:pre [(not (nil? (fetch user-id)))
          (not (nil? (tweet/fetch id-other-tweet)))]}
   (let [tweet-id (tweet/create! message id-other-tweet)]
     (associate! user-retweets user-id tweet-id)
     tweet-id)))

(defn undo-retweet! [user-id, retweet-id]
  (let [retweet-list (get @user-retweets user-id)]
    (if (contains? retweet-list retweet-id)
      (let []
        (dissociate! user-retweets user-id retweet-id)
        (tweet/del! retweet-id)
        retweet-id)
      nil)))

(defn tweets [user-id]
  (map (fn [tweet-id] (tweet/fetch tweet-id))(get @user-tweets user-id)))

(defn tweet [user-id tweet-id]
  (tweet/fetch (first (filter #(= % tweet-id) (get @user-tweets user-id)))))

(defn retweets [user-id]
  (map (fn [retweet-id] (tweet/fetch retweet-id)) (get @user-retweets user-id)))