(ns inf4375.controller
  (:gen-class)
  (:require [inf4375.model.tweet :as tweet]
            [inf4375.model.user :as user]
            [inf4375.model.subscriptions :as sub]

            [inf4375.request :as req]
            [inf4375.response :as res]
            [inf4375.router :as router]

            [clojure.string :as str]
            [clojure.data.json :as json]))

(defn not-found [& args] ;any number of param because we don't care about arity
  (res/generate-response :404 {"message" "ressource introuvable"}))

(defn resolve!
  ([routes method resource]
   (resolve! routes method resource []))
  ([routes method resource injected-params]
    (binding [router/routes routes
              router/unbound not-found]
      (let [resource-list (str/split resource #"/")
            func-and-params (router/route
                             (if (empty? resource-list)
                               [""]
                               resource-list)
                             method)
            function (first func-and-params)
            params (last func-and-params)]
        (apply function (concat params injected-params))))))

(defn get-users
  ([]
   (res/generate-response :200 {"users" (user/all)}))
  ([user-id]
   (let [user (user/fetch user-id)]
     (if (nil? user)
       (res/generate-response :404 {"message" "utilisateur introuvable"})
       (res/generate-response :200 {"user" user})))))

(defn post-user-tweet [user-id body]
  (let [user (user/fetch user-id)
        message (:message body)]
    (if (nil? user)
      (res/generate-response :404 {"message" "utilisateur introuvable"})
      (if (nil? message)
        (res/generate-response :400 {"message" "Le json du tweet devrait contenir la clé message"})
        (res/generate-response :201 {"tweet-id" (user/tweet-as! user-id message)})))))

(defn get-user-tweets
  ([user-id]
   (let [user (user/fetch user-id)]
     (if (nil? user)
       (res/generate-response :404 {"message" "utilisateur introuvable"})
       (res/generate-response :200 {"tweets" (user/tweets user-id)}))))
  ([user-id tweet-id]
   (let [user (user/fetch user-id)]
     (if (nil? user)
       (res/generate-response :404 {"message" "utilisateur introuvable"})
       (let [tweet (user/tweet user-id tweet-id)]
         (if (nil? tweet)
           (res/generate-response :404 {"message" "tweet introuvable"})
           (res/generate-response :200 {"tweet" tweet})))))))

(defn get-user-feed [user-id]
  (let [user (user/fetch user-id)]
    (if (nil? user)
      (res/generate-response :404 {"message" "utilisateur introuvable"})
      (res/generate-response :200 {"feed" (user/feed user-id)}))))



