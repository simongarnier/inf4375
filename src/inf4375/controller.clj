(ns inf4375.controller
  (:gen-class)
  (:require [inf4375.model.tweet :as tweet]
            [inf4375.model.user :as user]
            [inf4375.model.subscriptions :as sub]

            [inf4375.request :as req]
            [inf4375.response :as res]
            [inf4375.routing :as routing]

            [clojure.string :as str]
            [clojure.data.json :as json]))

(defn not-found [& args] ;any number of param because we don't care about arity
  "the function to use when the outing fails"
  (res/generate-response :404 {"message" "ressource introuvable"}))

(defn resolve-and-execute!
  ([routes method resource]
   (resolve-and-execute! routes method resource []))
  ([routes method resource injected-params]
   "resolve a resource for the given route and methods and then return
    the result of the function associated with this resource and method.
    Param can also be injected, so extra param can be given to the called
    function. Example of this injection include the body of a given request"
    (binding [routing/routes routes
              routing/unbound not-found]
      (let [resource-list (str/split resource #"/")
            func-and-params (routing/route
                             (if (empty? resource-list)
                               [""]
                               resource-list)
                             method)
            function (first func-and-params)
            params (last func-and-params)]
        (apply function (concat params injected-params))))))

;controller functions.
;generally, there is one function for every resource and method combination
;except where we can differantiate behavior with param arity.
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

(defn put-user-sub [user-id other-user-id]
  (if (or (nil? (user/fetch user-id)) (nil? (user/fetch other-user-id)))
    (res/generate-response :404 {"message" "un ou plusieurs utilisateurs introuvables"})
    (if (= user-id other-user-id)
      (res/generate-response :400 {"message" "Un utilisateur ne peut s'abonner à lui-même"})
      (res/generate-response :201 {"subscription-id" (sub/create-if-not-found! user-id other-user-id)}))))

(defn get-user-subs [user-id]
  (if (nil? (user/fetch user-id))
    (res/generate-response :404 {"message" "utilisateur introuvable"})
    (res/generate-response :200 {"subscriptions" (sub/fetch-for-user user-id)})))

(defn delete-user-subs [user-id other-user-id]
  (if (or (nil? (user/fetch user-id)) (nil? (user/fetch other-user-id)))
    (res/generate-response :404 {"message" "un ou plusieurs utilisateurs introuvables"})
    (let []
      (sub/del! user-id other-user-id)
      (res/generate-response :200 {"deleted-subscription" {"subscriber" user-id
                                                           "subscribee" other-user-id}}))))

(defn post-user-retweet [user-id tweet-id]
  (if (nil? (user/fetch user-id))
    (res/generate-response :404 {"message" "utilisateur introuvable"})
    (if (nil? (tweet/fetch tweet-id))
      (res/generate-response :404 {"message" "tweet introuvable"})
      (if (user/tweet user-id tweet-id)
        (res/generate-response :400 {"message" "Un utilisateur ne peut retweeter son propre tweet"})
        (res/generate-response :201 {"tweet-id" (user/retweet-as! user-id tweet-id)})))))

(defn delete-user-retweet [user-id tweet-id]
  (if (nil? (user/fetch user-id))
    (res/generate-response :404 {"message" "utilisateur introuvable"})
    (if (nil? (tweet/fetch tweet-id))
      (res/generate-response :404 {"message" "tweet introuvable"})
      (res/generate-response :200 {"deleted-retweets" (user/undo-retweet! user-id tweet-id)}))))