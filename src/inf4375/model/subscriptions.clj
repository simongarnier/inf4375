(ns inf4375.model.subscriptions
  (:gen-class)
  (:require [inf4375.model-util :as util]
            [inf4375.model.user :as user]))

(def subscriptions
  (atom {}))

(defn- lookup [user-id other-user-id]
  (:id (first (filter (fn [sub] (and
                                  (= (:user-id sub) user-id)
                                  (= (:other-user-id sub) other-user-id)))
                      (vals @subscriptions)))))

(defn create-if-not-found! [user-id other-user-id]
  {:pre [(not (nil? (user/fetch user-id)))
         (not (nil? (user/fetch other-user-id)))
         (not= user-id other-user-id)]}
  (let [found (lookup user-id other-user-id)]
    (if (nil? found)
      (let [id (util/gen-id)]
        (swap! subscriptions conj {id {:id id
                                       :user-id user-id
                                       :other-user-id other-user-id}})
        id)     ;if created, return the id of the new sub
      found)))  ;if found, return the id of the existing sub

(defn fetch-subscriptions-for-user [user-id]
  (map (fn [sub] (:other-user-id sub))
       (filter
         (fn [sub] (= (:user-id sub ) user-id))
         (vals @subscriptions))))

(defn fetch-subscribers-for-user [user-id]
  (map (fn [sub] (:user-id sub))
       (filter
         (fn [sub] (= (:other-user-id sub) user-id))
         (vals @subscriptions))))

(defn del! [user-id other-user-id]
  (let [found (lookup user-id other-user-id)]
    (swap! subscriptions dissoc found)
    found))
