(ns inf4375.model.subscriptions
  (:gen-class)
  (:require [inf4375.model-util :as util]
            [inf4375.model.user :as user]))

(def subscriptions
  (atom {}))

(defn lookup [user-id other-user-id]
  (:id (first (filter (fn [sub] (and
                                  (= (:user-id sub) user-id)
                                  (= (:other-user-id sub) other-user-id)))
                      (vals @subscriptions)))))

(defn create [user-id other-user-id]
  (let [found (lookup user-id other-user-id)
        id (util/gen-id)]
    (if (and (nil? found) (not= user-id other-user-id))
      (swap! subscriptions conj {id {:id id
                                     :user-id user-id
                                     :other-user-id other-user-id}}))
    id)); need to fix this, should not provide an id when unsuccessful

(defn fetch-for-user [user-id]
  (map (fn [sub] (get sub :other-user-id ))
       (filter
         (fn [sub] (= (get sub :user-id) user-id))
         (vals @subscriptions))))

(defn delete [user-id other-user-id]
  (let [found (lookup user-id other-user-id)]
    (swap! subscriptions dissoc found)
    found))

