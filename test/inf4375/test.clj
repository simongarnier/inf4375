(ns inf4375.test
  (:require [clojure.test :refer :all]
            [inf4375.core :refer :all]
            [inf4375.router :refer :all]
            [inf4375.model.tweet :refer :all]))


; router
(def demo-routes
  [
   ["" {:get :root-get}
    ["utilisateurs" {}
     [":user-id" {}
      ["fil" {:get :user-fil-get}]
      ["tweets" {:get :user-tweets-get}
       [":tweet-id" {:get :user-tweet-get}]]
      ["retweets" {:get :user-retweets-get}]
      ["abonnements" {:get :user-abonnements-get}]]]]]
  )

(deftest root
  (testing "root should match empty"
    (binding [routes demo-routes]
      (is (= (match [""] :get) (list :root-get {}))))))

(deftest user
  (testing "parent node should match"
    (binding [routes demo-routes]
      (is (= (match ["", "utilisateurs"] :get) (list :unbound {}))))))

(deftest tweets
  (testing "parent node should match"
    (binding [routes demo-routes]
      (is (= (match ["", "utilisateurs", "1234", "tweets"] :get) (list :user-tweets-get {:user-id "1234"}))))))

(deftest tweet-id
  (testing "parent node should match"
    (binding [routes demo-routes]
      (is (= (match ["", "utilisateurs", "1234", "tweets", "5"] :get) (list :user-tweet-get {:user-id "1234" :tweet-id "5"}))))))

(deftest tweets-unbound
  (testing "parent node should match"
    (binding [routes demo-routes]
      (is (= (match ["", "utilisateurs", "1234", "tweet"] :get) (list :unbound {}))))))


; tweet model
(deftest create-tweet
  (testing "fetch a tweet after creation"
    (let [m "my first tweet! #helloworld"
          id (create m)]
      (is (= (fetch id) {:id id :message m})))))
