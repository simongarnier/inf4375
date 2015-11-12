(ns inf4375.test
  (:require [clojure.test :refer :all]
            [inf4375.core :refer :all]
            [inf4375.routing :as router]
            [inf4375.model.tweet :as tweet]
            [inf4375.model.user :as user]))


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

(deftest router-root
  (testing "root should match empty"
    (binding [router/routes demo-routes]
      (is (= (router/match [""] :get) (list :root-get '()))))))

(deftest router-user
  (testing "parent node should match"
    (binding [router/routes demo-routes]
      (is (= (router/match ["", "utilisateurs"] :get) (list :unbound '()))))))

(deftest router-tweets
  (testing "parent node should match"
    (binding [router/routes demo-routes]
      (is (= (router/match ["", "utilisateurs", "1234", "tweets"] :get) (list :user-tweets-get '("1234")))))))

(deftest router-tweet-id
  (testing "parent node should match"
    (binding [router/routes demo-routes]
      (is (= (router/match ["", "utilisateurs", "1234", "tweets", "5"] :get) (list :user-tweet-get '("1234" "5")))))))

(deftest router-tweets-unbound
  (testing "parent node should match"
    (binding [router/routes demo-routes]
      (is (= (router/match ["", "utilisateurs", "1234", "tweet"] :get) (list :unbound '()))))))


; tweet model
(deftest tweet-create
  (testing "fetch a tweet after creation"
    (let [m "my first tweet! #helloworld"
          id (tweet/create! m)]
      (is (= (tweet/fetch id) {:id id :message m})))))

; user model
(deftest user-create
  (testing "fetch a user after creation"
    (let [u1 "simongarnier"
          u2 "camillegarnier"
          id1 (user/create! u1)
          id2 (user/create! u2)]
      (is (and
            (= (:handle (user/fetch id1)) u1 )
            (= (:handle (user/fetch id2)) u2 ))))))
