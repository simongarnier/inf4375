(ns inf4375.tweet-test
  (:require [clojure.test :refer :all]
            [inf4375.model.tweet :refer :all]))

(deftest create-tweet
  (testing "fetch a tweet after creation"
    (let [m "my first tweet! #helloworld"
          id (create m)]
      (is (= (fetch id) {:id id :message m})))))
