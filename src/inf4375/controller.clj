(ns inf4375.controller
  (:gen-class)
  (:require [inf4375.model.tweet :as tweet]
            [inf4375.model.user :as user]
            [inf4375.model.subscriptions :as sub]

            [inf4375.request :as req]
            [inf4375.response :as res]

            [inf4375.router :as router]
            [clojure.string :as str]))

(def service-routes
  [
   ["" {}
    ["utilisateurs" {}
     [":user-id" {}
      ["fil" {"GET" :impl}]
      ["tweets" {"POST" :impl
                 "GET" :impl}
       [":tweet-id" {"DELETE" :impl}]]
      ["retweets" {}
       [":tweet-id" {"POST" :impl
                     "DELETE" :impl}]]
      ["abonnements" {"GET" :impl}
       [":other-user-id"] {"PUT" :impl
                           "DELETE" :impl}]]]]])

(defn resolve [request-lines]
  (binding [router/routes service-routes
            router/unbound :unbound]
    (let [request (req/request request-lines)
          resources (str/split (req/uri request) #"/")
          func-and-params (router/match
                           (if (empty? resources)
                             [""]
                             resources)
                           (req/method request))
          function (first func-and-params)
          params (last func-and-params)]
      (apply function params))))


