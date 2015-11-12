(ns inf4375.core
  (:gen-class)
  (:import (java.io InputStreamReader PrintWriter BufferedReader)
           (java.net ServerSocket))
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]

            [inf4375.controller :as controller]
            [inf4375.request :as request]))

(def service-routes
  [
   ["" {}
    ["utilisateurs" {"GET" controller/get-users}
     ["#user-id" {"GET" controller/get-users}
      ["fil" {"GET" controller/get-user-feed}]

      ["tweets" {"POST" controller/post-user-tweet
                 "GET" controller/get-user-tweets}
       ["#tweet-id" {"GET" controller/get-user-tweets}]]

      ["retweets" {}
       ["#tweet-id" {"POST" controller/post-user-retweet
                     "DELETE" controller/delete-user-retweet}]]

      ["abonnements" {"GET" controller/get-user-subs}
       ["#other-user-id" {"PUT" controller/put-user-sub
                          "DELETE" controller/delete-user-subs}]]]]]])

(defn run-server [port]
  "main server loop; will 404 if request is empty or not found"
  (println (format "server accepting request on %s" port))
  (loop [server (new ServerSocket port)]
    (let [socket (.accept server)]
      (println "request received")
      (let [out (new PrintWriter (.getOutputStream socket))
            in (new BufferedReader (new InputStreamReader (.getInputStream socket)))]
        (let [request (request/read-request in)
              parsed-body (try
                            (json/read-str (:body request)
                                           :key-fn #(keyword %))
                            (catch Exception json-e
                              nil))
              response (if (nil? parsed-body)
                         (controller/resolve! service-routes (:method request) (:uri request))
                         (controller/resolve! service-routes (:method request) (:uri request) [parsed-body]))]
          (pp/pprint request)
          (.println out response)
          (.flush out)
          (.close out)
          (.close in)
          (.close socket)
          (recur server))))))

(defn- initials []
  (let [simon (inf4375.model.user/create! "simongarnier")
        camille (inf4375.model.user/create! "camillegarnier")]
    (Thread/sleep 1000)
    (inf4375.model.user/tweet-as! simon "my first tweet")
    (Thread/sleep 1000)
    (inf4375.model.user/tweet-as! simon "my second tweet")
    (Thread/sleep 1000)
    (inf4375.model.user/tweet-as! simon "my last tweet")
    (Thread/sleep 1000)
    (inf4375.model.user/tweet-as! camille "my first tweet")
    (Thread/sleep 1000)
    (inf4375.model.user/tweet-as! camille "my second tweet")
    (Thread/sleep 1000)
    (inf4375.model.user/tweet-as! camille "my last tweet")
    (println "initials users and tweets created")))

(defn -main
  ([]
   (let [f (future (initials))
         s (run-server 8080)]))
  ([arg]
   (let [f (future (initials))
         s (run-server (Integer/parseInt arg))])))
