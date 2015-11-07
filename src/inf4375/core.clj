(ns inf4375.core
  (:gen-class)
  (:import (java.io InputStreamReader PrintWriter BufferedReader)
           (java.net ServerSocket))
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [me.raynes.fs   :as fs]
            [inf4375.router :as router]
            [inf4375.request :as request]
            [inf4375.response :as response]
            [inf4375.model.tweet :as tweet]
            [inf4375.model.user :as user]))

(def stub
  nil)

(def service-routes
  [
   ["" {}
    ["utilisateurs" {}
     [":user-id" {}
      ["fil" {:get stub}]
      ["tweets" {:post stub
                 :get stub}
       [":tweet-id" {:delete stub}]]
      ["retweets" {}
       [":tweet-id" {:post stub
                     :delete stub}]]
      ["abonnements" {:get stub}
       [":other-user-id"] {:put stub
                           :delete stub}]]]]])

(defn consume-buffer [input]
  "Consume a input buffer and gives it content as a list of string"
  (loop  [lines '()]
    (if (.ready input)
      (recur(concat lines (list (.readLine input))))
      (remove (fn [l] (empty? l))lines))))

(defn run-server [port]
  "main server loop; will 404 if request is empty or not found"
  (println (format "server accepting request on %s" port))
  (loop [server (new ServerSocket port)]
    (let [socket (.accept server)]
      (println "request received")
      (let [out (new PrintWriter (.getOutputStream socket))
            in (new BufferedReader (new InputStreamReader (.getInputStream socket)))]
        (let [lines (consume-buffer in)]
          (if (not (empty? lines))
            (let [request (request/parse-request lines)]
              (pp/pprint request)
              (let [[_ & relative] ((request :request-line) :uri)]
                (if (and (not (empty? relative)) (fs/exists? (str/join relative)))
                  (.println out (response/generate-response :200 (slurp(str/join relative))))
                  (.println out (response/generate-response :404 nil)))))
            (.println out (response/generate-response :404 nil)))
          (.flush out)
          (.close out)
          (.close in)
          (.close socket)
          (recur server))))))

(defn -main
  ([] (run-server 8080))
  ([arg] (run-server (Integer/parseInt arg))))



