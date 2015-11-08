(ns inf4375.core
  (:gen-class)
  (:import (java.io InputStreamReader PrintWriter BufferedReader)
           (java.net ServerSocket))
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [me.raynes.fs :as fs]
            [inf4375.controller :as controller]
            [inf4375.response :as response]
            [clojure.data.json :as json]))

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
            (let []
              (pp/pprint lines)
              (pp/pprint (controller/resolve! lines)))
            (.println out (response/generate-response :404 nil)))
          (.flush out)
          (.close out)
          (.close in)
          (.close socket)
          (recur server))))))

(defn -main
  ([] (run-server 8080))
  ([arg] (run-server (Integer/parseInt arg))))



