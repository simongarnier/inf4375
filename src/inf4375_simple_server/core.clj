(ns inf4375-simple-server.core
  (:gen-class)
  (:import (java.io InputStreamReader PrintWriter BufferedReader)
           (java.net ServerSocket))
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(defn consume-buffer [input]
  (loop  [lines '()]
    (if (.ready input)
      (let [line (.readLine input)]
        (if (str/blank? line)
          lines
          (recur(concat (concat lines (list line))))))
      lines)))

(defn parse-request [lines]
  (let [[request-line & headers] lines
        [method uri version] (str/split request-line #" ")]
    {:request-line
     {:method method
      :uri uri
      :version version}
     :headers (reduce (fn [memo header]
                           (let [[k v](str/split header #":")]
                             (assoc memo (keyword k) (str/trim v))))
                      (sorted-map) headers)}))

(defn -main
  [& args]
  (println "server accepting request")
  (loop [server (new ServerSocket 8080)]
    (let [socket (.accept server)]
      (println "request received")
      (let [out (new PrintWriter (.getOutputStream socket))
            in (new BufferedReader (new InputStreamReader (.getInputStream socket)))]
        (let [lines (consume-buffer in)]
          (if (not (empty? lines))
            (pp/pprint (parse-request lines))
            )
          (recur server))))))







