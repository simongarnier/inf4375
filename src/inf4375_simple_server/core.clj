(ns inf4375-simple-server.core
  (:gen-class)
  (:import (java.io InputStreamReader PrintWriter BufferedReader)
           (java.net ServerSocket))
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [me.raynes.fs   :as fs]))

(defn consume-buffer [input]
  "Consume a input buffer and gives it content as a list of string"
  (loop  [lines '()]
    (if (.ready input)
      (recur(concat lines (list (.readLine input))))
      (remove (fn [l] (empty? l))lines))))

(defn parse-request [lines]
  "Structure a http requests"
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

(defn compose-list [lines]
  "compose a list of lines with the proper line feeds and returns"
  (reduce
    (fn [res, el] (str/join (list res el "\r\n")))
    lines))

(defmulti generate-response
          "generate a response base on the response code given"
          (fn [code _] code))

(defmethod generate-response :404 [_ _]
  (let [page-content "<h1>404 Not Found</h1>"]
    (compose-list
      (list
        "HTTP/1.1 404 Not Found"
        "Content-Type: text/html; charset=utf-8"
        (format "content-length: %s" (count page-content))
        ""
        page-content))))

(defmethod generate-response :200 [_ res-as-string]
  (compose-list (list
             "HTTP/1.1 200 OK"
             "content-type: text/html; charset=utf-8"
             (format "content-length: %s" (count res-as-string))
             ""
             res-as-string)))

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
            (let [request (parse-request lines)]
              (pp/pprint request)
              (let [[_ & relative] ((request :request-line) :uri)]
                (if (and (not (empty? relative)) (fs/exists? (str/join relative)))
                  (.println out (generate-response :200 (slurp(str/join relative))))
                  (.println out (generate-response :404 nil)))))
            (.println out (generate-response :404 nil)))
          (.flush out)
          (.close out)
          (.close in)
          (.close socket)
          (recur server))))))


(defn -main
  ([] (run-server 8080))
  ([arg] (run-server (Integer/parseInt arg))))



