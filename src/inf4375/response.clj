(ns inf4375.response
  (:require [clojure.string :as str]))

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
