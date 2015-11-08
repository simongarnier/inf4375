(ns inf4375.response
  (:require [clojure.string :as str]
            [clojure.data.json :as json]))

(defn- compose-list [lines]
  "compose a list of lines with the proper line feeds and returns"
  (reduce
    (fn [res, el] (str/join (list res el "\r\n")))
    lines))

(defmulti generate-response
          "generate a response base on the response code given"
          (fn [code _] code))

(defmethod generate-response :404 [_ content]
  (let [status {"status" "404 Not Found"}
        payload (json/write-str (conj status content))]
    (compose-list
      (list
        "HTTP/1.1 404 Not Found"
        "Content-Type: text/html; charset=utf-8"
        (format "content-length: %s" (count payload))
        ""
        payload))))

(defmethod generate-response :200 [_ content]
  (let [status {"status" "200 OK"}
        payload (json/write-str (conj status content))]
    (compose-list
      (list
        "HTTP/1.1 200 OK"
        "content-type: text/html; charset=utf-8"
        (format "content-length: %s" (count payload))
        ""
        payload))))


