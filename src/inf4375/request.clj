(ns inf4375.request
  (:gen-class)
  (:import (java.io BufferedReader))
  (:require [clojure.string :as str]))

(defprotocol Parsed
  (method [this])
  (uri [this])
  (version [this])
  (headers [this])
  (content-length [this]))

(defrecord HttpRequest [method uri version headers]
  Parsed
  (method [this] method)
  (uri [this] uri)
  (version [this] version)
  (headers [this] headers)
  (content-length [this] (read-string (get headers :content-length "0"))))

(defn- read-until-body [in]
  (loop [lines []]
    (if (and
          (str/blank? (last lines))
          (> (count lines) 1))
      lines
      (recur (conj lines (.readLine in))))))

(defn- read-body [in content-length]
  (let [body (char-array content-length)]
    (.read in body 0 content-length)
    (apply str body)))

(defn- content-length [status-and-header]
  (read-string (get (get status-and-header :headers) :content-length "0")))

(defn parse-status-and-header [lines]
  "Structure a http requests"
  (let [[request-line & headers] lines
        [method uri version] (str/split request-line #" ")]
    {:method method
     :uri uri
     :version uri
     :headers (reduce (fn [memo header]
                        (if (str/blank? header)
                          memo
                          (let [[k v](str/split header #":")]
                            (assoc memo (keyword (str/lower-case k)) (str/trim v)))))
                      (sorted-map)
                      headers)}))

(defn read-request [reader]
  (let [status-and-header (parse-status-and-header (read-until-body reader))
        content-length (content-length status-and-header)]
    (if (> content-length 0)
      (assoc status-and-header :body (read-body reader content-length))
      status-and-header)))

