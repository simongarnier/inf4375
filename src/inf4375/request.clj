(ns inf4375.request
  (:gen-class)
  (:import (java.io BufferedReader))
  (:require [clojure.string :as str]))

(defn- read-until-body [in]
  "consume the buffer as line until a body is detected"
  (loop [lines []]
    (if (and
          (str/blank? (last lines))
          (> (count lines) 1))
      lines
      (recur (conj lines (.readLine in))))))

(defn- read-body [in content-length]
  "consume the buffer as character for the specified lenght"
  (let [body (char-array content-length)]
    (.read in body 0 content-length)
    (apply str body)))

(defn- content-length [status-and-header]
  "look for a content-length header, check if the value is numerical
   and then return "
  (let [length (get (get status-and-header :headers) :content-length "0")
        valid (not (nil? (re-find #"[\d.]+" length)))]
    (if valid
      (read-string length))))

(defn parse-status-and-header [lines]
  "Structure a http requests"
  (let [[request-line & headers] lines
        [method uri version] (str/split request-line #" ")
        uri (first (str/split uri #"\?"))]
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
  "return a map of the given request"
  (let [status-and-header (parse-status-and-header (read-until-body reader))
        content-length (content-length status-and-header)]
    (if (> content-length 0)
      (assoc status-and-header :body (read-body reader content-length))
      status-and-header)))

