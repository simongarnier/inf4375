(ns inf4375.request
  (:gen-class)
  (:require [clojure.string :as str]))

(defprotocol Parsed
  (method [this])
  (uri [this])
  (version [this])
  (headers [this]))

(defrecord HttpRequest [method uri version headers]
  Parsed
  (method [this] method)
  (uri [this] uri)
  (version [this] version)
  (headers [this] headers))

(defn request [lines]
  "Structure a http requests"
  (let [[request-line & headers] lines
        [method uri version] (str/split request-line #" ")]
    (->HttpRequest
      method
      uri
      version
      (reduce (fn [memo header]
                (let [[k v](str/split header #":")]
                  (assoc memo (keyword k) (str/trim v))))
              (sorted-map)
              headers))))