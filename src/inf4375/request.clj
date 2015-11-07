(ns inf4375.request
  (:gen-class)
  (:require [clojure.string :as str]))

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