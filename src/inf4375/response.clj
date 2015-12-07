(ns inf4375.response
  (:require [clojure.string :as str]
            [clojure.data.json :as json]))

(def status-for-code {:400 "400 Bad Request"
                      :404 "404 Not Found"
                      :200 "200 OK"
                      :201 "201 Created"
                      :204 "204 No Content"})

(def version "HTTP/1.1")

(def content-type "content-type: application/json; charset=utf-8")

(defn generate-response [code content]
  "Return a json response containing the given content and http code"
  (let [status (get status-for-code code)
        body (json/write-str {"status" status
                              "payload" content})]
    (str/join "\r\n" (list
                       (str/join [version " " status])
                       content-type
                       (format "content-length: %s" (count body))
                       "Access-Control-Allow-Origin: http://localhost:8080"
                       ""
                       body
                       ""))))