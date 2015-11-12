(ns inf4375.router
  (:gen-class))

(def ^:dynamic routes
  [])

(def ^:dynamic unbound
  :unbound)

(defn node-children [node]
  (-> node rest rest))

(defn node-dict [node]
  (second node))

(defn node-res [node]
  (first node))

(defmulti match
          (fn [node, _]
            (-> node node-res (get 0))))

(defmethod match \# [node, str-val]
  "the numerical wildcard; will match any numerical value and return as int param
   Perfect for IDs"
  (let [num-check (re-find #"[\d.]+" str-val)
        match (not (nil? num-check))
        param (if match
                (list (Integer/parseInt str-val))
                (list))]
    {:match? match
     :param param
     :node node}))

(defmethod match \: [node, str-val]
  "the string wildcard; will match any non-empty string and return as string param
   Perfect for handles"
  (let [match (empty? str-val)
        param (if match
                (list str-val)
                (list))]
    {:match? match
     :param param
     :node node}))

(defmethod match :default [node, str-val]
  "match for an exact value"
  {:match? (= (node-res node) str-val)
   :param '()
   :node node})

(defn route [resource method]
  (loop [res resource nodes routes params []]
    (let [matcher-results (map (fn [node] (match node (first res))) nodes)
          matches (filter (fn [result] (get result :match?)) matcher-results)]
      (if (-> matches count (= 1)) ; check for a single match on current level
        (let [match (first matches)
              selected-node (get match :node)
              params (concat params (:param match))
              func (-> selected-node node-dict (get method))
              children (node-children selected-node)]
          (if-not (-> res rest empty?) ; check if we are not at the end of the uri
            (recur (rest res) children params)
            (if-not (nil? func) ; check if we have a function for the method
              (list func params)
              (list unbound '()))))
        (list unbound '())))))

