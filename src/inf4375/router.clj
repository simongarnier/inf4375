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

(defmulti matcher
          (fn [node, _] (if (-> node node-res (get 0) (= \:))
                          :wildcard
                          :res)))

(defmethod matcher :wildcard [node, str]
  {:match? true
   :params {(-> node node-res rest clojure.string/join keyword) str}
   :node node})

(defmethod matcher :res [node, str]
  {:match? (= (node-res node) str)
   :params {}
   :node node})

(defn match [u method]
  (loop [uri u nodes routes params {}]
    (let [matcher-results (map (fn [node] (matcher node (first uri))) nodes)
          matches (filter (fn [result] (get result :match?)) matcher-results)]
      (if (-> matches count (= 1)) ; check for a single match on current level
        (let [match (first matches)
              selected-node (get match :node)
              params (merge params(get match :params))
              func (-> selected-node node-dict (get method))
              children (node-children selected-node)]
          (if-not (-> uri rest empty?) ; check if we are not at the end of the uri
            (recur (rest uri) children params)
            (if-not (nil? func) ; check if we have a function for the method
              (list func params)
              (list unbound {}))))
        (list unbound {})))))

