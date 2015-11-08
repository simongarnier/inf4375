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
   :param (list str)
   :node node})

(defmethod matcher :res [node, str]
  {:match? (= (node-res node) str)
   :param '()
   :node node})

(defn match [resource method]
  (loop [res resource nodes routes params {}]
    (let [matcher-results (map (fn [node] (matcher node (first res))) nodes)
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

