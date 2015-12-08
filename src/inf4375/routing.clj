(ns inf4375.routing
  (:gen-class)
  (:require [clojure.string :as str]))

(def ^:dynamic routes
  [])

(def ^:dynamic unbound
  :unbound)

(def ^:dynamic options
  :options)

(defn node-children [node]
  "Children of the node"
  (-> node rest rest))

(defn node-method [node]
  "The map specifying each function for each http method"
  (second node))

(defn node-name [node]
  "The name for this node"
  (first node))

(defn expand-splitable-node [node]
  "If the node name has on or many | in it,
   expand the node into an array of node(of for each terms)"
  (map (fn [name] (concat [name (node-method node)] (node-children node))) (str/split (node-name node) #"\|")))

(defn- numerical? [str-val]
  (not (nil? (re-find #"[\d.]+" str-val))))

(defmulti match
          "Match a node in the hierarchy with the provided string.
           Will return a map containing the match outcome and, if
           the match is succesful and if the node is a wildcard,
           the param to be use."
          (fn [node, _]
            (-> node node-name (get 0))))

(defmethod match \# [node, str-val]
  "the numerical wildcard; will match any numerical value and return as int param
   Perfect for IDs"
  (let [match (and (not (empty? str-val)) (numerical? str-val))
        param (if match
                (list (Integer/parseInt str-val))
                (list))]
    {:match? match
     :param param
     :node node}))

(defmethod match \: [node, str-val]
  "the string wildcard; will match any non-empty string and return as string param
   Perfect for handles"
  (let [match (and (not (empty? str-val)) (not (numerical? str-val)))
        param (if match
                (list str-val)
                (list))]
    {:match? match
     :param param
     :node node}))

(defmethod match :default [node, str-val]
  "match for an exact value"
  {:match? (= (node-name node) str-val)
   :param '()
   :node node})

(defn route [resource method]
  "Taking the route into account, return the function to be run and the
   params to give to this function, for a given resource and method."
  (if (= method "OPTIONS")
    (list options '())
    (loop [res resource nodes routes params []]
      (let [expanded-nodes (reduce (fn [ns, node] (concat ns (expand-splitable-node node))) [] nodes)
            matcher-results (map (fn [node] (match node (first res))) expanded-nodes)
            matches (filter (fn [result] (get result :match?)) matcher-results)]
        (if (-> matches count (= 1)) ; check for a single match on current level
          (let [match (first matches)
                selected-node (get match :node)
                params (concat params (:param match))
                func (-> selected-node node-method (get method))
                children (node-children selected-node)]
            (if-not (-> res rest empty?) ; check if we are not at the end of the uri
              (recur (rest res) children params)
              (if-not (nil? func) ; check if we have a function for the method
                (list func params)
                (list unbound '()))))
          (list unbound '())))))
  )
