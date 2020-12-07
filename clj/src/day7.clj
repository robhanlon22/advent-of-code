(ns day7
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [instaparse.core :refer [defparser]]
    [loom.alg :as alg]
    [loom.graph :as graph]
    [loom.io :as lio]))


(def sample-input
  (slurp (io/resource "day7-sample.txt")))


(def actual-input
  (slurp (io/resource "day7-actual.txt")))


(defparser parser
  "S = name <s> <bag> <s> <contain> <s> ((child <s> <bag> <p> <s>?)+ | <no>)
s = ' '
p = #'[,\\.]'
number = #'[0-9]+'
name = #'[a-z]+ [a-z]+'
child = number <s> name
bag = #'bags?'
no = 'no other bags.'
contain = 'contain'")


(defn parse
  [line]
  (->> line
       parser
       (drop 1)))


(defn ^:private add-rule
  [g rule]
  (let [[[_ node] & children] rule]
    (reduce (fn [g' name']
              (let [[_ [_ n] [_ node']] name']
                (-> g'
                    (graph/add-nodes node')
                    (graph/add-edges [node node' (Integer/parseInt n)]))))
            (graph/add-nodes g node)
            children)))


(defn ^:private build-graph
  [rules]
  (->> rules
       str/split-lines
       (map parse)
       (reduce add-rule (graph/weighted-digraph))))


(defn part1
  [rules]
  (let [graph (build-graph rules)]
    (->> graph
         graph/nodes
         (map #(alg/bf-path graph % "shiny gold"))
         (filter identity)
         count)))


(defn ^:private search
  [graph node]
  (let [edges (graph/out-edges graph node)]
    (reduce (fn [acc edge]
              (let [weight (graph/weight graph edge)
                    node'  (graph/dest edge)
                    bags   (search graph node')]
                (+ acc weight (* weight bags))))
            0
            edges)))


(defn part2
  [rules]
  (spit "g.dot" (lio/dot-str (build-graph rules)))
  (search (build-graph rules) "shiny gold"))


(def s
  "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")
