(ns day10
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as str]))


(def sample1
  (slurp (io/resource "day10-sample1.txt")))


(def actual
  (slurp (io/resource "day10-actual.txt")))


(defn adapters
  [input]
  (let [adapters (map edn/read-string (str/split-lines input))]
    (->> (+ (apply max adapters) 3)
         (conj adapters)
         (cons 0)
         sort)))


(defn part1
  [input]
  (->> input
       adapters
       (partition 2 1)
       (map reverse)
       (map #(apply - %))
       frequencies
       vals
       (reduce *)))


(def ^:dynamic *p*)


(defn part2
  [input]
  (let [a (adapters input)
        m (last a)
        a (set a)
        p (memoize
            (fn [s]
              (if (= s m)
                1
                (reduce
                  (fn [acc v]
                    (let [n (+ s v)]
                      (if (contains? a n)
                        (+ acc (*p* n))
                        acc)))
                  0
                  (range 1 4)))))]
    (binding [*p* p]
      (p 0))))


(part1 sample1)

(part1 actual)
