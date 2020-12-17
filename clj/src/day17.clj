(ns day17
  (:require
    [clojure.java.io :as io]
    [clojure.math.combinatorics :as combo]
    [clojure.string :as str]))


(defn deltas
  [dimensions]
  (->> (combo/selections [-1 0 1] dimensions)
       (filter #(not (every? zero? %)))
       vec))


(defn neighbors
  [deltas coordinate]
  (reduce (fn [acc delta]
            (conj acc (mapv + coordinate delta))) []
          deltas))


(defn parse
  [dimensions input]
  (let [lines (str/split-lines input)]
    (reduce-kv
      (fn [a y row]
        (reduce-kv
          (fn [b x c]
            (assoc b
                   (->> [x y]
                        (partition dimensions dimensions (repeat 0))
                        first
                        vec)
                   (= c \#)))
          a
          (vec row)))
      {}
      lines)))


(defn grow
  [deltas space]
  (reduce
    (fn [a k]
      (reduce
        (fn [b neighbor]
          (if (some? (b neighbor))
            b
            (assoc b neighbor false)))
        a
        (neighbors deltas k)))
    space
    (keys space)))


(defn iteration
  [deltas space]
  (let [space' (grow deltas space)]
    (reduce-kv
      (fn [acc k v]
        (let [active (->> k
                          (neighbors deltas)
                          (select-keys space')
                          vals
                          (filter identity)
                          count)]
          (if v
            (if (or (= active 2) (= active 3))
              acc
              (assoc acc k false))
            (if (= active 3)
              (assoc acc k true)
              acc))))
      space'
      space')))


(defn solve
  [dimensions input]
  (->> input
       (parse dimensions)
       (iterate (partial iteration (deltas dimensions)))
       (drop 6)
       first
       vals
       (filter identity)
       count))


(def sample
  ".#.
..#
###")


(def actual
  (slurp (io/resource "day-17.txt")))
