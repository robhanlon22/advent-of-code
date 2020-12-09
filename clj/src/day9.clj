(ns day9
  (:require
    [clojure.java.io :as io]
    [clojure.math.combinatorics :as combo]
    [clojure.string :as str]))


(def sample
  (io/resource "day9-sample.txt"))


(def actual
  (io/resource "day9-actual.txt"))


(defn read-xmas
  [input]
  (map #(Long/parseLong %) (str/split-lines (slurp input))))


(defn ^:private sum
  [xmas len]
  (loop [ps (partition (inc len) 1 xmas)]
    (let [p (first ps)
          c (combo/combinations (take len p) 2)
          n (last p)]
      (if (some (fn [[a b]] (= (+ a b) n)) c)
        (recur (next ps))
        n))))


(defn part1
  [input len]
  (sum (read-xmas input) len))


(defn part2
  [input len]
  (let [xmas (read-xmas input)
        s    (sum xmas len)
        x    (->> (range 2 (.indexOf xmas s))
                  (map #(partition % 1 xmas))
                  (apply concat)
                  (some #(when (= (reduce + %) s) %)))]
    (+ (apply min x) (apply max x))))
