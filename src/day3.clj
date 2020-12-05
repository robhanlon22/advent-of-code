(ns day3
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))


(def sample-input
  (slurp (io/resource "day3-sample.txt")))


(def actual-input
  (slurp (io/resource "day3-actual.txt")))


(defn ^:private descend
  [rows dr dc]
  (loop [r 0 c 0 s 0]
    (if-let [v (and (< r (count rows)) (nth (nth rows r) c))]
      (let [i (if (= v \#) 1 0)]
        (recur (+ r dr) (+ c dc) (+ s i)))
      s)))


(defn solution
  [input]
  (let [lines (str/split-lines input)
        rows  (map #(cycle (seq (char-array %))) lines)]
    (->> [[1 1] [1 3] [1 5] [1 7] [2 1]]
         (map (fn [[dr dc]] (descend rows dr dc)))
         (reduce *))))


(solution sample-input)

(solution actual-input)
