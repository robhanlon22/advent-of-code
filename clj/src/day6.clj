(ns day6
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

(def sample-input
  (slurp (io/resource "day6-sample.txt")))

(def actual-input
  (slurp (io/resource "day6-actual.txt")))

(def answers-pattern
  #"(([a-z]+\n)+\n)*([a-z]+\n)*([a-z]+)")

(defn part1
  [answers]
  (->> (str/split answers #"\n\n")
       (map #(str/replace % #"[^a-z]" ""))
       (map distinct)
       (map count)
       (reduce +)))

(s/fdef part1
  :args (s/cat :answers (s/and string? #(re-matches answers-pattern %)))
  :ret  nat-int?)

(defn part2
  [answers]
  (->> (str/split answers #"\n\n")
       (map #(->> (str/split % #"\n")
                  (map set)
                  (reduce set/intersection)
                  count))
       (reduce +)))

(deftest part1-test
  (is (= 11 (part1 sample-input)))
  (is (= 6506 (part1 actual-input)))
  (is (= 6 (part2 sample-input)))
  (is (= 3243 (part2 actual-input))))
