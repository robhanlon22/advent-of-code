(ns robhanlon22.aoc2020.day2
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))


(def sample-input
  ["1-3 a: abcde"
   "1-3 b: cdefg"
   "2-9 c: ccccccccc"])


(def actual-input
  (-> "day2.txt"
      io/resource
      slurp
      str/split-lines))


(defn parse
  [v]
  (let [[_ m x c p] (re-matches #"^(\d+)-(\d+) (.): (.*)$" v)
        m           (Integer/parseInt m)
        x           (Integer/parseInt x)
        c           (nth c 0)]
    [m x c p]))


(defn ^:private valid-1?
  [v]
  (let [[m x c p] (parse v)
        r         (count (filter #(= % c) p))]
    (<= m r x)))


(defn ^:private valid-2?
  [v]
  (let [[m x c p] (parse v)
        a         (nth p (dec m))
        b         (nth p (dec x))]
    (or (and (= c a) (not= c b))
        (and (not= c a) (= c b)))))


(defn solution
  [input valid?]
  (->> input
       (map valid?)
       (filter identity)
       count))


(solution sample-input valid-1?)
(solution actual-input valid-1?)

(solution sample-input valid-2?)
(solution actual-input valid-2?)
