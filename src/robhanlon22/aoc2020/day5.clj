(ns robhanlon22.aoc2020.day5
  (:require
    [clojure.java.io :as io]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [clojure.test :as t :refer [is deftest]]
    [com.gfredericks.test.chuck.generators :as gen']))


(def actual-input
  (slurp (io/resource "day5-actual.txt")))


(defn anchored
  [s]
  (re-pattern (str "^" s "$")))


(def input-patt
  "[FB]{7}[LR]{3}")


(defn exp-count
  [v]
  (Math/pow 2 (count v)))


(defn id-in-range?
  [v]
  (s/int-in-range? 0 (exp-count (:id (:args v))) (:ret v)))


(def mappings
  {\F false
   \B true
   \L false
   \R true})


(defn parse-id
  [id]
  (letfn [(search
            [bs s]
            (loop [bs bs
                   s  s]
              (if-some [b (first bs)]
                (let [c     (count s)
                      v     (/ c 2)
                      [l r] (if b [v c] [0 v])]
                  (recur (next bs) (subvec s l r)))
                (first s))))]
    (search (map mappings id)
            (vec (range 0 (exp-count id))))))


(s/fdef parse-id
        :args (s/cat :id (s/coll-of #{\F \B \L \R}))
        :ret  nat-int?
        :fn   id-in-range?)


(defn parse-seat-id
  [input]
  (let [id (vec input)]
    (+ (* (parse-id (subvec id 0 7)) 8)
       (parse-id (subvec id 7)))))


(s/def ::input
  (s/with-gen (s/and string? #(re-matches (anchored input-patt) %))
              #(gen'/string-from-regex (re-pattern input-patt))))


(s/fdef parse-seat-id
        :args (s/cat :input ::input)
        :ret  (s/int-in 0 1024))


(defn solution
  [input]
  (let [ids   (->> (str/split input #"\n")
                   (map parse-seat-id)
                   sort
                   vec)
        pairs (map vector ids (subvec ids 1))]
    (some (fn [[l r]]
            (and (= (- r l) 2) (inc l)))
          pairs)))


(deftest sample-input-test
  (is (= 357 (parse-seat-id "FBFBBFFRLR")))
  (is (= 567 (parse-seat-id "BFFFBBFRRR")))
  (is (= 119 (parse-seat-id "FFFBBBFRRR")))
  (is (= 820 (parse-seat-id "BBFFBBFRLL"))))
