(ns robhanlon22.aoc2020.day5
  (:require
    [clojure.java.io :as io]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [clojure.test :as t :refer [deftest is]]
    [com.gfredericks.test.chuck.generators :as gen']))


(def actual-input
  (slurp (io/resource "day5-actual.txt")))


(def input-patt
  "[FB]{7}[LR]{3}")


(def chars->bits
  {\F 0
   \B 1
   \L 0
   \R 1})


(defn boarding-pass->seat-id
  [input]
  (Integer/parseInt (str/join (map chars->bits input)) 2))


(s/def ::input
  (s/with-gen (s/and string?
                     #(re-matches (re-pattern (format "^%s$" input-patt)) %))
              #(gen'/string-from-regex (re-pattern input-patt))))


(s/fdef boarding-pass->seat-id
        :args (s/cat :input ::input)
        :ret  nat-int?
        :fn   #(s/int-in-range? 0
                                (Math/pow 2 (count (:input (:args %))))
                                (:ret %)))


(defn solution
  [input]
  (->> (str/split input #"\n")
       (map boarding-pass->seat-id)
       sort
       (reduce
         (fn [l r]
           (if (= (- r l) 2)
             (reduced (inc l))
             r)))))


(deftest sample-input-test
  (is (= 357 (boarding-pass->seat-id "FBFBBFFRLR")))
  (is (= 567 (boarding-pass->seat-id "BFFFBBFRRR")))
  (is (= 119 (boarding-pass->seat-id "FFFBBBFRRR")))
  (is (= 820 (boarding-pass->seat-id "BBFFBBFRLL"))))
