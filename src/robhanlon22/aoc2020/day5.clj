(ns robhanlon22.aoc2020.day5
  (:require
    [clojure.java.io :as io]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [clojure.test :as t :refer [is deftest]]
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


(defn parse-seat-id
  [input]
  (-> (map chars->bits input)
      str/join
      (Integer/parseInt 2)))


(s/def ::input
  (s/with-gen (s/and string?
                     #(re-matches (re-pattern (format "^%s$" input-patt)) %))
              #(gen'/string-from-regex (re-pattern input-patt))))


(s/fdef parse-seat-id
        :args (s/cat :input ::input)
        :ret  nat-int?
        :fn   #(s/int-in-range? 0
                                (->> %
                                     :args
                                     :input
                                     count
                                     (Math/pow 2))
                                (:ret %)))


(defn solution
  [input]
  (->> (str/split input #"\n")
       (map parse-seat-id)
       sort
       (reduce
         (fn [l r]
           (if (= (- r l) 2)
             (reduced (inc l))
             r)))))


(deftest sample-input-test
  (is (= 357 (parse-seat-id "FBFBBFFRLR")))
  (is (= 567 (parse-seat-id "BFFFBBFRRR")))
  (is (= 119 (parse-seat-id "FFFBBBFRRR")))
  (is (= 820 (parse-seat-id "BBFFBBFRLL"))))
