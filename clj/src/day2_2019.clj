(ns day2-2019
  (:require
    [clojure.math.combinatorics :as combo]
    [clojure.test :refer [is]]
    [intcode]))


(def actual
  "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,5,19,23,2,9,23,27,1,27,5,31,2,31,13,35,1,35,9,39,1,39,10,43,2,43,9,47,1,47,5,51,2,13,51,55,1,9,55,59,1,5,59,63,2,6,63,67,1,5,67,71,1,6,71,75,2,9,75,79,1,79,13,83,1,83,13,87,1,87,5,91,1,6,91,95,2,95,13,99,2,13,99,103,1,5,103,107,1,107,10,111,1,111,13,115,1,10,115,119,1,9,119,123,2,6,123,127,1,5,127,131,2,6,131,135,1,135,2,139,1,139,9,0,99,2,14,0,0")


(defn result
  [program]
  (first program))


(defn evaluate
  [input]
  (result (intcode/run (intcode/parse input))))


(defn part1
  [input]
  (let [p (-> (intcode/parse input)
              (assoc 1 12)
              (assoc 2 2))]
    (result (intcode/run p))))


(def expected 19690720)


(defn part2
  [input]
  (let [p           (intcode/parse input)
        [noun verb] (some (fn [[n v]]
                            (let [p' (-> p
                                         (assoc 1 n)
                                         (assoc 2 v))
                                  r  (result (intcode/run p'))]
                              (when (= r expected)
                                [n v])))
                          (combo/selections (range 0 100) 2))]
    (+ (* 100 noun) verb)))


(is (= [3500 9 10 70 2 3 11 0 99 30 40 50] (:intcode/prog (intcode/run [1 9 10 3 2 3 11 0 99 30 40 50]))))
(is (= [2 0 0 0 99] (:intcode/prog (intcode/run [1 0 0 0 99]))))
(is (= [2 3 0 6 99] (:intcode/prog (intcode/run [2 3 0 3 99]))))
(is (= [2 4 4 5 99 9801] (:intcode/prog (intcode/run [2 4 4 5 99]))))
(is (= [30 1 1 4 2 5 6 0 99] (:intcode/prog (intcode/run [1 1 1 4 99 5 6 0 99]))))
