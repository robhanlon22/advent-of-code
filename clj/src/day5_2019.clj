(ns day5-2019
  (:require
    [clojure.core.async :refer [onto-chan! chan <!! >!!] :as a]
    [clojure.java.io :as io]
    [intcode]))


(defn part1
  []
  (let [in  (chan 10000)
        out (chan 10000)]
    (>!! in 1)
    (intcode/run {:intcode/prog (intcode/parse (slurp (io/resource "day5_2019.txt")))
                  :intcode/in   in
                  :intcode/out  out})
    (a/close! out)
    (last (<!! (a/into [] out)))))


(defn part2
  []
  (-> {:intcode/prog (intcode/parse (slurp (io/resource "day5_2019.txt")))
       :intcode/in   (onto-chan! (chan 1) [5])
       :intcode/out  (chan 1)}
      intcode/run))
