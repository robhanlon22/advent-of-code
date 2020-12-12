(ns day7-2019
  (:require
    [clojure.core.async :refer [chan >!! <!!]]
    [clojure.java.io :as io]
    [clojure.math.combinatorics :as combo]
    [intcode]))


(defmethod print-method clojure.lang.PersistentQueue [q, w] ; Overload the printer for queues so they look like fish
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))


(def actual
  (slurp (io/resource "day7_2019.txt")))


(def sample1
  "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")


(def sample2
  "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")


(def sample3
  "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")


(defn part1
  [input]
  (let [prog (intcode/parse input)]
    (->> (combo/permutations (range 0 5))
         (map (fn [setting]
                (reduce (fn [acc v]
                          (let [in  (chan 2)
                                out (chan 1)]
                            (>!! in v)
                            (>!! in acc)
                            (let [s' (intcode/run {:intcode/prog prog
                                                   :intcode/in   in
                                                   :intcode/out  out})]
                              (<!! (:intcode/out s')))))
                        0
                        setting)))
         (apply max))))


(defn part2
  [input]
  (let [prog (intcode/parse input)]
    (->> (combo/permutations (range 5 10))
         (map (fn [setting]
                (let [chans   (cycle (map (fn [_] (chan 10000)) setting))
                      states  (vec (map-indexed (fn [i s]
                                                  (let [in  (nth chans i)
                                                        out (nth chans (inc i))]
                                                    (>!! in s)
                                                    {:intcode/prog prog
                                                     :intcode/in   in
                                                     :intcode/out  out}))
                                                setting))
                      _       (>!! (first chans) 0)
                      futures (mapv (fn [state]
                                      (future
                                        (intcode/run state))) states)]
                  (<!! (:intcode/out (last (map deref futures)))))))
         (apply max))))
