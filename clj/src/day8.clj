(ns day8
  (:require
    [clojure.edn]
    [clojure.java.io :as io]))


(def sample
  (io/resource "day8-sample.txt"))


(def actual
  (io/resource "day8-actual.txt"))


(defmulti run
  (fn [instruction _value _acc _ptr]
    instruction))


(defmethod run 'nop
  [_ _value acc ptr]
  [acc (inc ptr)])


(defmethod run 'acc
  [_ value acc ptr]
  [(+ acc value) (inc ptr)])


(defmethod run 'jmp
  [_ value acc ptr]
  [acc (+ ptr value)])


(def eof (Object.))


(defn read-instruction
  [reader]
  (clojure.edn/read {:eof eof} reader))


(defn read-program
  [source]
  (with-open [r (java.io.PushbackReader. (io/reader source))]
    (let [read (partial read-instruction r)]
      (loop [program []]
        (let [instruction (read)
              value       (read)]
          (if (or (= instruction eof) (= value eof))
            program
            (recur (conj program [instruction value]))))))))


(defn ^:private exec
  [program]
  (loop [visited #{}
         acc     0
         ptr     0]
    (cond
      (contains? visited ptr) [::infinite acc]
      (= ptr (count program)) [::done acc]
      (> ptr (count program)) [::illegal acc]
      :else
      (let [[inst v]    (nth program ptr)
            [acc' ptr'] (run inst v acc ptr)]
        (recur (conj visited ptr) acc' ptr')))))


(defn part1
  [source]
  (last (exec (read-program source))))


(defn part2
  [source]
  (let [program (read-program source)]
    (reduce-kv
      (fn [_ idx [inst v]]
        (let [inst'        (case inst
                             nop 'jmp
                             jmp 'nop
                             inst)
              [status acc] (exec (assoc program idx [inst' v]))]
          (when (= status ::done)
            (reduced acc))))
      nil
      program)))
