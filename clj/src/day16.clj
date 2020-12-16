(ns day16
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as str]))


(defn read-vec
  [v]
  (edn/read-string (format "[%s]" v)))


(def sample
  (slurp (io/resource "day16-sample.txt")))


(def sample2
  (slurp (io/resource "day16-sample2.txt")))


(def actual
  (slurp (io/resource "day16-actual.txt")))


(defn parse-rules-section
  [rules-section]
  (into {} (map (fn [rule]
                  (let [[_ place range-1-lhs range-1-rhs range-2-lhs range-2-rhs]
                        (re-matches #"^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$" rule)]
                    [place [[(edn/read-string range-1-lhs)
                             (edn/read-string range-1-rhs)]
                            [(edn/read-string range-2-lhs)
                             (edn/read-string range-2-rhs)]]]))
                (str/split-lines rules-section))))


(defn parse-your-ticket-section
  [your-ticket-section]
  (let [[_ your-ticket] (str/split-lines your-ticket-section)]
    (read-vec your-ticket)))


(defn parse-nearby-tickets-section
  [nearby-tickets-section]
  (let [[_ & nearby-tickets] (str/split-lines nearby-tickets-section)]
    (mapv read-vec nearby-tickets)))


(defn parse
  [input]
  (let [[rules-section your-ticket-section nearby-tickets-section] (str/split input #"\n\n")]
    {:rules          (parse-rules-section rules-section)
     :your-ticket    (parse-your-ticket-section your-ticket-section)
     :nearby-tickets (parse-nearby-tickets-section nearby-tickets-section)}))


(defn part1
  [input]
  (let [{:keys [rules nearby-tickets]} (parse input)]
    (reduce
      +
      (flatten
        (map (fn [nearby-ticket]
               (filter
                 (fn [value]
                   (not-any?
                     (fn [[lhs rhs]]
                       (<= lhs value rhs))
                     (apply concat (vals rules))))
                 nearby-ticket))
             nearby-tickets)))))


(defn valid-nearby-tickets
  [rules nearby-tickets]
  (filter (fn [nearby-ticket]
            (every?
              (fn [value]
                (some
                  (fn [[lhs rhs]]
                    (<= lhs value rhs))
                  (apply concat (vals rules))))
              nearby-ticket))
          nearby-tickets))


(defn transpose
  [all-tickets]
  (apply mapv vector all-tickets))


(defn all-possibilities
  [rules all-tickets]
  (reduce-kv
    (fn [a idx row]
      (reduce-kv
        (fn [b rule ranges]
          (if (every? (fn [value]
                        (some (fn [[lhs rhs]]
                                (<= lhs value rhs))
                              ranges)) row)
            (update b idx conj rule)
            b))
        (assoc a idx #{})
        rules))
    {}
    (transpose all-tickets)))


(defn part2
  [input]
  (let [notes                   (parse input)
        all-tickets             (conj (valid-nearby-tickets (:rules notes)
                                                            (:nearby-tickets notes))
                                      (:your-ticket notes))
        possibilities           (all-possibilities (:rules notes) all-tickets)
        in-cardinality-order    (reverse (sort-by #(count (last %)) possibilities))
        adjacent-pairs          (partition 2 1 in-cardinality-order)
        differences-by-position (reduce
                                  (fn [acc [[position lhs] [_ rhs]]]
                                    (assoc acc position (set/difference lhs rhs)))
                                  {}
                                  adjacent-pairs)

        departures (keys shrunk)]
    (reduce * (vals (select-keys (:your-ticket notes) departures)))))
