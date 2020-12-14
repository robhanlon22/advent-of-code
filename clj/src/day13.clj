(ns day13
  (:require
    [clojure.core.logic :as logic]
    [clojure.core.logic.fd :as fd]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as str]))


(def actual
  (slurp (io/resource "day13.txt")))


(defn parse
  [input]
  (let [[timestamp schedules] (str/split-lines input)
        timestamp (edn/read-string timestamp)
        schedules (->> (str/split schedules #",")
                       (map edn/read-string))]
    {::timestamp timestamp
     ::schedules schedules}))


(defn part1
  [input]
  (let [{::keys [timestamp schedules]} (parse input)]
    (->> schedules
         (remove #{'x})
         (map #(vector (-> timestamp
                           (/ %)
                           Math/ceil
                           int
                           (* %)
                           (- timestamp))
                       %))
         sort
         first
         (reduce *))))


(defn part2
  [input]
  (let [pairs (->> input
                   parse
                   ::schedules
                   (map-indexed (fn [& args] args))
                   (remove (fn [[_ x]] (= x 'x)))
                   (map (fn [[i x]] [x (- x i)])))
        m     (apply max-key second pairs)]))


(def s
  (logic/run
    1 [q]
    (logic/fresh
      [x]
      (fd/in x (fd/interval 0 100000000000000000))
      (fd/eq
        (= (- x (* (/ x 1789) 1789)) 0)
        (= (- (+ x 1) (* (/ (+ x 1) 37) 37)) 0)
        (= (- (+ x 2) (* (/ (+ x 2) 47) 47)) 0)
        (= (- (+ x 3) (* (/ (+ x 3) 1889) 1889)) 0))
      (logic/== q x))))

;; (is (= 1068781 (part2 "939\n7,13,x,x,59,x,31,19")))
;; (is (= 3417 (part2 "0\n17,x,13,19")))
;; (is (= 754018 (part2 "0\n67,7,59,61")))
;; (is (= 779210 (part2 "0\n67,x,7,59,61")))
;; (is (= 1261476 (part2 "0\n67,7,x,59,61")))
;; (is (= 1202161486 (part2 "0\n1789,37,47,1889")))
