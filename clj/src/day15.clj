(ns day15
  (:require
    [clojure.edn :as edn]
    [clojure.test :refer [is]]))


(def ^:dynamic *n*)

(def sample "0,3,6")

(def actual "1,0,18,10,19,6")


(defn solve
  [input n]
  (let [numbers (edn/read-string (format "[%s]" input))
        spoken  (reduce-kv (fn [acc i v]
                             (let [turn (inc i)]
                               (assoc acc v [turn turn])))
                           {}
                           numbers)]
    (loop [prev   (last numbers)
           spoken spoken
           turn   (inc (count numbers))]
      (let [number (reduce - (spoken prev))]
        (if (= turn n)
          number
          (recur number
                 (assoc spoken
                        number
                        [turn (get-in spoken [number 0] turn)])
                 (inc turn)))))))


(is (= 436 (part1 "0,3,6" 2020)))
(is (= 1 (part1 "1,3,2" 2020)))
(is (= 10 (part1 "2,1,3" 2020)))
(is (= 27 (part1 "1,2,3" 2020)))
(is (= 78 (part1 "2,3,1" 2020)))
(is (= 438 (part1 "3,2,1" 2020)))
(is (= 1836 (part1 "3,1,2" 2020)))


(is (= 175594 (part1 "0,3,6" 30000000)))
