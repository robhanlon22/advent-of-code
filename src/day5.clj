(ns day5
  (:require
    [clojure.java.io :as io]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [clojure.test :as t :refer [deftest is]]
    [com.gfredericks.test.chuck.generators :as gen']))


(def ^:private actual-input
  (slurp (io/resource "day5-actual.txt")))


(def ^:private boarding-pass-pattern
  "[FBLR]{1,63}")


(def ^:private boarding-passes-pattern
  (format "(%s\n)+" boarding-pass-pattern boarding-pass-pattern))


(def ^:private chars->bits
  {\F 0
   \B 1
   \L 0
   \R 1})


(defn boarding-pass->seat-id
  [boarding-pass]
  (Long/parseLong (str/join (map chars->bits boarding-pass)) 2))


(defn ^:private boarding-pass-spec
  [pattern]
  (s/with-gen (s/and string?
                     #(re-matches (re-pattern (format "^%s$" pattern)) %))
              #(gen'/string-from-regex (re-pattern pattern))))


(s/def ::boarding-pass
  (boarding-pass-spec boarding-pass-pattern))


(s/def ::seat-id
  nat-int?)


(defn ^:private in-range?
  [v]
  (s/int-in-range? 0
                   (Math/pow 2 (count (:boarding-pass (:args v))))
                   (:ret v)))


(s/fdef boarding-pass->seat-id
        :args (s/cat :boarding-pass ::boarding-pass)
        :ret  ::seat-id
        :fn   in-range?)


(s/def ::boarding-passes
  (boarding-pass-spec boarding-passes-pattern))


(defn boarding-passes->seat-ids
  [boarding-passes]
  (map boarding-pass->seat-id (str/split boarding-passes #"\n")))


(s/fdef boarding-passes->seat-ids
        :args (s/cat :boarding-passes ::boarding-passes)
        :ret  (s/coll-of ::seat-id))


(defn part-1
  [boarding-passes]
  (apply max (boarding-passes->seat-ids boarding-passes)))


(s/def ::boarding-passes->seat-id
  (s/fspec :args (s/cat :boarding-passes ::boarding-passes)
           :ret  ::seat-id))


(s/def `~part-1 ::boarding-passes->seat-id)


(defn part-2
  [boarding-passes]
  (reduce
    (fn [l r]
      (if (= (- r l) 2)
        (reduced (inc l))
        r))
    (sort (boarding-passes->seat-ids boarding-passes))))


(s/def `~part-2 ::boarding-passes->seat-id)


(deftest day5-test
  (is (= 357 (boarding-pass->seat-id "FBFBBFFRLR")))
  (is (= 567 (boarding-pass->seat-id "BFFFBBFRRR")))
  (is (= 119 (boarding-pass->seat-id "FFFBBBFRRR")))
  (is (= 820 (boarding-pass->seat-id "BBFFBBFRLL")))
  (is (= 994 (part-1 actual-input)))
  (is (= 741 (part-2 actual-input))))
