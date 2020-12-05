(ns robhanlon22.aoc2020.day4
  (:require
    [clojure.java.io :as io]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [spec-tools.core :as t]))


(def sample-input
  (slurp (io/resource "day4-sample.txt")))


(def actual-input
  (slurp (io/resource "day4-actual.txt")))


(defn input->passport-strs
  [input]
  (str/split input #"\n\n"))


(s/fdef input->passport-strs
        :args (s/cat :input string?)
        :ret  (s/coll-of string?))


(defn passport-str->passport-attrs
  [passport-str]
  (into {}
        (map #(let [[k v] (str/split % #":")] [(keyword k) v]))
        (str/split passport-str #"\s")))


(s/fdef passport-str->passport-attrs
        :args (s/cat :passport-str string?)
        :ret  (s/map-of keyword? any?))


(s/def ::byr
  (s/int-in 1920 (inc 2002)))


(s/def ::iyr
  (s/int-in 2010 (inc 2020)))


(s/def ::eyr
  (s/int-in 2020 (inc 2030)))


(defmulti hgt :unit)


(s/def :robhanlon22.aoc2020.day4.cm/height
  (s/int-in 150 (inc 193)))


(s/def :robhanlon22.aoc2020.day4.cm/unit
  #{"cm"})


(defmethod hgt "cm"
  [_]
  (s/keys :req-un [:robhanlon22.aoc2020.day4.cm/height
                   :robhanlon22.aoc2020.day4.cm/unit]))


(s/def :robhanlon22.aoc2020.day4.in/height
  (s/int-in 59 (inc 76)))


(s/def :robhanlon22.aoc2020.day4.in/unit
  #{"in"})


(defmethod hgt "in"
  [_]
  (s/keys :req-un [:robhanlon22.aoc2020.day4.in/height
                   :robhanlon22.aoc2020.day4.in/unit]))


(s/def ::height
  int?)


(s/def ::unit
  string?)


(s/def ::hgt
  (t/spec
    {:spec
     (s/and (s/keys :req-un [::height ::unit]) (s/multi-spec hgt :hgt))
     :decode/string
     (fn [_ v]
       (if-let [m (re-matches #"^(\d+)([a-z]+)$" v)]
         (let [[_ height unit] m]
           {:height (Integer/parseInt height)
            :unit   unit})
         v))}))


(s/def ::hcl
  (s/and string? #(re-matches #"^#[0-9a-f]{6}$" %)))


(s/def ::ecl
  #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})


(s/def ::pid
  (s/and string? #(re-matches #"^[0-9]{9}$" %)))


(s/def ::cid
  any?)


(s/def ::passport
  (t/spec
    {:spec
     (s/keys :req-un [::ecl ::pid ::eyr ::hcl ::byr ::iyr ::hgt]
             :opt-un [::cid])
     :decode/string
     (fn [_ v] (passport-str->passport-attrs v))}))


(s/def ::passports
  (t/spec
    {:spec
     (s/coll-of ::passport)
     :decode/string
     (fn [_ v] (input->passport-strs v))}))


(defn input->passports
  [input]
  (as-> input v
        (t/coerce ::passports v t/string-transformer)
        (filter #(s/valid? ::passport %) v)))


(s/fdef input->passports
        :args (s/cat :input string?)
        :ret  ::passports)


(defn solution
  [input]
  (count (input->passports input)))


(s/fdef solution
        :args (s/cat :input string?)
        :ret  nat-int?)


(comment
  (solution sample-input)
  (solution actual-input))
