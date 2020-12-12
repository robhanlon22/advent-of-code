(ns day11-2
  (:require
    [clojure.java.io :as io]
    [clojure.string :as s]))


(def data (s/split-lines (slurp (io/resource "part11-actual.txt"))))

(def coords (for [x (range (count data)) y (range (count (data 0)))] [x y]))

(def state-init (into {} (for [[x y] coords] [[x y] (get (data x) y)])))

(def adjacent (remove #{[0 0]} (for [i (range -1 2) j (range -1 2)] [i j])))


(defn watch
  [m dir p start]
  (cond
    (not (contains? m p))             0
    (and (= \L (m p)) (not= p start)) 0
    (and (= (m p) \#) (not= p start)) 1
    :else                             (watch m dir (map + p dir) start)))


(defn count-occ
  [seats p]
  (let [neighbours (map (partial map + p) adjacent)]
    (count (filter #{\#} (map seats neighbours)))))


(defn count-occ-2
  [seats p]
  (reduce + (map #(watch seats % p p) adjacent)))


(defn update-seat
  [g p]
  (let [n (count-occ g p)] ;; count-occ for part 1
    (condp = (g p)
      \L (if (zero? n) \# (g p))
      \# (if (>= n 4) \L (g p)) ;; (>= n 4) for part 1
      (g p))))


(defn update-plane
  [m]
  (into {} (map (fn [p] [p (update-seat m p)]) coords)))


(defn solution
  [old new]
  (if (= old new)
    (count (filter #{\#} (vals new)))
    (recur new (update-plane new))))
