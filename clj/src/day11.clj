(ns day11
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [trikl.core :as t]))


(def sample
  (slurp (io/resource "part11-sample.txt")))


(def actual
  (slurp (io/resource "part11-actual.txt")))


(def p
  (for [i     (range -1 2)
        j     (range -1 2)
        :when (not (and (zero? i) (zero? j)))]
    [i j]))


(defn neighbors
  [grid [r c]]
  (vec (filter identity (map (fn [[i j]] (get grid [(+ i r) (+ j c)])) p))))


(def occupied
  \#)


(defn occupied?
  [c]
  (= c occupied))


(def floor \.)


(defn floor?
  [c]
  (= c floor))


(def unoccupied
  \L)


(defn unoccupied?
  [c]
  (or (= c unoccupied)))


(defn occupied-neighbors
  [grid k]
  (count (filter occupied? (neighbors grid k))))


(defn parse
  [input]
  (let [grid (mapv vec (str/split-lines input))]
    (into {}
          (map (fn [c] [c (get-in grid c)]))
          (for [i (range 0 (count grid))
                j (range 0 (count (get grid i)))]
            [i j]))))


(defn next-grid-1
  [grid]
  (let [grid' (transient grid)]
    (doseq [[k seat] grid]
      (when-not (floor? seat)
        (let [on (occupied-neighbors grid k)]
          (cond
            (and (unoccupied? seat) (zero? on)) (assoc! grid' k occupied)
            (and (occupied? seat) (>= on 4))    (assoc! grid' k unoccupied)))))
    (persistent! grid')))


[-1 -1]
[-1 -1]

[(+ -1 -1) (+ -1 -1)]
[(+ -1 -2) (+ -1 -2)]


(take 10 (for [i (iterate dec -1)
               j (repeat 0)]
           [i j]))


(def rays
  (map (fn [coord]
         (iterate
           (fn [dcoord]
             (map + coord dcoord))
           coord))
       p))


(defn visible
  [grid c]
  (reduce
    (fn [a ray]
      (reduce
        (fn [b d]
          (let [c' (get grid (map + c d))]
            (cond
              (floor? c') b
              (nil? c')   (reduced b)
              :else       (reduced (conj b c')))))
        a
        ray))
    []
    rays))


(defn count-occupied
  [vs]
  (count (filter occupied? vs)))


(defn occupied-visible
  [grid k]
  (count-occupied (visible grid k)))


(defn next-grid-2
  [grid]
  (let [r (atom {})]
    (->> grid
         (mapv
           (fn [[k seat]]
             (future
               (let [seat'
                     (if (floor? seat)
                       seat
                       (let [ov (occupied-visible grid k)]
                         (cond
                           (and (unoccupied? seat) (zero? ov)) occupied
                           (and (occupied? seat) (>= ov 5))    unoccupied
                           :else                               seat)))]
                 (swap! r assoc k seat')))))
         (mapv deref))
    @r))


(def client
  (t/stdio-client))


(defn render-grid
  [grid]
  (t/render client (mapv (fn [[[y x] v]]
                           [:box {:x      x
                                  :y      y
                                  :width  1
                                  :height 1
                                  :styles {:bg (case v
                                                 \. [0 0 0]
                                                 \# [150 0 0]
                                                 \L [150 150 150])}}])
                         grid)))


(defn solution
  [input next-grid]
  (let [grid  (parse input)
        _     (render-grid grid)
        grid' (loop [grid grid]
                (let [grid' (next-grid grid)]
                  (render-grid grid')
                  (if (= grid grid')
                    grid
                    (recur grid'))))]
    (count-occupied (vals grid'))))


(defn -main
  [& _]
  (solution actual next-grid-2))
