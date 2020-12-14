(ns day12
  (:require
    [cheshire.core :as json]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.test :refer [is]]))


(def sample
  "F10
N3
F7
R90
F11")


(def actual
  (slurp (io/resource "day12.txt")))


(defn parse
  [input]
  (mapv (fn [v]
          (let [[_ & m] (re-matches #"^([A-Z])(\d+)$" v)]
            (mapv edn/read-string m)))
        (str/split-lines input)))


(def dirs
  {0   'N
   180 'S
   90  'E
   270 'W})


(defmulti act
  (fn [_state [a _v]]
    a))


(defmethod act 'N
  [state [_a v]]
  (update state :y + v))


(defmethod act 'S
  [state [_a v]]
  (update state :y - v))


(defmethod act 'E
  [state [_a v]]
  (update state :x + v))


(defmethod act 'W
  [state [_a v]]
  (update state :x - v))


(defmethod act 'L
  [state [_a v]]
  (update state :=> #(mod (- % v) 360)))


(defmethod act 'R
  [state [_a v]]
  (update state :=> #(mod (+ % v) 360)))


(defmethod act 'F
  [state [_a v]]
  (act state [(-> state :=> dirs) v]))


(defmulti act2
  (fn [_state [a _v]]
    a))


(defmethod act2 'N
  [state [_a v]]
  (update state :waypoint-y + v))


(defmethod act2 'S
  [state [_a v]]
  (update state :waypoint-y - v))


(defmethod act2 'E
  [state [_a v]]
  (update state :waypoint-x + v))


(defmethod act2 'W
  [state [_a v]]
  (update state :waypoint-x - v))


(defmethod act2 'L
  [state [_a v]]
  (reduce (fn [{:keys [waypoint-x waypoint-y] :as s} _]
            (assoc s
                   :waypoint-x (- waypoint-y)
                   :waypoint-y waypoint-x))
          state
          (range (quot v 90))))


(defmethod act2 'R
  [state [_a v]]
  (reduce (fn [{:keys [waypoint-x waypoint-y] :as s} _]
            (assoc s
                   :waypoint-x waypoint-y
                   :waypoint-y (- waypoint-x)))
          state
          (range (quot v 90))))


(defmethod act2 'F
  [{:keys [waypoint-x waypoint-y] :as state} [_a v]]
  (reduce (fn [s _]
            (-> s
                (update :x + waypoint-x)
                (update :y + waypoint-y)))
          state
          (range v)))


(defn navigate
  [instructions]
  (reduce act {:x 0 ::y 0 ::=> 90} instructions))


(defn manhattan
  [x y]
  (+ (Math/abs x) (Math/abs y)))


(defn solution1
  [input]
  (let [{:keys [x y]} (navigate (parse input))]
    (manhattan x y)))


(defn navigate2
  [instructions]
  (reduce act2
          {:waypoint-x 10
           :waypoint-y 1
           :x          0
           :y          0}
          instructions))


(defn path2
  [instructions]
  (json/generate-string (reductions act2
                                    {:waypoint-x 10
                                     :waypoint-y 1
                                     :x          0
                                     :y          0}
                                    instructions)
                        {:pretty true}))


(defn solution2
  [input]
  (let [{:keys [x y]} (navigate2 (parse input))]
    (manhattan x y)))


(is (= 25 (solution1 sample)))
