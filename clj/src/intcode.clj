(ns intcode
  (:require
    [clojure.core.async :refer [chan >!! <!!]]
    [clojure.edn :as edn]
    [clojure.spec.alpha :as s]))


(s/def ::prog (s/coll-of int? :kind vector?))
(s/def ::in (s/coll-of any? :kind vector?))
(s/def ::out (s/coll-of any? :kind vector?))
(s/def ::ptr nat-int?)
(s/def ::halt? boolean?)
(s/def ::base int?)


(s/def ::state
  (s/keys :req [::prog ::in ::out ::ptr]
          :opt [::halt?]))


(defn opcode
  [state]
  (mod (nth (::prog state) (::ptr state)) 100))


(defn param
  [{::keys [prog ptr]} n]
  (let [op   (nth prog ptr)
        p    (nth prog (+ ptr n))
        mode (-> op
                 (quot (int (Math/pow 10 (inc n))))
                 (mod 10))]
    (case mode
      0 (nth prog p)
      1 p
      2)))


(defn dest
  [{::keys [prog ptr]} n]
  (nth prog (+ ptr n)))


(defmulti exec opcode)

;; add
(defmethod exec 1
  [{::keys [prog ptr] :as state}]
  (let [lhs (param state 1)
        rhs (param state 2)
        d   (dest state 3)]
    (assoc state
           ::prog (assoc prog d (+ lhs rhs))
           ::ptr (+ ptr 4))))

;; multiply
(defmethod exec 2
  [{::keys [prog ptr] :as state}]
  (let [lhs (param state 1)
        rhs (param state 2)
        d   (dest state 3)]
    (assoc state
           ::prog (assoc prog d (* lhs rhs))
           ::ptr (+ 4 ptr))))

;; in
(defmethod exec 3
  [{::keys [prog ptr in] :as state}]
  (let [d (dest state 1)]
    (assoc state
           ::prog (assoc prog d (<!! in))
           ::ptr  (+ ptr 2))))

;; out
(defmethod exec 4
  [{::keys [ptr out] :as state}]
  (let [p (param state 1)]
    (>!! out p)
    (assoc state ::ptr (+ ptr 2))))


;; jump if zero
(defmethod exec 5
  [{::keys [ptr] :as state}]
  (let [i (param state 1)
        d (param state 2)]
    (assoc state
           ::ptr (if (= i 0) (+ ptr 3) d))))

;; jump if not zero
(defmethod exec 6
  [{::keys [ptr] :as state}]
  (let [i (param state 1)
        d (param state 2)]
    (assoc state
           ::ptr (if (= i 0) d (+ ptr 3)))))

;; less than
(defmethod exec 7
  [{::keys [prog ptr] :as state}]
  (let [lhs (param state 1)
        rhs (param state 2)
        d   (dest state 3)]
    (assoc state
           ::prog (assoc prog d (if (< lhs rhs) 1 0))
           ::ptr (+ ptr 4))))


;; equal
(defmethod exec 8
  [{::keys [prog ptr] :as state}]
  (let [lhs (param state 1)
        rhs (param state 2)
        d   (dest state 3)]
    (assoc state
           ::prog (assoc prog d (if (= lhs rhs) 1 0))
           ::ptr (+ ptr 4))))


(defmethod exec 9
  [{::keys [prog ptr] :as state}])

;; halt
(defmethod exec 99
  [state]
  (assoc state ::halt? true))


(defn parse
  [input]
  (edn/read-string (format "[%s]" input)))


(defn run
  [{::keys [prog in out]}]
  (loop [state {::prog prog
                ::ptr  0
                ::in   in
                ::out  out}]
    (let [state' (exec state)]
      (if (::halt? state')
        state'
        (recur state')))))
