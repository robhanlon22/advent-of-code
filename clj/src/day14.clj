(ns day14
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as str]))


(def sample
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")


(def actual
  (slurp (io/resource "day14.txt")))


(def or-mask)


(def and-mask)


(def default-state
  {:masks {:and 2r111111111111111111111111111111111111
           :or  2r000000000000000000000000000000000000}
   :memory {}})


(def default-state-v2
  {:mask   "000000000000000000000000000000000000"
   :memory {}})


(defn parse
  [input]
  (let [lines (str/split-lines input)]
    (map (fn [line]
           (if-let [[_ mask] (re-matches #"^mask = ([X01]{36})$" line)]
             {:type :mask
              :and  (edn/read-string (str "2r" (str/replace mask "X" "1")))
              :or   (edn/read-string (str "2r" (str/replace mask "X" "0")))}
             (if-let [[_ address value] (re-matches #"^mem\[(\d+)\] = (\d+)$" line)]
               {:type    :mem
                :address (edn/read-string address)
                :value   (edn/read-string value)}
               (throw (ex-info "Invalid line" {:line  line :lines lines})))))
         lines)))


(defn binarize
  [num-str]
  (str/replace (format "%36s" (Long/toBinaryString (edn/read-string num-str))) " " "0"))


(defn parse-v2
  [input]
  (let [lines (str/split-lines input)]
    (map (fn [line]
           (if-let [[_ mask] (re-matches #"^mask = ([X01]{36})$" line)]
             {:type :mask
              :mask mask}
             (if-let [[_ address value] (re-matches #"^mem\[(\d+)\] = (\d+)$" line)]
               {:type    :mem
                :address (binarize address)
                :value   (edn/read-string value)}
               (throw (ex-info "Invalid line" {:line  line :lines lines})))))
         lines)))


(defn inflate
  [address mask]
  (let [f (fn f
            [[pair & pairs] current]
            (if-let [[address-bit mask-bit] pair]
              (let [bits (case mask-bit
                           \0 [address-bit]
                           \1 [\1]
                           \X [\0 \1])]
                (mapcat #(f pairs (conj current %)) bits))
              [(edn/read-string (apply str "2r" current))]))]
    (f (map vector address mask) [])))


(defn run
  [instructions]
  (reduce (fn [state instruction]
            (case (:type instruction)
              :mask (assoc state :masks (select-keys instruction [:and :or]))
              :mem  (let [masks (:masks state)]
                      (->> instruction
                           :value
                           (bit-and (:and masks))
                           (bit-or (:or masks))
                           (assoc-in state [:memory (:address instruction)])))))
          default-state
          instructions))


(defn run-v2
  [instructions]
  (reduce (fn [state instruction]
            (case (:type instruction)
              :mask (assoc state :mask (:mask instruction))
              :mem  (let [addresses (inflate (:address instruction) (:mask state))]
                      (update state
                              :memory
                              (fn [memory]
                                (reduce (fn [acc address]
                                          (assoc acc address (:value instruction)))
                                        memory
                                        addresses))))))
          default-state-v2
          instructions))


(defn part1
  [input]
  (->> input
       parse
       run
       :memory
       vals
       (reduce +)))


(defn part2
  [input]
  (->> input
       parse-v2
       run-v2
       :memory
       vals
       (reduce +)))
