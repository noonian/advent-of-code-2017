(ns com.jedclinger.advent-of-code-2017.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn bank-to-redistribute
  "Given a vector of memory banks, return the index of the next memory
  bank to redistribute. The bank to redistribute is the bank with the
  most blocks with the bank with the lower index winning ties."
  [banks]
  [0 3]
  (first
   (reduce (fn [[i value] [next-i bank]]
             (if (> bank value)
               [next-i bank]
               [i value]))
           [0 0]
           (map-indexed vector banks))))

(defn next-index [n banks]
  (let [i (inc n)]
    (if (= i (count banks)) 0 i)))

(defn mancala [banks]
  (let [starting-i (bank-to-redistribute banks)
        stones (get banks starting-i)]
    (loop [slots (assoc banks starting-i 0)
           stones stones
           i (next-index starting-i banks)]
      (if (zero? stones)
        slots
        (recur (update slots i inc)
               (dec stones)
               (next-index i slots))))))

(defn how-many-redistribution-cycles [banks]
  (loop [seen #{}
         cur banks
         i 0]
    (if (seen cur)
      i
      (recur (conj seen cur)
             (mancala cur)
             (inc i)))))

(defn how-many-redistribution-cycles-part2 [banks]
  (loop [seen {}
         cur banks
         i 0]
    (if-let [n (seen cur)]
      (- i n)
      (recur (assoc seen cur i)
             (mancala cur)
             (inc i)))))

(comment

  (def input (str/trim (slurp (io/resource "day6.txt"))))
  (def banks (mapv #(Integer/parseInt %) (str/split input #"\s+")))

  (how-many-redistribution-cycles banks) ;=> 11137
  (how-many-redistribution-cycles-part2 banks) ;=> 1037

  )
