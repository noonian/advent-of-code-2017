(ns com.jedclinger.advent-of-code-2017.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(defn string->row [s]
  (map edn/read-string (string/split (string/trim s) #"\s+")))

(defn string->rows [s]
  (map string->row (string/split-lines s)))

(defn difference [row]
  (- (apply max row)
     (apply min row)))

(defn checksum [rows]
  (reduce + (map difference rows)))

(defn divisible? [a b]
  (zero? (rem a b)))

(defn find-divisor [n nums]
  (let [divisor (first (filter (partial divisible? n) nums))]
    (when divisor
      [n divisor])))

(defn find-divisible [nums]
  (let [divisible-nums #(find-divisor % (remove (partial = %) nums))]
    (some divisible-nums nums)))

(defn difference2 [row]
  (apply / (find-divisible row)))

(defn checksum-part2 [rows]
  (reduce + (map difference2 rows)))

(comment

  (def input (string/trim (slurp (io/resource "day2.txt"))))
  (checksum (string->rows input))
  (checksum-part2 (string->rows input))

  )
