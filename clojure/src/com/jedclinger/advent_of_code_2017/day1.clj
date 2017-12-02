(ns com.jedclinger.advent-of-code-2017.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;; http://adventofcode.com/2017/day/1

(defn as-digits [s-or-coll]
  (if (string? s-or-coll)
    (map #(Integer/parseInt (str %)) s-or-coll)
    s-or-coll))

(defn reverse-captcha [s-or-coll]
  (let [digits (as-digits s-or-coll)
        pairs (partition 2 1 (cycle digits)) ;no transducer for partition :(
        xf (comp (take (count digits))
                 (filter (partial apply =))
                 (map first))]
    (transduce xf + pairs)))

(defn reverse-captcha-part-2 [s-or-coll]
  (let [digits (as-digits s-or-coll)
        sub-seqs (partition (inc (/ (count digits) 2)) 1 (cycle digits))
        xf (comp (take (count digits))
                 (map (juxt first last))
                 (filter (partial apply =))
                 (map first))]
    (transduce xf + sub-seqs)))

(comment

  (def input (string/trim (slurp (io/resource "day1.txt"))))
  (reverse-captcha input)
  (reverse-captcha-part-2 input)

  )
