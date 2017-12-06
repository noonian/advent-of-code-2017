(ns com.jedclinger.advent-of-code-2017.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn process
  "Take a vector and an index n, processes the instructions at n
  returning an updated vector of instructions and the new current
  instruction using the provided update-instruction function to update
  each instruction along the way"
  [update-instruction [instructions n]]
  (let [instruction (nth instructions n)
        next-n (+ n instruction)]
    [(update instructions n update-instruction) next-n]))

(defn done?
  [[instructions n]]
  (or (< n 0) (>= n (count instructions))))

(defn take-until
  "Like drop-while but includes the first element that pred returns
  logical true for"
  [pred coll]
  (let [[results others] (split-with (complement pred) coll)]
    (concat results [(first others)])))

(defn process-instructions
  [instructions & [f]]
  (take-while (complement done?)
              (iterate (partial process (or f inc)) [instructions 0])))

(comment

  (process-instructions [1 3])

  (def input (string/trim (slurp (io/resource "day5.txt"))))
  (def instructions (mapv #(Integer/parseInt %) (string/split-lines input)))

  ;; part 1
  (count (process-instructions instructions))

  ;; part 2
  (defn inc-or-dec [n] (if (>= n 3) (dec n) (inc n)))
  (count (process-instructions instructions inc-or-dec))
  )
