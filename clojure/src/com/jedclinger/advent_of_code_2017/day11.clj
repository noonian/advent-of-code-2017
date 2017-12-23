(ns com.jedclinger.advent-of-code-2017.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def simplification-rules
  {#{"ne" "s"} "se"
   #{"se" "n"} "ne"
   #{"sw" "se"} "s"
   #{"nw" "ne"} "n"
   #{"sw" "n"} "nw"
   #{"nw" "s"} "sw"})

(defn applicable-rule [directions]
  (let [dirs (set directions)]
    (first (filter (fn [[rule replacement]] (set/subset? rule dirs))
                   simplification-rules))))

(defn remove-set [s coll]
  (if (or (not (seq s)) (not (seq coll)))
    coll
    (let [ele (first s)
          [left right] (split-with (partial not= ele) coll)]
      (recur (rest s)
             (concat left (rest right))))))

(defn simplify-1 [[rule replacement] directions]
  (conj (remove-set rule directions) replacement))

(defn simplify [directions]
  (if-let [simplification (applicable-rule directions)]
    (recur (simplify-1 simplification directions))
    directions))

(defn directions [s] (str/split s #","))

(defn max-distance [directions]
  (loop [res 0
         dirs []
         others directions]
    (if-not (seq others)
      res
      (let [simplified (simplify dirs)]
        (recur (max res (count simplified))
               (conj simplified (first others))
               (rest others))))))

(comment

  (simplify (directions "se,sw,se,sw,sw"))
  (applicable-rule ["s" "ne" "ne"])

  (def input (str/trim (slurp (io/resource "day11.txt"))))

  ;; part1
  (count (simplify (directions input)))

  ;; part2
  (max-distance (directions input))

  )
