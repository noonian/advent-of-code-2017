(ns com.jedclinger.advent-of-code-2017.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]))

(defn twist [list pos skip length]
  (let [n (count list)
        [base others] (split-at pos (cycle list))
        [sublist tail] (split-at length others)
        reversed (reverse sublist)
        diff (- (+ (count base) (count sublist)) n)
        prefix (if-not (pos? diff)
                 base
                 (concat (take-last diff reversed) (drop diff base)))]
    (vec (take n (concat prefix reversed tail)))))

(defn round [state lengths]
  (reduce (fn [{:keys [pos skip list] :as acc} length]
            (assoc acc
              :pos (mod (+ pos length skip) (count list))
              :skip (inc skip)
              :list (twist list pos skip length)))
          state
          lengths))

(defn rounds [list lengths]
  (iterate #(round % lengths) {:pos 0 :skip 0 :list list}))

(defn tie-knot
  ([list lengths] (tie-knot list lengths 1))
  ([list lengths n]
   (-> (rounds list lengths)
       (nth n)
       :list)))

(defn hash-block [block] (apply bit-xor block))

(defn hex-str [k]
  (let [s (Integer/toHexString k)]
    (cond->> s
      (= 1 (count s)) (str "0"))))

(defn dense-hash [sparse-hash]
  (map hash-block (partition 16 sparse-hash)))

(defn to-lengths [s]
  (into [] (concat (map int s) [17 31 73 47 23])))

(defn knot-hash [s]
  (->> (tie-knot (vec (range 256)) (to-lengths s) 64)
       dense-hash
       (map hex-str)
       (apply str)))

(comment

  (twist (range 5) 0 0 3)
  (twist [2 1 0 3 4] 3 1 4)
  (twist [4 3 0 1 2] 5 2 1)
  (twist [4 3 0 1 2] 1 3 5)

  (def sparse-hash [65 27  9  1  4  3  40  50  91  7  6  0  2  5  68  22])
  (hash-block sparse-hash)

  (round {:pos 0 :skip 0 :list (range 5)} [3 4 1 7])
  (take 6 (map #(update % :list (partial take 5)) (rounds (range 256) (to-lengths ""))))

  (def input (-> "day10.txt" io/resource slurp str/trim))
  (def lengths (map #(Integer/parseInt %) (str/split input #",")))
  (apply * ((juxt first second) (tie-knot (range 256) lengths)))

  ;; part2
  (knot-hash "")
  (println (knot-hash input))

  )
