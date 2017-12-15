(ns com.jedclinger.advent-of-code-2017.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [spec-tools.spec :as spec]
            [spec-tools.core :as st])
  (:refer-clojure :exclude [compare]))

(s/def ::register-name string?)
(s/def ::op-name #{"inc" "dec"})
(s/def ::value spec/integer?)
(s/def ::operation (s/cat :reg ::register-name
                          :op ::op-name
                          :arg ::value))
(s/def ::comparator #{">" "<" ">=" "<=" "==" "!="})
(s/def ::condition (s/cat :if #{"if"}
                          :reg ::register-name
                          :test ::comparator
                          :target ::value))
(s/def ::raw-instruction (s/cat :operation ::operation :condition ::condition))

(defn tokenize [s] (str/split s #"\s+"))

(defn parse-instruction [s]
  (st/conform ::raw-instruction (tokenize s) st/string-conforming))

(defn parse-program [s]
  (->> s str/trim str/split-lines (map parse-instruction)))

(def comparators
  {">" >
   "<" <
   "<=" <=
   ">=" >=
   "!=" not=
   "==" =})

(def operations {"inc" +, "dec" -})

(defn reg-value [registers k] (get registers k 0))

(defn apply-op [registers {:keys [reg op arg]}]
  (let [f (get operations op)
        new-val (f (reg-value registers reg) arg)]
    (-> (assoc registers reg new-val)
        (vary-meta (fn [m] (if (> new-val (:highest m 0)) (assoc m :highest new-val) m))))))

(defn test-condition [registers {:keys [reg test target]}]
  (let [compare (get comparators test)
        value (get registers reg 0)]
    (compare value target)))

(defn process [registers {:keys [operation condition]}]
  (if (test-condition registers condition)
    (apply-op registers operation)
    registers))

(defn evaluate
  ([program] (evaluate {} program))
  ([registers program] (reduce process registers program)))

(comment

  (def program
    (parse-program
     "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"))

  (def program (parse-program (slurp (io/resource "day8.txt"))))

  (def result (evaluate program))

  result

  ;; part1
  (apply max (vals result))

  ;; part2
  (:highest (meta result))

  )
