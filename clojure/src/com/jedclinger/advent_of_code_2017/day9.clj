(ns com.jedclinger.advent-of-code-2017.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]))

;; TODO: clean this up
(def grammar "
  S = GROUP
  GROUP = <'{'> [ CHILDREN ] <'}'>
  <ELEMENT> = GROUP | GARBAGE
  <CHILDREN> = ELEMENT [ <','> ELEMENT ]*
  GARBAGE = <'<'> GARBAGE_CONTENT* <'>'>
  C = #'[a-zA-Z<>\"{},]' | \"'\"
  CANCELED_C = C | '!'
  <CANCELED> = '!' CANCELED_C
  <GARBAGE_CONTENT> = #'[a-zA-Z<\"{},]+' | <CANCELED> | \"'\"
  ")

(def p (insta/parser grammar :output-format :enlive))

(defn group? [{:keys [tag]}] (= tag :GROUP))
(defn garbage? [{:keys [tag]}] (= tag :GARBAGE))

(defn sum-scores
  ([group] (sum-scores 0 group))
  ([parent-score group]
   (let [score (inc parent-score)]
     (if-let [content (filter group? (:content group))]
       (+ score (reduce + 0 (map (partial sum-scores score) content)))
       score))))

(defn count-garbage [{:keys [content] :as group}]
  (reduce + 0 (->> (tree-seq :content :content group)
                   (filter garbage?)
                   (map #(count (apply str (:content %)))))))

(comment

  (def input (str/trim (slurp (io/resource "day9.txt"))))
  (sum-scores (-> (p input) :content first))
  (count-garbage (-> (p input) :content first))

  )
