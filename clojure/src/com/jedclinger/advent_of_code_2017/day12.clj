(ns com.jedclinger.advent-of-code-2017.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [spec-tools.spec :as spec]
            [spec-tools.core :as st]))

(s/def ::program spec/integer?)
(s/def ::pipe #{"<->"})
(s/def ::neighbors (s/+ ::program))
(s/def ::mapping (s/cat :program ::program
                        :pipe ::pipe
                        :neighbors ::neighbors))

(defn tokenize [s] (str/split s #",?\s+"))
(defn parse-line [s] (st/conform ::mapping (tokenize s) st/string-conforming))

(defn build-mapping [s]
  (into {}
    (for [{:keys [program neighbors]} (map parse-line (str/split-lines s))]
      [program neighbors])))

(defn build-group [p->neighbors program]
  (loop [res #{program}
         others (set (p->neighbors program))]
    (if-not (seq others)
      res
      (let [p (first others)]
        (if (res p)
          (recur res (rest others))
          (recur (conj res p)
                 (into (rest others) (p->neighbors p))))))))

(defn groups [p->neighbors]
  (reduce (fn [res program]
            (conj res (build-group p->neighbors program)))
          #{}
          (keys p->neighbors)))

(comment

  (def mapping
    {0 #{2}
     1 #{1}
     2 #{0 3 4}
     3 #{2 4}
     4 #{2 3 6}
     5 #{6}
     6 #{4 5}})

  (build-mapping
   "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5")

  (build-group mapping 0)

  (def input (str/trim (slurp (io/resource "day12.txt"))))

  ;; part1
  (count (build-group (build-mapping input) 0))

  ;; part2
  (count (groups (build-mapping input)))

  )
