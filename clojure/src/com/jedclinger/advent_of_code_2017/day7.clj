(ns com.jedclinger.advent-of-code-2017.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]))

(defn parse-program [s]
  (let [[left right] (str/split s #"\s->\s")
        [pname weight] (str/split left #"\s")
        weight (Integer/parseInt (str/replace weight #"\(|\)" ""))]
    (cond-> {:name pname
             :weight weight}
      right (assoc :children (set (str/split right #",\s"))))))

(defn find-root [programs]
  (first
   (reduce (fn [candidates p] (set/difference candidates (:children p)))
           (set (map :name programs))
           programs)))

(defn check-balance [children]
  (let [totals (set/map-invert (frequencies (mapv :total children)))
        balanced? (= 1 (count totals))]
    (cond-> {:balanced? balanced?}
      (and (not balanced?) (every? :balanced? children))
      (assoc :correct-weight
        (let [bad-total (get totals 1)
              good-total (first (vals (dissoc totals 1)))
              bad-node (first (filter (comp (partial = bad-total) :total) children))]
          (- good-total (- bad-total (:weight bad-node))))))))

(defn annotate [{:keys [children weight] :as node}]
  (if-not children
    (assoc node :balanced? true)
    (merge node (check-balance children))))

(defn build [lookup pname]
  (let [{:keys [weight children name] :as node} (lookup pname)]
    (if-not children
      (assoc node :total weight :balanced? true)
      (let [children (mapv (partial build lookup) children)]
        (annotate
         (assoc node
           :children children
           :total (+ weight (reduce + 0 (map :total children)))))))))

(defn build-tree [programs]
  (let [lookup (into {} (for [p programs] [(:name p) p]))
        root-name (find-root programs)]
    (build lookup root-name)))

(defn weight-adjustment [tree]
  (some :correct-weight (tree-seq :children :children tree)))

(comment

  (def example
    "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)")

  (def input (str/trim (slurp (io/resource "day7.txt"))))

  (def programs (mapv parse-program (str/split-lines example)))
  (def programs (mapv parse-program (str/split-lines input)))
  (find-root programs)
  (def tree (build-tree programs))
  (pprint tree)
  (weight-adjustment tree)

  )
