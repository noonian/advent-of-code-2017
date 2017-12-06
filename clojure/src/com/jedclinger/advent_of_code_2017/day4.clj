(ns com.jedclinger.advent-of-code-2017.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn words [passphrase] (string/split passphrase #"\s+"))

(defn valid? [passphrase]
  (let [words (frequencies (words passphrase))]
    (not-any? #(> % 1) (vals words))))

(defn anagram? [a b] (= (frequencies a) (frequencies b)))
(defn anagram-of? [a] (partial anagram? a))

(defn remove-one [pred coll]
  (loop [acc []
         coll coll]
    (cond
      (not (seq coll)) acc
      (pred (first coll)) (into acc (rest coll))
      :else (recur (conj acc (first coll)) (rest coll)))))

(defn contains-anagrams? [passphrase]
  (let [words (words passphrase)
        f (fn [w] (some (anagram-of? w) (remove-one (partial = w) words)))]
    (some f words)))

(def anagram-free? (complement contains-anagrams?))

(comment

  (def input (string/trim (slurp (io/resource "day4.txt"))))
  (def passphrases (string/split-lines input))

  (count (filter valid? passphrases))
  (count (filter anagram-free? passphrases))

  )
