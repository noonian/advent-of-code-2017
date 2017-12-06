(ns com.jedclinger.advent-of-code-2017.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn left [[x y]] [(dec x) y])
(defn right [[x y]] [(inc x) y])
(defn up [[x y]] [x (inc y)])
(defn down [[x y]] [x (dec y)])

(defn half [side-length] (int (Math/floor (/ side-length 2))))
(defn top? [side-length [x y]] (= y (half side-length)))
(defn bottom? [side-length [x y]] (= y (- (half side-length))))
(defn left? [side-length [x y]] (= x (- (half side-length))))
(defn right? [side-length [x y]] (= x (half side-length)))

(defn every-fn [& fns]
  (let [f (apply juxt fns)]
    (fn [& args]
      (every? identity (apply f args)))))

(def top-right? (every-fn top? right?))
(def top-left? (every-fn top? left?))
(def bottom-left? (every-fn bottom? left?))
(def bottom-right? (every-fn bottom? right?))

(defn spiral*
  "Return the cartesian coordinates of an index in spiral memory."
  [index]
  (loop [i 1
         pos [0 0]
         side-length 1
         current-direction right]
    (cond
      (= i index) pos

      (bottom-right? side-length pos)
      (recur (inc i) (right pos) (+ side-length 2) up)

      (top-left? side-length pos)
      (recur (inc i) (down pos) side-length down)

      (bottom-left? side-length pos)
      (recur (inc i) (right pos) side-length right)

      (top-right? side-length pos)
      (recur (inc i) (left pos) side-length left)

      :else
      (recur (inc i) (current-direction pos) side-length current-direction))))

(def spiral (memoize spiral*))

(defn manhattan-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn neighbors [pos]
  [(right pos)
   (up (right pos))
   (up pos)
   (left (up pos))
   (left pos)
   (down (left pos))
   (down pos)
   (right (down pos))])

(defn compute-value
  "Given the already filled in values, return the value for the given
  position"
  [values pos]
  (reduce + 0 (vals (select-keys values (neighbors pos)))))

(defn spiral-value [index]
  (loop [i 1
         values {}]
    (let [pos (spiral i)]
      (cond
        (= index 1) 1
        (= i 1) (recur (inc i) (assoc values pos 1))
        (= i index) (compute-value values pos)

        :else
        (recur (inc i) (assoc values pos (compute-value values pos)))))))

(comment

  (def input 347991)

  ;; part 1
  (manhattan-distance (spiral input))

  ;; part 2
  (first (filter #(> % input) (map spiral-value (drop 1 (range)))))

  )
