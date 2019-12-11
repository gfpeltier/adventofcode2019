(ns advent2019.day1.solution
  (:require [advent2019.day1.data :as data]))


(defn fuel-calc [mv]
  (reduce + (map #(-> %
                      (quot 3)
                      int
                      (- 2))
                 mv)))

(defn fuel-recur [mf]
  (if (> (-> mf (quot 3) int (- 2)) 0)
    (+ mf (fuel-recur (-> mf (quot 3) int (- 2))))
    mf))

(defn mod-fuel-calc [mm]
  (let [mf (-> mm (quot 3) int (- 2))]
    (fuel-recur mf)))

(defn fuel-calc2 [mv]
  (reduce + (map mod-fuel-calc mv)))