(ns advent2019.day6.solution
  (:require [clojure.string :as string]))


(defn ocount [g n]
  (loop [c 0 cn n]
    (if (= cn "COM")
      c
      (recur (inc c) (get g cn)))))

(defn part1 [data]
  (let [orbits (string/split-lines data)
        graph (reduce (fn [m o]
                        (let [[p c] (string/split o #"\)")]
                          (assoc m c p)))
                      {}
                      orbits)]
    (reduce + (map #(ocount graph %) (keys graph)))))

(defn opath [g sn tn]
  (loop [p (list) cn sn]
    (if (= cn tn)
      (conj p cn)
      (recur (conj p cn) (get g cn)))))

(defn part2 [data]
  (let [orbits (string/split-lines data)
        graph (reduce (fn [m o]
                        (let [[p c] (string/split o #"\)")]
                          (assoc m c p)))
                      {}
                      orbits)
        yp (opath graph "YOU" "COM")
        sp (opath graph "SAN" "COM")
        lca (loop [y yp s sp ln nil]
              (if (= (first y)
                     (first s))
                (recur (rest y) (rest s) (first y))
                ln))]
    (- (+ (count (opath graph "YOU" lca))
          (count (opath graph "SAN" lca)))
       4)))
