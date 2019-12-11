(ns advent2019.day4.solution
  (:require [clojure.string :as string]))


(defn digits [n]
  (-> n
      str
      (string/split #"{0,1}")
      (->> (map #(Integer/parseInt %)))))

(defn increasing? [digs]
  (apply <= digs))

(defn has-double? [digs]
  (loop [ds digs ld nil]
    (cond
      (empty? ds) false
      (= (first ds) ld) true
      :else (recur (rest ds) (first ds)))))

(defn part1 [start stop]
  (count (filter (every-pred increasing? has-double?)
                 (map digits (range start stop)))))

(defn exact-double?
  "Caveat: this will just determine that the number has at least some digit that
  appears twice but not necessarily sequentially."
  [digs]
  (not-empty (filter (fn [[_ v]] (= v 2)) (frequencies digs))))

(defn part2 [start stop]
  (count (filter (every-pred increasing? exact-double?)
                 (map digits (range start stop)))))