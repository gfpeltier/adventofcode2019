(ns advent2019.day2.solution
  (:require [clojure.string :as string]
            [advent2019.day2.data :as data]))


(defn intcode [data]
  (loop [pc 0 dv data]
    (let [opc (nth dv pc)
          op1 (nth dv (inc pc))
          op2 (nth dv (+ pc 2))
          out (nth dv (+ pc 3))]
      (case opc
        1 (recur (+ pc 4) (assoc dv out (+ (nth dv op1) (nth dv op2))))
        2 (recur (+ pc 4) (assoc dv out (* (nth dv op1) (nth dv op2))))
        99 (nth dv 0)))))

(defn part1 [data]
  (let [parsed (-> data
                   (string/split #",")
                   (->> (mapv #(Integer/parseInt %))))]
    (intcode parsed)))

(defn part2 [data]
  (let [parsed (-> data
                   (string/split #",")
                   (->> (mapv #(Integer/parseInt %))))]
    (remove nil?
            (for [n (range 100) v (range 100)]
              (let [res (intcode (-> parsed
                                     (assoc 1 n)
                                     (assoc 2 v)))]
                (if (= res 19690720)
                  (str "Noun: " n " Verb: " v)))))))
