(ns advent2019.day9.solution
  (:require [clojure.core.async :refer [go offer! <! >!! <!! chan]]
            [clojure.string :as string]))


(defn parse-op [code]
  {:opcode (mod code 100)
   :mop1 (mod (quot code 100) 10)
   :mop2 (mod (quot code 1000) 10)
   :mop3 (mod (quot code 10000) 10)})

(defn eval-operand [val mode rb dv]
  (case mode
    0 (nth dv val)
    1 val
    2 (nth dv (+ rb val))))

(defn intcode [data]
  (try
    (loop [pc 0 rb 0 dv data]
      (let [{:keys [opcode mop1 mop2 mop3]} (parse-op (nth dv pc))
            op1 (when (not= opcode 99)
                  (nth dv (inc pc)))
            op2 (when (and (not= opcode 99)
                           (not= opcode 3)
                           (not= opcode 4))
                  (nth dv (+ pc 2)))
            op3 (when (and (not= opcode 99)
                           (not= opcode 3)
                           (not= opcode 4)
                           (not= opcode 5)
                           (not= opcode 6))
                  (nth dv (+ pc 3)))]
        (case opcode
          1 (recur (+ pc 4) rb (assoc dv (eval-operand op3 mop3 rb dv)
                                         (+ (eval-operand op1 mop1 rb dv)
                                            (eval-operand op2 mop2 rb dv))))
          2 (recur (+ pc 4) rb (assoc dv (eval-operand op3 mop3 rb dv)
                                         (* (eval-operand op1 mop1 rb dv)
                                            (eval-operand op2 mop2 rb dv))))
          3 (let [in (Integer/parseInt (read-line))]
              (recur (+ pc 2) rb (assoc dv (eval-operand op1 mop1 rb dv) in)))
          4 (let [out (eval-operand op1 mop1 rb dv)]
              (println out)
              (recur (+ pc 2) rb dv))
          5 (if (not= 0 (eval-operand op1 mop1 rb dv))
              (recur (eval-operand op2 mop2 rb dv) rb dv)
              (recur (+ pc 3) rb dv))
          6 (if (= 0 (eval-operand op1 mop1 rb dv))
              (recur (eval-operand op2 mop2 rb dv) rb dv)
              (recur (+ pc 3) rb dv))
          7 (if (< (eval-operand op1 mop1 rb dv)
                   (eval-operand op2 mop2 rb dv))
              (recur (+ pc 4) rb (assoc dv (eval-operand op3 mop3 rb dv) 1))
              (recur (+ pc 4) rb (assoc dv (eval-operand op3 mop3 rb dv) 0)))
          8 (if (= (eval-operand op1 mop1 rb dv)
                   (eval-operand op2 mop2 rb dv))
              (recur (+ pc 4) rb (assoc dv (eval-operand op3 mop3 rb dv) 1))
              (recur (+ pc 4) rb (assoc dv (eval-operand op3 mop3 rb dv) 0)))
          9 (recur (+ pc 2) (+ rb op1) dv)
          99 (do
               ;(swap! done assoc id true)
               (take 5 dv)))))
    (catch Exception e
      (println "Caught exception while executing")
      (println e))))

(defn part1 [data]
  (let [icode (-> data
                  (string/split #",")
                  (->> (mapv #(Integer/parseInt %)))
                  (concat (repeat 10000 0))
                  vec)]
    (intcode icode)))

(defn part2 [data])
