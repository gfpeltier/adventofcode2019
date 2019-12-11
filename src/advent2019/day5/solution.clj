(ns advent2019.day5.solution
  (:require [clojure.string :as string]))


(defn parse-op [code]
  {:opcode (mod code 100)
   :mop1 (mod (quot code 100) 10)
   :mop2 (mod (quot code 1000) 10)})

(defn eval-operand [val mode dv]
  (case mode
    0 (nth dv val)
    1 val))

(defn intcode [data]
  (loop [pc 0 dv data]
    (let [{:keys [opcode mop1 mop2]} (parse-op (nth dv pc))
          op1 (nth dv (inc pc))
          op2 (nth dv (+ pc 2))
          op3 (nth dv (+ pc 3))]
      (case opcode
        1 (recur (+ pc 4) (assoc dv op3
                                    (+ (eval-operand op1 mop1 dv)
                                       (eval-operand op2 mop2 dv))))
        2 (recur (+ pc 4) (assoc dv op3
                                    (* (eval-operand op1 mop1 dv)
                                       (eval-operand op2 mop2 dv))))
        3 (let [in (-> (read-line) Integer/parseInt)]
            (recur (+ pc 2) (assoc dv op1 in)))
        4 (let [out (eval-operand op1 mop1 dv)]
            (println "Out: " out)
            (recur (+ pc 2) dv))
        5 (if (not= 0 (eval-operand op1 mop1 dv))
            (recur (eval-operand op2 mop2 dv) dv)
            (recur (+ pc 3) dv))
        6 (if (= 0 (eval-operand op1 mop1 dv))
            (recur (eval-operand op2 mop2 dv) dv)
            (recur (+ pc 3) dv))
        7 (if (< (eval-operand op1 mop1 dv)
                 (eval-operand op2 mop2 dv))
            (recur (+ pc 4) (assoc dv op3 1))
            (recur (+ pc 4) (assoc dv op3 0)))
        8 (if (= (eval-operand op1 mop1 dv)
                 (eval-operand op2 mop2 dv))
            (recur (+ pc 4) (assoc dv op3 1))
            (recur (+ pc 4) (assoc dv op3 0)))
        99 dv))))

(defn part1 [data]
  (let [parsed (-> data
                   (string/split #",")
                   (->> (mapv #(Integer/parseInt %))))]
    (println (intcode parsed))))
