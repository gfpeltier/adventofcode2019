(ns advent2019.day7.solution
  (:require [clojure.core.async :refer [thread chan <!! >!! close! go >! <! offer! poll!]]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as string]
            [clojure.core.async.impl.protocols :as impl]))


(defn parse-op [code]
  {:opcode (mod code 100)
   :mop1 (mod (quot code 100) 10)
   :mop2 (mod (quot code 1000) 10)})

(defn eval-operand [val mode dv]
  (case mode
    0 (nth dv val)
    1 val))

(defn intcode [id data ich och done]
  (go
    (try
      (loop [pc 0 dv data]
        (let [{:keys [opcode mop1 mop2]} (parse-op (nth dv pc))
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
            1 (recur (+ pc 4) (assoc dv op3
                                        (+ (eval-operand op1 mop1 dv)
                                           (eval-operand op2 mop2 dv))))
            2 (recur (+ pc 4) (assoc dv op3
                                        (* (eval-operand op1 mop1 dv)
                                           (eval-operand op2 mop2 dv))))
            3 (let [in (<! ich)]
                (recur (+ pc 2) (assoc dv op1 in)))
            4 (let [out (eval-operand op1 mop1 dv)]
                (offer! och out)
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
            99 (do
                 (swap! done assoc id true)
                 dv))))
      (catch Exception e
        (println "Caught exception in: " id)
        (println e)))))

(defn test-thrusters [icode config]
  (let [ch (chan)
        isig (atom 0)]
    (doseq [ph config]
      (thread (intcode "blah" icode ch ch (atom {})))
      (>!! ch ph)
      (>!! ch @isig)
      (reset! isig (<!! ch)))
    (close! ch)
    @isig))

(defn part1 [data]
  (let [icode (-> data
                  (string/split #",")
                  (->> (mapv #(Integer/parseInt %))))]
    (reduce max (pmap #(test-thrusters icode %) (combo/permutations (range 5))))))

(defn feedback-amps [icode config]
  (let [config (vec config)
        ichans [(chan 5) (chan) (chan) (chan) (chan)]
        ochans (conj (vec (rest ichans)) (first ichans))
        cfgs (map vector config ichans ochans (range 5))
        fin-chan (first ichans)
        done (atom {"t0" false
                    "t1" false
                    "t2" false
                    "t3" false
                    "t4" false})]
    (doseq [[cfg ichan ochan id] cfgs]
      (intcode (str "t" id) icode ichan ochan done)
      (>!! ichan cfg))
    (Thread/sleep 10)
    (>!! fin-chan 0)
    (while (not (every? true? (vals @done))))
    (let [out (poll! fin-chan)]
      (doseq [ch ichans] (close! ch))
      out)))


(defn part2 [data]
  (let [icode (-> data
                  (string/split #",")
                  (->> (mapv #(Integer/parseInt %))))]
    (reduce max (map #(feedback-amps icode %) (combo/permutations (range 5 10))))))