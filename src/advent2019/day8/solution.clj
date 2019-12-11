(ns advent2019.day8.solution
  (:require [clojure.string :as string]))



(defn part1 [data]
  (let [parsed (string/split data #"{0,1}")
        rows (map vec (partition 25 parsed))
        layers (mapv vec (partition 6 rows))]
    (-> (for [n (range (count layers))]
          (apply (partial merge-with +)
                 (map #(frequencies (get-in layers [n %])) (range 6))))
        (->> (sort-by #(get % "0")))
        first
        (select-keys ["1" "2"])
        vals
        (->> (apply *)))))

(defn part2 [data]
  (let [parsed (-> data
                   (string/split #"{0,1}")
                   (->> (map #(Integer/parseInt %))))
        rows (map vec (partition 25 parsed))
        layers (mapv vec (partition 6 rows))]
    (reduce (fn [fi layer]
              (let [img (atom fi)]
                (doseq [c (range 25)
                        r (range 6)]
                  (when (and (= 2 (get-in @img [r c]))
                             (not= 2 (get-in layer [r c])))
                    (swap! img assoc-in [r c] (get-in layer [r c]))))
                @img))
            (vec (repeat 6 (vec (repeat 25 2))))
            layers)))
