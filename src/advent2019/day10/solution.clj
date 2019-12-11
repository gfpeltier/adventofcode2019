(ns advent2019.day10.solution
  (:require [clojure.string :as string]))

(def t1 "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####")

(defn blocked? [start block end]
  (if (= (:x start) (:x end))
    (and (= (:x start) (:x end))
         (< (:y start) (:y block))
         (> (:y end) (:y block)))
    (let [slope (/ (- (:y end) (:y start))
                   (- (:x end) (:x start)))
          y-int (- (:y start) (* slope (:x start)))]
      (= (:y block)
         (+ y-int (* slope (:x block)))))))

(defn visible-asteroids [all-asts ast]
  (reduce (fn [vis curr]
            (when (every? false? (map #(blocked? ast % curr) vis))
              (conj vis curr)))
          []
          all-asts))

(defn part1 [data]
  (let [ast-pts (-> data
                    string/split-lines
                    (->> (map-indexed
                           (fn [y row]
                             (map-indexed
                               (fn [x char]
                                 (when (= char \#)
                                   (hash-map :x x :y y)))
                               row)))
                         flatten
                         (remove nil?)))]
    (reduce max (map #(count (visible-asteroids ast-pts %)) ast-pts))))

(defn part2 [data]
  )