(ns advent2019.day10.solution
  (:require [clojure.string :as string]))

(def t0 ".#..#\n.....\n#####\n....#\n...##")
(def t1 "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####")

(defn blocked? [start block end]
  (if (= (:x start) (:x end))
    (or (and (= (:x start) (:x block))
             (< (:y start) (:y block) (:y end)))
        (and (= (:x start) (:x block))
             (> (:y start) (:y block) (:y end))))
    (let [slope (/ (- (:y end) (:y start))
                   (- (:x end) (:x start)))
          y-int (- (:y start) (* slope (:x start)))]
      (or (and (= (:y block)
                  (+ y-int (* slope (:x block))))
               (< (:x start) (:x block) (:x end)))
          (and (= (:y block)
                  (+ y-int (* slope (:x block))))
               (> (:x start) (:x block) (:x end)))))))

(defn visible-asteroids [all-asts ast-idx]
  (let [part-vis (fn [rng]
                   (reduce (fn [vis curr]
                             (if (every? false? (map #(blocked? (nth all-asts ast-idx)
                                                                %
                                                                (nth all-asts curr))
                                                     vis))
                               (conj vis (nth all-asts curr))
                               vis))
                           []
                           rng))]
    (concat (part-vis (reverse (range 0 ast-idx)))
            (part-vis (range (inc ast-idx) (count all-asts))))))

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
                         (remove nil?)
                         vec))]
    (mapv #(count (visible-asteroids ast-pts %))
          (range (count ast-pts)))
    ;(reduce max (map #(count (visible-asteroids ast-pts %))
    ;                 (range (count ast-pts))))
    ))

(defn dist [p1 p2]
  (Math/sqrt (+ (Math/pow (- (:x p2) (:x p1)) 2)
                (Math/pow (- (:y p2) (:y p1)) 2))))

(defn angle [p1 p2]
  (cond
    (= (:x p1) (:x p2))
    (if (> (:y p1) (:y p2))
      0
      Math/PI)

    (= (:y p1) (:y p2))
    (if (> (:x p2) (:x p1))
      (/ Math/PI 2)
      (* 3 (/ Math/PI 2)))

    :else
    (let [p3 {:x (:x p1) :y (:y p2)}
          base-angle (Math/atan (/ (dist p3 p2)
                                   (dist p1 p3)))]
      (cond
        (and (> (:y p2) (:y p1))                            ;; Q2
             (> (:x p2) (:x p1)))
        (+ (- (/ Math/PI 2) base-angle)
           (/ Math/PI 2))

        (and (> (:y p2) (:y p1))                            ;; Q3
             (< (:x p2) (:x p1)))
        (+ base-angle Math/PI)

        (and (< (:y p2) (:y p1))                            ;; Q4
             (< (:x p2) (:x p1)))
        (+ (- (/ Math/PI 2) base-angle)
           (* 3 (/ Math/PI 2)))

        :else base-angle))))

(defn part2 [data]
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
                         (remove nil?)
                         vec))
        all-viz (mapv #(visible-asteroids ast-pts %)
                      (range (count ast-pts)))
        viz-counts (mapv count all-viz)
        max-count (reduce max viz-counts)
        root (nth ast-pts (.indexOf viz-counts max-count))
        vis-pts (nth all-viz (.indexOf viz-counts max-count))]
    (-> vis-pts
        (->> (sort-by (partial angle root)))
        vec
        (nth 199))))