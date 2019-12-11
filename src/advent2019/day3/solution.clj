(ns advent2019.day3.solution
  (:require [clojure.string :as string]
            [clojure.set :as set]))


(defn build-path [ps]
  (let [parts (string/split ps #",")]
    (loop [pps parts out [[0 0]]]
      (if (empty? pps)
        out
        (let [curr (first pps)
              dir (first curr)
              dist (Integer/parseInt (apply str (rest curr)))
              spt (last out)]
          (case dir
            \U (recur (rest pps) (vec (concat out (map #(vector (first spt) (+ % (last spt)))
                                                       (range 1 (inc dist))))))
            \R (recur (rest pps) (vec (concat out (map #(vector (+ % (first spt)) (last spt))
                                                       (range 1 (inc dist))))))
            \D (recur (rest pps) (vec (concat out (map #(vector (first spt) (- (last spt) %))
                                                       (range 1 (inc dist))))))
            \L (recur (rest pps) (vec (concat out (map #(vector (- (first spt) %) (last spt))
                                                       (range 1 (inc dist))))))))))))

(defn min-dist [p1 p2]
  (let [d1 (+ (Math/abs (first p1))
              (Math/abs (second p1)))
        d2 (+ (Math/abs (first p2))
              (Math/abs (second p2)))]
    (if (> d2 d1)
      p1
      p2)))

(defn part1 [data]
  (let [[path1 path2] (string/split-lines data)]
    (reduce min-dist
            [Integer/MAX_VALUE Integer/MAX_VALUE]
            (set/intersection (set/difference (set (build-path path1))
                                              #{[0 0]})
                              (set/difference (set (build-path path2))
                                              #{[0 0]})))))

(defn min-latency [p1 p2 ipt1 ipt2]
  (let [l1 (+ (.indexOf p1 ipt1) (.indexOf p2 ipt1))
        l2 (+ (.indexOf p1 ipt2) (.indexOf p2 ipt2))]
    (if (> l2 l1)
      ipt1
      ipt2)))

(defn part2 [data]
  (let [[path1 path2] (string/split-lines data)
        p1 (build-path path1)
        p2 (build-path path2)
        int-pts (set/intersection (set/difference (set (build-path path1))
                                                  #{[0 0]})
                                  (set/difference (set (build-path path2))
                                                  #{[0 0]}))
        min-lat (reduce (partial min-latency p1 p2)
                        int-pts)]
    (print "Best point:" min-lat)
    (+ (.indexOf p1 min-lat) (.indexOf p2 min-lat))))
