(ns aoc.2021.09a
  (:require
    [clojure.string :as str]))

(defn or-else [x default] (if (some? x) x default))
(def input (->> *in*
                (slurp)
                (str/split-lines)
                (mapv (partial mapv #(Integer/parseInt (str %))))))

(defn at [v [x y]] (some-> v (get y) (get x)))
(defn is-minimum [v [x y]]
  (let [m (at v [x y])]
    (every? identity (for [dx [-1 0 +1]
                           dy [-1 0 +1] :when (or (not= dx 0) (not= dy 0))]
                       (< m (or-else (at v [(+ x dx) (+ y dy)]) 10))))))
(defn all-coords [v]
  (for [x (range (count (first v)))
        y (range (count v))]
    [x y]))

(println (->> input
              (all-coords)
              (filter (partial is-minimum input))
              (map (partial at input))
              (map inc)
              (apply +)))
