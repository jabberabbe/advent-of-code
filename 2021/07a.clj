(ns aoc.2021.07a
  (:require
    [clojure.string :as str]))

(defn compute-cost [pos x]
  (->> pos
       (map #(Math/abs (- x %)))
       (apply +)))

(println (as-> (read-line) pos
           (str/split pos #",")
           (mapv #(Integer/parseInt %) pos)
           (map (partial compute-cost pos) (range (apply max pos)))
           (apply min pos)))
