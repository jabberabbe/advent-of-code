(ns aoc.2021.05a
  (:require
    [clojure.string :as str]))

(defn parse-line [line]
  (let [splitted (str/split line #" ")
        parse-coord (fn [x] (zipmap [:x :y] (map #(Integer/parseInt %) (str/split x #","))))
        start (parse-coord (first splitted))
        end (parse-coord (last splitted))]
    {:start start, :end end}))

(defn draw-line [board line]
  (let [delta (merge-with - (:end line) (:start line))
        axe (key (apply max-key #(Math/abs (val %)) delta))
        [shift dir] [(Math/abs (axe delta)), (Integer/signum (axe delta))]
        is-hor-ver (some #(= % 0) (vals delta))
        points (map #(update (:start line) axe (partial + (* % dir))) (range 0 (inc shift)))
        updated-board (reduce #(update %1 %2 (fnil inc 0)) board points)]
    (if is-hor-ver updated-board board)))

(println (->> *in*
              (java.io.BufferedReader.)
              (line-seq)
              (map parse-line)
              (reduce draw-line {})
              (filter #(>= (val %) 2))
              (count)))
