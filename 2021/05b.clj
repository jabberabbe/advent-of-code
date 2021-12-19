; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.05b
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
        axe1 (key (apply max-key #(Math/abs (val %)) delta))
        [shift dir1] [(Math/abs (axe1 delta)), (Integer/signum (axe1 delta))]
        axe2 (if (= axe1 :x) :y :x)
        dir2 (Integer/signum (axe2 delta))
        points (map #(update (update (:start line) axe1 (partial + (* % dir1))) axe2 (partial + (* % dir2))) (range 0 (inc shift)))
        updated-board (reduce #(update %1 %2 (fnil inc 0)) board points)]
    updated-board))

(println (->> *in*
              (java.io.BufferedReader.)
              (line-seq)
              (map parse-line)
              (reduce draw-line {})
              (filter #(>= (val %) 2))
              (count)))
