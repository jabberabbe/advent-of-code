(ns aoc.2021.02a
  (:require
    [clojure.string :as str]))

(defn parse [coords line]
  (let [x (Integer/parseInt (last line))]
    (case (first line)
      "up" (update coords :depth - x)
      "down" (update coords :depth + x)
      "forward" (update coords :hor + x))))

(println (->> *in*
              (java.io.BufferedReader.)
              (line-seq)
              (map #(str/split % #" "))
              (reduce parse {:hor 0 :depth 0})
              (vals)
              (reduce *)))
