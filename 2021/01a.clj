(ns aoc.2021.01a
  (:require
    [clojure.edn :as edn]))

(defn count-increments [xs]
  (loop [acc 0
         x (first xs)
         l (rest xs)]
    (if (empty? l) acc
      (if
        (< x (first l)) (recur (inc acc) (first l) (rest l))
        (recur acc (first l) (rest l))))))

(println (->> *in*
              (java.io.BufferedReader.)
              (line-seq)
              (map edn/read-string)
              (count-increments)))
