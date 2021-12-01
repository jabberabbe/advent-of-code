(ns aoc.2021.01b
  (:require
    [clojure.edn :as edn]))

(defn sliding-window [n xs]
  (loop [window (vec (take n xs))
         tail (drop n xs)
         cursors []]
    (if (= (count tail) 0) (conj cursors window)
      (recur
        (conj (vec (rest window)) (first tail))
        (rest tail)
        (conj cursors window)))))

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
              (sliding-window 3)
              (map #(apply + %))
              (count-increments)))
