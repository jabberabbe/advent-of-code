; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

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
