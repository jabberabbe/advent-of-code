; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

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
