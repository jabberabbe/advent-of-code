; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.02b
  (:require
    [clojure.string :as str]))

(defn parse [coords line]
  (let [x (Integer/parseInt (last line))
        y (* x (:aim coords))]
    (case (first line)
      "up" (update coords :aim - x)
      "down" (update coords :aim + x)
      "forward" (-> coords
                    (update :hor + x)
                    (update :depth + (* y))))))

(println (->> *in*
              (java.io.BufferedReader.)
              (line-seq)
              (map #(str/split % #" "))
              (reduce parse {:hor 0 :depth 0 :aim 0})
              (#(select-keys % [:hor :depth]))
              (vals)
              (reduce *)))
