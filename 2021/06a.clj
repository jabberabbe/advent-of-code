; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.06a
  (:require
    [clojure.string :as str]))

(defn parse [line]
  (vec (map #(Integer/parseInt %) (str/split line #","))))

(defn step [xs]
  (let [timers (map dec xs)
        new (count (filter #(< % 0) timers))
        reset (mapv #(if (< % 0) 6 %) timers)]
    (into reset (repeat new 8))))

(def n 80)
(println (->> (read-line)
              (parse)
              ((apply comp (repeat n step)))
              (count)))
