; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.04a
  (:require
    [clojure.string :as str]))

(def blocks (as-> *in* l
              (slurp l)
              (str/split l #"\n\n")
              (map str/split-lines l)
              (map (partial map #(str/split % #",| |\n")) l)
              (map (partial map (partial filter #(not (= % "")))) l)
              (map (partial map (partial map #(Integer/parseInt %))) l)))

(def numbers (first (first blocks)))
; Tag all entries in the boards as not already drawn
(def boards (map (partial map (partial map #(vector % :not-winning))) (rest blocks)))

(defn transpose [x] (map (fn [i] (map #(nth % i) x)) (range (count (first x))))) ; this is sweet!
(defn is-winning [board]
  (or
    (some (partial every? #(= (last %) :winning)) board)
    (some (partial every? #(= (last %) :winning)) (transpose board))))

(defn update-boards [boards n]
  (let
    [tagged-boards
     (map (partial map (partial map #(if (= (first %) n) [n :winning] %))) boards),
     winning-board ; nil if no board has won
     (some #(when (is-winning %) %) tagged-boards)]
    ; 'reduced' short-circuits the fold: if we found a winning board,
    ; stop extracting new numbers
    (if winning-board (reduced [winning-board n]) tagged-boards)))
(def winning (reduce update-boards boards numbers))

(def winning-board  (first winning))
(def winning-number (last winning))

(println (->> winning-board
              (map (partial filter #(= (last %) :not-winning)))
              (map (partial map first))
              (map (partial apply +))
              (apply +)
              (* winning-number)))
