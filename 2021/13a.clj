; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

; NOTE: This program requires preprocessing of the input. Lines containing
; "fold" should come first and there should be no blank lines in the input
; file. The required format can easily be obtained with the following
; ex-commands run in batch mode:

;   vim -u NONE -n -e input13.txt <<EOF
;   :normal! Gdap
;   :normal! ggP
;   :normal! dd
;   :wq
;   EOF

(ns aoc.2021.13a
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]))

(defn parse-line [line]
  (if (str/starts-with? line "fold")
    (let [components (str/split line #" |=")
          axe        (case (nth components 2)
                       "x" 0
                       "y" 1)
          coord      (Integer/parseInt (nth components 3))]
      [:fold [axe coord]])
    [:point (mapv #(Integer/parseInt %) (str/split line #","))]))

; My first take on transducers :)
; Leaves elements unchanged but drops every element satisfying `pred` except
; the first one.
; Example: (into [] (drop-rest-satisfying even) (range 10)) = [0 1 3 5 7 9]
(defn drop-rest-satisfying [pred]
  (fn [xf]
    (let [done (volatile! false)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (if (pred input)
           (if @done result (do (vreset! done true) (xf result input)))
           (xf result input)))))))

(defn fold-point [point [axe coord]]
  (update point axe #(if (< % coord) % (- (* 2 coord) %))))

; Another transducer that processes fold lines and points
(defn fold-paper []
  (fn [xf]
    (let [folding-marks      (volatile! (transient []))
          done-reading-folds (volatile! false)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result [instr op]]
         (case instr
           :fold  (do (when (not @done-reading-folds)
                        (vswap! folding-marks #(conj! % op)))
                      result)
           :point (do
                    (when (not @done-reading-folds)
                      (vreset! done-reading-folds true)
                      (vswap! folding-marks persistent!))
                    (xf result (reduce fold-point op @folding-marks)))))))))


(def input *in*)
(println (count (into #{}
                      (comp
                        (map parse-line)
                        (drop-rest-satisfying #(= (first %) :fold))
                        (fold-paper))
                      (line-seq (io/reader input)))))
