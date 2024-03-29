; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.14a
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]))

(defn parse-rule [line]
  (let [[in out] (str/split line #" -> ")]
    [(vec in) (first out)]))
(def input *in*)
(def reader (io/reader input))
(def polymer-template (first (line-seq reader)))
(def insertion-rules
  (persistent!
    (reduce #(conj! %1 (parse-rule %2))
            (transient {})
            (rest (line-seq reader)))))

; Transducer that construct sliding windows of size 2 with step 1
(defn adjacent-pairs []
  (fn [xf]
    (let [l (volatile! nil)]
      (fn
        ([]         (xf))
        ([result]   (xf (xf result [@l])))
        ([result x] (let [l- @l]
                      (vreset! l x)
                      (if l- (xf result [l- x]) result)))))))

(defn apply-rule [[a b]]
  (let [c (insertion-rules [a b])]
    (if c [a c] [a])))
(def substitute-polymers
  (comp
    (adjacent-pairs)
    (mapcat apply-rule)))

(defn update-freqs [freqs x]
  (assoc! freqs x ((fnil inc 0) (freqs x))))
(let [freqs (transduce
              (apply comp (repeat 10 substitute-polymers))
              (completing update-freqs persistent!) (transient {})
              polymer-template)
      counts (vals freqs)]
  (println (- (apply max counts) (apply min counts))))
