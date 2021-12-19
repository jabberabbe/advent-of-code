; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.15a
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.data.priority-map :refer [priority-map-keyfn]]))

(def input *in*)
(def grid (->> input
               (io/reader)
               (line-seq)
               (mapcat (partial map #(Integer/parseInt (str %))))
               (vec)))
(def n (Math/round (Math/sqrt (count grid))))
(defn at [[x y]] (+ (* y n) x))
(defn all-coords [maxx maxy]
  (for [x (range maxx)
        y (range maxy)]
       [x y]))

(defn xor [a b] (and (not (and a b)) (or a b)))
(defn neighbours [[x y]]
  (for [dx    [-1 0 1]
        dy    [-1 0 1]
        :when (xor (not= dx 0) (not= dy 0))
        :let  [newx (+ x dx)
               newy (+ y dy)]
        :when (and (>= newx 0) (< newx n))
        :when (and (>= newy 0) (< newy n))]
    [newx newy]))
(defn dijkstra [costs start end]
  (loop [pq (assoc
              (reduce
                #(assoc %1 %2 [##Inf nil]) ; [dist, prev]
                (priority-map-keyfn first)
                (all-coords n n))
              start [0 nil])
         prevs {}]
    (let [[node [this-dist prev]] (first pq)
          popd                    (dissoc pq node)
          saved-prev              (assoc prevs node prev)]
      (if (= node end) saved-prev
        (recur
          (into popd (for [neigh                (neighbours node)
                           :let [cost           (costs (at neigh))
                                 new-neigh-dist (+ this-dist cost)
                                 old-neigh-dist (first (pq neigh))]
                           :when                (some? old-neigh-dist) ; If neighbour is still in the priority queue
                           :when                (< new-neigh-dist old-neigh-dist)]
                       [neigh [new-neigh-dist node]]))
          saved-prev)))))
(defn acc-path [dst edges]
  (loop [path [dst]]
    (let [prev (edges (peek path))]
             (if (some? prev)
               (recur (conj path prev))
               path))))

(def corner0 [0 0])
(def corner1 [(dec n) (dec n)])
(println (->> (dijkstra grid corner0 corner1)
              (acc-path corner1)
              (drop-last)
              (map (comp grid at))
              (apply +)))
