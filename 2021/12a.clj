; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.12a
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]))

(def input *in*)

(defn parse-line [l] (str/split l #"-"))
(defn add-edge [edges [v1 v2]]
  (update edges v1 (fnil #(conj % v2) #{})))
(defn single-visit [node] (= node (str/lower-case node)))
(defn implies [x y] (if x y true))
(defn search [edges]
  (loop [queue '([["start" "end"] #{"start"}])
         cur-count 0]
    (if (empty? queue) cur-count
      (let [[[start end] visited] (peek queue)
            popd                  (pop queue)
            new-paths             (for [neigh (edges start)
                                        :when (implies
                                                (single-visit neigh)
                                                (not (visited neigh)))
                                        :let  [updated (if (single-visit neigh)
                                                         (conj visited neigh)
                                                         visited)]]
                                        [[neigh end] updated])]
        (if (= start end)
          (recur popd (inc cur-count))
          (recur (reduce conj popd new-paths) cur-count))))))

(println (->> input
              (io/reader)
              (line-seq)
              (map parse-line)
              (reduce (fn [edges [v1 v2]]
                        (-> edges
                            (add-edge [v1 v2])
                            (add-edge [v2 v1]))) {})
              (search)))
