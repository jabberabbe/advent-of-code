(ns aoc.2021.03a)

(defn parse-line [line] (map #(Integer/parseInt (str %)) line))

(defn update-counts [counts line]
  (->> line
       (parse-line)
       (map #(update %1 %2 inc) counts)))

(def most-common-bits
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (reduce update-counts (repeat {0 0, 1 0}))
       (map #(key (apply max-key val %)))))

(defn from-bin [xs]
  (reduce
    #(case %2
       0 (* 2 %1)
       1 (inc (* 2 %1))) xs))

(def gamma (from-bin most-common-bits))
(def epsilon (from-bin (map #(- 1 %) most-common-bits)))
(println (* gamma epsilon))
