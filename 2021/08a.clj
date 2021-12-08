(ns aoc.2021.08a
  (:require
    [clojure.string :as str]))

(defn prdbg [x] (do (println x) x))

(def digits
  {1 #{\c \f}
   4 #{\b \c \d \f}
   7 #{\a \c \f}
   8 #{\a \b \c \d \e \f \g}})

(def digits-segments-count (set (map #(count (val %)) digits)))

(defn parse-line [line]
  (->> (str/split line #" \| ")
       (map #(str/split % #" "))))

; Count how many 1, 4, 7, 8 there are in lit-up-digits
(defn count-digits [[all-digits lit-up-digits]]
  (count (prdbg (filter #(digits-segments-count (count %)) lit-up-digits))))

(println (->> *in*
              (java.io.BufferedReader.)
              (line-seq)
              (map parse-line)
              (map count-digits)
              (apply +)))
