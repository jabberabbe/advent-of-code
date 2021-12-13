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

(ns aoc.2021.13b
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

(defn fold-point [point [axe coord]]
  (update point axe #(if (< % coord) % (- (* 2 coord) %))))

; A transducer that processes fold lines and points
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
(let [marked      (into #{}
                        (comp
                          (map parse-line)
                          (fold-paper))
                        (line-seq (io/reader input)))
      [maxx maxy] (mapv inc
                        (reduce #(map max %1 %2) [0 0] marked))]
  (doall (for [y (range maxy)
               x (range (inc maxx))]
           (cond
             (= x maxx)     (println)
             (marked [x y]) (print "#")
             :else          (print ".")))))
