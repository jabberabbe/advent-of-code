(ns aoc.2021.10a
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]))

(defn score [x]
  (case x
    \) 3
    \] 57
    \} 1197
    \> 25137
    nil 0))

(defn matching [x]
  (case x
    \( \)
    \[ \]
    \{ \}
    \< \>
    \) \(
    \] \[
    \} \{
    \> \<))

(defn opening [x]
  (case x
    \( true
    \[ true
    \{ true
    \< true
    \) false
    \] false
    \} false
    \> false))

(defn find-corrupted [line]
  (let [resulting (reduce (fn [acc chr]
                            (if (opening chr)
                              (conj acc chr)
                              (if
                                (= (peek acc) (matching chr)) (pop acc)
                                (reduced chr)))) () line)]
    (if (= (type resulting) java.lang.Character) resulting nil)))

(println (->> *in*
              (java.io.BufferedReader.)
              (line-seq)
              (map find-corrupted)
              (map score)
              (apply +)))
