(ns aoc.2021.08a
  (:require
    [clojure.string :as str]
    [clojure.set    :as set]
    [clojure.pprint :as pprint]))

; (defn prdbg [x & xs] (do (println xs) (pprint/pprint x) x))
(defn prdbg [x & xs] x)

(def segments #{\a \b \c \d \e \f \g})
(def digits
  (set/map-invert {0 #{\a \b \c \e \f \g}
                   1 #{\c \f}
                   2 #{\a \c \d \e \g}
                   3 #{\a \c \d \f \g}
                   4 #{\b \c \d \f}
                   5 #{\a \b \d \f \g}
                   6 #{\a \b \d \e \f \g}
                   7 #{\a \c \f}
                   8 #{\a \b \c \d \e \f \g}
                   9 #{\a \b \c \d \f \g}}))

(defn map-on-vals [f m]
  (persistent! (reduce-kv #(assoc! %1 %2 (f %3)) (transient {}) m)))
(defn map-on-keys-vals [f m]
  (persistent! (reduce-kv #(conj! %1 (f %2 %3)) (transient {}) m)))

(def digits-by-segment-count
  (map-on-vals set (group-by count (keys digits))))

(defn parse-decimal [x] (reduce #(+ (* %1 10) %2) 0 x))
(defn parse-line [line]
  (as-> line l
    (str/split l #" \| ")
    (map #(str/split % #" ") l)
    (mapv (partial map set) l)
    (update l 0 set) ; Set that contains all digits
    (update l 1 vec) ; Vec (ordered) that contains the four lit up digits
    ))

(defn keep-least [xs ys] (if (> (count xs) (count ys)) ys xs))

(defn build-segments [all-digits]
  (loop [tries (zipmap all-digits (map (comp digits-by-segment-count count) all-digits))
         old-defined-keys #{}]
    (let [defined-mappings (filter #(= (count (val %)) 1) (prdbg tries "TRIES: "))
          defined-keys (prdbg (set (map key defined-mappings)) "DEFINED KEYS: ")
          completed (every? #(some? (defined-keys (hash-set %))) segments)]
      (if completed defined-mappings
        (if (= old-defined-keys defined-keys)
          (throw (ex-info "Splash!" {}))
          (recur
            (reduce (fn [mappings defined-key]
                      (let
                        [defined-val (prdbg (first (tries (prdbg defined-key "SUBTRACTING FROM DOMAIN:"))) "SUBTRACTING FROM CODOMAIN:")]
                        (merge-with keep-least mappings
                                    ; (reduce-kv (fn [workmap thiskey thisval]
                                    ; (assoc workmap
                                    ; (set/difference (prdbg thiskey "SUBTRACTING TO KEY:") (prdbg defined-key "THE KEY:"))
                                    ; (set (map #(set/difference %1 (prdbg (first (tries defined-key)) "VALS FOR WD-KEY:")) (prdbg thisval "SUBTRACTING TO VALS:"))))) {} mappings)
                                    (map-on-keys-vals
                                      (fn [thiskey thisval] (let
                                                              [subtracted-key (set/difference (prdbg thiskey "SUBTRACTING TO DOMAIN:") defined-key)] [subtracted-key,
                                                                                                                                                      (prdbg (set (filter #(= (count %) (prdbg (count subtracted-key) "COUNT:")) (map #(set/difference %1 defined-val) (prdbg thisval "SUBTRACTING TO CODOMAIN(S):")))) "YIELDING: ")])) mappings))))
                    tries defined-keys) defined-keys))))))

(defn parse-digits [[all-digits lit-up-digits]]
  (let [segment-mappings (->> (build-segments all-digits)
                              (filter #(= (count (first %)) 1))
                              (mapv (partial mapv first))
                              (mapv #(update % 1 first))
                              (into {})
                              (prdbg))
        ordered-digits (map (comp digits set (partial map segment-mappings)) lit-up-digits)]
    (parse-decimal ordered-digits)))
; (defn read-integer [[all-digits lit-up-digits]]
; (count (prdbg (filter #(digits-segments-count (count %)) lit-up-digits))))

(println (->> *in*
              (java.io.BufferedReader.)
              (line-seq)
              (map parse-line)
              (map parse-digits)
              (reduce +)))
