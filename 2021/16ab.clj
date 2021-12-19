; SPDX-License-Identifier: MIT
; Copyright (C) 2021 Tito Sacchi <tito@tilde.team>
; WARNING: These solutions were written while I was still learning Clojure and
; should by no means be taken as examples of good programming practice or fast
; implementations.

(ns aoc.2021.16)


; Functions to convert between different bases

(defn to-bits [x n]
  (loop [b (), x x, n n]
    (let [q (quot x 2)
          r (rem x 2)
          digit (if (= r 0) 0 1)]
      (if (= n 0) b
        (recur (conj b digit) q (dec n))))))

(defn from-base [n]
  (fn [digits]
    (loop [digits digits, x 0]
      (let [d  (first digits)
            ds (rest digits)]
        (if (some? d)
          (recur ds (+ (* n x) d))
          x)))))
(def from-bits (from-base 2))

(defn hex-digit-to-bits [x] (to-bits (Character/digit x 16) 4))
(def hex-string-to-bits (mapcat hex-digit-to-bits))


; Parsec-style composable parser combinators

; This macro implements something that loosely resembles do-notation. It takes
; a sequence, a vector of monadic bindings (i.e. symbol - subparser pairs) and
; an expression that will be the result of this whole parser. Monadic actions
; are allowed to depend on results of previous actions (i.e. we are in a monad
; and not just an applicative functor).

(defmacro parse-through [input passes result]
  (if (empty? passes) `[~result ~input]
    (let [sym     (first passes)
          l       (next passes)
          action  (first l)
          pending (rest l)]
      (if (nil? l)
        (throw (IllegalArgumentException.
                 "Odd number of elements in monadic binding vector")))
      `(let [[~sym stream#] (~action ~input)]
         (when (some? stream#)
           (parse-through stream# ~pending ~result))))))

; Curried version of parse-through - returns a function that takes a string and
; returns a [result, rest of the stream] pair on success and nil on failure.
; parse-through and monadic-chain allow to compose functions with this
; signature.

(defmacro monadic-chain [passes result]
  `(fn [input#] (parse-through input# ~passes ~result)))

; Implements optional parsers - i.e. trying different parsers without consuming
; any input until one doesn't fail. `alternative ...` never fails - it returns
; nil if none of the supplied parsers succeeded. `alternative p` can be used to
; build an optional parser.

(defn alternatives [& actions]
  (fn [input]
    (if-let [succeeded (some #(% input) actions)]
      succeeded
      [nil input])))

; fmap: applies a function to the result of the supplied parser.

(defn mapping [f parser]
  (fn [input]
    (if-let [[v s] (parser input)]
      [(f v) s])))

; (pure x) is a parser that doesn't consume any input and always returns x.

(defn pure [x]
  (fn [input]
    [x input]))

; Parser that always fails.

(def fail (constantly nil))

; Build a parser for a sequence of elements matched by the same parser. Allows
; for an optional parameter n that specifies the maximum amount of elements.
; This combinator never fails, it returns an empty list when the supplied
; parser doesn't match any element.

(defn many
  ([parser] (many ##Inf parser)) ; You won't reach 0 by decrementing Inf :)
  ([n parser]
   (fn [input]
     (loop [n n, i input, q []]
       (if (= n 0) [q i]
         (let [[v s] (parser i)]
           (if (some? s)
             (recur (dec n) s (conj q v))
             [q i])))))))

; Takes exactly n elements from the stream.

(defn take-exactly [n]
  (fn [bits]
    (let [[b bs] (split-at n bits)]
      (if (= n (count b)) [b bs]))))
(defn take-bits [n] (mapping from-bits (take-exactly n)))
(defn take-at-most [n] (partial split-at n))

; Limits the maximum amount of characters that can be consumed from the stream.

(defn consuming [maxn parser]
  (fn [input]
    (let [[consumable tail] (split-at maxn input)
          [v leftover]      (parser consumable)]
      (if (some? leftover)
        [v (concat leftover tail)]))))


; BITS transmission parsers

(def nibble-stream
  (monadic-chain [not-last-block    (take-bits 1)
                  nibble            (take-bits 4)
                  following-nibbles (if (= 1 not-last-block) nibble-stream (pure ()))]
                 (conj following-nibbles nibble)))

(def literal-packet (mapping (from-base 16) nibble-stream))

(declare parse-packet)

(def operator-packet (monadic-chain
                       [length-type (take-bits 1)
                        length      (case length-type
                                      0 (take-bits 15)
                                      1 (take-bits 11)
                                      fail)
                        subpackets  (case length-type
                                      0 (consuming length (many parse-packet))
                                      1 (many length parse-packet))]
                       subpackets))

(def parse-packet (monadic-chain
                    [version  (take-bits 3)
                     typ      (take-bits 3)
                     contents (case typ
                                4 literal-packet
                                operator-packet)]
                    {:version version :typ typ :contents contents}))

(def p (->> (read-line)
            (eduction hex-string-to-bits)
            (parse-packet)
            (first)))


; Traversals over packet trees

(defn sum-versions [packet]
  (+ (:version packet) (if (not= (:typ packet) 4) (apply + (map sum-versions (:contents packet))) 0)))

(println (sum-versions p))


(def operator-for-type
  {0 +
   1 *
   2 min
   3 max
   5 #(if (> %1 %2) 1 0)
   6 #(if (< %1 %2) 1 0)
   7 #(if (= %1 %2) 1 0)})

(defn reduce-packet [packet]
  (if (= 4 (:typ packet))
    (:contents packet)
    (apply (operator-for-type (:typ packet)) (map reduce-packet (:contents packet)))))

(println (reduce-packet p))
