(ns challenge.core
  [:require [clojure.string :as string]])

(def filename "input")

(def input (string/trim (slurp filename)))

(def ascii-spacing 32)

(defn matching [code1 code2]
  (= ascii-spacing (Math/abs (- code1 code2))))

(defn remove-pair [coll pos]
  (into [] (concat (subvec coll 0 pos) (subvec coll (+ pos 2)))))

(defn remove-matching [codes]
  (loop [pos 0 result (vec codes)]
    (if (= pos (dec (count result))) result
        (if (matching (nth result pos) (nth result (inc pos)))
          (recur (max (dec pos) 0) (remove-pair result pos))
          (recur (inc pos) result)))))

(defn remove-code [coll matcher]
  (reduce (fn [result code]
            (if (or
                 (= code matcher)
                 (= code (+ ascii-spacing matcher)))
              result (conj result code))) '() coll))

(defn react-without-codes [coll]
  (reduce min
          (map last
               (map
                #(list % (count (remove-matching (remove-code coll %))))
                (range (int \A) (inc (int \Z)))))))

; PART 1
; (count (remove-matching (map int input)))

; PART 2
(react-without-codes (map int input))
