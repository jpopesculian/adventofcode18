(ns challenge.core
  [:require
   [clojure.string :as s]
   [promesa.core :as p]])

(def start-recipes [3 7])
(def start-pos [0 1])
(def input 360781)

(defn split-num [number]
  (vec (map
        (fn [n] (-> n str read-string))
        (vec (str number)))))

(defn conj-new-scores [pos recipes]
  (let [scores (map recipes pos)]
    (apply conj recipes (split-num (reduce + scores)))))

(defn move-pos [pos recipes]
  (let [recipe-count (count recipes)]
    (reduce-kv (fn [new-pos k p]
                 (assoc
                  new-pos
                  k
                  (mod
                   (+ p (inc (recipes p)))
                   recipe-count)))
               pos
               pos)))

(defn print-state [pos recipes]
  (do
    (reduce-kv (fn [m k v]
                 (print
                  (apply str
                         (if (= (first pos) k)
                           (concat "[" (str v) "]")
                           (if (= (last pos) k)
                             (concat "(" (str v) ")")
                             (concat " " (str v) " "))))))
               recipes
               recipes)
    (println "")))

(defn recipes-contain [recipes num-vec]
  (let [len (count num-vec) recipe-len (count recipes)]
    (if (> (+ len 3) recipe-len) nil
        (let [start-at (- recipe-len (+ len 3))]
          (let [end-at (+ start-at 3)]
            (reduce (fn [found start]
                      (if (nil? found)
                        (if (= (subvec recipes start (+ start len)) num-vec)
                          start
                          found)
                        found))
                    nil
                    (range start-at end-at)))))))

(defn loop-recipes [start-pos start-recipes n]
  (loop [pos start-pos recipes start-recipes]
    (if (> (count recipes) (+ 10 n))
      (subvec recipes n (+ 10 n))
      (let [new-recipes (conj-new-scores pos recipes)]
        (let [new-pos (move-pos pos new-recipes)]
          ; (do (print-state new-pos new-recipes)
          (recur new-pos new-recipes))))))

(defn find-recipes-num [start-pos start-recipes search]
  (let [num-vec (split-num search)]
    (loop [pos start-pos recipes start-recipes]
      (if (not (nil? (recipes-contain recipes num-vec)))
        (recipes-contain recipes num-vec)
        (let [new-recipes (conj-new-scores pos recipes)]
          (let [new-pos (move-pos pos new-recipes)]
          ; (do (print-state new-pos new-recipes)
            (recur new-pos new-recipes)))))))

; Part 1
; (apply str (loop-recipes start-pos start-recipes input))

; Part 2
; (find-recipes-num start-pos start-recipes input)
