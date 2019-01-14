(ns challenge.core
  [:require [clojure.core.rrb-vector :as fv]])

(def num-players 458)
(def last-marble 72019)

(defn build-scores [amount]
  (into {} (map #(vector % 0) (range amount))))

; (defn get-position [coll cur dir dist]
;   (mod (dir cur dist) (count coll)))

; (defn insert-at [coll pos value]
;   (if (= pos (count coll)) (vec (concat coll (vector value)))
;       (vec (concat (subvec coll 0 pos) (vector value) (subvec coll pos)))))

; (defn remove-at [coll pos]
;   (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

; (defn normal-move [coll cur scores turn value]
;   (let [pos (let [pos (get-position coll cur + 2)] (if (= 0 pos) (count coll) pos))]
;     (list (insert-at coll pos value) pos scores)))

; (defn twenty-three-move [coll cur scores turn value]
;   (let [pos (get-position coll cur - 7)]
;     (let [new-scores (assoc scores turn (+ (scores turn) (coll pos) value))]
;       (list
;        (remove-at coll pos)
;        pos
;        new-scores))))

; (defn get-scores [num-players last-marble]
;   (loop [coll (vector 0 1)
;          cur 1
;          scores (build-scores num-players)
;          turn 1
;          value 2]
;     (if (> value last-marble) scores
;         (let [fun (if (= (mod value 23) 0) twenty-three-move normal-move)]
;           (let [[next-coll next-cur next-scores]
;                 (fun coll cur scores turn value)]
;             (recur
;              next-coll
;              next-cur
;              next-scores
;              (mod (inc turn) num-players)
;              (inc value)))))))

(defn cqueue [& values]
  (java.util.ArrayDeque. values))

(defn cqueue-rot-right [queue amount]
  (reduce (fn [q _] (do (.addLast q (.removeFirst q)) q)) queue (range amount)))

(defn cqueue-rot-left [queue amount]
  (reduce (fn [q _] (do (.addFirst q (.removeLast q)) q)) queue (range amount)))

(defn cqueue-push [queue value]
  (do (.addFirst queue value) queue))

(defn cqueue-pop [queue]
  (do (.removeFirst queue) queue))

(defn cqueue-peek [queue]
  (.peek queue))

(defn normal-move [coll scores turn value]
  (let [queue (cqueue-rot-left coll 1)]
    (list (cqueue-push queue value) scores)))

(defn twenty-three-move [coll scores turn value]
  (let [queue (cqueue-rot-right coll 7)]
    (let [removing (cqueue-peek queue)]
      (list
       (cqueue-rot-left (cqueue-pop queue) 1)
       (assoc scores turn (+ (scores turn) removing value))))))

(defn get-scores [num-players last-marble]
  (loop [coll (cqueue 0)
         scores (build-scores num-players)
         turn 0
         value 1]
    (if (> value last-marble) scores
        (let [fun (if (= (mod value 23) 0) twenty-three-move normal-move)]
          (let [[next-coll next-scores]
                (fun coll scores turn value)]
            (recur
             next-coll
             next-scores
             (mod (inc turn) num-players)
             (inc value)))))))

(apply max (map last (get-scores num-players (* 100 last-marble))))
