(ns challenge.core
  [:require [clojure.string :as string]])

(def filename "input")

(defn parse-input [input-line]
  (map read-string (re-seq #"\d+" input-line)))

(def input (map parse-input (string/split (slurp filename) #"\n")))

; (println input)

(defn build-grid [coords]
  (let [x-coords (range (+ 2 (reduce max (map first coords))))
        y-coords (range (+ 2 (reduce max (map last coords))))]
    (into {}
          (map #(vector % (list -1 Integer/MAX_VALUE))
               (apply concat
                      (map (fn [x] (map #(list x %) y-coords))
                           x-coords))))))

(defn coord-dist [coord1 coord2]
  (+
   (Math/abs (- (first coord1) (first coord2)))
   (Math/abs (- (last coord1) (last coord2)))))

(defn mark-claim [claims claim-num coord]
  (into {}
        (reduce
         (fn [result [claim-coord [other-claim dist]]]
           (let [new-dist (coord-dist coord claim-coord)]
             (assoc
              result
              claim-coord
              (cond
                (< dist new-dist) (list other-claim dist)
                (< new-dist dist) (list claim-num new-dist)
                :else (list -1 dist)))))
         claims
         (into `() claims))))

(defn mark-claims [coords]
  (loop [claims (build-grid coords) claim-num 0]
    (if (= claim-num (count coords)) claims
        (let [coord (nth coords claim-num)]
          (recur
           (mark-claim claims claim-num coord)
           (inc claim-num))))))

(defn is-infinite [max-x max-y [x y]]
  (or
   (= x max-x)
   (= x 0)
   (= y max-y)
   (= y 0)))

(defn claim-size [claims]
  (let [check-infinite
        (partial is-infinite
                 (reduce max (map first (keys claims)))
                 (reduce max (map last (keys claims))))]
    (loop [counters {} remaining   (into `() claims)]
      (if (empty? remaining) counters
          (let [[[coord [claim-num]] & rclaims] remaining]
            (if (= -1 claim-num)
              (recur counters rclaims)
              (let [counter (counters claim-num)]
                (if (= -1 counter)
                  (recur counters rclaims)
                  (if (check-infinite coord)
                    (recur
                     (assoc counters claim-num -1)
                     rclaims)
                    (recur
                     (assoc counters claim-num (inc (or counter 0)))
                     rclaims))))))))))

(defn coord-total-dist [coord coords]
  (reduce + (map #(coord-dist coord %) coords)))

(defn label-coord-dist [coords]
  (let [grid (build-grid coords)]
    (reduce #(assoc %1 %2 (coord-total-dist %2 coords)) grid (keys grid))))

; PART 1
; (reduce max (map last (claim-size (mark-claims input))))

; PART 2
(count (filter #(< % 10000) (map last (into `() (label-coord-dist input)))))
