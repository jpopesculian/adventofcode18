(ns three.core
  [:require [clojure.string :as string]])

(def input (string/split (slurp "input") #"\n"))
; (def input ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"])

(defn parse-claim [claim]
  (let [[id x y width height] (map read-string (re-seq #"\d+" claim))]
    (apply hash-map [:id id
                     :x x
                     :y y
                     :width width
                     :height height])))

(def parsed-input (map parse-claim input))

(defn claim-1d-coords [claim direction]
  (range
   (claim direction)
   (+
    (claim direction)
    (claim (if (= direction :x) :width :height)))))

(defn claim-2d-coords [claim]
  (reduce
   into
   (map
    (fn [y] (map #(apply list (vector % y)) (claim-1d-coords claim :x)))
    (claim-1d-coords claim :y))))

(defn mark-claims [claims]
  (loop [marked {} remaining claims]
    (if (empty? remaining) marked
        (let [[claim & rest-claims] remaining]
          (let [coords (claim-2d-coords claim)]
            (recur
             (reduce
              (fn [marked-map coord]
                (assoc marked-map coord
                       (conj (get marked-map coord) (claim :id)))) marked coords)
             rest-claims))))))

(defn overlapped [marked]
  (into {} (filter #(> (count (last %)) 1) (into `() marked))))

(defn non-overlapped-claims [claims]
  (loop
   [overlapped-ids (into {} (map #(into [] (list (% :id) false)) claims))
    remaining (map last (into `() (overlapped (mark-claims claims))))]
    (if (empty? remaining) (map first (filter #(= false (last %)) (into `() overlapped-ids)))
        (let [[ids & rest-ids] remaining]
          (recur (reduce #(assoc %1 %2 true) overlapped-ids ids) rest-ids)))))

; PART 1
; (count (overlapped (mark-claims parsed-input)))
; PART 2
(non-overlapped-claims parsed-input)

; (defn overlapped-range [range1 range2]
;   (let [[start1 len1] range1 [start2 len2] range2]
;     (let [end1 (+ start1 len1) end2 (+ start2 len2)]
;       (if (and (>= len1 len2) (>= start2 start1) (<= end2 end1)) range2
;           (if (and (>= len2 len1) (>= start1 start2) (<= end1 end2)) range1
;               (if (and (<= start1 start2) (<= start2 end1)) [start2 (- end1 start2)]
;                   (if (and (<= start2 start1) (<= start1 end2)) [start1 (- end2 start1)] [start1 0])))))))

; (defn overlapped-claim [claim1 claim2]
;   (let [[x width]
;         (overlapped-range
;          [(claim1 :x) (claim1 :width)]
;          [(claim2 :x) (claim2 :width)])]
;     (let [[y height]
;           (overlapped-range
;            [(claim1 :y) (claim1 :height)]
;            [(claim2 :y) (claim2 :height)])]
;       (if (or (= width 0) (= height 0)) nil
;           (apply hash-map [:x x :y y :width width :height height])))))

; (defn overlapped-claims [claim claims]
;   (loop [overlapped [] remaining claims]
;     (if (empty? remaining) overlapped
;         (let [[fclaim & rclaims] remaining]
;           (let [overlapping (overlapped-claim claim fclaim)]
;             (recur (if (nil? overlapping) overlapped (conj overlapped overlapping)) rclaims))))))

; (defn all-overlapped-claims [claims]
;   (loop [overlapped [] remaining claims]
;     (if (empty? remaining) (distinct overlapped)
;         (let [[fclaim & rclaims] remaining]
;           (recur (into overlapped (overlapped-claims fclaim rclaims)) rclaims)))))

; (defn size [claim]
;   (let [width (claim :width) height (claim :height)] (* width height)))

; (defn sum-sizes [claims]
;   (reduce + (map size claims)))

; (defn overlapped-claim-size [claims]
;   (loop [sum 0 overlapping (all-overlapped-claims claims) fun +]
;     (let [claimsize (sum-sizes overlapping)]
;       (if (= claimsize 0) sum
;           (do (println claimsize)
;               (recur (fun sum claimsize) (all-overlapped-claims overlapping) (if (= fun +) - +)))))))

; (overlapped-claim-size coll)
