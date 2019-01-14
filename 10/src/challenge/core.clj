(ns challenge.core
  [:require [clojure.core.rrb-vector :as fv]])

(ns challenge.core
  [:require [clojure.string :as string]])

(def filename "input")

(def output "output")

(def recur-num 10000000000)

(defn parse-input [input-line]
  (let [[x y dx dy] (map read-string (re-seq #"-?\d+" input-line))]
    {:x x :y y :dx dx :dy dy}))

(def input (map parse-input (string/split (slurp filename) #"\n")))

(defn has-coord [coords x y]
  (some #(and (= x (% :x)) (= y (% :y))) coords))

(defn get-min-max [coords]
  (let [minx (reduce #(min %1 (%2 :x)) Integer/MAX_VALUE coords)
        miny (reduce #(min %1 (%2 :y)) Integer/MAX_VALUE coords)
        maxx (reduce #(max %1 (%2 :x)) Integer/MIN_VALUE coords)
        maxy (reduce #(max %1 (%2 :y)) Integer/MIN_VALUE coords)]
    (list minx miny maxx maxy)))

(defn calc-area [coords]
  (let [[minx miny maxx maxy] (get-min-max coords)]
    (* (- maxx minx) (- maxy miny))))

(defn print-coords [coords]
  (let [[minx miny maxx maxy] (get-min-max coords)]
    (do (spit output "") (doseq [y (range (dec miny) (inc maxy))
                                 x (range (dec minx) (inc maxx))]
                           (do
                             (if (= x minx) (spit output "\n" :append true))
                             (spit output (if (has-coord coords x y) "#" ".") :append true))))))

(defn do-update [coords]
  (map #(-> %
            (assoc :x (+ (% :x) (% :dx)))
            (assoc :y (+ (% :y) (% :dy))))
       coords))

(defn find-words [coords]
  (loop [state coords idx 0 done 0]
    (if (= idx recur-num) (do (print-coords state) done)
        (let [new-state (do-update state)]
          (if (> (calc-area new-state) (calc-area state))
            (recur state recur-num idx)
            (recur new-state (inc idx) done))))))

(find-words input)
