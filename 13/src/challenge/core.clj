(ns challenge.core
  [:require
   [clojure.string :as s]
   [promesa.core :as p]])

(def filename "input")

(defn parse-row [row]
  (reduce-kv (fn [[carts track] x ch]
               (case ch
                 \/ (list carts (conj track [x :curve-r]))
                 \\ (list carts (conj track [x :curve-l]))
                 \- (list carts (conj track [x :horiz]))
                 \| (list carts (conj track [x :vert]))
                 \+ (list carts (conj track [x :inter]))
                 \^ (list (conj carts [x (list 0 0)]) track)
                 \> (list (conj carts [x (list 1 0)]) track)
                 \v (list (conj carts [x (list 2 0)]) track)
                 \< (list (conj carts [x (list 3 0)]) track)
                 (list carts track)))
             (list `() `())
             (vec row)))

(defn parse-rows [rows]
  (loop [y 0
         carts {}
         track {}
         remaining rows]
    (if (empty? remaining)
      (list carts track)
      (let [[row & rows] remaining]
        (let [[new-carts new-track] (parse-row row)]
          (recur
           (inc y)
           (reduce
            (fn [result [x state]]
              (assoc result (list x y) state))
            carts
            new-carts)
           (reduce
            (fn [result [x typ]]
              (assoc result (list x y) typ))
            track
            new-track)
           rows))))))

(def input (parse-rows (s/split (slurp filename) #"\n")))

(defn print-map [[carts track]]
  (let [x-coords (range (inc (reduce max (map first (map first track)))))
        y-coords (range (inc (reduce max (map last (map first track)))))]
    (do
      (println "")
      (reduce
       (fn [_ y]
         (do
           (reduce
            (fn [_ x]
              (let [cart (carts (list x y)) typ (track (list x y))]
                (if (not (nil? cart))
                  (print (case (first cart)
                           0 "^"
                           1 ">"
                           2 "v"
                           3 "<"))
                  (if (not (nil? typ))
                    (print ".")
                    (print " ")))))
            0
            x-coords)
           (println "")))
       0
       y-coords)
      (println "")
      (list carts track))))

(defn update-cart-state [[dir turn] typ]
  (case typ
    :curve-r (list (case dir
                     0 1
                     1 0
                     2 3
                     3 2) turn)
    :curve-l (list (case dir
                     0 3
                     3 0
                     2 1
                     1 2) turn)
    :inter (list (mod (+ (dec turn) dir) 4) (mod (inc turn) 3))
    (list dir turn)))

(defn move-cart [[x y] state track]
  (let [coord
        (case (first state)
          0 (list x (dec y))
          1 (list (inc x) y)
          2 (list x (inc y))
          3 (list (dec x) y))]
    (list
     coord
     (update-cart-state state (track coord)))))

(defn update-cart [cart carts track]
  (let [[coord state] cart]
    (let [[new-coord new-state] (move-cart coord state track)]
      (let [new-carts (dissoc carts coord)]
        (if (contains? new-carts new-coord)
          (list new-carts new-coord)
          (list (assoc new-carts new-coord new-state) nil))))))

(defn update-cart-part2 [cart carts track]
  (let [[coord state] cart]
    (let [[new-coord new-state] (move-cart coord state track)]
      (let [new-carts (dissoc carts coord)]
        (if (contains? new-carts new-coord)
          (dissoc new-carts new-coord)
          (assoc new-carts new-coord new-state))))))

(defn carts-sorter [carts]
  (let [mag-x-exp
        (->> (map first carts)
             (map first)
             (reduce max)
             str
             count)]
    (let [mag-x (reduce * (repeat mag-x-exp 10))]
      (fn [[[x y] _]] (+ x (* mag-x y))))))

(defn do-update [[carts track]]
  (loop [remaining (sort-by (carts-sorter carts) carts)
         updated-carts carts
         collision nil]
    (if (or (empty? remaining) (not (nil? collision)))
      (list (into {} updated-carts) track collision)
      (let [[cart & rcarts] remaining]
        (let [[carts-after-move new-collision] (update-cart cart updated-carts track)]
          (recur rcarts carts-after-move new-collision))))))

(defn do-update-part2 [[carts track]]
  (loop [remaining (sort-by (carts-sorter carts) carts)
         updated-carts carts]
    (if (empty? remaining)
      (list (into {} updated-carts) track)
      (let [[cart & rcarts] remaining]
        (let [carts-after-move (update-cart-part2 cart updated-carts track)]
          (recur rcarts carts-after-move))))))

(defn run-sim [[carts track]]
  (loop [state (list carts track) collision nil]
    (if (not (nil? collision))
      collision
      (let [[new-carts new-track new-collision] (do-update state)]
        (recur (list new-carts new-track) new-collision)))))

(defn run-sim-part2 [[carts track]]
  (loop [state (list carts track)]
    (if (= (count (first state)) 1)
      (-> state first first first)
      (recur (do-update-part2 state)))))

(run-sim-part2 input)
