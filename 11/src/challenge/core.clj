(ns challenge.core
  [:require [promesa.core :as p]])

(def input 4151)

(defn hundreds-dig [number]
  (mod (/ (- number (mod number 100)) 100) 10))

(defn power-level [[x y] serial]
  (let [rack-id (+ x 10)]
    (-
     (hundreds-dig
      (*
       rack-id
       (+
        (* rack-id y)
        serial)))
     5)))

(defn build-grid [[start-x start-y] size]
  (let [x-coords (range start-x (+ start-x size))
        y-coords (range start-y (+ start-y size))]
    (p/then
     (p/all (map
             (fn [x] (p/do* (map #(list x %) y-coords)))
             x-coords))
     (fn [result] (reduce #(apply conj %1 %2) `() result)))))

(defn get-edge-coords [[start-x start-y] size]
  (p/do*
   (let [end-x (+ start-x (dec size))
         end-y (+ start-y (dec size))]
     (let [x-coords (range start-x end-x)
           y-coords (range start-y end-y)]
       (concat
        (map #(list % end-y) x-coords)
        (map #(list end-x %) y-coords)
        (list (list end-x end-y)))))))

(defn build-power-grid [serial]
  (->> (build-grid `(1 1) 300)
       (p/map
        (fn [coords]
          (reduce
           (fn [m coord]
             (assoc m coord (power-level coord serial)))
           {}
           coords)))))

(defn calc-size-grid [power-grid start-coord serial size]
  (->> (build-grid start-coord size)
       (p/map (fn [coords] (map #(power-grid %) coords)))
       (p/map #(reduce + %))))

(defn calc-size-grids [power-grid serial size]
  (->> (build-grid `(1 1) (- 301 size))
       (p/map (fn [coords]
                (map
                 #(vector % (calc-size-grid power-grid % serial size))
                 coords)))
       (p/map #(into {} %))))

(defn calc-new-size-grids [size-grids power-grid serial size]
  (->> (into `() (apply dissoc size-grids
                        (deref (get-edge-coords `(1 1) (- 302 size)))))
       (map
        (fn [[coord old-power-p]]
          (vector
           coord
           (p/alet [coords (p/await (get-edge-coords coord size))
                    old-power (p/await old-power-p)]
                   (+ old-power
                      (reduce +
                              (map #(power-grid %) coords)))))))
       (into {})))

(defn find-max [size-grids]
  (reduce (fn [[max-coord max-power] [coord power-p]]
            (let [power (deref power-p)]
              (if (> power max-power)
                (list coord power)
                (list max-coord max-power))))
          (list `(0 0) Integer/MIN_VALUE)
          (into `() size-grids)))

(defn find-largest-size [serial size]
  (-> (build-power-grid serial)
      (p/then #(calc-size-grids % serial size))
      (p/then #(find-max %))))

(defn find-largest [serial]
  (p/alet [power-grid (p/await (build-power-grid serial))]
          (let [first-size-grids (deref (calc-size-grids power-grid serial 1))]
            (let [[first-max-coord first-max-power] (find-max first-size-grids)]
              (let [[_ coord _ size]
                    (reduce (fn [[size-grids max-coord max-power max-size] size]
                              (let [new-size-grids
                                    (calc-new-size-grids size-grids power-grid serial size)]
                                (let [[coord power] (find-max new-size-grids)]
                                  (if (> power max-power)
                                    (list new-size-grids coord power size)
                                    (list new-size-grids max-coord max-power max-size)))))
                            (list first-size-grids first-max-coord first-max-power 1)
                            (range 1 301))]
                (list coord size))))))

; Part 1
; (deref (find-largest-size input 3))

; Part 2
; (deref (find-largest input))
