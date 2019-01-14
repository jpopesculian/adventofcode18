(ns challenge.core
  [:require
   [clojure.string :as string]
   [promesa.core :as p]])

(def filename "input")

(defn parse-rule [rule-line]
  (let [[rule result] (string/split rule-line #" => ")]
    (list rule (first result))))

(defn empty-string [len]
  (apply str (map (fn [_] \.) (range 0 len))))

(defn parse-state [state]
  (last (string/split state #":\s")))

(def input (let [[line1 _ & lines] (string/split (slurp filename) #"\n")]
             (list (parse-state line1) (map parse-rule lines))))

(defn find-matches [state rule]
  (loop [matches `() start-at 0]
    (let [next-index (string/index-of state rule start-at)]
      (if (nil? next-index)
        matches
        (recur (conj matches (+ next-index 2)) (inc next-index))))))

(defn find-all-matches [state rules]
  (p/map (fn [matches]
           (reduce (fn [m [result idxs]]
                     (assoc m result (concat (m result) idxs)))
                   {}
                   matches))
         (p/all
          (map (fn [[rule result]]
                 (p/do* (list result (find-matches state rule))))
               rules))))

(defn apply-matches [state matches]
  (apply str
         (reduce
          (fn [new-state [replacement idxs]]
            (reduce #(assoc %1 %2 replacement) new-state idxs))
          ; (apply vector state)
          (apply vector (empty-string (count state)))
          (into `() matches))))

(defn trim-expand [state zero-pos]
  (let [first-dots-count (count (re-find #"^\.*" state))
        last-dots-count (count (re-find #"\.*$" state))]
    (list (apply str (concat
                      "...."
                      (->>
                       state
                       (drop first-dots-count)
                       (drop-last last-dots-count))
                      "...."))
          (- zero-pos (- first-dots-count 4)))))

(defn add-pots [state zero-pos]
  (reduce-kv (fn [result k v]
               (if (= v \#)
                 (+ (- k zero-pos) result)
                 result))
             0
             (apply vector state)))

(defn evolve-state [[state rules] times]
  (reduce (fn [[new-state zero-pos] iter]
            (do
              (if (= 0 (mod iter 10000))
                (spit "output"
                      (apply str (concat
                                  (str iter)
                                  " "
                                  (str zero-pos)
                                  " "
                                  (str (add-pots new-state zero-pos))
                                  " "
                                  new-state
                                  "\n"))
                      :append true))
              (let [[adj-state adj-pos] (trim-expand new-state zero-pos)]
                (list (apply-matches
                       adj-state
                       (deref (find-all-matches adj-state rules)))
                      adj-pos))))
          [state 0]
          (range 0 times)))

; Part 1
; (apply add-pots (evolve-state input 20))

; Part 2 (pattern found?)
(+ 697 (* 15 50000000000))
