(ns challenge.core
  [:require [clojure.string :as string]])

(def filename "input")

(defn parse-input [input-line]
  (reverse (map last (re-seq #"[Ss]tep ([A-Z])" input-line))))

(def input (map parse-input (string/split (slurp filename) #"\n")))

(defn map-deps [inputs]
  (reduce
   (fn [deps [from to]]
     (assoc deps from (conj (deps from) to)))
   (into {} (distinct (map #(vector (last %) (set `())) inputs)))
   inputs))

(defn find-no-deps [deps & [no-deps]]
  (sort
   (distinct
    (concat no-deps
            (map first (filter #(empty? (last %)) deps))))))

(defn remove-dep [deps dep]
  (dissoc
   (reduce-kv
    (fn [m k v] (assoc m k (disj (set v) dep)))
    {}
    deps)
   dep))

(defn get-order [deps]
  (loop [order `() no-deps `() remaining deps]
    (if (and (empty? remaining) (empty? no-deps)) (reverse order)
        (let [[dep & rest-no-deps] (find-no-deps remaining no-deps)]
          (recur
           (conj order dep)
           rest-no-deps
           (remove-dep remaining dep))))))

(defn get-char-val [char-str]
  (- (int (first char-str)) 4))

(defn build-queues [amount]
  (into [] (map (fn [_] nil) (range amount))))

(defn queues-empty [queues]
  (reduce-kv (fn [m k v] (if v m (conj m k))) `() queues))

(defn assign-queues [queues deps start]
  (let [empty-queues (queues-empty queues)]
    (list (reduce (fn [new-queues [idx job]]
                    (assoc new-queues idx job))
                  queues
                  (map vector
                       empty-queues
                       (map #(list % (+ start (get-char-val %)))
                            deps)))
          (or (take-last
               (- (count deps) (min (count empty-queues) (count deps)))
               deps) `()))))

(defn finish-queues [queues stop]
  (loop [new-queues queues deps `() idx 0]
    (if (= idx (count queues)) (list new-queues deps)
        (let [[dep dep-stop] (queues idx)]
          (if (nil? dep) (recur new-queues deps (inc idx))
              (if (>= stop dep-stop)
                (recur (assoc new-queues idx nil) (conj deps dep) (inc idx))
                (recur new-queues deps (inc idx))))))))

(defn except-assigned [queues deps]
  (filter (fn [dep]
            (reduce (fn [truthiness queue-dep]
                      (and truthiness (not (= dep queue-dep))))
                    true
                    (map first queues))) deps))

(defn put-together-deps [deps num-workers]
  (loop [queues (build-queues num-workers)
         remaining deps
         no-deps `()
         cur-time 0]
    (if (and
         (= (count (queues-empty queues)) num-workers)
         (empty? remaining))
      (dec cur-time)
      (let [[finished-queues finished-deps] (finish-queues queues cur-time)]
        (let [rdeps (reduce #(remove-dep %1 %2) remaining finished-deps)]
          (let [new-deps (except-assigned finished-queues (find-no-deps rdeps no-deps))]
            (let [[assigned-queues rest-deps]
                  (assign-queues finished-queues new-deps cur-time)]
              (recur
               assigned-queues
               rdeps
               rest-deps
               (inc cur-time)))))))))

; PART 1
; (apply str (get-order (map-deps input)))

; PART 2
(put-together-deps (map-deps input) 5)
