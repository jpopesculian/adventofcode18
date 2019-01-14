(ns challenge.core
  [:require [clojure.string :as string]])

(def filename "input")

(def input (sort (string/split (slurp filename) #"\n")))

; (println (string/join "\n" input))

(defn parse_input [input]
  (let [[time-part event-part] (string/split input #"\]\s")]
    (let [event (cond
                  (re-matches #"Guard.*" event-part) :start
                  (re-matches #"falls.*" event-part) :sleep
                  (re-matches #"wakes.*" event-part) :wake)]
      (do
        {:time (read-string (re-find #"[1-9]?[0-9]$" time-part))
         :event event
         :id (if (= event :start) (read-string (re-find #"\d+" event-part)))}))))

(defn parse_inputs [inputs]
  (loop [parsed [] remaining inputs]
    (if (empty? remaining) parsed
        (let [[input & rinputs] remaining]
          (recur (conj parsed (parse_input input)) rinputs)))))

(defn sleep-ranges [parsed-inputs]
  (loop [cur-id nil ranges {} remaining parsed-inputs]
    (if (empty? remaining) ranges
        (let [[input & rinputs] remaining]
          (if (= (input :event) :start) (recur (input :id) ranges rinputs)
              (recur
               cur-id
               (assoc
                ranges
                cur-id
                (conj
                 (get ranges cur-id)
                 (range (input :time) ((first rinputs) :time))))
               (rest rinputs)))))))

(defn sleep-time [ranges]
  (sort-by last > (map #(list (first %) (count (flatten (last %)))) (into `() ranges))))

(defn mode [numbers]
  (->> numbers
       flatten
       frequencies
       (sort-by val >)
       first))

; PART 1
; (let [ranges (sleep-ranges (parse_inputs input))]
;   (let [most-sleep (first (first (sleep-time ranges)))]
;     (let [minute (first (mode (ranges most-sleep)))]
;       (* most-sleep minute))))

; PART 2
(let [most-repeated (first (sort-by #(last (last %)) >
                                    (map
                                     #(list (first %) (mode (last %)))
                                     (into `() (sleep-ranges (parse_inputs input))))))]
  (* (first most-repeated) (first (last most-repeated))))
