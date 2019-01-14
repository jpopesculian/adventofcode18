(ns two.core
  [:require [clojure.string :as string]])

(def coll (string/split (slurp "input") #"\n"))

(defn count-letts [word]
  (loop [remaining (string/split word #"") letter-counts {}]
    (if (empty? remaining) letter-counts
        (let [[letter & rest-letters] remaining]
          (let [letter-count (get letter-counts letter)]
            (recur
             rest-letters
             (conj letter-counts
                   (if (nil? letter-count)
                     {letter 1}
                     {letter (inc letter-count)}))))))))

(defn has-n [letter-counts number]
  (some #(= % number) (vals letter-counts)))

(defn loop-ids [ids]
  (loop [remaining ids twos 0 threes 0]
    (if (empty? remaining) [twos threes]
        (let [[id & rest-ids] remaining]
          (let [letter-counts (count-letts id)]
            (recur
             rest-ids
             (if (has-n letter-counts 2) (inc twos) twos)
             (if (has-n letter-counts 3) (inc threes) threes)))))))

(defn part-one []
  (reduce #(* %1 %2) (loop-ids coll)))

(defn remove-char [word pos]
  (apply str (concat (subs word 0 pos) (subs word (inc pos)))))

(defn find-matching [ids]
  (let [[needle & stack] (sort ids)]
    (loop [remaining stack subject needle]
      (if (empty? remaining) nil
          (let [[id & rest-ids] remaining]
            (if (= id subject) subject (recur rest-ids id)))))))

(defn remove-char-list [words pos]
  (map #(remove-char % pos) words))

(defn part-two [ids]
  (some #(and (not (nil? %)) %)
        (map (fn [letter] (find-matching (remove-char-list ids letter)))
             (range 0 (count (get ids 0))))))
