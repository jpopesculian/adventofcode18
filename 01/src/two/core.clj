(ns two.core
  [:require [clojure.string :as str]])

(def coll (map read-string (str/split (slurp "input") #"\n")))

(defn gen-freqs [start]
  (loop [remaining coll
         result []]
    (if (empty? remaining)
      result
      (let [[f-rem & l-rem] remaining]
        (recur l-rem (conj result (+ (or (last result) start) f-rem)))))))

(defn loop-freqs []
  (loop [remaining (gen-freqs 0) seen-freqs #{}]
    (let [[f-rem & l-rem] remaining]
      (if (contains? seen-freqs f-rem)
        f-rem
        (if (empty? l-rem)
          (recur (gen-freqs f-rem) (conj seen-freqs f-rem))
          (recur l-rem (conj seen-freqs f-rem)))))))

(loop-freqs)
