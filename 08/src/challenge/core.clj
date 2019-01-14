(ns challenge.core
  [:require [clojure.string :as string]])

(def filename "input")

(def input (map read-string (string/split (slurp filename) #"\s")))

(defn build-tree [[child-num meta-num & rtree]]
  (if (= child-num 0)
    (list {:children `() :meta (take meta-num rtree)} (drop meta-num rtree))
    (loop [children `() remaining rtree]
      (if (= (count children) child-num)
        (list {:children (reverse children) :meta (take meta-num remaining)} (drop meta-num remaining))
        (let [[child next-tree] (build-tree remaining)]
          (recur (conj children child) next-tree))))))

(defn add-meta [{children :children meta-nums :meta}]
  (reduce + (concat meta-nums (map add-meta children))))

(defn add-complex-meta [{children :children meta-nums :meta}]
  (if (= 0 (count children))
    (reduce + meta-nums)
    (let [childs (vec children) idxs (map dec meta-nums)]
      (reduce + (map (fn [idx]
                       (if (contains? childs idx)
                         (add-complex-meta (childs idx))
                         0))
                     idxs)))))

; PART 1
; (add-meta (first (build-tree input)))

; PART 2
(add-complex-meta (first (build-tree input)))
