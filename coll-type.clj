;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.
(defn is-list? [l] 
  (= (first (conj l {:foo :bar})) {:foo :bar}))

(defn is-map? [m]
  (if (is-vector? (first m))
    (let [[k v] (first m)]
      (= (m k) v))
    false))

(defn is-vector? [v]
  (= (last (conj v {:foo :bar})) {:foo :bar}))

(defn is-set? [s]
  (= (conj (conj s 5) 5) (conj s 5)))

(defn coll-type [coll]
  (cond 
   (is-map? coll) :map
   (is-set? coll) :set
   (is-list? coll) :list
   (is-vector? coll) :vector
   ))

(= (coll-type {:a 1, :b 2}) :map)
(= (coll-type (range (rand-int 20))) :list)
(= (coll-type [1 2 3 4]) :vector)
(= (coll-type #{10 (rand-int 5)}) :set)

(first {:a 1, :b 2})
(is-set? #{2 3})

(conj (conj #{1 2} 5) 5)
