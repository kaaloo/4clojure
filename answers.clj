;26 - Fibonacci Sequence
;==========================

; Write a function which returns the first X fibonacci numbers.

#(take % (map second (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

;27 - Palindrome Dectector
;==========================

; Write a function which returns true if the given sequence is a palindrome

(def palindrome?
  (fn [[x & xs]]
  (cond
    (nil? xs) true
    (= x (last xs)) (recur (butlast xs))
    :default false)))

(palindrome? '(1 2 4 3 2 1))

;29 - Get The Caps
;==========================

; Write a function which takes a string and returns a new string containing only the capital letters.

#(clojure.string/replace % #"[^A-Z]" "")

;30 - Compress a Sequence
;==========================

; Write a function which removes consecutive duplicates from a sequence.

#(map first (partition-by identity %))

;31 - Pack a sequence
;==========================

; Write a function which packs consecutive duplicates into sub-lists.

(partial partition-by identity)

;32 - Duplicate a sequence
;==========================

; Write a function which duplicates each element of a sequence.

#(reduce concat (for [l %] (repeat 2 l)))

; Better solution
#(interleave % %)

;34 - Implement range
;==========================

; The some function takes a predicate function and a collection. It returns the first logical true value of (predicate x) where x is an item in the collection.
#(take (- %2 %1) (iterate inc %1))

; alternative solution
#(map-indexed + (repeat (- %2 %1) %1))

; 41 - Drop Every Nth Item
;==========================

; Write a function which drops every Nth item from a sequence.

#(for [[a b] (map-indexed (fn [a b] [a b]) %1) :when (not= (dec %2) (mod a %2))] b)

; 42 - Factorial Fun
;==========================

; Write a function which calculates factorials

#(reduce * (range 1 (inc %)))

; 42 - Split a sequence
;==========================

; Write a function which will split a sequence into two parts.

#(vector (take %1 %2) (drop %1 %2))

; 61 - Map construction
;==========================

; Write a function which takes a vector of keys and a vector of values and constructs a map from them

#(array-map (interleave %1 %2))

; better solutions

#(apply assoc {}
         (interleave %1 %2))

(fn [keys values]
  (apply array-map (interleave keys values)))

; 61 - Re-implement iterate
;==========================

; Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.

(fn t [f x]
    (cons x (lazy-seq (t f (f x)))))

(take 2 (g #(*2 %) 1))


; 63 - Group a sequence
;==========================

; Given a function f and a sequence s, write a function which returns a map. The keys should be 
; the values of f applied to each item in s. The value at each key should be a vector of corresponding items
; in the order they appear in s.

(fn [f s]
  (into {} (for [k (set (map f s))]
                [k (vec (filter #(= k (f %)) s))])))

; 66 - Greatest Common Dividor
;=============================

; Given two integers, write a function which returns the greatest common divisor.
; (see http://en.wikipedia.org/wiki/Greatest_common_divisor#Calculation)

#(if (= %2 0) %1 (recur %2 (mod %1 %2)))

; 81 - Set intersection
;==========================

; Write a function which returns the intersection of two sets. The intersection is the sub-set of items that each set has in common

(fn [a b]
  (set (filter #(and (contains? a %) (contains? b %)) (clojure.set/union a b))))

;
; 83 - A half truth
;==========================

; Write a function which takes a variable number of booleans. Your function should return true
; if some of the parameters are true, but not all of the parameters are true. Otherwise your function should return false.

(fn [& args] (and ((comp boolean some) identity args) (not-every? identity args)))

; 90 - Cartesian product
;==========================

; Write a function which calculates the Cartesian product of two sets.

#(set (for [a %1 b %2] [a b]))

; 99 - Product Digits
;==========================

; Write a function which multiplies two numbers and returns the result as a sequence of its digits.

#(map (fn [c] (- (int c) (int \0))) (seq (str (* %1 %2))))

;
; 107 - Simple closures
;==========================

; Lexical scope and first-class functions are two of the most basic building blocks of a functional language like Clojure.
; When you combine the two together, you get something very powerful called lexical closures. With these, you can exercise
; a great deal of control over the lifetime of your local bindings, saving their values for use later, long after the code
; you're running now has finished.

; It can be hard to follow in the abstract, so let's build a simple closure. Given a positive integer n, return
; a function (f x) which computes xn. Observe that the effect of this is to preserve the value of n for use outside
; the scope in which it is defined.

partial #(reduce * (repeat %1 %2))

; 122 - Read a binary number
;===========================

; Convert a binary number, provided in the form of a string, to its numerical value.

#(reduce + 0 (map-indexed (fn [n item] (if (= item \0) 0 (Math.pow 2 n))) (reverse %)))


; 166 - Compairisons
;==========================

; For any orderable data type it's possible to derive all of the  basic comparison operations (<, ≤, =, ≠, ≥, and >) from a single operation
; (any operator but = or ≠ will work). Write a function that takes three arguments, a less than operator for the data and two items to compare.
; The function should return a keyword describing the relationship between the two items. The keywords for the relationship between x and y are as follows:
;    x = y → :eq
;    x > y → :gt
;    x < y → :lt

(fn [op x y]
    (cond (op x y) :lt
          (op y x) :gt
          :default :eq))
