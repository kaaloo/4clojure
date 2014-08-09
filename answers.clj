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

; 88 - Symmetric Difference
; =========================

; Write a function which returns the symmetric difference of two sets. The symmetric difference is the set of items
; belonging to one but not both of the two sets.

(fn [s1 s2]
  (set
    (for [e (clojure.set/union s1 s2)
            :when (or (nil? (s1 e)) (nil? (s2 e)))]
      e)))

; 90 - Cartesian product
;==========================

; Write a function which calculates the Cartesian product of two sets.

#(set (for [a %1 b %2] [a b]))

; 95 - To Tree, or not to Tree
;=============================

; Write a predicate which checks whether or not a given sequence represents a binary tree. 
; Each node in the tree must have a value, a left child, and a right child.

(defn tree?
  ([] false)
  ([t] 
   (if (and (coll? t) (= (count t) 3))
     (let [[v l r] t]
       (and (or (nil? l) (tree? l)) (or (nil? r) (tree? r)))) 
     false))
  ([a b & rest] false))

; 96 - Beauty is Symmetry
;=============================

; Let us define a binary tree as "symmetric" if the left half of the tree is the mirror image of the right half of the tree. 
; Write a predicate to determine whether or not a given binary tree is symmetric. 
; (see To Tree, or not to Tree for a reminder on the tree representation we're using).

(fn symmetric?
  ([[v l r]]
   (or (and (nil? l) (nil? r))
       (symmetric? l r)))
  ([[v1 l1 r1] [v2 l2 r2]]
   (and (= v1 v2)
        (or (and (nil? l1) (nil? r2))
            (symmetric? l1 r2))
        (or (and (nil? r1) (nil? l2))
            (symmetric? r1 l2)))))

; Learn - really cool answer, uses implicit traversal of the = operator.

(fn [[v l r]]
  (= r ((fn flip [[v l r :as t]]
          (when t
            [v (flip r) (flip l)])) l)))
; 97 - Pascal's Triangle
;==========================

; Pascal's triangle is a triangle of numbers computed using the following rules:
;
; - The first row is 1.
; - Each successive row is computed by adding together adjacent numbers in the row above, and adding a 1 to the beginning and end of the row.
;
; Write a function which returns the nth row of Pascal's Triangle.

(fn pascal [n]
  (condp = n
    1 [1]
    2 [1 1]
    (conj (into [1] (mapv #(apply + %) (partition 2 1 (pascal (dec n))))) 1)))

; 99 - Product Digits
;==========================

; Write a function which multiplies two numbers and returns the result as a sequence of its digits.

#(map (fn [c] (- (int c) (int \0))) (seq (str (* %1 %2))))

; 100 - Least Common Multiple
;============================

; Write a function which calculates the least common multiple. 
; Your function should accept a variable number of positive integers or ratios.

; See http://en.wikipedia.org/wiki/Least_common_multiple#A_simple_algorithm

; Learned:
; - recur uses a fixed number of arguments, can't "apply" recur

(fn lcm [& args]
  (loop [x args]
    (if (apply = x) (first x)
      (let [least (apply min x)
          index (.indexOf x least)
          delta (nth args index)
          next (map-indexed #(if (= %1 index) (+ %2 delta) %2) x)]
        (recur next)))))

(let [args '(2 3) 
      least (first (sort args)) 
      index (.indexOf args least)] 
  (map-indexed #(if (= %1 index) (+ %2 least) %2)))

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

; 118 - Re-implement Map
;==========================

; Map is one of the core elements of a functional programming language. Given a function f and an input sequence s, 
; return a lazy sequence of (f x) for each element x in s.

(fn m [f x]
  (if (empty? x) ()
    (cons (f (first x)) (lazy-seq (m f (rest x))))))


; 118 - Sum of square of digits
;==============================

; Write a function which takes a collection of integers as an argument. Return the count of how many elements are smaller 
; than the sum of their squared component digits. 
; For example: 10 is larger than 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared

(comp count (partial filter (fn [n] (< n (reduce + (map #(* % %) (map #(- (int %) 48) (str n))))))))

; 122 - Read a binary number
;===========================

; Convert a binary number, provided in the form of a string, to its numerical value.

#(reduce + 0 (map-indexed (fn [n item] (if (= item \0) 0 (int (Math/pow 2 n)))) (reverse %)))

; 126 - Through the Looking Class
;================================

; Enter a value which satisfies the following.

(let [x Class]
  (and (= (class x) x) x))

; 128 - Recognize Playing Cards
;================================

; A standard American deck of playing cards has four suits - spades, hearts, diamonds, and clubs - 
; and thirteen cards in each suit. Two is the lowest rank, followed by other integers up to ten; 
; then the jack, queen, king, and ace.
;
; It's convenient for humans to represent these cards as suit/rank pairs, such as H5 or DQ: the heart five 
; and diamond queen respectively. But these forms are not convenient for programmers, so to write a card game
; you need some way to parse an input string into meaningful components. For purposes of determining rank,
; we will define the cards to be valued from 0 (the two) to 12 (the ace)
;
; Write a function which converts (for example) the string "SJ" into a map of {:suit :spade, :rank 9}. 
; A ten will always be represented with the single character "T", rather than the two characters "10".

(defn parse-card [spec]
  (zipmap 
      [:suit :rank]
      (map {\S :spade \H :heart \D :diamond \C :club
        \2 0 \3 1 \4 2 \5 3 \6 4 \7 5 \8 6 \9 7 \T 8 \J 9 \Q 10 \K 11 \A 12 } spec)))

; 135 - Infix Calculator
;================================

; Your friend Joe is always whining about Lisps using the prefix notation for math. Show him how you could easily write a function that does math using the infix notation.
; Is your favorite language that flexible, Joe? Write a function that accepts a variable length mathematical expression consisting of numbers and the
; operations +, -, *, and /. Assume a simple calculator that does not do precedence and instead just calculates left to right.

(fn infix [a op b & rest]
  (let [r (op a b)]
    (if (nil? rest) r
      (apply infix (conj rest r)))))

; Much better solution:

(fn [x & ops]
  (reduce
    (fn [x [op operand]] (op x operand))
    x
    (partition 2 ops)))

; 143 - Dot product
;===========================

; Create a function that computes the dot product of two sequences. You may assume that the vectors will have the same length.

(fn [s1 s2]
  (apply + (map #(* (first %) (second %)) (partition 2 (interleave s1 s2)))))

; Much better solution

(fn [as bs]
  (reduce + (map * as bs)))


; 146 - Trees Into Tables
;===========================

; Because Clojure's for macro allows you to "walk" over multiple sequences in a nested fashion, 
; it is excellent for transforming all sorts of sequences. If you don't want a sequence as your 
; final output (say you want a map), you are often still best-off using for, because you can produce
; a sequence and feed it into a map, for example.

; For this problem, your goal is to "flatten" a map of hashmaps. Each key in your output map 
; should be the "path"1 that you would have to take in the original map to get to a value, 
; so for example {1 {2 3}} should result in {[1 2] 3}. You only need to flatten one level of 
; maps: if one of the values is a map, just leave it alone.

#(into 
  {}
  (for [[k v] %
        [k2 v2] v]
       [[k k2] v2]))

; 147 - Pascal's Trapezoid
;===========================

; Write a function that, for any given input vector of numbers, returns an infinite lazy sequence of vectors, 
; where each next one is constructed from the previous following the rules used in Pascal's Triangle. 
; For example, for [3 1 2], the next row is [3 4 3 2].

; Beware of arithmetic overflow! In clojure (since version 1.3 in 2011), 
; if you use an arithmetic operator like + and the result is too large to fit into a 64-bit integer, an exception is thrown. 
; You can use +' to indicate that you would rather overflow into Clojure's slower, arbitrary-precision bigint.

#(iterate 
  (fn [a]
    (mapv (fn [[a b]] (+' a b))
         (partition 2
                   (interleave (conj a 0) (into [0] a))))) %)


; 153 - Pairwise Disjoint Sets
;=============================

; Given a set of sets, create a function which returns true if no two of those sets have any 
; elements in common1 and false otherwise. Some of the test cases are a bit tricky, 
; so pay a little more attention to them.

; Learned : To manipulate head and tail, you need to convert set into a list using
; (into '() set), (seq set) works too.
; - and is a macro and can't be applied or used as a reducer
; - it's sometimes better to code based on a logical conclusion (all elements in each set have to
; be distinct, if the sets are pairwise disjoint) instead of coding a literal solution.

#(let [none-in-common (fn [[s1 s2]]
                       (= #{} (clojure.set/intersection s1 s2)))
       all-pairs (fn all-pairs [[h & t :as s]]
                    (if (nil? t) nil
                      (concat (for [x t] [h x])
                              (all-pairs t))))]
   (reduce (fn [a b] (and a b)) 
           (map none-in-common (all-pairs (seq %)))))

(defn none-in-common [[s1 s2]]
                       (= #{} (clojure.set/intersection s1 s2)))
(defn all-pairs [[h & t :as s]]
  (if (nil? t) nil
    (concat (for [x t] [h x])
            (all-pairs t))))

; Much better answer, codes a consequence of being pairwise disjoint

(fn [src]
  (let [x (mapcat #(distinct (seq %)) src)]
    (= (count x) (count (distinct x)))
   )
  )

; 157 - Indexing Sequences
;===========================

; Transform a sequence into a sequence of pairs containing the original elements along with their index.

#(map (fn [a b] [a b]) % (range))

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

; 173 - Intro to Destructuring 2
;===============================

; Sequential destructuring allows you to bind symbols to parts of sequential things (vectors, lists, seqs, etc.): (let [bindings* ] exprs*) 
; Complete the bindings so all let-parts evaluate to 3.

(= 3
  (let [[__] [+ (range 3)]] (apply __))
  (let [[[__] b] [[+ 1] 2]] (__ b))
  (let [[__] [inc 2]] (__)))

; answer "a c"