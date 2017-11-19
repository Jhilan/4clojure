;19- Write a function which returns the last element in a sequence. (Special Restrictions last).
;(= (__ [1 2 3 4 5]) 5)
;(= (__ '(5 4 3)) 3)
;(= (__ ["b" "c" "d"]) "d")
;(= (__ ["b" "c" "d"]) "d")
(fn [s] (nth s (dec(count s))))

;20- Write a function which returns the second to last element from a sequence.
;(= (__ (list 1 2 3 4 5)) 4)
;(= (__ ["a" "b" "c"]) "b")
;(= (__ [[1 2] [3 4]]) [1 2])
(fn [s] (nth s (- (count s) 2)))

;21- Write a function which returns the Nth element from a sequence. (Special Restrictions nth).
;(= (__ '(4 5 6 7) 2) 6)
;(= (__ [:a :b :c] 0) :a)
;(= (__ [1 2 3 4] 1) 2)
;(= (__ '([1 2] [3 4] [5 6]) 2) [5 6])
#(last (take (+ %2 1) %1))

;22- Write a function which returns the total number of elements in a sequence.
;(Special Restrictions count)
;(= (__ '(1 2 3 3 1)) 5)
;(= (__ "Hello World") 11)
;(= (__ [[1 2] [3 4] [5 6]]) 3)
;(= (__ '(13)) 1)
;(= (__ '(:a :b :c)) 3)
(fn countSequense [sq]
       (loop [coll sq
              cnt 0
                    (if (first coll)
                      (recur (rest coll) (inc cnt))
                      cnt)]))



;23- Write a function which reverses a sequence. (Special Restrictions reverse, rseq).
;(= (__ [1 2 3 4 5]) [5 4 3 2 1])
;(= (__ (sorted-set 5 7 2 7)) '(7 5 2))
;(= (__ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])
#(into '() %)

;24- Write a function which returns the sum of a sequence of numbers.
;(= (__ [1 2 3]) 6)
;(= (__ (list 0 -2 5 5)) 8)
;(= (__ #{4 2 1}) 7)
;(= (__ '(0 0 -1)) -1)
;(= (__ '(1 10 3)) 14)
#(reduce + %)

;25- Write a function which returns only the odd numbers from a sequence.
;(= (__ #{1 2 3 4 5}) '(1 3 5))
;(= (__ [4 2 1 6]) '(1))
;(= (__ [2 2 4 6]) '())
;(= (__ [1 1 1 3]) '(1 1 1 3))
#(filter odd? %)

;27- Write a function which returns true if the given sequence is a palindrome.
;Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)
;(false? (__ '(1 2 3 4 5)))
;(true? (__ "racecar"))
;(true? (__ [:foo :bar :foo]))
;(true? (__ '(1 1 3 3 1 1)))
;(false? (__ '(:a :b :c)))
(fn palindrome? [coll]
  (let [cnt (quot (count coll) 2)
        first-half (take cnt coll)])
  (if (even? (count coll)))
  (= first-half (reverse (drop cnt coll)))
  (= first-half (reverse (drop (inc cnt) coll))))

;26- Write a function which returns the first X fibonacci numbers.
;(= (__ 3) '(1 1 2))
;(= (__ 6) '(1 1 2 3 5 8))
;(= (__ 8) '(1 1 2 3 5 8 13 21))
(fn fib [n]
  (loop [a 0
         b 1
         size n
         acc []]
   (if (= 0 size)
     acc
     (recur b (+ a b) (dec size) (conj acc b)))))

;38- Write a function which takes a variable number of parameters and returns the maximum value.
;(= (__ 1 8 3 4) 8)
;(= (__ 30 20) 30)
;(= (__ 45 67 11) 67)
(fn [& params] (last (sort params)))

;29- Write a function which takes a string and returns a new string containing only the capital letters.
;(= (__ "HeLlO, WoRlD!") "HLOWRD")
;(empty? (__ "nothing"))
;(= (__ "$#A(*&987Zf") "AZ")
(fn [s])
(->>
 (char-array s)
 (filter #(and (> (int %) 64) (< (int %) 91)))
 (apply str))

;32- Write a function which duplicates each element of a sequence.
;(= (__ [1 2 3]) '(1 1 2 2 3 3))
;(= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
;(= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
;(= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
(fn dupe [xs] (mapcat #(vector % %) xs))

;48- The some function takes a predicate function and a collection. It returns the first logical true value of (predicate x) where x is an item in the collection.
;(= __ (some #{2 7 6} [5 6 7 8]))
;(= __ (some #(when (even? %) %) [5 6 7 8]))
6

;34- Write a function which creates a list of all integers in a given range. (Special Restrictions range).
;(= (__ 1 4) '(1 2 3))
;(= (__ -2 2) '(-2 -1 0 1))
;(= (__ 5 8) '(5 6 7))
#(take (- %2 %1) (iterate inc %1))

;42- Write a function which calculates factorials.
;(= (__ 1) 1)
;(= (__ 3) 6)
;(= (__ 5) 120)
;(= (__ 8) 40320)
(fn [n]  (reduce * (range 1 (inc n))))

;28- Write a function which flattens a sequence. (Special Restrictions flatten).
;(= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
;(= (__ ["a" ["b"] "c"]) '("a" "b" "c"))
;(= (__ '((((:a))))) '(:a))
(fn flat-seq [sx]
  (let [flatter (fn flat [acc my-list]
                  (cond
                    (empty? my-list) acc
                    (sequential? (first my-list)) (flat (flat acc (first my-list)) (rest my-list))
                    :else (flat (conj acc (first my-list)) (rest my-list))))]
    (flatter [] sx)))

;33- Write a function which replicates each element of a sequence a variable number of times.
;(= (__ [1 2 3] 2) '(1 1 2 2 3 3))
;(= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b))
;(= (__ [4 5 6] 1) '(4 5 6))
;(= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
;(= (__ [44 33] 2) [44 44 33 33])
(fn rep [coll n]  (mapcat #(repeat n %) coll))

;45- The iterate function can be used to produce an infinite lazy sequence.
;(= __ (take 5 (iterate #(+ 3 %) 1)))
'(1 4 7 10 13)

;47- The contains? function checks if a KEY is present in a given collection. This often leads beginner clojurians to use it incorrectly with numerically indexed collections like vectors and lists.
;(contains? #{4 5 6} __)
;(contains? [1 1 1 1 1] __)
;(contains? {4 :a 2 :b} __)
;(not (contains? [1 2 4] __))
4

;49- Write a function which will split a sequence into two parts. (Special Restrictions split-at).
;(= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
;(= (__ 1 [:a :b :c :d]) [[:a] [:b :c :d]])
;(= (__ 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])
#(concat (vector (take %1 %2)) (vector (drop  %1 %2)))

;51- Here is an example of some more sophisticated destructuring.
;(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d]))
[1 2 3 4 5]

;83- Write a function which takes a variable number of booleans. Your function should return true if some of the parameters are true, but not all of the parameters are true. Otherwise your function should return false.
;(= false (__ false false))
;(= true (__ true false))
;(= false (__ true))
;(= true (__ false true false))
;(= false (__ true true true))
;(= true (__ true true true false))
(fn [& coll]
  (let [co (set coll)]
   (and (.contains coll true) (.contains coll false))))

;88- Write a function which returns the symmetric difference of two sets. The symmetric difference is the set of items belonging to one but not both of the two sets.
;(= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
;(= (__ #{:a :b :c} #{}) #{:a :b :c})
;(= (__ #{} #{4 5 6}) #{4 5 6})
;(= (__ #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})
(fn [set1 set2]
  (set (remove #(and (contains? set1 %) (contains? set2 %)) (clojure.set/union set1 set2))))

;143- Create a function that computes the dot product of two sequences. You may assume that the vectors will have the same length.
;(= 0 (__ [0 1 0] [1 0 0]))
;(= 3 (__ [1 1 1] [1 1 1]))
;(= 32 (__ [1 2 3] [4 5 6]))
;(= 256 (__ [2 5 6] [100 10 1]))
(fn dot-product [vec1 vec2]
 (->>
  (map vector vec1 vec2)
  (map #(* (first %)  (second %)))
  (reduce +)))

;157- Transform a sequence into a sequence of pairs containing the original elements along with their index.
;(= (__ [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
;(= (__ [0 1 3]) '((0 0) (1 1) (3 2)))
;(= (__ [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])
(fn index [coll] (map #(vector % (.indexOf coll %)) coll))

;95- Write a predicate which checks whether or not a given sequence represents a binary tree. Each node in the tree must have a value, a left child, and a right child.
;(= (__ '(:a (:b nil nil) nil)))
;   true)
;(= (__ '(:a (:b nil nil))))
;   false)
;(= (__ [1 nil [2 [3 nil nil] [4 nil nil]]]))
;   true)
;(= (__ [1 [2 nil nil] [3 nil nil] [4 nil nil]]))
;   false)
;(= (__ [1 [2 [3 [4 nil nil] nil] nil] nil]))
;   true)
;(= (__ [1 [2 [3 [4 false nil] nil] nil] nil]))
;   false)
;(= (__ '(:a nil ())))
;   false)
(fn is-tree [tree]
  (cond
    (coll? tree) (if (= (count tree) 3))
           (= true (is-tree (second tree)) (is-tree (nth tree 2))
              false)
    (false? tree) false
    :else true))

;173- Sequential destructuring allows you to bind symbols to parts of sequential things (vectors, lists, seqs, etc.): (let [bindings* ] exprs*) Complete the bindings so all let-parts evaluate to 3.
;(= 3)
;  (let [[__] [+ (range 3)]] (apply __)))
;  (let [[[__] b] [[+ 1] 2]] (__ b)))
;  (let [[__] [inc 2]] (__)))
a +

;96- Let us define a binary tree as "symmetric" if the left half of the tree is the mirror image of the right half of the tree. Write a predicate to determine whether or not a given binary tree is symmetric. (see To Tree, or not to Tree for a reminder on the tree representation we're using).
;(= (__ '(:a (:b nil nil) (:b nil nil))) true)
;(= (__ '(:a (:b nil nil) nil)) false)
;(= (__ '(:a (:b nil nil) (:c nil nil))) false)
;
;(= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]]))
;          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]]))
;  true)
;
;(= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]]))
;          [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]]))
;   false)
;(= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]]))
;          [2 [3 nil [4 [6 nil nil] nil]] nil]]))
;   false)
(fn symmetric? [tree]
  (let [mirror? (fn equal-sides? [left right]
                 (cond
                   (not= (type left) (type right)) false
                   (coll? left) (and (= (first left) (first right))
                                     (equal-sides? (second left) (last right))
                                     (equal-sides? (last left) (second right)))
                   :else (= left right)))]
    (mirror? (second tree) (last tree))))
