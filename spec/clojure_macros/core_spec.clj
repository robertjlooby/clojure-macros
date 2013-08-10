(ns clojure_macros.core-spec
  (:require [speclj.core :refer :all]
            [clojure_macros.core :refer :all]))

(describe "and"
  (it "should be true for no args"
    (should= true (my-and)))

  (it "should be true for one true arg"
    (should= true (my-and true)))

  (it "should be false for one false arg"
    (should= false (my-and false)))

  (it "should be false for '(false true)"
    (should= false (my-and false true)))

  (it "should be false for '(true false)"
    (should= false (my-and true false)))

  (it "should be true for all trues"
    (should= true (my-and true true true true)))

  (it "should be false if one false"
    (should= false (my-and true true true false)))
  
  (it "should be nil for nil"
    (should= nil (my-and nil)))

  (it "should be nil for '(true nil false)"
    (should= nil (my-and true nil false)))

  (it "should be false for '(true false nil)"
    (should= false (my-and true false nil)))

  (it "should be 1 for '(0 1)"
    (should= 1 (my-and 0 1)))

  (it "should be 0 for '(1 0)"
    (should= 0 (my-and 1 0)))

  (it "should be [] for '([] [])"
    (should= [] (my-and [] [])))

  (it "should evaluate a single true form"
    (should= true (my-and (> 3 1))))

  (it "should evaluate a single false form"
    (should= false (my-and (> 1 3))))

  (it "should evaluate false then true forms"
    (should= false (my-and (> 1 3) (> 3 1))))

  (it "should evaluate true then false forms"
    (should= false (my-and (> 3 1) (> 1 3))))

  (it "should not call functions passed as args"
    (should= true (my-and (constantly false) (> 2 1))))

  (it "should not evaluate forms past the first false one"
    (should-not-throw (my-and (> 1 3) (throw (Exception. "")))))

  (it "should only evaluate forms once"
    (let [a (atom 0)]
      (my-and (= 0 (swap! a inc)) true)
      (should= 1 @a)))
)

(describe "or"
  (it "should return nil for no args"
    (should= nil (my-or)))

  (it "should return true for '(true)"
    (should= true (my-or true)))

  (it "should return 1 for '(1)"
    (should= 1 (my-or 1)))

  (it "should return nil for '(nil)"
    (should= nil (my-or nil)))

  (it "should return true for '(true false)"
    (should= true (my-or true false)))

  (it "should return false for '(nil nil false)"
    (should= false (my-or nil nil false)))

  (it "should evaluate a single true form"
    (should= true (my-or (> 3 1))))

  (it "should not call functions passed as args"
    (should= true (fn? (my-or (constantly false) (> 2 1)))))

  (it "should not evaluate forms past the first true one"
    (should-not-throw (my-or (> 3 1) (throw (Exception. "")))))

  (it "should only evaluate forms once"
    (let [a (atom 0)]
      (my-or (= 1 (swap! a inc)) true)
      (should= 1 @a)))
)

(describe "when"
  (it "should return nil for '(nil)"
    (should= nil (my-when nil)))

  (it "should return nil for '(true)"
    (should= nil (my-when true)))
  
  (it "should evaluate a form that is passed in (once) and return nil"
    (let [a (atom 0)]
      (should= nil (my-when (swap! a inc)))
      (should= 1 @a)))

  (it "should return true for '(true true)"
    (should= true (my-when true true)))

  (it "should return false for '(true false)"
    (should= false (my-when true false)))

  (it "should evaluate all forms that are passed in, return last"
    (let [a (atom 0)
          b (atom 0)]
      (should= 2 (my-when (swap! a inc) (swap! b inc) (swap! a inc)))
      (should= 2 @a)
      (should= 1 @b)))

  (it "should not evaluate forms after first if it returns false"
    (should-not-throw (my-when (> 1 3) (throw (Exception. "")))))
)

(describe "when-not"
  (it "should return nil for '(nil)"
    (should= nil (my-when-not nil)))

  (it "should return nil for '(true)"
    (should= nil (my-when-not true)))

  (it "should return nil for '(false)"
    (should= nil (my-when-not false)))
  
  (it "should evaluate a form that is passed in (once) and return nil"
    (let [a (atom 0)]
      (should= nil (my-when-not (swap! a inc)))
      (should= 1 @a)))

  (it "should return nil for '(true true)"
    (should= nil (my-when-not true true)))

  (it "should return true for '(false true)"
    (should= true (my-when-not false true)))

  (it "should evaluate all forms that are passed in, return last"
    (let [a (atom 0)
          b (atom 0)]
      (should= 2 (my-when-not (= 0 (swap! a inc)) (swap! b inc) (swap! a inc)))
      (should= 2 @a)
      (should= 1 @b)))

  (it "should not evaluate forms after first if it returns true"
    (should-not-throw (my-when-not (> 3 1) (throw (Exception. "")))))
)

(describe "when-let"
  (it "should return nil for [a 1]"
    (should= nil (my-when-let [a 1])))

  (it "should return nil for [[][]]"
    (should= nil (my-when-let [[][]])))

  (it "should throw IllegalArgumentException if num of vecs in binding is != 2"
    (should-throw IllegalArgumentException (my-when-let [a]))
    (should-not-throw (my-when-let [a 1]))
    (should-not-throw (my-when-let [[a b] [1 2]]))
    (should-throw IllegalArgumentException (my-when-let [a 1 b 2])))

  (it "should throw IllegalArgumentException if binding is not a vector"
    (should-not-throw (my-when-let [[][]]))
    (should-throw IllegalArgumentException (my-when-let '(a 1))))

  (it "should evaluate 'test' once and not body if test is false, return nil"
    (let [a (atom 0)
          b (atom 0)]
      (should= nil (my-when-let [x (do (swap! a inc) nil)] 
                      (swap! b inc)))
      (should= 1 @a)
      (should= 0 @b)))

  (it "should evaluate 'test' once and body if test is true, return output of body"
    (let [a (atom 0)
          b (atom 5)]
      (should= 6 (my-when-let [x (swap! a inc)] 
                      (swap! b inc)))
      (should= 1 @a)
      (should= 6 @b)))

  (it "should bind the results of 'test' and use in body"
    (should= 5 (my-when-let [x 5] x)))

  (it "should evaluate all the statements in body"
    (let [a (atom 0)
          b (atom 5)]
      (should= 6 (my-when-let [x true] (swap! a inc) (swap! b inc)))
      (should= 1 @a)
      (should= 6 @b)))
)

(describe "while"
  (it "should return nil for '(false)"
    (should= nil (my-while false)))

  (it "should return nil for '(> 1 3)"
    (should= nil (my-while (> 1 3))))

  (it "should evaluate the test"
    (let [a (atom 0)]
      (should= nil (my-while (do (swap! a inc) nil)))
      (should= 1 @a)))

  (it "should not evaluate the body if test is false"
    (let [a (atom 0)]
      (should= nil (my-while false (swap! a inc)))
      (should= 0 @a)))

  (it "should evaluate the body if the test is true, until it is false"
    (let [a (atom 0)]
      (should= nil (my-while (< @a 5) (swap! a inc)))
      (should= 5 @a)))

  (it "should evaluate all the body expressions"
    (let [a (atom 0)
          b (atom 5)]
      (should= nil (my-while (< @a 3) (swap! a inc) (swap! b inc)))
      (should= 3 @a)
      (should= 8 @b)))
)

(describe "->"
  (it "should return nil for nil"
    (should= nil (my-> nil)))

  (it "should return 1 for 1"
    (should= 1 (my-> 1)))

  (it "should return true for false not"
    (should= true (my-> false not)))
   
  (it "should return false for (< 3 1)"
    (should= false (my-> (< 3 1))))

  (it "should return false for 3 (< 1)"
    (should= false (my-> 3 (< 1))))

  (it "should return [5 4] for 5 (vector (+ 2 2))"
    (should= [5 4] (my-> 5 (vector (+ 2 2)))))

  (it "should return [[[5 4] 3] 2] for series of vector args"
    (should= [[[5 4] 3] 2] (my-> 5 (vector 4)
                                   (vector 3)
                                   (vector 2))))

  (it "should return true for false not not (= false)"
    (should= true (my-> false not not (= false))))
)

(describe "->>"
  (it "should return 7 for 5 (+ 2)"
    (should= 7 (my->> 5 (+ 2))))

  (it "should return true for 3 (< 1)"
    (should= true (my->> 3 (< 1))))

  (it "should return true for false not"
    (should= true (my->> false not)))

  (it "should return true for false not not (= false)"
    (should= true (my->> false not not (= false))))

  (it "should return [2 [3 [4 5]]] for series of vector args"
    (should= [2 [3 [4 5]]] (my->> 5 (vector 4)
                                    (vector 3)
                                    (vector 2))))
)

(describe "cond"
  (it "should return nil for no args"
    (should= nil (my-cond)))

  (it "should return :a for '(true :a)"
    (should= :a (my-cond true :a)))

  (it "should return nil for '(false :a)"
    (should= nil (my-cond false :a)))

  (it "should return :b for '(false :a true :b)"
    (should= :b (my-cond false :a true :b)))

  (it "should only evaluate the tests once, and only until one returns truthy"
    (let [a (atom 0)]
      (should= :a (my-cond (swap! a inc) :a (swap! a inc) :b))
      (should= 1 @a)))

  (it "should not evaluate the return values unless the test is truthy"
    (let [a (atom 0)]
      (should= :c (my-cond nil (swap! a inc) nil (swap! a inc) 1 :c))
      (should= 0 @a)))

  (it "should evaluate the :else condition if no tests are truthy"
    (let [a (atom 0)]
      (should= 1 (my-cond nil :a nil :b :else (swap! a inc)))
      (should= 1 @a)))

  (it "should throw an Illegal argument exception if there are an odd number of forms"
    (should-throw IllegalArgumentException (my-cond :a))
    (should-throw IllegalArgumentException (my-cond :a :b :c))
    (should-throw IllegalArgumentException (my-cond :a :b :c :d :e)))

  (it "should not evaluate any forms if there are an odd number of forms"
    (let [a (atom 0)]
      (should-throw IllegalArgumentException
                    (my-cond (swap! a inc) :a :b))
      (should= 0 @a)))
)

(describe "condp"
  (it "should return :a for '(= true true :a)"
    (should= :a (my-condp = true true :a)))

  (it "should evaluate (pred test-expr expr) once"
    (let [a (atom 0)]
      (should= :b (my-condp swap! inc a :b))
      (should= 1 @a)))

  (it "should return :b for '(= true false :a true :b)"
    (should= :b (my-condp = true false :a true :b)))

  (it "should throw IllegalArgumentException if no matching clauses"
    (should-throw IllegalArgumentException 
                  (my-condp = true false :a false :b false :c)))

  (it "should return the default clause if present and no matching clauses"
    (should= :c (my-condp = true false :a false :b :c)))

  (it "should call result fn on (pred test-expr expr) when :>> and truthy"
    (should= 5 (my-condp + 3 1 :>> inc)))

  (it "should only call (pred test-expr expr) once when :>> and truthy"
    (let [a (atom 0)]
      (should= 2 (my-condp swap! inc a :>> inc))
      (should= 1 @a)))

  (it "should call (pred test-expr expr) for each :>> group until one is truthy"
    (let [a (atom 0)
          b (atom 0)
          c (atom 1)]
      (should= false (my-condp = 2 (swap! a inc) :>> inc 
                                (swap! b inc) :>> inc 
                                (swap! c inc) :>> not))
      (should= 1 @a)
      (should= 1 @b)
      (should= 2 @c)))

  (it "should call default when no :>> groups are truthy"
    (should= :c (my-condp = 3 1 :>> not 2 :>> not :c)))

  (it "should allow :>> as the last result expr of a normal pair"
    (should= :>> (my-condp = true false :a false :b true :>>)))

  (it "should allow a mix of normal pairs and :>> groups, resulting in a normal pair"
    (should= :c (my-condp = true false :>> not true :c true :>> not)))

  (it "should allow a mix of normal pairs and :>> groups, resulting in a group"
    (should= false (my-condp = true false :>> inc true :>> not true :c)))
)

(describe "cond->"
  (it "should return nil for nil"
    (should= nil (my-cond-> nil)))

  (it "should return 1 for 1"
    (should= 1 (my-cond-> 1)))

  (it "should return true for '(< 1 3)"
    (should= true (my-cond-> (< 1 3))))

  (it "should return 5 for '(5 false inc)"
    (should= 5 (my-cond-> 5 false inc)))

  (it "should return 6 for '(5 true inc)"
    (should= 6 (my-cond-> 5 true inc)))

  (it "should return 6 for '(5 true inc false inc)"
    (should= 6 (my-cond-> 5 true inc false inc)))

  (it "should return 7 for '(5 true inc true inc)"
    (should= 7 (my-cond-> 5 true inc true inc)))

  (it "should evaluate all test-exprs"
    (let [a (atom 0)]
      (should= 8 (my-cond-> 5 (swap! a inc) inc
                              (swap! a inc) inc
                              (swap! a inc) inc))
      (should= 3 @a)))

  (it "should evaluate expr for each truthy test-expr and pass the result as the expr for the next evaluation"
    (let [a (atom 0)]
      (should= 3 (my-cond-> (swap! a inc) true inc
                                       true inc
                                       false inc))
      (should= 1 @a)))

  (it "should not evaluate any exprs if there are an odd number of forms, should throw AssertionError"
    (let [a (atom 0)]
      (should= :a (try 
                     (macroexpand '(my-cond-> (swap! a inc) 
                                            true inc 
                                            false))
                     (catch AssertionError e :a)
                     ))
      (should= 0 @a)))

  (it "should not evaluate any exprs if there are no forms, should throw AssertionError"
    (let [a (atom 0)]
      (should= :a (try 
                     (macroexpand '(my-cond-> (swap! a inc) 
                                              true))
                     (catch AssertionError e :a)
                     ))
      (should= 0 @a)))
)

(describe "for"
  (it "should return '() for '([[][]] nil)"
    (should= '() (my-for [[][]] nil)))

  (it "should return '(1 2 3) for '([x [1 2 3]] x)"
    (should= '(1 2 3) (my-for [x [1 2 3]] x)))

  (it "should return '(0 1 4 9 16) for '([x (take 5 (range))] (* x x))"
    (should= '(0 1 4 9 16) (my-for [x (take 5 (range))] (* x x))))

  (it "should return '([1 2]) for '([x [1] y [2]] [x y])"
    (should= '([1 2]) (my-for [x [1] y [2]] [x y])))

  (it "should return '([1 1] [1 2] [2 1] [2 2]) for '([x [1 2] y [1 2]] [x y])"
    (should= '([1 1] [1 2] [2 1] [2 2]) (my-for [x [1 2] y [1 2]] [x y])))

  (it "should be '([1 nil]) for '([[x y] ['(1)]])"
    (should= '([1 nil]) (my-for [[x y] ['(1)]] [x y])))

  (it "should ignore extra values in right half of binding"
    (should= '([0 1] [3 4] [6 7]) 
             (my-for [[x y] (partition 3 (take 9 (range)))] [x y])))

  (it "should use nil for extra values in left half of binding"
    (should= '([0 1 nil] [2 3 nil] [4 5 nil]) 
             (my-for [[x y z] (partition 2 (take 6 (range)))] [x y z])))

  (it "should be correct for complicated case"
    (should=
      (for [[x y] (repeat 5 '(1 2 3)) z '(1 2 3 4) a '(1 2 nil)]
          [x y (+ x y) (* z z) 0 a])
      (my-for [[x y] (repeat 5 '(1 2 3)) z '(1 2 3 4) a '(1 2 nil)]
          [x y (+ x y) (* z z) 0 a])))

  (it "should throw IllegalArgumentException if bindings is not a vector"
    (should= :a (try 
                  (macroexpand-1 '(my-for '(a [1]) [a]))
                  (catch IllegalArgumentException e :a))))

  (it "should throw IllegalArgumentException if odd number of forms in binding"
    (should= :a (try
                 (macroexpand-1 '(my-for [x [1 2] y] [x y]))
                 (catch IllegalArgumentException e :a))))
)
