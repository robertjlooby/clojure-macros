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
