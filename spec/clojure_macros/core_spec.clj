(ns clojure_macros.core-spec
  (:require [speclj.core :refer :all]
            [clojure_macros.core :refer :all]))

(describe "and"
  (it "should be true for no args"
    (should (my-and)))

  (it "should be true for one true arg"
    (should (my-and true)))

  (it "should be false for one false arg"
    (should-not (my-and false)))

  (it "should be false for '(false true)"
    (should-not (my-and false true)))

  (it "should be false for '(true false)"
    (should-not (my-and true false)))

  (it "should be true for all trues"
    (should (my-and true true true true)))

  (it "should be false if one false"
    (should-not (my-and true true true false)))
  
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

  (it "should not call functions passed as args"
    (should (my-and (constantly false) (> 2 1))))
)
