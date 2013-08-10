(ns clojure_macros.core)

(defmacro my-and 
  ([] true)
  ([arg] arg)
  ([arg & args]
    `(let [result# ~arg]
       (if result#
         (my-and ~@args)
         result#))))

(defmacro my-or
  ([] nil)
  ([arg] arg)
  ([arg & args]
    `(let [result# ~arg]
       (if result#
         result#
         (my-or ~@args)))))

(defmacro my-when [arg & args]
  `(if ~arg 
     (do ~@args)))

(defmacro my-when-not [arg & args]
  `(if ~arg
     nil
     (do ~@args)))

(defmacro my-when-let [bindings & body] 
  (if (and (vector? bindings) (= 2 (count bindings)))
    `(let [test# ~(second bindings)]
       (when test#
         (let [~(first bindings) test#]
           ~@body)))
    `(throw (IllegalArgumentException.))))

(defmacro my-while [arg & args]
  `(loop []
    (if ~arg 
      (do ~@args (recur)))))

(defmacro my->
  ([arg] arg)
  ([arg lst]
   (let [llst (if (list? lst) lst (list lst))]
     (list* (first llst) arg (rest llst))))
  ([arg lst-first & lst-rest]
   `(my-> (my-> ~arg ~lst-first) ~@lst-rest)))

(defmacro my->>
  ([arg lst]
     (concat (if (list? lst) lst (list lst)) [arg]))
  ([arg lst-first & lst-rest]
   `(my->> (my->> ~arg ~lst-first) ~@lst-rest)))

(defmacro my-cond
  ([] nil)
  ([arg] `(throw (IllegalArgumentException.)))
  ([tst arg] (if tst arg))
  ([tst arg & args] 
   (if (even? (count args))
     `(if ~tst ~arg (my-cond ~@args))
     `(throw (IllegalArgumentException.)))))

(defmacro my-condp 
  ([pred expr] `(throw (IllegalArgumentException.)))
  ([pred expr default] default)
  ([pred expr test-expr & clauses]
   (if (and (= (first clauses) :>>) (> (count clauses) 1))
     `(let [result# (~pred ~test-expr ~expr)]
        (if result# (~(second clauses) result#)
          (my-condp ~pred ~expr ~@(drop 2 clauses))))
     `(if (~pred ~test-expr ~expr) ~(first clauses)
        (my-condp ~pred ~expr ~@(rest clauses))))))

(defmacro my-cond->
  ([expr] expr)
  ([expr test-expr] (throw (AssertionError.)))
  ([expr test-expr form]
   `(if ~test-expr (-> ~expr ~form) ~expr))
  ([expr test-expr form & forms]
   (if (odd? (count forms)) (throw (AssertionError.))
     `(if ~test-expr (my-cond-> (-> ~expr ~form) ~@forms)
        (my-cond-> ~expr ~@forms)))))

(defmacro my-for [bindings & body]
  (if-not (and (vector? bindings) (even? (count bindings)))
    (throw (IllegalArgumentException.))
    (if (= 2 (count bindings))
      `(loop [values# ~(second bindings)
              ~(first bindings) (first ~(second bindings))
              result# '()]
        (if (empty? values#)
          (reverse result#)
          (recur (rest values#)
                 (second values#)
                 (cons ~@body result#))))
      `(loop [values# ~(second bindings)
              ~(first bindings) (first ~(second bindings))
              result# '()]
        (if (empty? values#)
          result#
          (recur (rest values#)
                 (second values#)
                 (concat result# 
                         (my-for ~(subvec bindings 2) ~@body))))))))
