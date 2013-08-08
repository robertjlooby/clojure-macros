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
