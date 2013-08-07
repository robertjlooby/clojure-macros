(ns clojure_macros.core)

(defmacro my-and 
  ([] true)
  ([arg] arg)
  ([arg & args]
    `(if ~arg
       (my-and ~@args)
       ~arg)))
