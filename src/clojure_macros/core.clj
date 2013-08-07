(ns clojure_macros.core)

(defmacro my-and 
  ([] true)
  ([arg] arg)
  ([arg & args]
    `(let [result# ~arg]
       (if result#
         (my-and ~@args)
         result#))))
