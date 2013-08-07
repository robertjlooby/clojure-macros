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

(defmacro my-when
  ([arg & args]
   `(if ~arg 
      (do ~@args))))
