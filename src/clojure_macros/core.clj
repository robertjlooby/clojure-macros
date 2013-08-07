(ns clojure_macros.core)

(defmacro my-and [& args]
  `(if (empty? '~args)
     true
     (loop [head# (first '~args)
            tail# (rest  '~args)]
       (if (not head#)
         head#
         (if (not (eval head#))
           (eval head#)
           (if (empty? tail#)
             (eval head#)
             (recur (first tail#) (rest tail#))))))))
