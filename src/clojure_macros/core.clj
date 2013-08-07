(ns clojure_macros.core)

(defmacro my-and [& args]
  `(if (empty? '~args)
     true
     (loop [head# (first '~args)
            tail# (rest  '~args)]
       (if (not head#)
         head#
         (if (empty? tail#)
           head#
           (recur (first tail#) (rest tail#)))))))
