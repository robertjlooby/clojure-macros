(ns clojure_macros.core)

(defmacro my-and [& args]
  `(if (empty? '~args)
     true
     (loop [head# (eval (first '~args))
            tail# (rest  '~args)]
       (if (not head#)
         head#
         (if (empty? tail#)
           head#
           (recur (eval (first tail#)) (rest tail#)))))))
