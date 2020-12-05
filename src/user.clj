(ns user
  (:require
    [clojure.spec.alpha :as s]
    [expound.alpha :as expound]
    [kaocha.repl]))


(alter-var-root #'s/*explain-out* (constantly expound/printer))


(when (thread-bound? #'s/*explain-out*)
  (set! s/*explain-out* expound/printer))
