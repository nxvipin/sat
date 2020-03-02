(ns sat.dimacs
  (:require [instaparse.core :as insta])
  (:gen-class))

(def dimacs-spec
  "dimacs     = <comments> problem clauses

   <comments> = comment*
   <comment>  = 'c' #'.*' '\n'

   problem    = <whitespace*> <'p'> <whitespace> <'cnf'> <whitespace+> nbvar <whitespace+> nbclauses <'\n'>
   whitespace = #'\\s'
   nbvar      = number
   nbclauses  = number
   <number>   = #'[0-9]+'

   clauses    = clause+
   clause     = <whitespace*> (variable <whitespace>)+ <'0'> <'\n'>
   <variable> = #'-*[0-9]+'")

(def dimacs-parser (insta/parser dimacs-spec))

(defn parse [input]
  (->> (dimacs-parser input)
       (insta/transform {:nbvar #(Integer/parseInt %)})
       (insta/transform {:nbclauses #(Integer/parseInt %)})
       (insta/transform {:clause #(mapv (fn [x] (Integer/parseInt x)) %&)})
       (reduce (fn [m x]
                 (cond
                   (= x :dimacs)
                   (assoc m :dimacs {})

                   (and (vector? x) (= :problem (first x)))
                   (assoc-in m [:dimacs :problem] (rest x))

                   (and (vector? x) (= :clauses (first x)))
                   (assoc-in m [:dimacs :clauses] (rest x))))
               {})))
