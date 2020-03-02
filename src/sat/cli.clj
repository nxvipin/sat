(ns sat.cli
  (:require [sat.core :as sc]
            [sat.dimacs :as dimacs])
  (:gen-class))

(defn -main [dimacs-file]
  (let [dimacs-input (slurp dimacs-file)
        context (-> (dimacs/parse dimacs-input)
                    (get-in [:dimacs :clauses])
                    sc/process-clauses
                    sc/dpll)]
    (if (sc/valid-solution? context)
      (do
        (println "s SATISFIABLE")
        (apply println (concat "v" (sc/get-solution context))))
      (println "s UNSATISFIABLE"))))
