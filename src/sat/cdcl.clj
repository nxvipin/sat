(ns sat.cdcl
  {:doc "CDCL Algorithm"
   :author "Vipin Nair"}
  (:require
   [clojure.spec.alpha :as s])
  (:gen-class))


;; **CDCL Algorithm**
;; Ref: Handbook of Satisfiability
;;
;; CDCL(φ, ν)
;; if (UnitPropagation(φ, ν) == CONFLICT)
;;    then return UNSAT
;; dl ← 0                                        <-- Decision level
;; while (not AllVariablesAssigned(φ, ν))
;;    do (x, v) = PickBranchingVariable(φ, ν)
;;    dl ← dl + 1                                <-- Increment decision level due to new decision
;;    ν ← ν ∪ {(x, v)}
;; if (UnitPropagation(φ, ν) == CONFLICT)        <-- Decide stage
;;    then β = ConflictAnalysis(φ, ν)            <-- Diagnose stage
;;       if (β<0)
;;          then return UNSAT
;;          else Backtrack(φ, ν, β)
;;               dl ← β                          <-- Decrement decision level due to backtracking
;; return SAT


(s/def ::literal int?)
(s/def ::clause (s/coll-of ::literal))
(s/def ::clauses (s/coll-of ::clause))
(s/def ::watched-literals (s/map-of ::clause
                                    (s/tuple ::literal ::literal)))

(s/def ::decision-level (s/and int? #(> % 0)))
(s/def :variable/value boolean?)
(s/def :variable/decision-level ::decision-level)
(s/def :variable/flipped? boolean?)
(s/def :variable/antecedent (s/or ::clause nil))
(s/def ::variable-name (s/and int? #(> % 0)))
(s/def ::variables-assigned (s/map-of ::variable-name
                                      (s/keys :req-un [:variable/value
                                                       :variable/decision-level
                                                       :variable/antecedent
                                                       :variable/flipped?])))
(s/def ::variables (s/coll-of ::variable-name))
(s/def ::variables-unassigned (s/coll-of ::variable-name))

(s/def ::context (s/keys :req-un [::clauses
                                  ::variables
                                  ::decision-level
                                  ::variables-assigned
                                  ::variables-unassigned]))
