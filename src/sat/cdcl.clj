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
                                  ::watched-literals
                                  ::variables-assigned
                                  ::variables-unassigned]))

(def test-input {:dimacs
                 {:problem (list 3 5),
                  :clauses (list [1 -5 4]
                                 [-1 5 3 4]
                                 [-3 -4])}})


(defn literal->variable
  "Given a clause literal, return the variable it represents"
  [literal]
  (max literal (- literal)))

(defn input->clauses
  "Given a dimacs input, return the list of clauses"
  [input]
  (-> input
      :dimacs
      :clauses))

(defn init-context-variable-fields
  "Given a context with `:clauses`, set context `:variables`,
  `:variables-assigned` and `:variables-unassigned` and return the updated
  context"
  [context]
  (let [variables (->> context
                       :clauses
                       flatten
                       (map literal->variable)
                       set)]
    (-> context
        (assoc :variables variables)
        (assoc :variables-assigned {})
        (assoc :variables-unassigned variables))))

(defn init-context-watched-literals
  "Given a context with clauses, set context watched literals and return updated
  context"
  [context]
  (->> context
       :clauses
       (map #(hash-map %1 [(first %1) (second %1)]))
       (assoc context :watched-literals)))

(defn initialize-context
  "Given a dimacs input, intialize and return the context map"
  [input]
  (-> {:clauses (input->clauses input)
       :decision-level 0}
      init-context-variable-fields
      init-context-watched-literals))

(defn get-unassigned-variable
  "Given a context, return an unassigned variable if available otherwise return
  nil"
  [context]
  (-> context
      :variables-unassigned
      first))

(defn variable-assigned?
  "Given a context and a variable, return true if the variable has been assigned
  otherwise return false"
  [context variable]
  (contains? (:variables-assigned context) variable))

(defn variable-unassigned?
  "Given a context and a variable, return true if the variable is unassigned
  otherwise returns false"
  [context variable]
  (not (assigned? context variable)))

(defn literal-assigned?
  "Given a context and a literal, return true if the literal is assigned
  otherwise return false"
  [context literal]
  (-> literal
      literal->variable
      variable-assigned?))

(defn literal-unassigned?
  "Given a context and a literal, return true if the literal is unassigned
  otherwise return false"
  [context literal]
  (not (literal-unassigned? context literal)))

(defn watched-literal-assigned?
  "Given a context and a clause, return true if ar least one of the two watched
  literals of the clause is assigned"
  [context clause]
  (let [[l1 l2] (get-in context [:watched-literals clause])]
        (or (assigned? l1)
            (assigned? l2))))

(defn get-watched-literals
  "Given a context and a clause, return a list of watched literals of the clause"
  [context clause]
  (-> context :watched-literals clause))

(defn get-variable-value
  "Given a context and a variable, return the value of the variable if assigned
  otherwise returns nil"
  [context variable]
  (get-in context [:variables-assigned variable :value]))

(defn get-literal-value
  "Given a context and a literal, return the value of the literal if assigned
  otherwise returns nil"
  [context literal]
  (let [variable (literal->variable literal)
        value (get-variable-value variable)]
    (cond
      (nil? value) ;; Literal is unassigned
      value

      (= literal variable) ;; Literal is not negated
      value

      (not= literal variable) ;; Literal is negated
      (not value))))

(defn get-clause-unassigned-literal
  "Given a context, clause and optionally a list of excluded literals with zero or
  more elements, return a clause literal which is unassigned and not present in
  the excluded literal list. Return nil if no such literal exists."
  ([context clause]
   (get-clause-unassigned-literal context clause []))
  ([context clause excluded-literals]
   (let [el-set (set excluded-literals)]
     (some #(when (and (variable-unassigned? (literal->variable %))
                      (not (contains? el-set %)))
              %)
           clause))))
