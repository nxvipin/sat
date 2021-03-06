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
(s/def ::conflict? boolean?)

(s/def ::context (s/keys :req-un [::clauses
                                  ::variables
                                  ::decision-level
                                  ::watched-literals
                                  ::variables-assigned
                                  ::variables-unassigned
                                  ::conflict?]))

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
       (into {} (map #(vector %1 [(first %1) (second %1)])))
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
  (not (variable-assigned? context variable)))

(defn literal-assigned?
  "Given a context and a literal, return true if the literal is assigned
  otherwise return false"
  [context literal]
  (->> literal
       literal->variable
       (variable-assigned? context)))

(defn literal-unassigned?
  "Given a context and a literal, return true if the literal is unassigned
  otherwise return false"
  [context literal]
  (not (literal-assigned? context literal)))

(defn get-clause-assigned-watched-literals
  "Given a context and a clause, return the list of watched literals that have
  been assigned"
  [context clause]
  (let [[l1 l2] (get-in context [:watched-literals clause])
        l1-assigned? (literal-assigned? context l1)
        l2-assigned? (literal-assigned? context l2)]
    (cond
      (and l1-assigned? l2-assigned?) [l1 l2]
      (and l1-assigned? (not l2-assigned?)) [l1]
      (and (not l1-assigned?) l2-assigned?) [l2]
      (and (not l1-assigned?) (not l2-assigned?)) [])))

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
        value (get-variable-value context variable)]
    (cond
      (nil? value) ;; Literal is unassigned
      value

      (= literal variable) ;; Literal is not negated
      value

      (not= literal variable) ;; Literal is negated
      (not value))))

(defn get-clause-unassigned-literals
  "Given a context, clause and optionally a list of excluded literals with zero or
  more elements, return a list of clause literals which is unassigned and not
  present in the excluded literal list."
  ([context clause]
   (get-clause-unassigned-literals context clause []))
  ([context clause excluded-literals]
   (let [el-set (set excluded-literals)]
     (filterv #(and (literal-unassigned? context %)
                    (not (contains? el-set %)))
              clause))))

(defn update-watched-literal
  "Given a context and a caluse and watched literal pair, update the clause's
  watched literal pair with the given input pair and return the updated context"
  [context clause watched-literal-pair]
  (assoc-in context [:watched-literals clause] watched-literal-pair))

(defn compute-clause-value [context clause]
  (reduce (fn [acc literal]
            (let [lv (get-literal-value context literal)]
              (cond
                (true? lv) (reduced true)
                (nil? lv) (reduced nil)
                (false? lv) (or acc lv))))
          false
          clause))

(defn compute-clause-status
  "Given a context and a clause, return the clause status map with key `:status`
  and value being one of `:unsatisfied`, `:statisfied`, `:unit` or `:resolved`.
  If `:status` is `:unit`, the status map has an additonal key called
  `:unassigned-literal` with the value being the lone unassigned literal in the
  clause"
  [context clause]
  (let [awls (get-clause-assigned-watched-literals context clause)]
    (if (empty? awls)
      ;; The watched literals are unassigned - the clause is definitely not unit
      [context {:status :unresolved}]

      ;; At least one of the watched literals is assigned - we need to compute
      ;; the status
      (let [ul (take 2 (get-clause-unassigned-literals context clause))
            ul-count (count ul)]
        (cond
          ;; There are at least 2 unassigned literals, this clause is
          ;; unnresolved. Update the watched literals of this caluse and return
          ;; the updated context and the status
          (= ul-count 2)
          [(update-watched-literal context clause ul) {:status :unresolved}]

          ;; There is only one unassigned literal, this clause is a unit
          (= ul-count 1)
          [context {:status :unit
                    :unassigned-literal (first ul)}]

          ;; This clause is either satisfied or unsatisfied
          (= ul-count 0)
          (if (true? (compute-clause-value context clause))
            [context {:status :satisfied}]
            [context {:status :unsatisfied}]))))))

(defn compute-all-clause-status
  "Given a context, compute the current status of all clauses and return a map
  with the status value as the key and and a list of clauses with that status as
  the value. Additionally, the map has a key `:unit-clause-literal` with value
  as a map with keys being the current unit clauses and the values being the
  unassigned literal in the unit clause"
  [{:keys [clauses] :as context}]
  (reduce (fn [[context status-map] clause]
            (let [[context {:keys [status unassigned-literal]}]
                  (compute-clause-status context clause)]
              [context (cond-> status-map
                         true (update status (fn [n] (conj n clause)))
                         (= :unit status) (assoc-in [:unit-clause-literal clause]
                                                    unassigned-literal))]))
          [context {}]
          clauses))

(defn get-variable-decision-level
  "Given a context and a variable, return the decision level of the variable if it
  is assigned, nil otherwise"
  [context variable]
  (get-in context [:variables-assigned variable :decision-level]))

(defn clause-max-decision-level
  "Given a context and a clause, return the highest decision level of all the
  assigned literals in the clause. If none of the literals are assigned, the
  retunr value is zero"
  [context clause]
  (reduce (fn [dl literal]
            (let [vdl (->> literal
                           literal->variable
                           (get-variable-decision-level context))
                  vdl (if (some? vdl) vdl -1)]
              (max dl vdl)))
          0 clause))

(defn get-next-decision-level
  "Given a context, get the next decision level for the next `decision
  assignment`"
  [context]
  (inc (:decision-level context)))

(defn assign-variable
  "Given a context, a variable and a value, assign the value to the variable and
  return the updated context. If the assignment is an `implcation assignment`,
  the 4-arity version of this function also accepts the antecedent"
  ([context variable value]
   (assign-variable context variable value nil))
  ([context variable value antecedent]
   (let [dl (if (some? antecedent)
              (clause-max-decision-level context antecedent)
              (get-next-decision-level context))]
     (-> context
         (update :decision-level #(if (some? antecedent) % dl))
         (update :variables-unassigned #(disj % variable))
         (assoc-in [:variables-assigned variable :value] value)
         (assoc-in [:variables-assigned variable :antecedent] antecedent)
         (assoc-in [:variables-assigned variable :decision-level] dl)
         (update-in [:variables-assigned variable :flipped?] #(if (nil? %)
                                                                false
                                                                true))))))

(defn unit-propagate
  "Given a context, assign true value to the unssigned literal of each unit
  clauses recursively until there are no more unit clauses or a conflict is
  detected. Return the updated context."
  [context]
  (let [[context clause-status] (compute-all-clause-status context)
        unsatisfied-clauses (:unsatisfied clause-status)
        unit-clauses (:unit clause-status)
        unit-clause-literal (:unit-clause-literal clause-status)]
    (cond
      ;; If there are unsatisfied clauses, either there is a conflict or the
      ;; input is unsatisfiable - stop processing and return immediately
      (some? unsatisfied-clauses)
      (assoc context :conflict? true)

      ;; If there are unit clauses, proceed with unit propagation
      (some? unit-clauses)
      (reduce
       (fn [context clause]
         (let [unassigned-literal (get unit-clause-literal clause)
               unassigned-variable (literal->variable unassigned-literal)
               variable-value (if (= unassigned-literal unassigned-variable)
                                true
                                false)]
           (-> (assign-variable context unassigned-variable variable-value clause)
               unit-propagate)))
       context
       unit-clauses)

      ;; There are no unit clauses and there are no unsatisfied clauses. Nothing
      ;; to do here, return the context
      :else
      context)))
