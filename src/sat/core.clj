(ns sat.core
  (:gen-class))

(defn get-var [var]
  (max var (- var)))

(defn pos-var? [var]
  (pos? var))

(defn neg-var? [var]
  (neg? var))

(defn cn [i]
  (keyword (str "c" i)))

(defn get-var-val [var context]
  (get-in context [:variable-table var :val]))

(defn get-clause-var-val [clause-var context]
  (let [v (get-var-val (get-var clause-var) context)]
    (if (pos-var? clause-var) v (not v))))

(defn process-clauses [clauses]
  (let [clause-mapping (->> clauses
                            (map-indexed #(hash-map %2 (cn %1) (cn %1) %2))
                            (into {}))
        clause-status (into {} (map #(hash-map (get clause-mapping %1) (count %1))
                                    clauses))
        variable-table (reduce
                        (fn [acc c]
                          (reduce
                           (fn [acc cv]
                             (update acc (get-var cv)
                                     (fn [{:keys [val +clauses -clauses]
                                           :or {val :unknown}}]
                                       (as-> (get clause-mapping c) cn
                                         {:val val
                                          :+clauses (if (pos-var? cv)
                                                      (conj +clauses cn)
                                                      +clauses)
                                          :-clauses (if (neg-var? cv)
                                                      (conj -clauses cn)
                                                      -clauses)}))))
                           acc c))
                        {} clauses)
        variable-count (count (keys variable-table))]
    {:clauses clauses
     :clause-names (map #(get clause-mapping %1) clauses)
     :clause-mapping clause-mapping
     :clause-status clause-status
     :variable-table variable-table
     :variable-count variable-count
     :variable-pending-count variable-count
     :satisfied nil}))

(defn assign-true [input var]
  (as-> input input
    (assoc-in input [:variable-table var :val] true)
    (update input :variable-pending-count dec)
    (reduce #(assoc-in %1 [:clause-status %2] 0) input (get-in input [:variable-table var :+clauses]))
    (reduce #(update-in %1 [:clause-status %2] dec)  input (get-in input [:variable-table var :-clauses]))))

(defn assign-false [input var]
  (as-> input input
    (assoc-in input [:variable-table var :val] false)
    (update input :variable-pending-count dec)
    (reduce #(update-in %1 [:clause-status %2] dec) input (get-in input [:variable-table var :+clauses]))
    (reduce #(assoc-in %1 [:clause-status %2] 0)  input (get-in input [:variable-table var :-clauses]))))

(defn get-unassigned-variable [context]
  (some #(when (= (:val (second %1)) :unknown) (first %1)) (:variable-table context)))

(defn get-variable-val [context var]
  (get-in context [:variable-table var :val]))

(defn var-is-neg? [var]
  (> var 0))

(defn solve-clause
  "If all the variables of the given clause has been assigned, return the
  result, otherwise return :unknown"
  [context c]
  (reduce (fn [res cvar]
            (let [var (get-var cvar)
                  val (get-variable-val context var)
                  cval (if (or (= cvar var)
                               (= val :unknown))
                         val
                         (not val))]
              (if (not= cval :unknown)
                (or res cval)
                (reduced :unknown))))
          false
          (get-in context [:clause-mapping c])))

(defn contradiction?
  "For every clause that has been assigned completely, check if there is any
  contradiction"
  [context]
  (or (= false (:satisfied context))
      (not-every? #(solve-clause context %1) (:clause-names context))))

(defn dpll [context]
  (if (>= 0 (:variable-pending-count context))
    context
    (let [uvar (get-unassigned-variable context)
          context+t (assign-true context uvar)
          context+f (assign-false context uvar)]
      (as-> (dpll context+t) ct
        (if (not (contradiction? ct))
          ct
          (as-> (dpll context+f) cf
            (if (not (contradiction? cf))
              cf
              (assoc context :satisfied false))))))))

(defn valid-solution?
  "Given a context with an assignment, verify if the assignment satisfies the
  clause"
  [context]
  (reduce (fn [acc clause]
            (and acc
                 (reduce  #(or %1 (get-clause-var-val %2 context))
                          false
                          clause)))
          true (:clauses context)))

(defn get-solution [context]
  (->> (map (fn [[k {val :val}]]
              (if (true? val)
                k
                (* -1 k))) (:variable-table context))
       (sort (fn [x y]
               (< (if (> x 0) x (* -1 x))
                  (if (> y 0) y (* -1 y)))))))
