(ns clox.resolver
  (:require [clox.expression :as expr]
            [clox.report :as report]
            [clox.statement :as stmt]))

(defn error
  [{:keys [message token]}]
  (report/error
   {:line (:line token)
    :where (str " at " (:lexeme token))
    :message message}))

(declare resolve-stmts)
(declare begin-scope)

(defn declare!
  [scopes name-token]
  (when-let [scope (last scopes)]
    (let [var-name (:lexeme name-token)]
      (when (contains? @scope var-name)
        (error {:message "Already a variable with this name in this scope."
                :token name-token}))
      (swap! scope assoc var-name :declared))))

(defn define!
  [scopes name-token]
  (when-let [scope (last scopes)]
    (swap! scope assoc (:lexeme name-token) :defined)))

(defn resolve-variable!
  [{:keys [locals]} expr depth]
  (swap! locals assoc expr depth))

(defn resolve-local!
  [{:keys [interpreter scopes]} expression name-token]
  (when-let [[i] (->> (reverse scopes)
                      (map (fn [i scope] [i (get @scope (:lexeme name-token))]) (range))
                      (filter (fn [[_ state]] state))
                      first)]
    (resolve-variable! interpreter expression (-> scopes count dec (- i)))))

(defn resolve-function!
  [r {:keys [params body]} fun-type]
  (let [{:keys [scopes] :as r'} (begin-scope r fun-type)]
    (doall (map #(define! scopes %) params))
    (doall (map #(stmt/accept % r') body))))

(defrecord Resolver [interpreter scopes current-function]
  expr/ExprVisitor
  (visit-assign-expr [r {:keys [name value] :as e}]
    (expr/accept value r)
    (resolve-local! r e name))
  (visit-binary-expr [r {:keys [left right]}]
    (expr/accept left r)
    (expr/accept right r))
  (visit-call-expr [r {:keys [callee arguments]}]
    (expr/accept callee r)
    (doall (map #(expr/accept % r) arguments)))
  (visit-grouping-expr [r {:keys [expression]}]
    (expr/accept expression r))
  (visit-literal-expr [_ _]
    nil)
  (visit-logical-expr [r {:keys [left right]}]
    (expr/accept left r)
    (expr/accept right r))
  (visit-unary-expr [r {:keys [right]}]
    (expr/accept right r))
  (visit-variable-expr [r {:keys [name] :as e}]
    (when (and (seq scopes)
               (= :declared (-> scopes last deref (get (:lexeme name)))))
      (error {:message "Can't read local variable in its own initializer." :token name}))
    (resolve-local! r e name))
  stmt/StmtVisitor
  (visit-block-stmt [r {:keys [stmts]}]
    (let [r' (begin-scope r)]
      (doall (map #(stmt/accept % r') stmts))))
  (visit-expression-stmt [r {:keys [expression]}]
    (expr/accept expression r))
  (visit-if-stmt [r {:keys [condition then-branch else-branch]}]
    (expr/accept condition r)
    (stmt/accept then-branch r)
    (when else-branch
      (stmt/accept else-branch r)))
  (visit-function-stmt [{:keys [scopes] :as r} {:keys [fun-name] :as s}]
    (define! scopes fun-name)
    (resolve-function! r s :function))
  (visit-print-stmt [r {:keys [expression]}]
    (expr/accept expression r))
  (visit-return-stmt [r {:keys [keyword value]}]
    (when (= :none current-function)
      (error {:message "Can't return from top-level code."
              :token keyword}))
    (when value
      (expr/accept value r)))
  (visit-var-stmt [{:keys [scopes] :as r} {:keys [name initializer]}]
    (declare! scopes name)
    (when initializer
      (expr/accept initializer r))
    (define! scopes name))
  (visit-while-stmt [r {:keys [condition body]}]
    (expr/accept condition r)
    (stmt/accept body r)))

(defn begin-scope
  ([{:keys [interpreter scopes]} fun-type]
   (->Resolver interpreter (conj scopes (atom {})) fun-type))
  ([{:keys [current-function] :as r}]
   (begin-scope r current-function)))

(defn create-resolver
  [interpreter]
  (->Resolver interpreter [] :none))

(defn resolve-stmts
  [interpreter stmts]
  (let [resolver (create-resolver interpreter)]
    (doall (map #(stmt/accept % resolver) stmts))))
