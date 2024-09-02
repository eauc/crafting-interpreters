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
    ; (println "==def==" scopes name-token)
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
    ; (println "==res-loc==" scopes expression i)
    (resolve-variable! interpreter expression i)))

(defn resolve-function!
  [r {:keys [parameters body]} fun-type]
  ; (println "==res-fun==" parameters body fun-type)
  (let [{:keys [scopes] :as r'} (begin-scope r {:fun-type fun-type})]
    (doall (map #(define! scopes %) parameters))
    (doall (map #(stmt/accept % r') body))))

(defrecord Resolver [interpreter scopes current-function current-class]
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
  (visit-get-expr [r {:keys [object]}]
    (expr/accept object r))
  (visit-grouping-expr [r {:keys [expr]}]
    (expr/accept expr r))
  (visit-literal-expr [_ _]
    nil)
  (visit-logical-expr [r {:keys [left right]}]
    (expr/accept left r)
    (expr/accept right r))
  (visit-set-expr [r {:keys [object value]}]
    (expr/accept object r)
    (expr/accept value r))
  (visit-this-expr [r {:keys [keyword-token] :as e}]
    (when-not (= :class current-class)
      (error {:message "Can't use 'this' outside of a class." :token keyword-token}))
    (resolve-local! r e keyword-token))
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
      ; (println "==blk==" r')
      (doall (map #(stmt/accept % r') stmts))))
  (visit-class-stmt [{:keys [scopes] :as r} {:keys [name-token method-stmts]}]
    (define! scopes name-token)
    (let [r' (begin-scope r {:class-type :class})]
      (define! (:scopes r') {:lexeme "this"})
      (doall
       (map
        #(resolve-function! r' % (if (= "init" (-> % :fun-name :lexeme)) :initializer :method))
        method-stmts))))
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
      (when (= :initializer current-function)
        (error {:message "Can't return a value from an initializer."
                :token keyword}))
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
  ([{:keys [interpreter scopes current-class current-function]}
    {:keys [fun-type class-type]
     :or {fun-type current-function
          class-type current-class}}]
   ; (println "==beg-scp==" scopes fun-type)
   (->Resolver interpreter (conj scopes (atom {})) fun-type class-type))
  ([r]
   (begin-scope r nil)))

(defn create-resolver
  [interpreter]
  (map->Resolver {:interpreter interpreter
                  :scopes []
                  :current-function :none
                  :current-class :none}))

(defn resolve-stmts
  [interpreter stmts]
  (let [resolver (create-resolver interpreter)]
    ; (println "==res-stmts==" stmts)
    (doall (map #(stmt/accept % resolver) stmts))))
