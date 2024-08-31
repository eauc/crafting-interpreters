(ns clox.interpreter
  (:require [clojure.string :refer [replace]]
            [clox.environment :as env]
            [clox.expression :as expr]
            [clox.report :refer [error?]]
            [clox.resolver :refer [resolve-stmts]]
            [clox.statement :as stmt]))

(defn stringify
  [value]
  (cond
    (nil? value) "nil"
    (double? value) (replace (str value) #"\.0+$" "")
    :else (str value)))

(defn runtime-error
  [token message]
  (ex-info message {:type ::runtime-error :token token}))

(defn check-number-operand
  [{:keys [operator operand]}]
  (when-not (double? operand)
    (throw (runtime-error operator "Operand must be number."))))

(defn check-number-operands
  [{:keys [operator left right]}]
  (when-not (and (double? left) (double? right))
    (throw (runtime-error operator "Operands must be number."))))

(declare execute-block)

(defn lookup-variable
  [{:keys [environment locals globals]} expr name-token]
  (if-let [depth (get @locals expr)]
    (env/get-var-at environment depth (:lexeme name-token))
    (env/get-var globals name-token)))

(defrecord Interpreter [globals locals environment]
  expr/ExprVisitor
  (visit-assign-expr [i {:keys [name value] :as expr}]
    (let [value (expr/accept value i)
          depth (get @locals expr)]
      (if depth
        (env/assign-var-at! environment depth name value)
        (env/assign-var! globals name value))
      value))
  (visit-binary-expr [i {:keys [left operator right]}]
    (let [left-value (expr/accept left i)
          right-value (expr/accept right i)]
      (case (:type operator)
        :plus (cond
                (and (string? left-value) (string? right-value))
                (str left-value right-value)
                (and (double? left-value) (double? right-value))
                (+ left-value right-value)
                :else
                (throw (runtime-error operator "Operators must be 2 numbers or 2 strings.")))
        :minus (do
                 (check-number-operands {:operator operator :left left-value :right right-value})
                 (- (double left-value) (double right-value)))
        :slash (do
                 (check-number-operands {:operator operator :left left-value :right right-value})
                 (/ (double left-value) (double right-value)))
        :star (do
                (check-number-operands {:operator operator :left left-value :right right-value})
                (* (double left-value) (double right-value)))
        :greater (do
                   (check-number-operands {:operator operator :left left-value :right right-value})
                   (> (double left-value) (double right-value)))
        :greater-equal (do
                         (check-number-operands {:operator operator :left left-value :right right-value})
                         (>= (double left-value) (double right-value)))
        :less (do
                (check-number-operands {:operator operator :left left-value :right right-value})
                (< (double left-value) (double right-value)))
        :less-equal (do
                      (check-number-operands {:operator operator :left left-value :right right-value})
                      (<= (double left-value) (double right-value)))
        :equal-equal (= left-value right-value)
        :bang-equal (not= left-value right-value)
        nil)))
  (visit-call-expr [i {:keys [callee paren arguments]}]
    (let [function (expr/accept callee i)
          _ (when-not (fn? function)
              (throw (runtime-error paren "Can only call functions and classes.")))
          {:keys [arity]} (meta function)
          arg-values (doall (map #(expr/accept % i) arguments))
          _ (when-not (= (count arg-values) arity)
              (throw (runtime-error paren (str "Expected " arity " arguments but got " (count arg-values) "."))))]
      (function i arg-values)))
  (visit-grouping-expr [i {:keys [expr]}]
    (expr/accept expr i))
  (visit-literal-expr [_ {:keys [value]}]
    value)
  (visit-logical-expr [i {:keys [left operator right]}]
    (let [left-value (expr/accept left i)]
      (if (= :or (:type operator))
        (if left-value
          left-value
          (expr/accept right i))
        (if-not left-value
          left-value
          (expr/accept right i)))))
  (visit-unary-expr [i {:keys [operator right]}]
    (let [value (expr/accept right i)]
      (case (:type operator)
        :minus (do
                 (check-number-operand {:operator operator :operand value})
                 (- (double value)))
        :bang (not value)
        nil)))
  (visit-variable-expr [i {:keys [name] :as expr}]
    (lookup-variable i expr name))
  stmt/StmtVisitor
  (visit-block-stmt [{:keys [environment] :as i} {:keys [stmts]}]
    (execute-block i stmts (env/->Environment environment))
    nil)
  (visit-expression-stmt [i {:keys [expression]}]
    (expr/accept expression i)
    nil)
  (visit-function-stmt [{closure :environment} {:keys [fun-name parameters body]}]
    (env/define-var!
      environment (:lexeme fun-name)
      (with-meta
        (fn [i arg-values]
          (let [environment (env/->Environment closure)]
            (doall (map #(env/define-var! environment (:lexeme %1) %2) parameters arg-values))
            (try
              (execute-block i body environment)
              (catch Exception error
                (if (= ::return (-> error ex-data :type))
                  (-> error ex-data :value)
                  (throw error))))))
        {:arity (count parameters)}))
    nil)
  (visit-if-stmt [i {:keys [condition then-branch else-branch]}]
    (if-let [_ (expr/accept condition i)]
      (stmt/accept then-branch i)
      (when else-branch
        (stmt/accept else-branch i)))
    nil)
  (visit-print-stmt [i {:keys [expression]}]
    (let [value (expr/accept expression i)]
      (println (stringify value))
      nil))
  (visit-return-stmt [i {:keys [value]}]
    (let [v (if value (expr/accept value i) nil)]
      (throw (ex-info "Return" {:type ::return :value v}))))
  (visit-var-stmt [i {:keys [name initializer]}]
    (let [value (when initializer (expr/accept initializer i))]
      (env/define-var! environment (:lexeme name) value)
      nil))
  (visit-while-stmt [i {:keys [condition body]}]
    (loop [_ nil]
      (if (expr/accept condition i)
        (recur (stmt/accept body i))
        nil))))

(defn execute-block
  [{:keys [globals locals]} stmts environment]
  (let [interpreter (->Interpreter globals locals environment)]
    (doall (map #(stmt/accept % interpreter) stmts)))
  nil)

(defn create-interpreter
  []
  (let [environment (env/->Environment)]
    (env/define-var!
      environment
      "clock"
      (with-meta
        (fn clock-native-fn
          []
          (/ (System/currentTimeMillis) 1000.0))
        {:arity 0}))
    (map->Interpreter
     {:globals environment
      :locals (atom {})
      :environment environment})))

(def runtime-error?
  (atom false))

(defn interpret
  [stmts]
  (let [interpreter (create-interpreter)]
    (resolve-stmts interpreter stmts)
    (when-not @error?
      (try
        (doall (map #(stmt/accept % interpreter) (remove nil? stmts)))
        (catch Exception error
          (case (-> error ex-data :type)
            ::runtime-error (do (println (str (.getMessage error) "\nat " (-> error ex-data :token :lexeme) " [line " (-> error ex-data :token :line) "]"))
                                (reset! runtime-error? true))
            (throw error)))))))
