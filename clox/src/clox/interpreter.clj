(ns clox.interpreter
  (:require [clojure.string :refer [replace]]
            [clox.environment :as env]
            [clox.expression :as expr]
            [clox.report :refer [error?]]
            [clox.resolver :refer [resolve-stmts]]
            [clox.statement :as stmt]))

(defn stringify
  [value]
  (or (-> value meta :to-string)
      (cond
        (nil? value) "nil"
        (double? value) (replace (str value) #"\.0+$" "")
        :else (str value))))

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

(declare bind-function)

(defn get-class-field
  [class-instance name-token]
  (let [field-name (:lexeme name-token)]
    (if-not (instance? clojure.lang.Atom (:fields class-instance))
      (throw (runtime-error name-token "Only instances have properties."))
      (let [{:keys [fields methods]} class-instance]
        (cond
          (contains? @fields field-name) (get @fields field-name)
          (contains? methods field-name) (bind-function (get methods field-name) class-instance)
          :else (throw (runtime-error name-token (str "Undefined property '" field-name "'."))))))))

(defn set-class-field
  [class-instance name-token value]
  (let [field-name (:lexeme name-token)]
    (if-not (instance? clojure.lang.Atom (:fields class-instance))
      (throw (runtime-error name-token "Only instances have properties."))
      (swap! (:fields class-instance) assoc field-name value))))

(declare execute-block)

(defn ->function
  [{closure :environment} {:keys [fun-name parameters body] :as declaration} & {:keys [initializer?]}]
  (with-meta
    (fn [i arg-values]
      (let [environment (env/->Environment closure)]
        (doall (map #(env/define-var! environment (:lexeme %1) %2) parameters arg-values))
        (try
          (execute-block i body environment)
          (when initializer? (env/get-var-at environment 1 "this"))
          (catch Exception error
            (if (= ::return (-> error ex-data :type))
              (if initializer?
                (env/get-var-at environment 1 "this")
                (-> error ex-data :value))
              (throw error))))))
    {:to-string (:lexeme fun-name)
     :closure closure
     :declaration declaration
     :initializer? initializer?
     :arity (count parameters)}))

(defn bind-function
  [fun instance]
  (let [{:keys [closure declaration initializer?]} (meta fun)
        environment (env/->Environment closure)]
    (env/define-var! environment "this" instance)
    (->function {:environment environment} declaration {:initializer? initializer?})))

(defn ->class
  [i {:keys [name-token method-stmts]}]
  (let [class-name (:lexeme name-token)
        method-funs (->> method-stmts
                         (map (fn [{:keys [fun-name] :as fun-stmt}]
                                [(:lexeme fun-name) (->function i fun-stmt {:initializer? (= "init" (:lexeme fun-name))})]))
                         (into {}))
        {:strs [init]} method-funs]
    (with-meta
      (fn [interpreter arg-values]
        (let [instance (with-meta
                         {:fields (atom {})
                          :methods method-funs}
                         {:to-string (str class-name " instance")})]
          (when init ((bind-function init instance) interpreter arg-values))
          instance))
      {:to-string class-name
       :arity (or (-> init meta :arity) 0)})))

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
  (visit-get-expr [i {:keys [object name-token]}]
    (let [object-value (expr/accept object i)]
      (get-class-field object-value name-token)))
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
  (visit-set-expr [i {:keys [object name-token value]}]
    (let [object-value (expr/accept object i)]
      (set-class-field object-value name-token (expr/accept value i))))
  (visit-this-expr [i {:keys [keyword-token] :as e}]
    (lookup-variable i e keyword-token))
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
  (visit-class-stmt [{:keys [environment] :as i} {:keys [name-token] :as e}]
    (env/define-var! environment (:lexeme name-token) nil)
    (env/assign-var! environment name-token (->class i e))
    nil)
  (visit-expression-stmt [i {:keys [expression]}]
    (expr/accept expression i)
    nil)
  (visit-function-stmt [i {:keys [fun-name] :as fun-stmt}]
    (env/define-var!
      environment (:lexeme fun-name)
      (->function i fun-stmt))
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
  [stmts']
  (let [stmts (remove nil? stmts')
        interpreter (create-interpreter)]
    (when-not @error?
      (resolve-stmts interpreter stmts))
      ; (println "==stmts==" stmts)
      ; (println "==intr==" interpreter)
    (when-not @error?
      (try
        (doall (map #(stmt/accept % interpreter) (remove nil? stmts)))
        (catch Exception error
          (case (-> error ex-data :type)
            ::runtime-error (do (println (str (.getMessage error) "\nat " (-> error ex-data :token :lexeme) " [line " (-> error ex-data :token :line) "]"))
                                (reset! runtime-error? true))
            (throw error)))))))
