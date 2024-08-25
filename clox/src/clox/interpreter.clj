(ns clox.interpreter
  (:require [clojure.string :refer [replace]]
            [clox.environment :as env]
            [clox.expression :as expr]
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

(defrecord Interpreter [environment]
  expr/ExprVisitor
  (visit-assign-expr [i {:keys [name value]}]
    (let [value (expr/accept value i)]
      (swap! environment env/assign-var name value)
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
  (visit-grouping-expr [i {:keys [expr]}]
    (expr/accept expr i))
  (visit-literal-expr [_ {:keys [value]}]
    value)
  (visit-unary-expr [i {:keys [operator right]}]
    (let [value (expr/accept right i)]
      (case (:type operator)
        :minus (do
                 (check-number-operand {:operator operator :operand value})
                 (- (double value)))
        :bang (not value)
        nil)))
  (visit-variable-expr [_ {:keys [name]}]
    (env/get-var @environment name))
  stmt/StmtVisitor
  (visit-expression-stmt [i {:keys [expression]}]
    (expr/accept expression i)
    nil)
  (visit-print-stmt [i {:keys [expression]}]
    (let [value (expr/accept expression i)]
      (println (stringify value))
      nil))
  (visit-var-stmt [i {:keys [name initializer]}]
    (let [value (when initializer (expr/accept initializer i))]
      (swap! environment env/define-var (:lexeme name) value)
      nil)))

(def runtime-error?
  (atom false))

(defn interpret
  [stmts]
  (let [interpreter (->Interpreter (atom {}))]
    (try
      (doall (map #(stmt/accept % interpreter) (remove nil? stmts)))
      (catch Exception error
        (case (-> error ex-data :type)
          ::runtime-error (do (println (str (.getMessage error) "\nat " (-> error ex-data :token :lexeme) " [line " (-> error ex-data :token :line) "]"))
                              (reset! runtime-error? true))
          (throw error))))))
