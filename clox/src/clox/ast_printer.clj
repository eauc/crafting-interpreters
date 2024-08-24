(ns clox.ast-printer
  (:require [clojure.string :refer [join]]
            [clox.expression :as expr]))

(defn parenthesize [& args]
  (str "(" (join " " args) ")"))

(defrecord AstPrinter [^expr/Expr ast]
  expr/ExprVisitor
  (visit-assign-expr [p ^expr/AssignExpr e]
    (parenthesize "assign" (:lexeme (:name e)) (expr/accept (:value e) p)))
  (visit-binary-expr [p ^expr/BinaryExpr e]
    (parenthesize (-> e :operator :lexeme) (-> e :left (expr/accept p)) (-> e :right (expr/accept p))))
  (visit-call-expr [p ^expr/CallExpr e]
    (apply parenthesize "call" (-> e :callee (expr/accept p)) (-> e :paren :lexeme) (map #(expr/accept % p) (-> e :arguments))))
  (visit-get-expr [p ^expr/GetExpr e]
    (parenthesize "get" (-> e :object (expr/accept p)) (-> e :name :lexeme)))
  (visit-grouping-expr [p ^expr/GroupingExpr e]
    (parenthesize (-> e :expr (expr/accept p))))
  (visit-literal-expr [_ ^expr/LiteralExpr {:keys [value]}]
    (if (nil? value) "nil" value))
  (visit-logical-expr [p ^expr/LogicalExpr e]
    (parenthesize (-> e :left (expr/accept p)) (-> e :operator :lexeme) (-> e :right (expr/accept p))))
  (visit-set-expr [p ^expr/SetExpr e]
    (parenthesize "set" (-> e :object (expr/accept p)) (-> e :name :lexeme) (-> e :value (expr/accept p))))
  (visit-super-expr [_ ^expr/SuperExpr e]
    (parenthesize "super" (-> e :keyword :lexeme) (-> e :method :lexeme)))
  (visit-this-expr [_ ^expr/ThisExpr e]
    (parenthesize "this" (-> e :keyword :lexeme)))
  (visit-unary-expr [p ^expr/UnaryExpr e]
    (parenthesize (-> e :operator :lexeme) (-> e :right (expr/accept p))))
  (visit-variable-expr [_ ^expr/VariableExpr e]
    (parenthesize "var" (-> e :name :lexeme))))

(defn print-ast
  [ast]
  (when ast
    (let [printer (->AstPrinter ast)]
      (println
       (expr/accept ast printer)))))
