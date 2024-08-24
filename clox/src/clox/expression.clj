(ns clox.expression
  (:require [clox.token :refer [Token]]))

(defprotocol Expr
  (accept [e ^ExprVisitor v]))

(defprotocol ExprVisitor
  (visit-assign-expr [_ ^AssignExpr e])
  (visit-binary-expr [_ ^BinaryExpr e])
  (visit-call-expr [_ ^CallExpr e])
  (visit-get-expr [_ ^GetExpr e])
  (visit-grouping-expr [_ ^GroupingExpr e])
  (visit-literal-expr [_ ^LiteralExpr e])
  (visit-logical-expr [_ ^LogicalExpr e])
  (visit-set-expr [_ ^SetExpr e])
  (visit-super-expr [_ ^SuperExpr e])
  (visit-this-expr [_ ^ThisExpr e])
  (visit-unary-expr [_ ^UnaryExpr e])
  (visit-variable-expr [_ ^VariableExpr e]))

(defrecord AssignExpr [^Token name ^Expr value]
  Expr
  (accept [e ^ExprVisitor v] (visit-assign-expr v e)))

(defrecord BinaryExpr [^Expr left ^Token operator ^Expr right]
  Expr
  (accept [e ^ExprVisitor v] (visit-binary-expr v e)))

(defrecord CallExpr [^Expr callee ^Token paren ^vector arguments]
  Expr
  (accept [e ^ExprVisitor v] (visit-call-expr v e)))

(defrecord GetExpr [^Expr object ^Token name]
  Expr
  (accept [e ^ExprVisitor v] (visit-get-expr v e)))

(defrecord GroupingExpr [^Expr expr]
  Expr
  (accept [e ^ExprVisitor v] (visit-grouping-expr v e)))

(defrecord LiteralExpr [value]
  Expr
  (accept [e ^ExprVisitor v] (visit-literal-expr v e)))

(defrecord LogicalExpr [^Expr left ^Token operator ^Expr right]
  Expr
  (accept [e ^ExprVisitor v] (visit-logical-expr v e)))

(defrecord SetExpr [^Expr object ^Token name ^Expr value]
  Expr
  (accept [e ^ExprVisitor v] (visit-set-expr v e)))

(defrecord SuperExpr [^Token keyword ^Token method]
  Expr
  (accept [e ^ExprVisitor v] (visit-super-expr v e)))

(defrecord ThisExpr [^Token keyword]
  Expr
  (accept [e ^ExprVisitor v] (visit-this-expr v e)))

(defrecord UnaryExpr [^Token operator ^Expr right]
  Expr
  (accept [e ^ExprVisitor v] (visit-unary-expr v e)))

(defrecord VariableExpr [^Token name]
  Expr
  (accept [e ^ExprVisitor v] (visit-variable-expr v e)))
