(ns clox.statement
  (:require [clox.expression :refer [Expr]]
            [clox.token :refer [Token]]))

(defprotocol Stmt
  (accept [e ^StmtVisitor v]))

(defprotocol StmtVisitor
  (visit-expression-stmt [_ s])
  (visit-print-stmt [_ s])
  (visit-var-stmt [_ s]))

(defrecord ExpressionStmt [^Expr expression]
  Stmt
  (accept [e ^StmtVisitor v] (visit-expression-stmt v e)))

(defrecord PrintStmt [^Expr expression]
  Stmt
  (accept [e ^StmtVisitor v] (visit-print-stmt v e)))

(defrecord VarStmt [^Token name ^Expr initializer]
  Stmt
  (accept [e ^StmtVisitor v] (visit-var-stmt v e)))
