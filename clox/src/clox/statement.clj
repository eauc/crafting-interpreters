(ns clox.statement
  (:require [clox.expression :refer [Expr]]
            [clox.token :refer [Token]]))

(defprotocol Stmt
  (accept [e ^StmtVisitor v]))

(defprotocol StmtVisitor
  (visit-block-stmt [v s])
  (visit-expression-stmt [v s])
  (visit-if-stmt [v s])
  (visit-print-stmt [v s])
  (visit-var-stmt [v s])
  (visit-while-stmt [v s]))

(defrecord BlockStmt [stmts]
  Stmt
  (accept [e ^StmtVisitor v] (visit-block-stmt v e)))

(defrecord ExpressionStmt [^Expr expression]
  Stmt
  (accept [e ^StmtVisitor v] (visit-expression-stmt v e)))

(defrecord IfStmt [^Expr condition ^Stmt then-branch ^Stmt else-branch]
  Stmt
  (accept [e ^StmtVisitor v] (visit-if-stmt v e)))

(defrecord PrintStmt [^Expr expression]
  Stmt
  (accept [e ^StmtVisitor v] (visit-print-stmt v e)))

(defrecord VarStmt [^Token name ^Expr initializer]
  Stmt
  (accept [e ^StmtVisitor v] (visit-var-stmt v e)))

(defrecord WhileStmt [^Expr condition ^Stmt body]
  Stmt
  (accept [e ^StmtVisitor v] (visit-while-stmt v e)))
