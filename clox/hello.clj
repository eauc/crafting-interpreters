(ns user
  (:require [clox.ast-printer :refer [print-ast]]
            [clox.expression :as expr]
            [clox.interpreter :refer [interpret runtime-error?]]
            [clox.parser :refer [parse]]
            [clox.report :refer [error?]]
            [clox.scanner :refer [scan-tokens]]
            [clox.token :refer [->Token]]))

(defn run [source]
  (let [tokens (scan-tokens source)
        {:keys [stmts]} (parse {:tokens tokens})]
    (interpret stmts)))

(defn run-file [path]
  (run (slurp path))
  (when @error?
    (System/exit 65))
  (when @runtime-error?
    (System/exit 70)))

(defn run-prompt []
  (loop []
    (print "> ")
    (flush)
    (let [line (read-line)]
      (when line
        (run line)
        (recur)))))

(comment
  (let [ast (expr/->BinaryExpr
             (expr/->UnaryExpr
              (->Token :minus "-")
              (expr/->LiteralExpr 123.45))
             (->Token :star "*")
             (expr/->GroupingExpr
              (expr/->LiteralExpr 45.67)))]
    (println
     (print-ast ast))))

(when (< 1 (count *command-line-args*))
  (println "Usage: clox [script]")
  (System/exit 64))

(if-let [[path] *command-line-args*]
  (run-file path)
  (run-prompt))
