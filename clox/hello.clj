(ns user
  (:require [clox.ast-printer :refer [print-ast]]
            [clox.expression :as expr]
            [clox.parser :refer [parse]]
            [clox.report :refer [error?]]
            [clox.scanner :refer [scan-tokens]]
            [clox.token :refer [->Token]]))

(defn run [source]
  (let [tokens (scan-tokens source)
        {ast :expr} (parse {:tokens tokens})]
    (print-ast ast)))

(defn run-file [path]
  (run (slurp path))
  (when @error?
    (System/exit 65)))

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
              (expr/->LiteralExpr 45.67)))
        printer (->AstPrinter ast)]
    (println
     (expr/accept ast printer))))

(when (< 1 (count *command-line-args*))
  (println "Usage: clox [script]")
  (System/exit 64))

(if-let [[path] *command-line-args*]
  (run-file path)
  (run-prompt))
