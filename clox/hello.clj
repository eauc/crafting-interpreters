(ns user
  (:require [clojure.string :refer [join]]
            [clox.ast-printer :refer [->AstPrinter]]
            [clox.expression :as expr]
            [clox.token :refer [->Token]]
            [clox.scanner :refer [scan-tokens]]
            [clox.report :refer [error?]]))

(defn run [source]
  (println
   (join
    "\n"
    (map str
         (scan-tokens source)))))

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
