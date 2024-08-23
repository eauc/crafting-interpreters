(ns user
  (:require [clojure.string :refer [join]]
            [clox.scanner :refer [scan-tokens]]
            [clox.report :refer [error?]]))

(import java.nio.file.Paths
        java.nio.file.Files)

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

(when (< 1 (count *command-line-args*))
  (println "Usage: clox [script]")
  (System/exit 64))

(if-let [[path] *command-line-args*]
  (run-file path)
  (run-prompt))
