(ns clox.report)

(def error?
  (atom false)) 

(defn report [{:keys [line where message]}]
  (println (str "[line " line "] Error" where ": " message)))  

(defn error [{:keys [line where message] :or {where ""}}]
  (report {:line line
           :where where
           :message message})
  (reset! error? true)
  nil)
