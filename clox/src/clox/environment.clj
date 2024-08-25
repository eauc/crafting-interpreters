(ns clox.environment
  (:require [clox.token :refer [Token]]))

(defn undefined-var-error
  [name]
  (ex-info (str "Undefined variable '" name "'.") {:type :clox.interpreter/runtime-error}))

(defn define-var
  [env ^String name ^Object value]
  (assoc env name value))

(defn assign-var
  [env ^Token name-token value]
  (let [name (:lexeme name-token)]
    (if (contains? env name)
      (assoc env name value)
      (throw (undefined-var-error name)))))

(defn get-var
  [env ^Token name-token]
  (let [name (:lexeme name-token)]
    (if (contains? env name)
      (get env name)
      (throw (undefined-var-error name)))))
