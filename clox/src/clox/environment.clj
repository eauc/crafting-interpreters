(ns clox.environment
  (:require [clox.token :refer [Token]]))

(defn ->Environment
  ([enclosing]
   {:enclosing enclosing
    :values (atom {})})
  ([]
   (->Environment nil)))

(defn undefined-var-error
  [var-name]
  (ex-info (str "Undefined variable '" var-name "'.") {:type :clox.interpreter/runtime-error}))

(defn define-var!
  [{:keys [values]} ^String var-name ^Object value]
  (swap! values assoc var-name value))

(defn assign-var!
  [{:keys [enclosing values]} ^Token name-token value]
  (let [var-name (:lexeme name-token)]
    (if (contains? @values var-name)
      (swap! values assoc var-name value)
      (if enclosing
        (assign-var! enclosing name-token value)
        (throw (undefined-var-error var-name))))))

(defn get-var
  [{:keys [enclosing values]} ^Token name-token]
  (let [var-name (:lexeme name-token)]
    (if (contains? @values var-name)
      (get @values var-name)
      (if enclosing
        (get-var enclosing name-token)
        (throw (undefined-var-error var-name))))))
