(ns clox.environment
  (:require [clox.token :refer [Token]]))

(defn ->Environment
  ([enclosing]
   {:enclosing enclosing
    :values (atom {})})
  ([]
   (->Environment nil)))

(defn ancestor
  [{:keys [enclosing] :as environment} depth]
  (if (= 0 depth)
    environment
    (recur enclosing (dec depth))))

(defn undefined-var-error
  [name-token]
  (ex-info (str "Undefined variable '" (:lexeme name-token) "'.") 
           {:type :clox.interpreter/runtime-error :token name-token}))

(defn define-var!
  [{:keys [values]} ^String var-name ^Object value]
  (swap! values assoc var-name value))

(defn assign-var-at!
  [environment depth name-token value]
  (-> environment (ancestor depth) :values (swap! assoc (:lexeme name-token) value)))

(defn assign-var!
  [{:keys [enclosing values]} ^Token name-token value]
  (let [var-name (:lexeme name-token)]
    (if (contains? @values var-name)
      (swap! values assoc var-name value)
      (if enclosing
        (assign-var! enclosing name-token value)
        (throw (undefined-var-error name-token))))))

(defn get-var
  [{:keys [enclosing values]} ^Token name-token]
  (let [var-name (:lexeme name-token)]
    (if (contains? @values var-name)
      (get @values var-name)
      (if enclosing
        (get-var enclosing name-token)
        (throw (undefined-var-error name-token))))))

(defn get-var-at
  [environment depth var-name]
  (-> environment (ancestor depth) :values deref (get var-name)))
