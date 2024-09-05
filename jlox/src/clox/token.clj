(ns clox.token)

(def last-id (atom 0))

(defrecord Token [id type lexeme literal line]
  Object
  (toString [_] (str type " " lexeme " " literal)))

(defn create-token
  [type lexeme literal line]
  (swap! last-id inc)
  (->Token @last-id type lexeme literal line))
