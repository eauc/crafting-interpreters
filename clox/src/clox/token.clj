(ns clox.token)

(defrecord Token [type lexeme literal line]
  Object
  (toString [_] (str type " " lexeme " " literal)))
