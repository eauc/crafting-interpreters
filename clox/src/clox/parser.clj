(ns clox.parser
  (:require [clox.expression :as e]
            [clox.report :refer [report]]))

(defn error
  [message tokens]
  (let [[{token-type :type line :line lexeme :lexeme}] tokens]
    (if (= token-type :eof)
      (report {:line line :where " at end" :message message})
      (report {:line line :where (str " at '" lexeme "'") :message message})))
  (ex-info message {:type ::parse-error :tokens tokens}))

(defn synchronize
  [{:keys [tokens]}]
  (let [tokens' (drop-while #(not (#{:semicolon :class :fun :var :for :if :while :print :return} %)) tokens)]
    (if (= :semicolon (:type (first tokens')))
      {:tokens (rest tokens')}
      {:tokens tokens'})))

(defn match
  [{:keys [tokens types]}]
  (let [[token & tokens'] tokens]
    (if (types (:type token))
      {:token token :tokens tokens'}
      nil)))

(declare expression)

(defn primary
  [{:keys [tokens]}]
  (let [[{token-type :type literal :literal} & tokens'] tokens]
    (case token-type
      :false {:expr (e/->LiteralExpr false) :tokens tokens'}
      :true {:expr (e/->LiteralExpr true) :tokens tokens'}
      :nil {:expr (e/->LiteralExpr nil) :tokens tokens'}
      :number {:expr (e/->LiteralExpr literal) :tokens tokens'}
      :string {:expr (e/->LiteralExpr literal) :tokens tokens'}
      :left-paren (let [{expr :expr tokens' :tokens} (expression {:tokens tokens'})]
                    (if-let [{tokens' :tokens} (match {:tokens tokens' :types #{:right-paren}})]
                      {:expr (e/->GroupingExpr expr) :tokens tokens'}
                      (throw (error "Expected ')' after expression." tokens))))
      (throw (error "Expected expression." tokens)))))

(defn unary
  [{:keys [tokens]}]
  (if-let [{operator :token tokens' :tokens} (match {:tokens tokens :types #{:minus :bang}})]
    (let [{right :expr tokens' :tokens} (unary {:tokens tokens'})]
      {:expr (e/->UnaryExpr operator right)
       :tokens tokens'})
    (primary {:tokens tokens})))

(defn factor
  [{:keys [tokens]}]
  (let [{left :expr tokens :tokens} (unary {:tokens tokens})]
    (loop [expr left
           tokens' tokens]
      (if-let [{operator :token tokens' :tokens} (match {:tokens tokens' :types #{:star :slash}})]
        (let [{right :expr tokens' :tokens} (unary {:tokens tokens'})]
          (recur (e/->BinaryExpr expr operator right) tokens'))
        {:expr expr :tokens tokens'}))))

(defn term
  [{:keys [tokens]}]
  (let [{left :expr tokens :tokens} (factor {:tokens tokens})]
    (loop [expr left
           tokens' tokens]
      (if-let [{operator :token tokens' :tokens} (match {:tokens tokens' :types #{:plus :minus}})]
        (let [{right :expr tokens' :tokens} (factor {:tokens tokens'})]
          (recur (e/->BinaryExpr expr operator right) tokens'))
        {:expr expr :tokens tokens'}))))

(defn comparison
  [{:keys [tokens]}]
  (let [{left :expr tokens :tokens} (term {:tokens tokens})]
    (loop [expr left
           tokens' tokens]
      (if-let [{operator :token tokens' :tokens} (match {:tokens tokens' :types #{:greater :greater-equal :less :less-equal}})]
        (let [{right :expr tokens' :tokens} (term {:tokens tokens'})]
          (recur (e/->BinaryExpr expr operator right) tokens'))
        {:expr expr :tokens tokens'}))))

(defn equality
  [{:keys [tokens]}]
  (let [{left :expr tokens :tokens} (comparison {:tokens tokens})]
    (loop [expr left
           tokens' tokens]
      (if-let [{operator :token tokens' :tokens} (match {:tokens tokens' :types #{:bang-equal :equal-equal}})]
        (let [{right :expr tokens' :tokens} (comparison {:tokens tokens'})]
          (recur (e/->BinaryExpr expr operator right) tokens'))
        {:expr expr :tokens tokens'}))))

(defn expression
  [{:keys [tokens]}]
  (equality {:tokens tokens}))

(defn parse
  [{:keys [tokens]}]
  (try
    (expression {:tokens tokens})
    (catch Exception _
      nil)))
