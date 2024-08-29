(ns clox.parser
  (:require [clox.expression :as e]
            [clox.report :refer [report]]
            [clox.statement :as s]))

(defn parse-error
  [message tokens]
  (let [[{token-type :type line :line lexeme :lexeme}] tokens]
    (if (= token-type :eof)
      (report {:line line :where " at end" :message message})
      (report {:line line :where (str " at '" lexeme "'") :message message})))
  (ex-info message {:type ::parse-error :tokens tokens}))

(defn synchronize
  [{:keys [tokens]}]
  (let [tokens' (drop-while #(not (#{:semicolon :class :fun :var :for :if :while :print :return} (:type %))) tokens)]
    (if (= :semicolon (:type (first tokens')))
      {:tokens (rest tokens')}
      {:tokens tokens'})))

(defn match
  [{:keys [tokens types]}]
  (let [[token & tokens'] tokens]
    (if (types (:type token))
      {:token token :tokens tokens'}
      nil)))

(defn consume
  [{:keys [tokens types error-message]}]
  (if-let [{token :token tokens' :tokens} (match {:tokens tokens :types types})]
    {:tokens tokens' :token token}
    (throw (parse-error error-message tokens))))

(declare expression)

(defn primary
  [{:keys [tokens]}]
  (let [[{token-type :type literal :literal :as token} & tokens'] tokens]
    (case token-type
      :false {:expr (e/->LiteralExpr false) :tokens tokens'}
      :true {:expr (e/->LiteralExpr true) :tokens tokens'}
      :nil {:expr (e/->LiteralExpr nil) :tokens tokens'}
      :number {:expr (e/->LiteralExpr literal) :tokens tokens'}
      :string {:expr (e/->LiteralExpr literal) :tokens tokens'}
      :identifier {:expr (e/->VariableExpr token) :tokens tokens'}
      :left-paren (let [{expr :expr tokens' :tokens} (expression {:tokens tokens'})]
                    (when-let [{tokens' :tokens} (consume {:tokens tokens' :types #{:right-paren}
                                                           :error-message "Expected ')' after expression."})]
                      {:expr (e/->GroupingExpr expr) :tokens tokens'}))
      (throw (parse-error "Expected expression." tokens)))))

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

(defn assignment
  [{:keys [tokens]}]
  (when (empty? tokens)
    (throw (ex-info "OOOOOPS" {})))
  (let [{left :expr tokens' :tokens} (equality {:tokens tokens})]
    (if-let [{tokens'' :tokens} (match {:tokens tokens' :types #{:equal}})]
      (let [{right :expr tokens'' :tokens} (assignment {:tokens tokens''})]
        (if (instance? e/VariableExpr left)
          {:expr (e/->AssignExpr (:name left) right)
           :tokens tokens''}
          (throw (parse-error "Invalid assignment target." tokens''))))
      {:expr left
       :tokens tokens'})))

(defn expression
  [{:keys [tokens]}]
  (assignment {:tokens tokens}))

(defn expression-statement
  [{:keys [tokens]}]
  (let [{expr :expr tokens' :tokens} (expression {:tokens tokens})
        {tokens' :tokens} (consume {:tokens tokens' :types #{:semicolon}
                                    :error-message "Expected ';' after expression."})]
    {:stmt (s/->ExpressionStmt expr)
     :tokens tokens'}))

(defn print-statement
  [{:keys [tokens]}]
  (let [{expr :expr tokens' :tokens} (expression {:tokens tokens})
        {tokens' :tokens} (consume {:tokens tokens' :types #{:semicolon}
                                    :error-message "Expected ';' after expression."})]
    {:stmt (s/->PrintStmt expr)
     :tokens tokens'}))

(declare declaration)

(defn block
  [{:keys [tokens]}]
  (loop [tokens' tokens
         stmts []]
    (if (#{:right-brace :eof} (-> tokens' first :type))
      (let [{tokens' :tokens} (consume {:tokens tokens' :types #{:right-brace}
                                        :error-message "Expected '}' after block."})]
        {:stmts stmts
         :tokens tokens'})
      (let [{stmt :stmt tokens' :tokens} (declaration {:tokens tokens'})]
        (recur tokens' (conj stmts stmt))))))

(defn statement
  [{:keys [tokens]}]
  (if-let [{tokens' :tokens} (match {:tokens tokens :types #{:print}})]
    (print-statement {:tokens tokens'})
    (if-let [{tokens' :tokens} (match {:tokens tokens :types #{:left-brace}})]
      (let [{stmts :stmts tokens' :tokens} (block {:tokens tokens'})]
        {:stmt (s/->BlockStmt stmts)
         :tokens tokens'})
      (expression-statement {:tokens tokens}))))

(defn var-declaration
  [{:keys [tokens]}]
  (let [{name :token tokens' :tokens} (consume {:tokens tokens :types #{:identifier}
                                                :error-message "Expected variable name."})
        {initializer :expr tokens' :tokens} (if-let [{tokens' :tokens} (match {:tokens tokens' :types #{:equal}})]
                                              (expression {:tokens tokens'})
                                              {:tokens tokens'})
        {tokens' :tokens} (consume {:tokens tokens' :types #{:semicolon}
                                    :error-message "Expected ';' after variable declaration."})]
    {:stmt (s/->VarStmt name initializer)
     :tokens tokens'}))

(defn declaration
  [{:keys [tokens]}]
  (try
    (if-let [{tokens' :tokens} (match {:tokens tokens :types #{:var}})]
      (var-declaration {:tokens tokens'})
      (statement {:tokens tokens}))
    (catch Exception error
      (case (-> error ex-data :type)
        ::parse-error (synchronize {:tokens (-> error ex-data :tokens)})
        (throw error)))))

(defn parse
  [{:keys [tokens]}]
  (try
    (loop [tokens' tokens
           stmts []]
      (if (= :eof (-> tokens' first :type))
        {:stmts stmts}
        (let [{tokens' :tokens stmt :stmt} (declaration {:tokens tokens'})]
          (recur tokens' (conj stmts stmt)))))
    (catch Exception _
      nil)))
