(ns clox.parser
  (:require [clox.expression :as e]
            [clox.report :refer [error]]
            [clox.statement :as s]))

(defn parse-error
  [message tokens]
  (let [[{token-type :type line :line lexeme :lexeme}] tokens]
    (if (= token-type :eof)
      (error {:line line :where " at end" :message message})
      (error {:line line :where (str " at '" lexeme "'") :message message})))
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
      :this {:expr (e/->ThisExpr token) :tokens tokens'}
      :left-paren (let [{expr :expr tokens' :tokens} (expression {:tokens tokens'})]
                    (when-let [{tokens' :tokens} (consume {:tokens tokens' :types #{:right-paren}
                                                           :error-message "Expected ')' after expression."})]
                      {:expr (e/->GroupingExpr expr) :tokens tokens'}))
      (throw (parse-error "Expected expression." tokens)))))

(defn finish-call
  [{:keys [callee tokens]}]
  (let [{args :args tokens' :tokens}
        (if (= :right-paren (-> tokens first :type))
          {:args [] :tokens tokens}
          (loop [ts tokens
                 args []]
            (let [{arg :expr tokens' :tokens} (expression {:tokens ts})
                  _ (when (>= (count args) 255)
                      (parse-error "Can't have more than 255 arguments." tokens'))
                  args' (conj args arg)]
              (if-let [{tokens' :tokens} (match {:tokens tokens' :types #{:comma}})]
                (recur tokens' args')
                {:args args' :tokens tokens'}))))
        {paren :token tokens' :tokens} (consume {:tokens tokens' :types #{:right-paren}
                                                 :error-message "Expected ')' after arguments."})]
    {:expr (e/->CallExpr callee paren args)
     :tokens tokens'}))

(defn call
  [{:keys [tokens]}]
  (let [{left :expr tokens' :tokens} (primary {:tokens tokens})]
    (loop [ts tokens'
           expr left]
      (if-let [{tokens' :tokens} (match {:tokens ts :types #{:left-paren}})]
        (let [{expr :expr tokens' :tokens} (finish-call {:callee expr :tokens tokens'})]
          (recur tokens' expr))
        (if-let [{tokens' :tokens} (match {:tokens ts :types #{:dot}})]
          (let [{name-token :token tokens' :tokens} (consume {:tokens tokens' :types #{:identifier}
                                                              :error-message "Expected property name after '.'."})]
            (recur tokens' (e/->GetExpr expr name-token)))
          {:expr expr
           :tokens ts})))))

(defn unary
  [{:keys [tokens]}]
  (if-let [{operator :token tokens' :tokens} (match {:tokens tokens :types #{:minus :bang}})]
    (let [{right :expr tokens' :tokens} (unary {:tokens tokens'})]
      {:expr (e/->UnaryExpr operator right)
       :tokens tokens'})
    (call {:tokens tokens})))

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

(defn logic-and
  [{:keys [tokens]}]
  (let [{left :expr tokens' :tokens} (equality {:tokens tokens})]
    (loop [ts tokens'
           expr left]
      (if-let [{tokens' :tokens operator :token} (match {:tokens ts :types #{:and}})]
        (let [{right :expr tokens' :tokens} (equality {:tokens tokens'})]
          (recur tokens' (e/->LogicalExpr expr operator right)))
        {:expr expr
         :tokens ts}))))

(defn logic-or
  [{:keys [tokens]}]
  (let [{left :expr tokens' :tokens} (logic-and {:tokens tokens})]
    (loop [ts tokens'
           expr left]
      (if-let [{tokens' :tokens operator :token} (match {:tokens ts :types #{:or}})]
        (let [{right :expr tokens' :tokens} (logic-and {:tokens tokens'})]
          (recur tokens' (e/->LogicalExpr expr operator right)))
        {:expr expr
         :tokens ts}))))

(defn assignment
  [{:keys [tokens]}]
  (let [{left :expr tokens' :tokens} (logic-or {:tokens tokens})]
    (if-let [{tokens'' :tokens} (match {:tokens tokens' :types #{:equal}})]
      (let [{right :expr tokens'' :tokens} (assignment {:tokens tokens''})]
        (cond
          (instance? e/VariableExpr left) {:expr (e/->AssignExpr (:name left) right)
                                           :tokens tokens''}
          (instance? e/GetExpr left) {:expr (e/->SetExpr (:object left) (:name-token left) right)
                                      :tokens tokens''}
          :else (throw (parse-error "Invalid assignment target." tokens''))))
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

(declare statement)
(declare declaration)
(declare var-declaration)

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

(defn while-statement
  [{:keys [tokens]}]
  (let [{tokens' :tokens} (consume {:tokens tokens :types #{:left-paren}
                                    :error-message "Expected '(' after 'while'."})
        {condition :expr tokens' :tokens} (expression {:tokens tokens'})
        {tokens' :tokens} (consume {:tokens tokens' :types #{:right-paren}
                                    :error-message "Expected ')' after condition."})
        {body :stmt tokens' :tokens} (statement {:tokens tokens'})]
    {:stmt (s/->WhileStmt condition body)
     :tokens tokens'}))

(defn return-statement
  [{:keys [token tokens]}]
  (let [{value :expr tokens' :tokens} (if (= :semicolon (-> tokens first :type))
                                        {:expr nil :tokens tokens}
                                        (expression {:tokens tokens}))
        {tokens' :tokens} (consume {:tokens tokens' :types #{:semicolon}
                                    :error-message "Expected ';' after return value."})]
    {:stmt (s/->ReturnStmt token value)
     :tokens tokens'}))

(defn print-statement
  [{:keys [tokens]}]
  (let [{expr :expr tokens' :tokens} (expression {:tokens tokens})
        {tokens' :tokens} (consume {:tokens tokens' :types #{:semicolon}
                                    :error-message "Expected ';' after expression."})]
    {:stmt (s/->PrintStmt expr)
     :tokens tokens'}))

(defn if-statement
  [{:keys [tokens]}]
  (let [{tokens' :tokens} (consume {:tokens tokens :types #{:left-paren}
                                    :error-message "Expected '(' after 'if'."})
        {tokens' :tokens condition :expr} (expression {:tokens tokens'})
        {tokens' :tokens} (consume {:tokens tokens' :types #{:right-paren}
                                    :error-message "Expected ')' after condition."})
        {tokens' :tokens then-branch :stmt} (statement {:tokens tokens'})]
    (if-let [{tokens' :tokens} (match {:tokens tokens' :types #{:else}})]
      (let [{tokens' :tokens else-branch :stmt} (statement {:tokens tokens'})]
        {:stmt (s/->IfStmt condition then-branch else-branch)
         :tokens tokens'})
      {:stmt (s/->IfStmt condition then-branch nil)
       :tokens tokens'})))

(defn for-statement
  [{:keys [tokens]}]
  (let [{tokens' :tokens} (consume {:tokens tokens :types #{:left-paren}
                                    :error-message "Expected '(' after 'for'."})
        {initializer :stmt tokens' :tokens} (if-let [{tokens' :tokens} (match {:tokens tokens' :types #{:semicolon}})]
                                              {:stmt nil :tokens tokens'}
                                              (if-let [{tokens' :tokens} (match {:tokens tokens' :types #{:var}})]
                                                (var-declaration {:tokens tokens'})
                                                (expression-statement {:tokens tokens'})))
        {condition :expr tokens' :tokens} (if-not (= :semicolon (-> tokens' first :type))
                                            (expression {:tokens tokens'})
                                            {:expr (e/->LiteralExpr true) :tokens tokens'})
        {tokens' :tokens} (consume {:tokens tokens' :types #{:semicolon}
                                    :error-message "Expected ';' after condition."})
        {increment :expr tokens' :tokens} (if-not (= :right-paren (-> tokens' first type))
                                            (expression {:tokens tokens'})
                                            {:expr nil :tokens tokens'})
        {tokens' :tokens} (consume {:tokens tokens' :types #{:right-paren}
                                    :error-message "Expected ')' after for clauses."})
        {body :stmt tokens' :tokens} (statement {:tokens tokens'})
        body' (if increment (s/->BlockStmt [body (s/->ExpressionStmt increment)]) body)
        body' (s/->WhileStmt condition body')
        body' (if initializer (s/->BlockStmt [initializer body']) body')]
    {:stmt body'
     :tokens tokens'}))

(defn statement
  [{:keys [tokens]}]
  (if-let [{tokens' :tokens} (match {:tokens tokens :types #{:for}})]
    (for-statement {:tokens tokens'})
    (if-let [{tokens' :tokens} (match {:tokens tokens :types #{:if}})]
      (if-statement {:tokens tokens'})
      (if-let [{tokens' :tokens} (match {:tokens tokens :types #{:print}})]
        (print-statement {:tokens tokens'})
        (if-let [{token :token tokens' :tokens} (match {:tokens tokens :types #{:return}})]
          (return-statement {:token token :tokens tokens'})
          (if-let [{tokens' :tokens} (match {:tokens tokens :types #{:while}})]
            (while-statement {:tokens tokens'})
            (if-let [{tokens' :tokens} (match {:tokens tokens :types #{:left-brace}})]
              (let [{stmts :stmts tokens' :tokens} (block {:tokens tokens'})]
                {:stmt (s/->BlockStmt stmts)
                 :tokens tokens'})
              (expression-statement {:tokens tokens}))))))))

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

(defn fun-declaration
  [{:keys [kind tokens]}]
  (let [{fun-name :token tokens' :tokens} (consume {:tokens tokens :types #{:identifier}
                                                    :error-message (str "Expected " (name kind) " name.")})
        {tokens' :tokens} (consume {:tokens tokens' :types #{:left-paren}
                                    :error-message (str "Expected '(' after " (name kind) " name.")})
        {params :params tokens' :tokens}
        (if (= :right-paren (-> tokens' first :type))
          {:params [] :tokens tokens'}
          (loop [ts tokens'
                 params []]
            (let [_ (when (>= (count params) 255)
                      (parse-error "Can't have more than 255 parameters" ts))
                  {param :token tokens' :tokens} (consume {:tokens ts :types #{:identifier}
                                                           :error-message "Expected parameter name."})
                  params' (conj params param)]
              (if-let [{tokens' :tokens} (match {:tokens tokens' :types #{:comma}})]
                (recur tokens' params')
                {:params params' :tokens tokens'}))))
        {tokens' :tokens} (consume {:tokens tokens' :types #{:right-paren}
                                    :error-message "Expected ')' after parameters."})
        {tokens' :tokens} (consume {:tokens tokens' :types #{:left-brace}
                                    :error-message (str "Expected '{' before " (name kind) " body.")})
        {body :stmts tokens' :tokens} (block {:tokens tokens'})]
    {:stmt (s/->FunctionStmt fun-name params body)
     :tokens tokens'}))

(defn class-declaration
  [{:keys [tokens]}]
  (let [{name-token :token tokens' :tokens} (consume {:tokens tokens :types #{:identifier}
                                                      :error-message "Expected class name."})
        {tokens' :tokens} (consume {:tokens tokens' :types #{:left-brace}
                                    :error-message "Expected '{' after class name."})
        {method-stmts :methods tokens' :tokens}
        (loop [ts tokens'
               methods []]
          (if (or (= :right-brace (-> ts first :type))
                  (empty? tokens'))
            {:methods methods :tokens ts}
            (let [{method :stmt tokens' :tokens} (fun-declaration {:tokens ts :kind :method})]
              (recur tokens' (conj methods method)))))
        {tokens' :tokens} (consume {:tokens tokens' :types #{:right-brace}
                                    :error-message "Expected '}' after class body."})]
    {:stmt (s/->ClassStmt name-token method-stmts)
     :tokens tokens'}))

(defn declaration
  [{:keys [tokens]}]
  (try
    (if-let [{tokens' :tokens} (match {:tokens tokens :types #{:class}})]
      (class-declaration {:tokens tokens'})
      (if-let [{tokens' :tokens} (match {:tokens tokens :types #{:fun}})]
        (fun-declaration {:kind :function :tokens tokens'})
        (if-let [{tokens' :tokens} (match {:tokens tokens :types #{:var}})]
          (var-declaration {:tokens tokens'})
          (statement {:tokens tokens}))))
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
