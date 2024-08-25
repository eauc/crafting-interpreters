(ns clox.scanner
  (:require [clojure.string :refer [index-of]]
            [clox.token :refer [->Token]]
            [clox.report :refer [error]]))

(def keywords
  {"and" :and
   "class" :class
   "else" :else
   "false" :false
   "for" :for
   "fun" :fun
   "if" :if
   "nil" :nil
   "or" :or
   "print" :print
   "return" :return
   "super" :super
   "this" :this
   "true" :true
   "var" :var
   "while" :while})

(defn alpha? [c]
  (or (<= (int \a) (int c) (int \z))
      (<= (int \A) (int c) (int \Z))
      (= \_ c)))

(defn digit? [c]
  (<= (int \0) (int c) (int \9)))

(defn alphanumeric? [c]
  (or (alpha? c)
      (digit? c)))

(defn scan-tokens [source']
  (loop [source source'
         line 1
         tokens []]
    (if (empty? source)
      (conj tokens (->Token :eof nil nil line))
      (let [next-char (first source)
            leftover (subs source 1)
            add-token (fn [type & [lexeme literal]] (conj tokens (->Token type (or lexeme next-char) literal line)))
            match? #(if (empty? leftover) false (= %1 (first leftover)))]
        (case next-char
          \space (recur leftover line tokens)
          \tab (recur leftover line tokens)
          \return (recur leftover line tokens)
          \newline (recur leftover (inc line) tokens)
          \( (recur leftover line (add-token :left-paren))
          \) (recur leftover line (add-token :right-paren))
          \{ (recur leftover line (add-token :left-brace))
          \} (recur leftover line (add-token :right-brace))
          \, (recur leftover line (add-token :comma))
          \; (recur leftover line (add-token :semicolon))
          \. (recur leftover line (add-token :dot))
          \- (recur leftover line (add-token :minus))
          \+ (recur leftover line (add-token :plus))
          \* (recur leftover line (add-token :star))
          \/ (if (match? \/)
               (recur (if-let [newline (index-of leftover "\n")] (subs leftover newline) "") line tokens)
               (recur leftover line (add-token :slash)))
          \! (if (match? \=)
               (recur (subs leftover 1) line (add-token :bang-equal "!="))
               (recur leftover line (add-token :bang)))
          \= (if (match? \=)
               (recur (subs leftover 1) line (add-token :equal-equal "=="))
               (recur leftover line (add-token :equal)))
          \< (if (match? \=)
               (recur (subs leftover 1) line (add-token :less-equal "<="))
               (recur leftover line (add-token :less)))
          \> (if (match? \=)
               (recur (subs leftover 1) line (add-token :greater-equal ">="))
               (recur leftover line (add-token :greater)))
          \" (if-let [closing (index-of leftover "\"")]
               (recur (subs leftover (inc closing))
                      (+ line (get (frequencies (subs leftover 0 closing)) \newline 0))
                      (add-token :string (subs source 0 (+ closing 2)) (subs leftover 0 closing)))
               (do (error {:line line :message "Unterminated string."})
                   tokens))
          (cond
            (digit? next-char)
            (let [digits' (take-while #(or (digit? %) (= \. %)) source)
                  digits (if (= \. (last digits')) (butlast digits') digits')
                  size (count digits)
                  lexeme (subs source 0 size)]
              (recur (subs source size)
                     line
                     (add-token :number lexeme (Double/parseDouble lexeme))))
            (alpha? next-char)
            (let [idents (take-while #(alphanumeric? %) source)
                  size (count idents)
                  lexeme (subs source 0 size)]
              (recur (subs source size)
                     line
                     (add-token (get keywords lexeme :identifier) lexeme)))
            :else
            (do (error {:line line :message "Unexpected character."})
                tokens)))))))

