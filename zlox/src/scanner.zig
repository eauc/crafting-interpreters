const std = @import("std");

pub const TokenType = enum(u8) {
    // Single-character tokens.
    TOKEN_LEFT_PAREN = 0,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_SLASH,
    TOKEN_STAR,
    // One or two character tokens.
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,
    // Literals.
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,
    // Keywords.
    TOKEN_AND,
    TOKEN_CLASS,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_IF,
    TOKEN_NIL,
    TOKEN_OR,
    TOKEN_PRINT,
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,
    TOKEN_ERROR,
    TOKEN_EOF,
    max_value,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: isize,
    pub fn create(type_: TokenType, start: []const u8, end: []const u8, line: isize) Token {
        const len = end.ptr - start.ptr;
        return Token{
            .type = type_,
            .lexeme = start[0..len],
            .line = line,
        };
    }
    pub fn errorToken(message: []const u8, line: isize) Token {
        return Token{
            .type = TokenType.TOKEN_ERROR,
            .lexeme = message,
            .line = line,
        };
    }
};

pub const Scanner = struct {
    start: []const u8,
    current: []const u8,
    line: isize,
    pub const default = Scanner{ .start = undefined, .current = undefined, .line = -1 };
    pub fn init(self: *Scanner, source: []const u8) void {
        self.start = source;
        self.current = source;
        self.line = 1;
    }
    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();
        self.start = self.current;
        if (self.isAtEnd()) {
            return Token.create(TokenType.TOKEN_EOF, self.start, self.current, self.line);
        }
        const c = self.advance();
        if (isAlpha(c)) {
            return self.identifier();
        }
        if (isDigit(c)) {
            return self.number();
        }
        switch (c) {
            '(' => return Token.create(TokenType.TOKEN_LEFT_PAREN, self.start, self.current, self.line),
            ')' => return Token.create(TokenType.TOKEN_RIGHT_PAREN, self.start, self.current, self.line),
            '{' => return Token.create(TokenType.TOKEN_LEFT_BRACE, self.start, self.current, self.line),
            '}' => return Token.create(TokenType.TOKEN_RIGHT_BRACE, self.start, self.current, self.line),
            ';' => return Token.create(TokenType.TOKEN_SEMICOLON, self.start, self.current, self.line),
            ',' => return Token.create(TokenType.TOKEN_COMMA, self.start, self.current, self.line),
            '.' => return Token.create(TokenType.TOKEN_DOT, self.start, self.current, self.line),
            '-' => return Token.create(TokenType.TOKEN_MINUS, self.start, self.current, self.line),
            '+' => return Token.create(TokenType.TOKEN_PLUS, self.start, self.current, self.line),
            '/' => return Token.create(TokenType.TOKEN_SLASH, self.start, self.current, self.line),
            '*' => return Token.create(TokenType.TOKEN_STAR, self.start, self.current, self.line),
            '!' => return Token.create(if (self.match('=')) TokenType.TOKEN_BANG_EQUAL else TokenType.TOKEN_BANG, self.start, self.current, self.line),
            '=' => return Token.create(if (self.match('=')) TokenType.TOKEN_EQUAL_EQUAL else TokenType.TOKEN_EQUAL, self.start, self.current, self.line),
            '<' => return Token.create(if (self.match('=')) TokenType.TOKEN_LESS_EQUAL else TokenType.TOKEN_LESS, self.start, self.current, self.line),
            '>' => return Token.create(if (self.match('=')) TokenType.TOKEN_GREATER_EQUAL else TokenType.TOKEN_GREATER, self.start, self.current, self.line),
            '"' => return self.string(),
            else => return Token.errorToken("Unexpected character.", self.line),
        }
    }
    fn skipWhitespace(self: *Scanner) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => {
                    return;
                },
            }
        }
    }
    fn isAtEnd(self: *Scanner) bool {
        return self.current[0] == 0;
    }
    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.current[0];
    }
    fn peekNext(self: *Scanner) u8 {
        if (self.current.len < 2) return 0;
        return self.current[1];
    }
    fn advance(self: *Scanner) u8 {
        const c = self.current[0];
        self.current = self.current[1..];
        return c;
    }
    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.current[0] != expected) return false;
        _ = self.advance();
        return true;
    }
    fn string(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }
        if (self.isAtEnd()) {
            return Token.errorToken("Unterminated string.", self.line);
        }
        _ = self.advance(); // closing quote
        return Token.create(TokenType.TOKEN_STRING, self.start, self.current, self.line);
    }
    fn number(self: *Scanner) Token {
        while (isDigit(self.peek())) {
            _ = self.advance();
        }
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance(); // consume the '.'
            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        return Token.create(TokenType.TOKEN_NUMBER, self.start, self.current, self.line);
    }
    fn identifier(self: *Scanner) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) {
            _ = self.advance();
        }
        return Token.create(identifierType(self.start, self.current), self.start, self.current, self.line);
    }
};

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn identifierType(start: []const u8, current: []const u8) TokenType {
    const len = current.ptr - start.ptr;
    const lexeme = start[0..len];
    switch (start[0]) {
        'a' => return checkKeyword(lexeme, "and", TokenType.TOKEN_AND),
        'c' => return checkKeyword(lexeme, "class", TokenType.TOKEN_CLASS),
        'e' => return checkKeyword(lexeme, "else", TokenType.TOKEN_ELSE),
        'f' => {
            if (lexeme.len > 1) {
                switch (lexeme[1]) {
                    'a' => return checkKeyword(lexeme, "false", TokenType.TOKEN_FALSE),
                    'o' => return checkKeyword(lexeme, "for", TokenType.TOKEN_FOR),
                    'u' => return checkKeyword(lexeme, "fun", TokenType.TOKEN_FUN),
                    else => return TokenType.TOKEN_IDENTIFIER,
                }
            } else {
                return TokenType.TOKEN_IDENTIFIER;
            }
        },
        'i' => return checkKeyword(lexeme, "if", TokenType.TOKEN_IF),
        'n' => return checkKeyword(lexeme, "nil", TokenType.TOKEN_NIL),
        'o' => return checkKeyword(lexeme, "or", TokenType.TOKEN_OR),
        'p' => return checkKeyword(lexeme, "print", TokenType.TOKEN_PRINT),
        'r' => return checkKeyword(lexeme, "return", TokenType.TOKEN_RETURN),
        's' => return checkKeyword(lexeme, "super", TokenType.TOKEN_SUPER),
        't' => {
            if (lexeme.len > 1) {
                switch (lexeme[1]) {
                    'h' => return checkKeyword(lexeme, "this", TokenType.TOKEN_THIS),
                    'r' => return checkKeyword(lexeme, "true", TokenType.TOKEN_TRUE),
                    else => return TokenType.TOKEN_IDENTIFIER,
                }
            } else {
                return TokenType.TOKEN_IDENTIFIER;
            }
        },
        'v' => return checkKeyword(lexeme, "var", TokenType.TOKEN_VAR),
        'w' => return checkKeyword(lexeme, "while", TokenType.TOKEN_WHILE),
        else => return TokenType.TOKEN_IDENTIFIER,
    }
}

fn checkKeyword(lexeme: []const u8, keyword: []const u8, tokenType: TokenType) TokenType {
    if (std.mem.eql(u8, lexeme, keyword)) {
        return tokenType;
    } else {
        return TokenType.TOKEN_IDENTIFIER;
    }
}
