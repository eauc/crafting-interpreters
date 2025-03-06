const std = @import("std");
const chk = @import("chunk.zig");
const dbg = @import("debug.zig");
const mem = @import("memory.zig");
const obj = @import("object.zig");
const scn = @import("scanner.zig");
const tbl = @import("table.zig");
const val = @import("value.zig");
const vm = @import("vm.zig");

const Precedence = enum(u4) {
    NONE = 0,
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // == !=
    COMPARISON, // < > <= >=
    TERM, // + -
    FACTOR, // * /
    UNARY, // ! -
    CALL, // . () []
    PRIMARY,
};

const ParseFn = fn (*Compiler, bool) std.mem.Allocator.Error!void;

const ParseRule = struct {
    prefix: ?*const ParseFn = null,
    infix: ?*const ParseFn = null,
    precedence: Precedence = .NONE,
};

const rules = init_rules: {
    var array = [_]ParseRule{.{}} ** @intFromEnum(scn.TokenType.max_value);
    array[@intFromEnum(scn.TokenType.TOKEN_LEFT_PAREN)] = .{
        .prefix = Compiler.grouping,
        .infix = Compiler.call,
        .precedence = .CALL,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_RIGHT_PAREN)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_LEFT_BRACE)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_RIGHT_BRACE)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_COMMA)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_DOT)] = .{
        .infix = Compiler.dot,
        .precedence = .CALL,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_MINUS)] = .{
        .prefix = Compiler.unary,
        .infix = Compiler.binary,
        .precedence = .TERM,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_PLUS)] = .{
        .infix = Compiler.binary,
        .precedence = .TERM,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_SEMICOLON)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_SLASH)] = .{
        .infix = Compiler.binary,
        .precedence = .FACTOR,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_STAR)] = .{
        .infix = Compiler.binary,
        .precedence = .FACTOR,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_BANG)] = .{
        .prefix = Compiler.unary,
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_BANG_EQUAL)] = .{
        .infix = Compiler.binary,
        .precedence = .EQUALITY,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_EQUAL)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_EQUAL_EQUAL)] = .{
        .infix = Compiler.binary,
        .precedence = .EQUALITY,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_GREATER)] = .{
        .infix = Compiler.binary,
        .precedence = .COMPARISON,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_GREATER_EQUAL)] = .{
        .infix = Compiler.binary,
        .precedence = .COMPARISON,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_LESS)] = .{
        .infix = Compiler.binary,
        .precedence = .COMPARISON,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_LESS_EQUAL)] = .{
        .infix = Compiler.binary,
        .precedence = .COMPARISON,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_IDENTIFIER)] = .{
        .prefix = Compiler.variable,
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_STRING)] = .{
        .prefix = Compiler.string,
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_NUMBER)] = .{
        .prefix = Compiler.number,
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_AND)] = .{
        .infix = Compiler.and_,
        .precedence = .AND,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_CLASS)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_ELSE)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_FALSE)] = .{
        .prefix = Compiler.literal,
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_FOR)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_FUN)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_IF)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_NIL)] = .{
        .prefix = Compiler.literal,
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_OR)] = .{
        .infix = Compiler.or_,
        .precedence = .OR,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_PRINT)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_RETURN)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_SUPER)] = .{
        .prefix = Compiler.super,
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_THIS)] = .{
        .prefix = Compiler.this,
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_TRUE)] = .{
        .prefix = Compiler.literal,
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_VAR)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_WHILE)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_ERROR)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_EOF)] = .{
        .precedence = .NONE,
    };
    break :init_rules array;
};

fn getRule(tokenType: scn.TokenType) ParseRule {
    return rules[@intFromEnum(tokenType)];
}

const Local = struct {
    name: scn.Token,
    depth: isize,
    isCaptured: bool,
};

pub const Upvalue = struct {
    pub const Type = enum {
        LOCAL,
        UPVALUE,
    };
    index: u8,
    isLocal: Type,
};

const Parser = struct {
    scanner: *scn.Scanner,
    current: scn.Token,
    previous: scn.Token,
    hadError: bool,
    panicMode: bool,
    const default = Parser{
        .scanner = undefined,
        .current = undefined,
        .previous = undefined,
        .hadError = false,
        .panicMode = false,
    };
    pub fn advance(self: *Parser) void {
        self.previous = self.current;
        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.type != .TOKEN_ERROR) return;
            self.printErrorAtCurrent(self.current.lexeme);
        }
    }
    pub fn consume(self: *Parser, tokenType: scn.TokenType, message: []const u8) void {
        if (self.current.type == tokenType) {
            self.advance();
        } else {
            self.printErrorAtCurrent(message);
        }
    }
    pub fn match(self: *Parser, tokenType: scn.TokenType) bool {
        if (!self.check(tokenType)) {
            return false;
        }
        self.advance();
        return true;
    }
    fn check(self: *Parser, tokenType: scn.TokenType) bool {
        return self.current.type == tokenType;
    }
    fn synchronize(self: *Parser) void {
        self.panicMode = false;

        while (!self.check(.TOKEN_EOF)) {
            if (self.previous.type == .TOKEN_SEMICOLON) {
                return;
            }
            switch (self.current.type) {
                .TOKEN_CLASS, .TOKEN_FUN, .TOKEN_VAR, .TOKEN_FOR, .TOKEN_IF, .TOKEN_WHILE, .TOKEN_PRINT, .TOKEN_RETURN => return,
                else => {},
            }
            self.advance();
        }
    }
    fn printErrorAtCurrent(self: *Parser, message: []const u8) void {
        self.printErrorAt(self.current, message);
    }
    fn printError(self: *Parser, message: []const u8) void {
        self.printErrorAt(self.previous, message);
    }
    fn printErrorAt(self: *Parser, token: scn.Token, message: []const u8) void {
        if (self.panicMode) {
            return;
        }
        self.panicMode = true;
        std.debug.print("[line {}] Error", .{token.line});
        if (token.type == .TOKEN_EOF) {
            std.debug.print(" at end", .{});
        } else if (token.type != .TOKEN_ERROR) {
            std.debug.print(" at '{s}'", .{token.lexeme});
        }
        std.debug.print(": {s}\n", .{message});
        self.hadError = true;
    }
};

const FunctionType = enum {
    INITIALIZER,
    FUNCTION,
    METHOD,
    SCRIPT,
};

const ClassCompiler = struct {
    enclosing: ?*const ClassCompiler,
    hasSuperclass: bool,
};

pub const Compiler = struct {
    allocator: *mem.Allocator,
    stack: *vm.Stack,
    enclosing: ?*Compiler,
    currentClass: ?*const ClassCompiler,
    parser: *Parser,
    strings: *tbl.Table,
    function: *obj.ObjFunction,
    type: FunctionType,
    locals: [std.math.maxInt(u8) + 1]Local,
    localCount: usize,
    upvalues: [std.math.maxInt(u8) + 1]Upvalue,
    scopeDepth: usize,
    const default = Compiler{
        .allocator = undefined,
        .stack = undefined,
        .enclosing = null,
        .currentClass = null,
        .parser = undefined,
        .strings = undefined,
        .function = undefined,
        .type = .SCRIPT,
        .locals = undefined,
        .localCount = 0,
        .upvalues = undefined,
        .scopeDepth = 0,
    };
    pub fn init(
        self: *Compiler,
        allocator: *mem.Allocator,
        stack: *vm.Stack,
        parser: *Parser,
        fnType: FunctionType,
        strings: *tbl.Table,
        currentClass: ?*const ClassCompiler,
    ) !void {
        self.allocator = allocator;
        self.stack = stack;
        self.currentClass = currentClass;
        self.parser = parser;
        self.strings = strings;
        self.function = try obj.ObjFunction.create(allocator);
        self.allocator.compiler = self;
        if (fnType != .SCRIPT) {
            self.function.name = @fieldParentPtr("obj", try strings.copyString(self.parser.previous.lexeme));
        }
        self.type = fnType;
        if (self.type != .FUNCTION) {
            self.locals[0] = .{
                .name = scn.Token{
                    .type = .TOKEN_STRING,
                    .lexeme = "this",
                    .line = 0,
                },
                .depth = 0,
                .isCaptured = false,
            };
        } else {
            self.locals[0] = .{
                .name = scn.Token{
                    .type = .TOKEN_STRING,
                    .lexeme = "",
                    .line = 0,
                },
                .depth = 0,
                .isCaptured = false,
            };
        }
        self.localCount = 1;
        self.scopeDepth = 0;
    }
    pub fn end(self: *Compiler) !*obj.ObjFunction {
        try self.emitReturn();
        if (dbg.PRINT_CODE and !self.parser.hadError) {
            dbg.disassembleChunk(
                self.function.chunk,
                if (self.function.name) |name| name.chars else "<script>",
            );
        }
        if (self.enclosing) |enclosing| {
            self.allocator.compiler = enclosing;
        } else {
            self.allocator.compiler = null;
        }
        return self.function;
    }
    fn parsePrecedence(self: *Compiler, precedence: Precedence) std.mem.Allocator.Error!void {
        self.parser.advance();
        const canAssign = @intFromEnum(precedence) <= @intFromEnum(Precedence.ASSIGNMENT);
        if (getRule(self.parser.previous.type).prefix) |prefixFn| {
            try prefixFn(self, canAssign);
        } else {
            self.parser.printError("Expect expression.");
            return;
        }
        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.parser.current.type).precedence)) {
            self.parser.advance();
            if (getRule(self.parser.previous.type).infix) |infixFn| {
                try infixFn(self, canAssign);
            }
        }
        if (canAssign and self.parser.match(.TOKEN_EQUAL)) {
            self.parser.printError("Invalid assignment target.");
        }
    }
    pub fn declaration(self: *Compiler) std.mem.Allocator.Error!void {
        if (self.parser.match(.TOKEN_CLASS)) {
            try self.classDeclaration();
        } else if (self.parser.match(.TOKEN_FUN)) {
            try self.funDeclaration();
        } else if (self.parser.match(.TOKEN_VAR)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }
        if (self.parser.panicMode) {
            self.parser.synchronize();
        }
    }
    fn syntheticToken(self: *Compiler, lexeme: []const u8) scn.Token {
        _ = self;
        return .{
            .type = .TOKEN_IDENTIFIER,
            .lexeme = lexeme,
            .line = 0,
        };
    }
    fn classDeclaration(self: *Compiler) std.mem.Allocator.Error!void {
        self.parser.consume(.TOKEN_IDENTIFIER, "Expect class name.");
        const className = self.parser.previous;
        const nameConstant = try self.identifierConstant(className);
        self.declareVariable();
        try self.emitBytes(.{ .instruction = .OP_CLASS }, .{ .constant = nameConstant });
        try self.defineVariable(nameConstant);

        var classCompiler = ClassCompiler{
            .enclosing = self.currentClass,
            .hasSuperclass = false,
        };
        self.currentClass = &classCompiler;

        if (self.parser.match(.TOKEN_LESS)) {
            self.parser.consume(.TOKEN_IDENTIFIER, "Expect superclass name.");
            try self.variable(false);
            if (self.parser.previous.identifiersEqual(className)) {
                self.parser.printError("A class can't inherit from itself.");
            }

            self.beginScope();
            self.addLocal(self.syntheticToken("super"));
            try self.defineVariable(0);

            try self.namedVariable(className, false);
            try self.emitByte(.{ .instruction = .OP_INHERIT });
            classCompiler.hasSuperclass = true;
        }

        try self.namedVariable(className, false);
        self.parser.consume(.TOKEN_LEFT_BRACE, "Expect '{' before class body.");
        while (!self.parser.check(.TOKEN_RIGHT_BRACE) and !self.parser.check(.TOKEN_EOF)) {
            try self.method();
        }
        self.parser.consume(.TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
        try self.emitByte(.{ .instruction = .OP_POP });

        if (classCompiler.hasSuperclass) {
            try self.endScope();
        }
        self.currentClass = self.currentClass.?.enclosing;
    }
    fn funDeclaration(self: *Compiler) std.mem.Allocator.Error!void {
        const global = try self.parseVariable("Expect function name.");
        self.markInitialized();
        try self.fun(.FUNCTION);
        try self.defineVariable(global);
    }
    fn varDeclaration(self: *Compiler) std.mem.Allocator.Error!void {
        const global = try self.parseVariable("Expect variable name.");
        if (self.parser.match(.TOKEN_EQUAL)) {
            try self.expression();
        } else {
            try self.emitByte(.{ .instruction = .OP_NIL });
        }
        self.parser.consume(.TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
        try self.defineVariable(global);
    }
    fn statement(self: *Compiler) std.mem.Allocator.Error!void {
        if (self.parser.match(.TOKEN_PRINT)) {
            try self.printStatement();
        } else if (self.parser.match(.TOKEN_IF)) {
            try self.ifStatement();
        } else if (self.parser.match(.TOKEN_RETURN)) {
            try self.returnStatement();
        } else if (self.parser.match(.TOKEN_WHILE)) {
            try self.whileStatement();
        } else if (self.parser.match(.TOKEN_FOR)) {
            try self.forStatement();
        } else if (self.parser.match(.TOKEN_LEFT_BRACE)) {
            self.beginScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }
    fn beginScope(self: *Compiler) void {
        self.scopeDepth += 1;
    }
    fn endScope(self: *Compiler) !void {
        self.scopeDepth -= 1;
        while (self.localCount > 0 and self.locals[self.localCount - 1].depth > self.scopeDepth) {
            if (self.locals[self.localCount - 1].isCaptured) {
                try self.emitByte(.{ .instruction = .OP_CLOSE_UPVALUE });
            } else {
                try self.emitByte(.{ .instruction = .OP_POP });
            }
            self.localCount -= 1;
        }
    }
    fn printStatement(self: *Compiler) std.mem.Allocator.Error!void {
        try self.expression();
        self.parser.consume(.TOKEN_SEMICOLON, "Expect ';' after value.");
        try self.emitByte(.{ .instruction = .OP_PRINT });
    }
    fn ifStatement(self: *Compiler) std.mem.Allocator.Error!void {
        self.parser.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
        try self.expression();
        self.parser.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

        const thenJump = try self.emitJump(.OP_JUMP_IF_FALSE);
        try self.emitByte(.{ .instruction = .OP_POP });
        try self.statement();

        const elseJump = try self.emitJump(.OP_JUMP);
        self.patchJump(thenJump);

        try self.emitByte(.{ .instruction = .OP_POP });
        if (self.parser.match(.TOKEN_ELSE)) {
            try self.statement();
        }
        self.patchJump(elseJump);
    }
    fn returnStatement(self: *Compiler) std.mem.Allocator.Error!void {
        if (self.scopeDepth == 0) {
            self.parser.printError("Can't return from top-level code.");
            return;
        }
        if (self.parser.match(.TOKEN_SEMICOLON)) {
            try self.emitReturn();
        } else {
            if (self.type == .INITIALIZER) {
                self.parser.printError("Can't return a value from an initializer.");
            }
            try self.expression();
            self.parser.consume(.TOKEN_SEMICOLON, "Expect ';' after return value.");
            try self.emitByte(.{ .instruction = .OP_RETURN });
        }
    }
    fn whileStatement(self: *Compiler) std.mem.Allocator.Error!void {
        const loopStart = self.function.chunk.count;
        self.parser.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
        try self.expression();
        self.parser.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

        const exitJump = try self.emitJump(.OP_JUMP_IF_FALSE);
        try self.emitByte(.{ .instruction = .OP_POP });
        try self.statement();
        try self.emitLoop(loopStart);

        self.patchJump(exitJump);
        try self.emitByte(.{ .instruction = .OP_POP });
    }
    fn forStatement(self: *Compiler) std.mem.Allocator.Error!void {
        self.beginScope();

        self.parser.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
        if (self.parser.match(.TOKEN_SEMICOLON)) {
            // No initializer.
        } else if (self.parser.match(.TOKEN_VAR)) {
            try self.varDeclaration();
        } else {
            try self.expressionStatement();
        }

        var loopStart = self.function.chunk.count;

        var exitJump: ?usize = null;
        if (!self.parser.match(.TOKEN_SEMICOLON)) {
            try self.expression();
            self.parser.consume(.TOKEN_SEMICOLON, "Expect ';' after 'for' condition.");

            exitJump = try self.emitJump(.OP_JUMP_IF_FALSE);
            try self.emitByte(.{ .instruction = .OP_POP });
        }

        if (!self.parser.match(.TOKEN_RIGHT_PAREN)) {
            const bodyJump = try self.emitJump(.OP_JUMP);
            const incrementStart = self.function.chunk.count;
            try self.expression();
            try self.emitByte(.{ .instruction = .OP_POP });
            self.parser.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after 'for' clauses.");

            try self.emitLoop(loopStart);
            loopStart = incrementStart;
            self.patchJump(bodyJump);
        }
        try self.statement();
        try self.emitLoop(loopStart);

        if (exitJump) |j| {
            self.patchJump(j);
            try self.emitByte(.{ .instruction = .OP_POP });
        }

        try self.endScope();
    }
    fn expressionStatement(self: *Compiler) std.mem.Allocator.Error!void {
        try self.expression();
        self.parser.consume(.TOKEN_SEMICOLON, "Expect ';' after expression.");
        try self.emitByte(.{ .instruction = .OP_POP });
    }
    fn block(self: *Compiler) std.mem.Allocator.Error!void {
        while (!self.parser.check(.TOKEN_RIGHT_BRACE) and !self.parser.check(.TOKEN_EOF)) {
            try self.declaration();
        }
        self.parser.consume(.TOKEN_RIGHT_BRACE, "Expect '}' after block.");
    }
    fn fun(self: *Compiler, fnType: FunctionType) std.mem.Allocator.Error!void {
        var compiler = Compiler.default;
        compiler.enclosing = self;
        try compiler.init(self.allocator, self.stack, self.parser, fnType, self.strings, self.currentClass);
        compiler.beginScope();
        compiler.parser.consume(.TOKEN_LEFT_PAREN, "Expect '(' after function name.");
        if (!compiler.parser.check(.TOKEN_RIGHT_PAREN)) {
            while (true) {
                compiler.function.arity += 1;
                if (compiler.function.arity == 255) {
                    compiler.parser.printErrorAtCurrent("Can't have more than 255 parameters.");
                }
                const constant = try compiler.parseVariable("Expect parameter name.");
                try compiler.defineVariable(constant);
                if (!compiler.parser.match(.TOKEN_COMMA)) break;
            }
        }
        compiler.parser.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
        compiler.parser.consume(.TOKEN_LEFT_BRACE, "Expect '{' before function body.");
        try compiler.block();
        const function = try compiler.end();
        const constant = try self.makeConstant(val.Value.objVal(&function.obj));
        try self.emitBytes(.{ .instruction = .OP_CLOSURE }, .{ .constant = constant });
        for (0..function.upvalueCount) |i| {
            try self.emitByte(.{ .constant = switch (compiler.upvalues[i].isLocal) {
                .LOCAL => 1,
                .UPVALUE => 0,
            } });
            try self.emitByte(.{ .constant = compiler.upvalues[i].index });
        }
    }
    fn method(self: *Compiler) std.mem.Allocator.Error!void {
        self.parser.consume(.TOKEN_IDENTIFIER, "Expect method name.");
        const constant = try self.identifierConstant(self.parser.previous);
        var fnType: FunctionType = .METHOD;
        if (std.mem.eql(u8, self.parser.previous.lexeme, "init")) {
            fnType = .INITIALIZER;
        }
        try self.fun(fnType);
        try self.emitBytes(.{ .instruction = .OP_METHOD }, .{ .constant = constant });
    }
    fn expression(self: *Compiler) std.mem.Allocator.Error!void {
        try self.parsePrecedence(.ASSIGNMENT);
    }
    fn binary(self: *Compiler, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        const operatorType = self.parser.previous.type;
        const rule = getRule(operatorType);
        try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));
        switch (operatorType) {
            .TOKEN_BANG_EQUAL => try self.emitBytes(.{ .instruction = .OP_EQUAL }, .{ .instruction = .OP_NOT }),
            .TOKEN_EQUAL_EQUAL => try self.emitByte(.{ .instruction = .OP_EQUAL }),
            .TOKEN_GREATER => try self.emitByte(.{ .instruction = .OP_GREATER }),
            .TOKEN_GREATER_EQUAL => try self.emitBytes(.{ .instruction = .OP_LESS }, .{ .instruction = .OP_NOT }),
            .TOKEN_LESS => try self.emitByte(.{ .instruction = .OP_LESS }),
            .TOKEN_LESS_EQUAL => try self.emitBytes(.{ .instruction = .OP_GREATER }, .{ .instruction = .OP_NOT }),
            .TOKEN_PLUS => try self.emitByte(.{ .instruction = .OP_ADD }),
            .TOKEN_MINUS => try self.emitByte(.{ .instruction = .OP_SUBTRACT }),
            .TOKEN_STAR => try self.emitByte(.{ .instruction = .OP_MULTIPLY }),
            .TOKEN_SLASH => try self.emitByte(.{ .instruction = .OP_DIVIDE }),
            else => unreachable,
        }
    }
    fn and_(self: *Compiler, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        const endJump = try self.emitJump(.OP_JUMP_IF_FALSE);
        try self.emitByte(.{ .instruction = .OP_POP });
        try self.parsePrecedence(.AND);
        self.patchJump(endJump);
    }
    fn or_(self: *Compiler, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        const elseJump = try self.emitJump(.OP_JUMP_IF_FALSE);
        const endJump = try self.emitJump(.OP_JUMP);
        self.patchJump(elseJump);
        try self.emitByte(.{ .instruction = .OP_POP });
        try self.parsePrecedence(.OR);
        self.patchJump(endJump);
    }
    fn call(self: *Compiler, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        const argCount = try self.argumentsList();
        try self.emitBytes(.{ .instruction = .OP_CALL }, .{ .constant = argCount });
    }
    fn dot(self: *Compiler, canAssign: bool) std.mem.Allocator.Error!void {
        self.parser.consume(.TOKEN_IDENTIFIER, "Expect property name after '.'.");
        const name = try self.identifierConstant(self.parser.previous);
        if (canAssign and self.parser.match(.TOKEN_EQUAL)) {
            try self.expression();
            try self.emitBytes(.{ .instruction = .OP_SET_PROPERTY }, .{ .constant = name });
        } else if (self.parser.match(.TOKEN_LEFT_PAREN)) {
            const argCount = try self.argumentsList();
            try self.emitBytes(.{ .instruction = .OP_INVOKE }, .{ .constant = name });
            try self.emitByte(.{ .constant = argCount });
        } else {
            try self.emitBytes(.{ .instruction = .OP_GET_PROPERTY }, .{ .constant = name });
        }
    }
    fn argumentsList(self: *Compiler) std.mem.Allocator.Error!u8 {
        var argCount: u8 = 0;
        if (!self.parser.check(.TOKEN_RIGHT_PAREN)) {
            while (true) {
                try self.expression();
                argCount += 1;
                if (argCount == 255) {
                    self.parser.printErrorAtCurrent("Can't have more than 255 arguments.");
                }
                if (!self.parser.match(.TOKEN_COMMA)) break;
            }
        }
        self.parser.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
        return argCount;
    }
    fn grouping(self: *Compiler, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        try self.expression();
        self.parser.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }
    fn literal(self: *Compiler, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        switch (self.parser.previous.type) {
            .TOKEN_NIL => try self.emitByte(.{ .instruction = .OP_NIL }),
            .TOKEN_TRUE => try self.emitByte(.{ .instruction = .OP_TRUE }),
            .TOKEN_FALSE => try self.emitByte(.{ .instruction = .OP_FALSE }),
            else => unreachable,
        }
    }
    fn number(self: *Compiler, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        const value: val.Number = std.fmt.parseFloat(val.Number, self.parser.previous.lexeme) catch unreachable;
        try self.emitConstant(val.Value.numberVal(value));
    }
    fn string(self: *Compiler, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        const object = try self.strings.copyString(self.parser.previous.lexeme[1 .. self.parser.previous.lexeme.len - 1]);
        try self.emitConstant(val.Value.objVal(object));
    }
    fn unary(self: *Compiler, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        const operatorType = self.parser.previous.type;
        try self.parsePrecedence(.UNARY);
        switch (operatorType) {
            .TOKEN_BANG => try self.emitByte(.{ .instruction = .OP_NOT }),
            .TOKEN_MINUS => try self.emitByte(.{ .instruction = .OP_NEGATE }),
            else => unreachable,
        }
    }
    fn variable(self: *Compiler, canAssign: bool) std.mem.Allocator.Error!void {
        try self.namedVariable(self.parser.previous, canAssign);
    }
    fn this(self: *Compiler, canAssign: bool) std.mem.Allocator.Error!void {
        if (self.currentClass) |_| {
            return self.variable(canAssign);
        } else {
            self.parser.printError("Can't use 'this' outside of a class.");
            return;
        }
    }
    fn super(self: *Compiler, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        if (self.currentClass) |currentClass| {
            if (!currentClass.hasSuperclass) {
                self.parser.printError("Can't use 'super' in a class with no superclass.");
            }
        } else {
            self.parser.printError("Can't use 'super' outside of a class.");
        }
        self.parser.consume(.TOKEN_DOT, "Expect '.' after 'super'.");
        self.parser.consume(.TOKEN_IDENTIFIER, "Expect superclass method name.");
        const name = try self.identifierConstant(self.parser.previous);

        try self.namedVariable(self.syntheticToken("this"), false);
        if (self.parser.match(.TOKEN_LEFT_PAREN)) {
            const argCount = try self.argumentsList();
            try self.namedVariable(self.syntheticToken("super"), false);
            try self.emitBytes(.{ .instruction = .OP_SUPER_INVOKE }, .{ .constant = name });
            try self.emitByte(.{ .constant = argCount });
        } else {
            try self.namedVariable(self.syntheticToken("super"), false);
            try self.emitBytes(.{ .instruction = .OP_GET_SUPER }, .{ .constant = name });
        }
    }
    fn namedVariable(self: *Compiler, name: scn.Token, canAssign: bool) std.mem.Allocator.Error!void {
        var getOp: chk.Instruction = undefined;
        var setOp: chk.Instruction = undefined;
        const arg: u8 = if (self.resolveLocal(name)) |depth| blk: {
            getOp = .OP_GET_LOCAL;
            setOp = .OP_SET_LOCAL;
            break :blk @intCast(depth);
        } else if (self.resolveUpvalue(name)) |index| blk: {
            getOp = .OP_GET_UPVALUE;
            setOp = .OP_SET_UPVALUE;
            break :blk @intCast(index);
        } else blk: {
            getOp = .OP_GET_GLOBAL;
            setOp = .OP_SET_GLOBAL;
            break :blk try self.identifierConstant(name);
        };
        if (canAssign and self.parser.match(.TOKEN_EQUAL)) {
            try self.expression();
            try self.emitBytes(.{ .instruction = setOp }, .{ .constant = arg });
        } else {
            try self.emitBytes(.{ .instruction = getOp }, .{ .constant = arg });
        }
    }
    fn parseVariable(self: *Compiler, message: []const u8) std.mem.Allocator.Error!u8 {
        self.parser.consume(.TOKEN_IDENTIFIER, message);
        self.declareVariable();
        if (self.scopeDepth > 0) return 0;
        return try self.identifierConstant(self.parser.previous);
    }
    fn identifierConstant(self: *Compiler, name: scn.Token) std.mem.Allocator.Error!u8 {
        const str = try self.strings.copyString(name.lexeme);
        return try self.makeConstant(val.Value.objVal(str));
    }
    fn makeConstant(self: *Compiler, value: val.Value) std.mem.Allocator.Error!u8 {
        const constant = try self.function.chunk.addConstant(value, self.stack);
        if (constant > std.math.maxInt(u8)) {
            self.parser.printError("Too many constants in one chunk.");
            return 0;
        }
        return @intCast(constant);
    }
    fn declareVariable(self: *Compiler) void {
        if (self.scopeDepth == 0) {
            return;
        }
        const name = self.parser.previous;
        if (self.localCount > 0) {
            for (1..self.localCount) |i| {
                const local = &self.locals[self.localCount - i];
                if (local.depth != -1 and local.depth < self.scopeDepth) {
                    break;
                }
                if (name.identifiersEqual(local.name)) {
                    self.parser.printError("Already a variable with this name in this scope.");
                }
            }
        }
        self.addLocal(name);
    }
    fn defineVariable(self: *Compiler, global: u8) std.mem.Allocator.Error!void {
        if (self.scopeDepth > 0) {
            self.markInitialized();
            return;
        }
        try self.emitBytes(.{ .instruction = .OP_DEFINE_GLOBAL }, .{ .constant = global });
    }
    fn emitConstant(self: *Compiler, value: val.Value) std.mem.Allocator.Error!void {
        const constant = try self.makeConstant(value);
        try self.emitBytes(.{ .instruction = .OP_CONSTANT }, .{ .constant = constant });
    }
    fn emitReturn(self: *Compiler) std.mem.Allocator.Error!void {
        if (self.type == .INITIALIZER) {
            try self.emitBytes(.{ .instruction = .OP_GET_LOCAL }, .{ .constant = 0 });
        } else {
            try self.emitByte(.{ .instruction = .OP_NIL });
        }
        try self.emitByte(.{ .instruction = .OP_RETURN });
    }
    fn emitLoop(self: *Compiler, loopStart: usize) std.mem.Allocator.Error!void {
        try self.emitByte(.{ .instruction = .OP_LOOP });
        const offset = self.function.chunk.count - loopStart + 2;
        if (offset > std.math.maxInt(u16)) {
            self.parser.printError("Loop body too large.");
        }
        try self.emitBytes(.{ .constant = @intCast((offset >> 8) & 0xff) }, .{ .constant = @intCast(offset & 0xff) });
    }
    fn emitJump(self: *Compiler, instruction: chk.Instruction) std.mem.Allocator.Error!usize {
        try self.emitByte(.{ .instruction = instruction });
        try self.emitByte(.{ .constant = 0xff });
        try self.emitByte(.{ .constant = 0xff });
        return self.function.chunk.count - 2;
    }
    fn patchJump(self: *Compiler, offset: usize) void {
        const jump = self.function.chunk.count - offset - 2;
        if (jump > std.math.maxInt(u16)) {
            self.parser.printError("Too much code to jump over.");
        }
        self.function.chunk.code[offset] = .{ .constant = @intCast(jump >> 8) };
        self.function.chunk.code[offset + 1] = .{ .constant = @intCast(jump & 0xff) };
    }
    fn emitBytes(self: *Compiler, opCode1: chk.OpCode, opCode2: chk.OpCode) std.mem.Allocator.Error!void {
        try self.emitByte(opCode1);
        try self.emitByte(opCode2);
    }
    fn emitByte(self: *Compiler, opCode: chk.OpCode) std.mem.Allocator.Error!void {
        try self.function.chunk.write(opCode, self.parser.previous.line);
    }
    fn addLocal(self: *Compiler, name: scn.Token) void {
        if (self.localCount == std.math.maxInt(u8) + 1) {
            self.parser.printError("Too many local variables in function.");
            return;
        }
        const local = &self.locals[self.localCount];
        local.name = name;
        local.depth = -1;
        local.isCaptured = false;
        self.localCount += 1;
    }
    fn markInitialized(self: *Compiler) void {
        if (self.scopeDepth == 0) {
            return;
        }
        self.locals[self.localCount - 1].depth = @intCast(self.scopeDepth);
    }
    fn resolveLocal(self: *Compiler, name: scn.Token) ?usize {
        if (self.localCount == 0) {
            return null;
        }
        for (1..self.localCount + 1) |i| {
            const local = &self.locals[self.localCount - i];
            if (name.identifiersEqual(local.name)) {
                if (local.depth == -1) {
                    self.parser.printError("Can't read local variable in its own initializer.");
                }
                return self.localCount - i;
            }
        }
        return null;
    }
    fn resolveUpvalue(self: *Compiler, name: scn.Token) ?usize {
        if (self.enclosing) |enclosing| {
            if (enclosing.resolveLocal(name)) |local| {
                enclosing.locals[local].isCaptured = true;
                return self.addUpvalue(local, .LOCAL);
            }
            if (enclosing.resolveUpvalue(name)) |upvalue| {
                return self.addUpvalue(upvalue, .UPVALUE);
            }
        }
        return null;
    }
    fn addUpvalue(self: *Compiler, index: usize, isLocal: Upvalue.Type) usize {
        const upvalueCount = self.function.upvalueCount;
        for (0..upvalueCount) |i| {
            const upvalue = self.upvalues[i];
            if (upvalue.index == index and upvalue.isLocal == isLocal) {
                return i;
            }
        }
        if (upvalueCount == std.math.maxInt(u8) + 1) {
            self.parser.printError("Too many closure variables in function.");
            return 0;
        }
        self.upvalues[upvalueCount].isLocal = isLocal;
        self.upvalues[upvalueCount].index = @intCast(index);
        self.function.upvalueCount += 1;
        return upvalueCount;
    }
    pub fn markRoots(self: *Compiler) void {
        self.function.obj.mark();
        if (self.enclosing) |enclosing| {
            enclosing.markRoots();
        }
    }
};

pub fn compile(source: []const u8, allocator: *mem.Allocator, stack: *vm.Stack, strings: *tbl.Table) !*obj.ObjFunction {
    var scanner = scn.Scanner.default;
    scanner.init(source);
    var parser = Parser.default;
    parser.scanner = &scanner;
    var compiler = Compiler.default;
    try compiler.init(allocator, stack, &parser, .SCRIPT, strings, null);

    parser.advance();
    while (!parser.match(.TOKEN_EOF)) {
        try compiler.declaration();
    }

    const function = try compiler.end();
    if (parser.hadError) {
        return error.CompileError;
    }
    return function;
}
