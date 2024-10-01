const std = @import("std");
const chk = @import("chunk.zig");
const dbg = @import("debug.zig");
const obj = @import("object.zig");
const scn = @import("scanner.zig");
const val = @import("value.zig");

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

const ParseFn = fn (*Parser, bool) std.mem.Allocator.Error!void;

const ParseRule = struct {
    prefix: ?*const ParseFn = null,
    infix: ?*const ParseFn = null,
    precedence: Precedence = .NONE,
};

const rules = init_rules: {
    var array = [_]ParseRule{.{}} ** @intFromEnum(scn.TokenType.max_value);
    array[@intFromEnum(scn.TokenType.TOKEN_LEFT_PAREN)] = .{
        .prefix = Parser.grouping,
        .precedence = .NONE,
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
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_MINUS)] = .{
        .prefix = Parser.unary,
        .infix = Parser.binary,
        .precedence = .TERM,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_PLUS)] = .{
        .infix = Parser.binary,
        .precedence = .TERM,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_SEMICOLON)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_SLASH)] = .{
        .infix = Parser.binary,
        .precedence = .FACTOR,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_STAR)] = .{
        .infix = Parser.binary,
        .precedence = .FACTOR,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_BANG)] = .{
        .prefix = Parser.unary,
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_BANG_EQUAL)] = .{
        .infix = Parser.binary,
        .precedence = .EQUALITY,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_EQUAL)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_EQUAL_EQUAL)] = .{
        .infix = Parser.binary,
        .precedence = .EQUALITY,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_GREATER)] = .{
        .infix = Parser.binary,
        .precedence = .COMPARISON,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_GREATER_EQUAL)] = .{
        .infix = Parser.binary,
        .precedence = .COMPARISON,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_LESS)] = .{
        .infix = Parser.binary,
        .precedence = .COMPARISON,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_LESS_EQUAL)] = .{
        .infix = Parser.binary,
        .precedence = .COMPARISON,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_IDENTIFIER)] = .{
        .prefix = Parser.variable,
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_STRING)] = .{
        .prefix = Parser.string,
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_NUMBER)] = .{
        .prefix = Parser.number,
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_AND)] = .{
        .infix = Parser.and_,
        .precedence = .AND,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_CLASS)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_ELSE)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_FALSE)] = .{
        .prefix = Parser.literal,
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
        .prefix = Parser.literal,
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_OR)] = .{
        .infix = Parser.or_,
        .precedence = .OR,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_PRINT)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_RETURN)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_SUPER)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_THIS)] = .{
        .precedence = .NONE,
    };
    array[@intFromEnum(scn.TokenType.TOKEN_TRUE)] = .{
        .prefix = Parser.literal,
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
};

const Parser = struct {
    scanner: scn.Scanner,
    chunk: *chk.Chunk,
    current: scn.Token,
    previous: scn.Token,
    hadError: bool,
    panicMode: bool,
    locals: [std.math.maxInt(u8) + 1]Local,
    localCount: usize,
    scopeDepth: usize,
    const default = Parser{
        .scanner = undefined,
        .chunk = undefined,
        .current = undefined,
        .previous = undefined,
        .hadError = false,
        .panicMode = false,
        .locals = undefined,
        .localCount = 0,
        .scopeDepth = 0,
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
    pub fn endCompiler(self: *Parser) !void {
        self.consume(.TOKEN_EOF, "Expect end of expression.");
        try self.emitReturn();
        if (dbg.PRINT_CODE and !self.hadError) {
            dbg.disassembleChunk(self.chunk.*, "code");
        }
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
    fn check(self: *Parser, tokenType: scn.TokenType) bool {
        return self.current.type == tokenType;
    }
    fn parsePrecedence(self: *Parser, precedence: Precedence) std.mem.Allocator.Error!void {
        self.advance();
        const canAssign = @intFromEnum(precedence) <= @intFromEnum(Precedence.ASSIGNMENT);
        if (getRule(self.previous.type).prefix) |prefixFn| {
            try prefixFn(self, canAssign);
        } else {
            self.printError("Expect expression.");
            return;
        }
        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.type).precedence)) {
            self.advance();
            if (getRule(self.previous.type).infix) |infixFn| {
                try infixFn(self, canAssign);
            }
        }
        if (canAssign and self.match(.TOKEN_EQUAL)) {
            self.printError("Invalid assignment target.");
        }
    }
    pub fn declaration(self: *Parser) std.mem.Allocator.Error!void {
        if (self.match(.TOKEN_VAR)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }
        if (self.panicMode) {
            self.synchronize();
        }
    }
    fn varDeclaration(self: *Parser) std.mem.Allocator.Error!void {
        const global = try self.parseVariable("Expect variable name.");
        if (self.match(.TOKEN_EQUAL)) {
            try self.expression();
        } else {
            try self.emitByte(.{ .instruction = .OP_NIL });
        }
        self.consume(.TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
        try self.defineVariable(global);
    }
    fn statement(self: *Parser) std.mem.Allocator.Error!void {
        if (self.match(.TOKEN_PRINT)) {
            try self.printStatement();
        } else if (self.match(.TOKEN_IF)) {
            try self.ifStatement();
        } else if (self.match(.TOKEN_WHILE)) {
            try self.whileStatement();
        } else if (self.match(.TOKEN_FOR)) {
            try self.forStatement();
        } else if (self.match(.TOKEN_LEFT_BRACE)) {
            self.beginScope();
            try self.block();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }
    fn beginScope(self: *Parser) void {
        self.scopeDepth += 1;
    }
    fn endScope(self: *Parser) !void {
        self.scopeDepth -= 1;
        while (self.localCount > 0 and self.locals[self.localCount - 1].depth > self.scopeDepth) {
            try self.emitByte(.{ .instruction = .OP_POP });
            self.localCount -= 1;
        }
    }
    fn printStatement(self: *Parser) std.mem.Allocator.Error!void {
        try self.expression();
        self.consume(.TOKEN_SEMICOLON, "Expect ';' after value.");
        try self.emitByte(.{ .instruction = .OP_PRINT });
    }
    fn ifStatement(self: *Parser) std.mem.Allocator.Error!void {
        self.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
        try self.expression();
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

        const thenJump = try self.emitJump(.OP_JUMP_IF_FALSE);
        try self.emitByte(.{ .instruction = .OP_POP });
        try self.statement();

        const elseJump = try self.emitJump(.OP_JUMP);
        self.patchJump(thenJump);

        try self.emitByte(.{ .instruction = .OP_POP });
        if (self.match(.TOKEN_ELSE)) {
            try self.statement();
        }
        self.patchJump(elseJump);
    }
    fn whileStatement(self: *Parser) std.mem.Allocator.Error!void {
        const loopStart = self.chunk.count;
        self.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
        try self.expression();
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

        const exitJump = try self.emitJump(.OP_JUMP_IF_FALSE);
        try self.emitByte(.{ .instruction = .OP_POP });
        try self.statement();
        try self.emitLoop(loopStart);

        self.patchJump(exitJump);
        try self.emitByte(.{ .instruction = .OP_POP });
    }
    fn forStatement(self: *Parser) std.mem.Allocator.Error!void {
        self.beginScope();

        self.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
        if (self.match(.TOKEN_SEMICOLON)) {
            // No initializer.
        } else if (self.match(.TOKEN_VAR)) {
            try self.varDeclaration();
        } else {
            try self.expressionStatement();
        }

        var loopStart = self.chunk.count;

        var exitJump: ?usize = null;
        if (!self.match(.TOKEN_SEMICOLON)) {
            try self.expression();
            self.consume(.TOKEN_SEMICOLON, "Expect ';' after 'for' condition.");

            exitJump = try self.emitJump(.OP_JUMP_IF_FALSE);
            try self.emitByte(.{ .instruction = .OP_POP });
        }

        if (!self.match(.TOKEN_RIGHT_PAREN)) {
            const bodyJump = try self.emitJump(.OP_JUMP);
            const incrementStart = self.chunk.count;
            try self.expression();
            try self.emitByte(.{ .instruction = .OP_POP });
            self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after 'for' clauses.");

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
    fn expressionStatement(self: *Parser) std.mem.Allocator.Error!void {
        try self.expression();
        self.consume(.TOKEN_SEMICOLON, "Expect ';' after expression.");
        try self.emitByte(.{ .instruction = .OP_POP });
    }
    fn block(self: *Parser) std.mem.Allocator.Error!void {
        while (!self.check(.TOKEN_RIGHT_BRACE) and !self.check(.TOKEN_EOF)) {
            try self.declaration();
        }
        self.consume(.TOKEN_RIGHT_BRACE, "Expect '}' after block.");
    }
    fn expression(self: *Parser) std.mem.Allocator.Error!void {
        try self.parsePrecedence(.ASSIGNMENT);
    }
    fn binary(self: *Parser, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        const operatorType = self.previous.type;
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
    fn and_(self: *Parser, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        const endJump = try self.emitJump(.OP_JUMP_IF_FALSE);
        try self.emitByte(.{ .instruction = .OP_POP });
        try self.parsePrecedence(.AND);
        self.patchJump(endJump);
    }
    fn or_(self: *Parser, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        const elseJump = try self.emitJump(.OP_JUMP_IF_FALSE);
        const endJump = try self.emitJump(.OP_JUMP);
        self.patchJump(elseJump);
        try self.emitByte(.{ .instruction = .OP_POP });
        try self.parsePrecedence(.OR);
        self.patchJump(endJump);
    }
    fn grouping(self: *Parser, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        try self.expression();
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }
    fn literal(self: *Parser, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        switch (self.previous.type) {
            .TOKEN_NIL => try self.emitByte(.{ .instruction = .OP_NIL }),
            .TOKEN_TRUE => try self.emitByte(.{ .instruction = .OP_TRUE }),
            .TOKEN_FALSE => try self.emitByte(.{ .instruction = .OP_FALSE }),
            else => unreachable,
        }
    }
    fn number(self: *Parser, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        const value: val.Number = std.fmt.parseFloat(val.Number, self.previous.lexeme) catch unreachable;
        try self.emitConstant(val.Value.numberVal(value));
    }
    fn string(self: *Parser, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        const object = try obj.copyString(self.previous.lexeme[1 .. self.previous.lexeme.len - 1], self.chunk);
        try self.emitConstant(val.Value.objVal(object));
    }
    fn unary(self: *Parser, canAssign: bool) std.mem.Allocator.Error!void {
        _ = canAssign;
        const operatorType = self.previous.type;
        try self.parsePrecedence(.UNARY);
        switch (operatorType) {
            .TOKEN_BANG => try self.emitByte(.{ .instruction = .OP_NOT }),
            .TOKEN_MINUS => try self.emitByte(.{ .instruction = .OP_NEGATE }),
            else => unreachable,
        }
    }
    fn variable(self: *Parser, canAssign: bool) std.mem.Allocator.Error!void {
        try self.namedVariable(self.previous, canAssign);
    }
    fn namedVariable(self: *Parser, name: scn.Token, canAssign: bool) std.mem.Allocator.Error!void {
        var getOp: chk.Instruction = undefined;
        var setOp: chk.Instruction = undefined;
        const arg: u8 = if (self.resolveLocal(name)) |depth| blk: {
            getOp = .OP_GET_LOCAL;
            setOp = .OP_SET_LOCAL;
            break :blk @intCast(depth);
        } else blk: {
            getOp = .OP_GET_GLOBAL;
            setOp = .OP_SET_GLOBAL;
            break :blk try self.identifierConstant(name);
        };
        if (canAssign and self.match(.TOKEN_EQUAL)) {
            try self.expression();
            try self.emitBytes(.{ .instruction = setOp }, .{ .constant = arg });
        } else {
            try self.emitBytes(.{ .instruction = getOp }, .{ .constant = arg });
        }
    }
    fn parseVariable(self: *Parser, message: []const u8) std.mem.Allocator.Error!u8 {
        self.consume(.TOKEN_IDENTIFIER, message);
        self.declareVariable();
        if (self.scopeDepth > 0) return 0;
        return try self.identifierConstant(self.previous);
    }
    fn identifierConstant(self: *Parser, name: scn.Token) std.mem.Allocator.Error!u8 {
        return try self.makeConstant(val.Value.objVal(try obj.copyString(name.lexeme, self.chunk)));
    }
    fn makeConstant(self: *Parser, value: val.Value) std.mem.Allocator.Error!u8 {
        const constant = try self.chunk.addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            self.printError("Too many constants in one chunk.");
            return 0;
        }
        return @intCast(constant);
    }
    fn declareVariable(self: *Parser) void {
        if (self.scopeDepth == 0) {
            return;
        }
        const name = self.previous;
        if (self.localCount > 0) {
            for (1..self.localCount) |i| {
                const local = &self.locals[self.localCount - i];
                if (local.depth != -1 and local.depth < self.scopeDepth) {
                    break;
                }
                if (name.identifiersEqual(local.name)) {
                    self.printError("Already a variable with this name in this scope.");
                }
            }
        }
        self.addLocal(name);
    }
    fn defineVariable(self: *Parser, global: u8) std.mem.Allocator.Error!void {
        if (self.scopeDepth > 0) {
            self.markInitialized();
            return;
        }
        try self.emitBytes(.{ .instruction = .OP_DEFINE_GLOBAL }, .{ .constant = global });
    }
    fn emitConstant(self: *Parser, value: val.Value) std.mem.Allocator.Error!void {
        try self.emitBytes(.{ .instruction = .OP_CONSTANT }, .{ .constant = try self.makeConstant(value) });
    }
    fn emitReturn(self: *Parser) std.mem.Allocator.Error!void {
        try self.emitByte(.{ .instruction = .OP_RETURN });
    }
    fn emitLoop(self: *Parser, loopStart: usize) std.mem.Allocator.Error!void {
        try self.emitByte(.{ .instruction = .OP_LOOP });
        const offset = self.chunk.count - loopStart + 2;
        if (offset > std.math.maxInt(u16)) {
            self.printError("Loop body too large.");
        }
        try self.emitBytes(.{ .constant = @intCast((offset >> 8) & 0xff) }, .{ .constant = @intCast(offset & 0xff) });
    }
    fn emitJump(self: *Parser, instruction: chk.Instruction) std.mem.Allocator.Error!usize {
        try self.emitByte(.{ .instruction = instruction });
        try self.emitByte(.{ .constant = 0xff });
        try self.emitByte(.{ .constant = 0xff });
        return self.chunk.count - 2;
    }
    fn patchJump(self: *Parser, offset: usize) void {
        const jump = self.chunk.count - offset - 2;
        if (jump > std.math.maxInt(u16)) {
            self.printError("Too much code to jump over.");
        }
        self.chunk.code[offset] = .{ .constant = @intCast(jump >> 8) };
        self.chunk.code[offset + 1] = .{ .constant = @intCast(jump & 0xff) };
    }
    fn emitBytes(self: *Parser, opCode1: chk.OpCode, opCode2: chk.OpCode) std.mem.Allocator.Error!void {
        try self.emitByte(opCode1);
        try self.emitByte(opCode2);
    }
    fn emitByte(self: *Parser, opCode: chk.OpCode) std.mem.Allocator.Error!void {
        try self.chunk.write(opCode, self.previous.line);
    }
    fn addLocal(self: *Parser, name: scn.Token) void {
        if (self.localCount == std.math.maxInt(u8) + 1) {
            self.printError("Too many local variables in function.");
            return;
        }
        const local = &self.locals[self.localCount];
        local.name = name;
        local.depth = -1;
        self.localCount += 1;
    }
    fn markInitialized(self: *Parser) void {
        self.locals[self.localCount - 1].depth = @intCast(self.scopeDepth);
    }
    fn resolveLocal(self: *Parser, name: scn.Token) ?usize {
        if (self.localCount == 0) {
            return null;
        }
        for (1..self.localCount + 1) |i| {
            const local = &self.locals[self.localCount - i];
            if (name.identifiersEqual(local.name)) {
                if (local.depth == -1) {
                    self.printError("Can't read local variable in its own initializer.");
                }
                return self.localCount - i;
            }
        }
        return null;
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

pub fn compile(source: []const u8, chunk: *chk.Chunk) !void {
    var scanner = scn.Scanner.default;
    scanner.init(source);
    var parser = Parser.default;
    parser.scanner = scanner;
    parser.chunk = chunk;
    parser.advance();
    while (!parser.match(.TOKEN_EOF)) {
        try parser.declaration();
    }
    try parser.endCompiler();
    if (parser.hadError) {
        return error.CompileError;
    }
}
