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

const ParseFn = fn (*Parser) anyerror!void;

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
        .precedence = .NONE,
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
        .precedence = .NONE,
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

const Parser = struct {
    scanner: scn.Scanner,
    chunk: *chk.Chunk,
    current: scn.Token,
    previous: scn.Token,
    hadError: bool,
    panicMode: bool,
    const default = Parser{
        .scanner = undefined,
        .chunk = undefined,
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
    pub fn endCompiler(self: *Parser) !void {
        self.consume(.TOKEN_EOF, "Expect end of expression.");
        try self.emitReturn();
        if (dbg.PRINT_CODE and !self.hadError) {
            dbg.disassembleChunk(self.chunk.*, "code");
        }
    }
    fn parsePrecedence(self: *Parser, precedence: Precedence) !void {
        self.advance();
        if (getRule(self.previous.type).prefix) |prefixFn| {
            try prefixFn(self);
        } else {
            self.printError("Expect expression.");
            return;
        }
        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.type).precedence)) {
            self.advance();
            if (getRule(self.previous.type).infix) |infixFn| {
                try infixFn(self);
            }
        }
    }
    fn binary(self: *Parser) !void {
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
    pub fn expression(self: *Parser) !void {
        try self.parsePrecedence(.ASSIGNMENT);
    }
    fn grouping(self: *Parser) !void {
        try self.expression();
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
    }
    fn literal(self: *Parser) !void {
        switch (self.previous.type) {
            .TOKEN_NIL => try self.emitByte(.{ .instruction = .OP_NIL }),
            .TOKEN_TRUE => try self.emitByte(.{ .instruction = .OP_TRUE }),
            .TOKEN_FALSE => try self.emitByte(.{ .instruction = .OP_FALSE }),
            else => unreachable,
        }
    }
    fn number(self: *Parser) !void {
        const value: val.Number = std.fmt.parseFloat(val.Number, self.previous.lexeme) catch unreachable;
        try self.emitConstant(val.Value.numberVal(value));
    }
    fn string(self: *Parser) !void {
        const objString = try obj.copyString(self.previous.lexeme[1 .. self.previous.lexeme.len - 1], self.chunk.allocator);
        self.chunk.addObject(objString);
        try self.emitConstant(val.Value.objVal(objString));
    }
    fn unary(self: *Parser) !void {
        const operatorType = self.previous.type;
        try self.parsePrecedence(.UNARY);
        switch (operatorType) {
            .TOKEN_BANG => try self.emitByte(.{ .instruction = .OP_NOT }),
            .TOKEN_MINUS => try self.emitByte(.{ .instruction = .OP_NEGATE }),
            else => unreachable,
        }
    }
    fn emitConstant(self: *Parser, value: val.Value) !void {
        try self.emitBytes(.{ .instruction = .OP_CONSTANT }, .{ .constant = try self.makeConstant(value) });
    }
    fn makeConstant(self: *Parser, value: val.Value) !u8 {
        const constant = try self.chunk.addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            self.printError("Too many constants in one chunk.");
            return 0;
        }
        return @intCast(constant);
    }
    fn emitReturn(self: *Parser) !void {
        try self.emitByte(.{ .instruction = .OP_RETURN });
    }
    fn emitBytes(self: *Parser, opCode1: chk.OpCode, opCode2: chk.OpCode) !void {
        try self.emitByte(opCode1);
        try self.emitByte(opCode2);
    }
    fn emitByte(self: *Parser, opCode: chk.OpCode) !void {
        try self.chunk.write(opCode, self.previous.line);
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
    try parser.expression();
    try parser.endCompiler();
    if (parser.hadError) {
        return error.CompileError;
    }
}
