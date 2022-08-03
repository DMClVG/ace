-- mod-version:2 -- lite-xl 2.0

local syntax = require "core.syntax"

syntax.add {
    files = "%.ace",
    comment = "#",
    patterns = {
        { pattern = { '"', '"', '\\' }, type = "string" },
        { pattern = "#.-\n", type = "comment" },
        { pattern = "-?%d+[%d%.eE]*", type = "number" },
        { pattern = "-?%.?%d+", type = "number" },
        { pattern = "[<>!=]=", type = "operator" },
        { pattern = "[%+%*%-%%/]=?", type = "operator" },
        { pattern = "[%a_][%w_]*%s*%f[(]", type = "function" },
        { pattern = "[%a_][%w_]*", type = "symbol" }
    },
    symbols = {
        ["if"] = "keyword",
        ["fun"] = "keyword",
        ["while"] = "keyword",
        ["for"] = "keyword",
        ["in"] = "keyword",
        ["else"] = "keyword",
        ["return"] = "keyword",
        ["and"] = "keyword",
        ["or"] = "keyword",
        ["loop"] = "keyword",
        ["let"] = "keyword",
        ["nil"] = "literal",
        ["true"] = "literal",
        ["false"] = "literal",
    }
}
