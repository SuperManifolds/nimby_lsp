-- Luacheck configuration for NimbyScript Neovim plugin

std = "lua51+luajit"

-- Neovim globals
globals = {
    "vim",
}

-- Ignore line length warnings and vim.bo field access
ignore = {
    "631", -- Line is too long
}

-- Include all lua files
include_files = {
    "lua/**/*.lua",
}
