-- NimbyScript Language Server configuration for Neovim
-- Usage: require('nimbyscript').setup()

local M = {}

-- Default configuration
M.config = {
    -- Path to the nimbyscript-lsp executable
    -- If nil, will search in PATH
    cmd = nil,

    -- Additional settings passed to the server
    settings = {},

    -- Filetypes to attach to
    filetypes = { 'nimbyscript' },

    -- Root directory detection patterns
    root_markers = { '.git', 'mod.txt' },

    -- Enable debug logging
    debug = false,
}

-- Setup function
function M.setup(opts)
    opts = opts or {}
    M.config = vim.tbl_deep_extend('force', M.config, opts)

    -- Register filetype
    vim.filetype.add({
        extension = {
            nimbyscript = 'nimbyscript',
        },
    })

    -- Set up syntax highlighting basics
    vim.api.nvim_create_autocmd('FileType', {
        pattern = 'nimbyscript',
        callback = function()
            vim.bo.commentstring = '// %s'
            vim.bo.comments = '://'
            -- Set up indentation
            vim.bo.tabstop = 4
            vim.bo.shiftwidth = 4
            vim.bo.expandtab = true
        end,
    })

    -- Configure LSP
    local cmd = M.config.cmd or { 'nimbyscript-lsp' }
    if type(cmd) == 'string' then
        cmd = { cmd }
    end

    -- For Neovim 0.11+ (native LSP config)
    if vim.lsp.config then
        vim.lsp.config('nimbyscript_lsp', {
            cmd = cmd,
            filetypes = M.config.filetypes,
            root_markers = M.config.root_markers,
            settings = M.config.settings,
        })
        vim.lsp.enable('nimbyscript_lsp')
        return
    end

    -- For older Neovim versions using nvim-lspconfig
    local lspconfig_ok, lspconfig = pcall(require, 'lspconfig')
    if lspconfig_ok then
        local configs = require('lspconfig.configs')

        if not configs.nimbyscript_lsp then
            configs.nimbyscript_lsp = {
                default_config = {
                    cmd = cmd,
                    filetypes = M.config.filetypes,
                    root_dir = function(fname)
                        return lspconfig.util.root_pattern(unpack(M.config.root_markers))(fname)
                            or lspconfig.util.find_git_ancestor(fname)
                            or vim.fn.getcwd()
                    end,
                    settings = M.config.settings,
                    single_file_support = true,
                },
            }
        end

        lspconfig.nimbyscript_lsp.setup({
            on_attach = function(client, bufnr)
                -- Set up keymaps
                local opts = { buffer = bufnr, noremap = true, silent = true }
                vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
                vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
                vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
                vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
                vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, opts)

                if M.config.debug then
                    print('NimbyScript LSP attached to buffer ' .. bufnr)
                end
            end,
        })
    else
        vim.notify(
            'nvim-lspconfig not found. Please install it or upgrade to Neovim 0.11+',
            vim.log.levels.WARN
        )
    end
end

-- Manual attach function (for debugging)
function M.attach()
    local clients = vim.lsp.get_clients({ name = 'nimbyscript_lsp' })
    if #clients > 0 then
        vim.lsp.buf_attach_client(0, clients[1].id)
    else
        vim.notify('NimbyScript LSP not running', vim.log.levels.WARN)
    end
end

-- Get LSP status
function M.status()
    local clients = vim.lsp.get_clients({ name = 'nimbyscript_lsp' })
    if #clients > 0 then
        return 'running'
    else
        return 'stopped'
    end
end

return M
