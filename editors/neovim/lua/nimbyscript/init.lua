-- NimbyScript Language Server configuration for Neovim
-- Usage: require('nimbyscript').setup()

local M = {}

local GITHUB_REPO = 'supermanifolds/nimby_lsp'
local GITHUB_API_URL = 'https://api.github.com/repos/' .. GITHUB_REPO .. '/releases/latest'

-- Get the installation directory for the LSP binary
local function get_install_dir()
    return vim.fn.stdpath('data') .. '/nimbyscript'
end

-- Get the path to the LSP binary
local function get_binary_path()
    local binary_name = vim.fn.has('win32') == 1 and 'nimbyscript-lsp.exe' or 'nimbyscript-lsp'
    return get_install_dir() .. '/' .. binary_name
end

-- Get the path to the version file
local function get_version_path()
    return get_install_dir() .. '/version'
end

-- Detect platform for asset selection
local function get_platform()
    local os_name = vim.loop.os_uname().sysname
    local arch = vim.loop.os_uname().machine

    if os_name == 'Darwin' then
        if arch == 'arm64' then
            return 'darwin-arm64'
        else
            return 'darwin-x64'
        end
    elseif os_name == 'Linux' then
        if arch == 'aarch64' then
            return 'linux-arm64'
        else
            return 'linux-x64'
        end
    elseif os_name:match('Windows') or vim.fn.has('win32') == 1 then
        if arch == 'ARM64' then
            return 'win32-arm64'
        else
            return 'win32-x64'
        end
    end

    return nil
end

-- Read the installed version
local function get_installed_version()
    local version_file = get_version_path()
    if vim.fn.filereadable(version_file) == 1 then
        local lines = vim.fn.readfile(version_file)
        if #lines > 0 then
            return lines[1]
        end
    end
    return nil
end

-- Save the installed version
local function save_version(version)
    local version_file = get_version_path()
    vim.fn.writefile({ version }, version_file)
end

-- Check if binary exists and is executable
local function binary_exists()
    local binary_path = get_binary_path()
    return vim.fn.executable(binary_path) == 1
end

-- Fetch latest release info from GitHub (async)
local function fetch_latest_release(callback)
    local cmd = { 'curl', '-sL', '-H', 'Accept: application/vnd.github+json', GITHUB_API_URL }

    vim.system(cmd, { text = true }, function(result)
        if result.code ~= 0 then
            vim.schedule(function()
                callback(nil, 'Failed to fetch release info: ' .. (result.stderr or 'unknown error'))
            end)
            return
        end

        local ok, json = pcall(vim.json.decode, result.stdout)
        if not ok then
            vim.schedule(function()
                callback(nil, 'Failed to parse release JSON')
            end)
            return
        end

        vim.schedule(function()
            callback(json, nil)
        end)
    end)
end

-- Find the asset URL for the current platform
local function find_asset_url(release, platform)
    if not release.assets then
        return nil
    end

    -- Look for standalone binary that matches our platform
    -- Format: nimbyscript-lsp-darwin-arm64 or nimbyscript-lsp-win32-x64.exe
    local is_windows = platform:match('^win32')
    local pattern = 'nimbyscript%-lsp%-' .. platform:gsub('%-', '%%-')
    if is_windows then
        pattern = pattern .. '%.exe$'
    else
        pattern = pattern .. '$'
    end

    for _, asset in ipairs(release.assets) do
        if asset.name:match(pattern) then
            return asset.browser_download_url, asset.name
        end
    end

    return nil
end

-- Download and install the binary (async)
local function download_binary(url, version, callback)
    local install_dir = get_install_dir()
    local binary_path = get_binary_path()

    -- Create install directory
    vim.fn.mkdir(install_dir, 'p')

    vim.notify('NimbyScript: Downloading LSP server...', vim.log.levels.INFO)

    -- Download directly to the binary path
    local download_cmd = { 'curl', '-sL', '-o', binary_path, url }

    vim.system(download_cmd, {}, function(result)
        vim.schedule(function()
            if result.code ~= 0 then
                callback(false, 'Download failed')
                return
            end

            -- Make executable on Unix
            if vim.fn.has('win32') ~= 1 then
                vim.fn.system({ 'chmod', '+x', binary_path })
            end

            -- Save version
            save_version(version)

            callback(true, nil)
        end)
    end)
end

-- Ensure the binary is installed and up-to-date
local function ensure_binary(auto_update, callback)
    local platform = get_platform()
    if not platform then
        callback(nil, 'Unsupported platform')
        return
    end

    fetch_latest_release(function(release, err)
        if err then
            -- If we can't fetch but have a binary, use it
            if binary_exists() then
                callback(get_binary_path(), nil)
            else
                callback(nil, err)
            end
            return
        end

        local latest_version = release.tag_name
        local installed_version = get_installed_version()

        -- Check if we need to download
        local needs_download = not binary_exists()
        local needs_update = auto_update and installed_version ~= latest_version

        if not needs_download and not needs_update then
            callback(get_binary_path(), nil)
            return
        end

        local asset_url = find_asset_url(release, platform)
        if not asset_url then
            if binary_exists() then
                callback(get_binary_path(), nil)
            else
                callback(nil, 'No release asset found for platform: ' .. platform)
            end
            return
        end

        if needs_update and not needs_download then
            vim.notify('NimbyScript: Updating LSP server to ' .. latest_version, vim.log.levels.INFO)
        end

        download_binary(asset_url, latest_version, function(success, download_err)
            if success then
                vim.notify('NimbyScript: LSP server installed successfully (' .. latest_version .. ')', vim.log.levels.INFO)
                callback(get_binary_path(), nil)
            else
                if binary_exists() then
                    vim.notify('NimbyScript: Update failed, using existing binary', vim.log.levels.WARN)
                    callback(get_binary_path(), nil)
                else
                    callback(nil, download_err)
                end
            end
        end)
    end)
end

-- Register filetype icon with nvim-web-devicons
local function setup_icons()
    local devicons_ok, devicons = pcall(require, 'nvim-web-devicons')
    if devicons_ok then
        devicons.set_icon({
            nimbyscript = {
                icon = '', -- U+F052C (nf-md-train)
                color = '#E5A00D',
                cterm_color = '178',
                name = 'NimbyScript',
            },
        })
    end

    -- Also support mini.icons if available
    local mini_ok, mini_icons = pcall(require, 'mini.icons')
    if mini_ok and mini_icons.set then
        mini_icons.set('filetype', 'nimbyscript', { glyph = '', hl = 'MiniIconsYellow' })
    end
end

-- Register tree-sitter parser (if nvim-treesitter is installed)
local function setup_treesitter()
    local ts_ok, parsers = pcall(require, 'nvim-treesitter.parsers')
    if not ts_ok then
        return
    end

    local parser_config = parsers.get_parser_configs()
    if not parser_config.nimbyscript then
        parser_config.nimbyscript = {
            install_info = {
                url = 'https://github.com/supermanifolds/nimby_lsp',
                files = { 'crates/tree-sitter-nimbyscript/src/parser.c' },
                branch = 'main',
            },
            filetype = 'nimbyscript',
        }
    end

    -- Auto-enable highlighting for nimbyscript buffers
    vim.api.nvim_create_autocmd('FileType', {
        pattern = 'nimbyscript',
        callback = function()
            if vim.fn.exists(':TSBufEnable') == 2 then
                vim.cmd('TSBufEnable highlight')
            end
        end,
    })
end

-- Default configuration
M.config = {
    -- Path to the nimbyscript-lsp executable
    -- If nil, will auto-download from GitHub
    cmd = nil,

    -- Automatically check for and install updates on startup
    auto_update = true,

    -- Additional settings passed to the server
    settings = {},

    -- Filetypes to attach to
    filetypes = { 'nimbyscript' },

    -- Root directory detection patterns
    root_markers = { '.git', 'mod.txt' },

    -- Enable debug logging
    debug = false,

    -- Enable inlay hints (type annotations and parameter names)
    inlay_hints_enabled = true,

    -- Enable semantic token highlighting
    semantic_tokens_enabled = true,
}

-- Build init_options from config
local function get_init_options()
    return {
        inlayHintsEnabled = M.config.inlay_hints_enabled,
        semanticTokensEnabled = M.config.semantic_tokens_enabled,
    }
end

-- Configure and start the LSP with the given command
local function setup_lsp(cmd)
    if type(cmd) == 'string' then
        cmd = { cmd }
    end

    local init_options = get_init_options()

    -- For Neovim 0.11+ (native LSP config)
    if vim.lsp.config then
        vim.lsp.config('nimbyscript_lsp', {
            cmd = cmd,
            filetypes = M.config.filetypes,
            root_markers = M.config.root_markers,
            settings = M.config.settings,
            init_options = init_options,
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
                    init_options = init_options,
                    single_file_support = true,
                },
            }
        end

        lspconfig.nimbyscript_lsp.setup({
            init_options = init_options,
            on_attach = function(_, bufnr)
                -- Set up keymaps
                local keymap_opts = { buffer = bufnr, noremap = true, silent = true }
                vim.keymap.set('n', 'gd', vim.lsp.buf.definition, keymap_opts)
                vim.keymap.set('n', 'K', vim.lsp.buf.hover, keymap_opts)
                vim.keymap.set('n', 'gr', vim.lsp.buf.references, keymap_opts)
                vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, keymap_opts)
                vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, keymap_opts)

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

    -- Set up tree-sitter parser registration
    setup_treesitter()

    -- Set up filetype icons
    setup_icons()

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

    -- If user specified a command, use it directly
    if M.config.cmd then
        setup_lsp(M.config.cmd)
        return
    end

    -- Otherwise, ensure binary is installed (async) and then set up LSP
    ensure_binary(M.config.auto_update, function(binary_path, err)
        if err then
            vim.notify('NimbyScript: ' .. err, vim.log.levels.ERROR)
            return
        end

        setup_lsp({ binary_path })
    end)
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
