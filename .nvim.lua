local root = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":p:h")
local cabal_gild = vim.fs.joinpath(root, ".cabal", "bin", "cabal-gild")

if vim.fn.executable(cabal_gild) ~= 1 then
  cabal_gild = "cabal-gild"
end

local hls_settings = {
  haskell = {
    cabalFormattingProvider = "cabal-gild",
    plugin = {
      ["cabal-gild"] = {
        config = {
          path = cabal_gild,
        },
      },
    },
  },
}

local function with_hls_settings(settings)
  return vim.tbl_deep_extend("force", settings or {}, hls_settings)
end

if vim.lsp and vim.lsp.config then
  vim.lsp.config("hls", {
    settings = with_hls_settings(),
  })
end

local ok_lspconfig, lspconfig = pcall(require, "lspconfig")
if ok_lspconfig then
  local _ = lspconfig.hls
  local ok_configs, configs = pcall(require, "lspconfig.configs")
  if ok_configs and configs.hls and configs.hls.default_config then
    configs.hls.default_config.settings = with_hls_settings(configs.hls.default_config.settings)
  end
end

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("boxctl_hls_settings", { clear = true }),
  callback = function(args)
    if not args.data or not args.data.client_id then
      return
    end

    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if not client or client.name ~= "hls" then
      return
    end

    client.config.settings = with_hls_settings(client.config.settings)
    client.notify("workspace/didChangeConfiguration", {
      settings = client.config.settings,
    })
  end,
})
