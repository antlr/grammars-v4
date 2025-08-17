local output = Job
    :new({
    command = "stylua",
    args = { "-" },
    writer = api.nvim_buf_get_lines(bufnr, 0, -1, false),
  })
    :sync()
