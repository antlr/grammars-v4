function H.get_unsaved_listed_buffers()
  return vim.tbl_filter(function(buf_id)
    return vim.api.nvim_buf_get_option(buf_id, 'modified') and vim.api.nvim_buf_get_option(buf_id, 'buflisted')
  end, vim.api.nvim_list_bufs())
end
