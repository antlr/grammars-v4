local comment_parts = vim.tbl_filter(function(x)
   return x ~= ''
end, vim.split(commentstring, '%s', true))
