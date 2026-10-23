local x = function(body, opts)
    return {
      top = body.top - 1,
      bottom = body.bottom + 1,
      indent = math.max(H.get_line_indent(body.top - 1, opts), H.get_line_indent(body.bottom + 1, opts)),
    }
end
