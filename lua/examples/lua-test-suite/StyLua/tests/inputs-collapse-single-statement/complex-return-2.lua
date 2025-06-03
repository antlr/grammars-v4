function H.is_item(x)
  return type(x) == 'table'
    and H.is_fun_or_string(x['action'], false)
    and type(x['name']) == 'string'
    and type(x['section']) == 'string'
end
