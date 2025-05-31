local _, tag_section = toc_entry.parent:has_descendant(function(x)
  return type(x) == 'table' and x.type == 'section' and x.info.id == '@tag'
end)
