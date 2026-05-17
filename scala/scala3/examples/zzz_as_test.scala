object Test:
  def renderShow = F: proxy ?=>
    limit:
      if cond
      then
        simpleExpr
      else
        for
          withPerfs <- fetchPerfs
          as <- fetchActivity
        yield result2(withPerfs, as)
