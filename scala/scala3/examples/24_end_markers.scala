// Covers: endMarker, endMarkerTag (id, IF, WHILE, FOR, MATCH, TRY, GIVEN, VAL)
// topStat endMarker, templateStat endMarker, blockStat endMarker

object EndMarkerDemo {
  def checkIf(x: Int): String = {
    if (x > 0) { "positive" } else { "non-positive" }
    end if
    "done"
  }

  def checkWhile(): Unit = {
    var i = 0
    while (i < 3) { i = i + 1 }
    end while
  }

  def checkFor(): Unit = {
    for { x <- List(1, 2, 3) } { println(x) }
    end for
  }

  def checkMatch(x: Int): String = {
    x match { case 1 => "one"; case _ => "other" }
    end match
    "done"
  }

  def checkTry(): Int = {
    try { 1 } catch { case _: Exception => 0 }
    end try
    0
  }

  given intVal: Int = 42
  end given

  val result: Int = 0
  end val

  end EndMarkerDemo
}

end EndMarkerDemo
