def applyRecursively[T](els: Seq[T]): Unit = els.foreach {
  case tree: Tree => apply(tree)
  case treeList: List[?] => applyRecursively(treeList)
  case _ =>
}
