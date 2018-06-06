package Tree

case class TreeNode[T](data: T, children: Seq[TreeNode[T]] = Nil)

object Tree {
  def asciiDisplay(root: TreeNode[String]): Seq[String] = {
    def helper(node: TreeNode[String], n: Int): Seq[String] =
      node match {
        case TreeNode(data, Nil) => Seq("+-" + data)
        case TreeNode(data, children) => helper(TreeNode(data, Nil), n) ++ {
          if (n == 1) children.flatMap(t => helper(t, n + 1).map(s => "| " + s)) ++ Seq("|")
          else children.flatMap(t => helper(t, n + 1).map(s => "  " + s))
        }
      }

    helper(root, 0)
  }

  def main(args: Array[String]): Unit = {
    asciiDisplay(TreeNode("Root",
      children = List(TreeNode("level1-1"),
        TreeNode("level1-2"),
        TreeNode("level1-3")))).foreach(println)

    asciiDisplay(TreeNode("Root",
      children = List(
        TreeNode("level1-1",
          children = TreeNode("level2-1", children = TreeNode("level3-1") :: Nil) :: Nil),
        TreeNode("level1-2"),
        TreeNode("level1-3")))).foreach(println)
    }
}

