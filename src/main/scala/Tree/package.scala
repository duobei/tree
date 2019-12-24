package Tree

case class TreeNode[T](data: T, children: Seq[TreeNode[T]] = Nil)

object Tree {
  def asciiDisplay(root: TreeNode[String]): Seq[String] = {
    def helper(node: TreeNode[String], n: Int): Seq[String] =
      node match {
        case TreeNode(data, Nil) => Seq("+-" + data)
        case TreeNode(data, children) => Seq("+-" + data) ++ {
          if (n == 1) children.flatMap(t => helper(t, n + 1).map(s => "| " + s)) ++ Seq("|")
          else children.flatMap(t => helper(t, n + 1).map(s => "  " + s))
        }
      }

    helper(root, 0)
  }

  def simpleDisplay(root: TreeNode[String]): Seq[String] =
    root match {
        case TreeNode(data, children) =>
          Seq(data) ++ children.flatMap(t => simpleDisplay(t).map(s => "    " + s))
    }

  def main(args: Array[String]): Unit = {
    val tree1 = TreeNode("Root",
      children = List(TreeNode("level1-1"),
        TreeNode("level1-2"),
        TreeNode("level1-3")))

    val tree2 = TreeNode("Root",
      children = List(
        TreeNode("level1-1",
          children = TreeNode("level2-1", children = TreeNode("level3-1") :: Nil) :: Nil),
        TreeNode("level1-2"),
        TreeNode("level1-3")))

    simpleDisplay(tree1).foreach(println)
    simpleDisplay(tree2).foreach(println)

    asciiDisplay(tree1).foreach(println)
    asciiDisplay(tree2).foreach(println)
    }
}

