package Tree

import org.scalatest.FunSuite

class TreeTestSuite extends FunSuite {

  test("测试一") {
    assert(List("+-Root", "  +-level1-1", "  +-level1-2", "  +-level1-3")
      == Tree.asciiDisplay(TreeNode("Root",
      children = List(TreeNode("level1-1"),
        TreeNode("level1-2"),
        TreeNode("level1-3")))))
  }

  test("测试二") {
    assert(List("+-Root", "  +-level1-1", "  | +-level2-1", "  |   +-level3-1", "  |", "  +-level1-2", "  +-level1-3")
      == Tree.asciiDisplay(TreeNode("Root",
      children = List(
        TreeNode("level1-1",
          children = TreeNode("level2-1", children = TreeNode("level3-1") :: Nil) :: Nil),
        TreeNode("level1-2"),
        TreeNode("level1-3")))))
  }
}