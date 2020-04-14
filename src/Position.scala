trait Position {
  val x: Int
  val y: Int
  require(x>=0 && x<9 && y>=0 && y<9)
}
