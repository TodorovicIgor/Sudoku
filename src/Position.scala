trait Position {
  val x: Int
  val y: Int
  require(x>=0 && x<10 && y>=0 && y<10)
}
