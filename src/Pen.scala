class Pen(val x: Int, val y: Int) extends Position {
  def goUp(): Pen = {
    this.y match{
      case 8 => new Pen(this.x, this.y)
      case _ => new Pen(this.x, this.y+1)
    }
  }
  def goDown(): Pen = {
    this.y match{
      case 0 => new Pen(this.x, this.y)
      case _ => new Pen(this.x, this.y-1)
    }
  }
  def goLeft(): Pen = {
    this.x match{
      case 0 => new Pen(this.x, this.y)
      case _ => new Pen(this.x-1, this.y)
    }
  }
  def goRight(): Pen = {
    this.x match{
      case 8 => new Pen(this.x, this.y)
      case _ => new Pen(this.x+1, this.y)
    }
  }
}

//object test {
//  def main(args: Array[String]): Unit = {
//    val p00: Pen = new Pen(0,0)
//    assert(p00.goLeft().x == 0)
//    assert(p00.goRight().x == 1)
//    assert(p00.goUp().y == 1)
//    assert(p00.goDown().y == 0)
//    val p88: Pen = new Pen(8,8)
//    assert(p88.goLeft().x == 7)
//    assert(p88.goRight().x == 8)
//    assert(p88.goUp().y == 8)
//    assert(p88.goDown().y == 7)
//
//    print("passed")
//  }
//}