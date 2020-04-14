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
