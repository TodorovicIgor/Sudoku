class Table(val content: List[List[Field]], val pen: Pen) {
  def movePen(direction: Char): Table = {
    direction match {
      case 'u' => new Table(content = this.content, pen.goUp())
      case 'd' => new Table(content = this.content, pen.goDown())
      case 'l' => new Table(content = this.content, pen.goLeft())
      case 'r' => new Table(content = this.content, pen.goRight())
      case _ => throw new Error
    }
  }
  def setPenPosition(x:Int, y:Int): Table={
    assert(x>=0 && x<10 && y>=0 && y<10)
    new Table(content = this.content, new Pen(x,y))
  }
  def writeFieldUsingSlicing(x:Int, y:Int, _val: Char): Table = {
    if (!content(x)(y).isOriginal) {
      var rows = content.slice(0, x)
      var row = content(x).slice(0, y)
      row = row ::: List(new Field(x, y, v = _val.toString, isOriginal = false))
      row = row ::: content(x).slice(y + 1, content(x).length)
      rows = rows ::: List(row) ::: content.slice(x + 1, content.length)
      new Table(rows, pen)
    }
    else {
      this
    }
  }
  def writeField(x:Int, y:Int, _val: Char): Table = {
    // To be deleted

    if (!content(x)(y).isOriginal){
      var rows = List[List[Field]]()
      var yCoord = 0
      for (line <- content) {
        var xCoord = 0
        var row = List[Field]()
        for (char <- line){
          if (xCoord == x && yCoord == y){
            row = row ::: List(new Field(x, y, v = _val.toString, isOriginal = false))
          }
          val newField = char.getVal match {
            case "-" => List(new Field(x = xCoord, y = yCoord, v = "", isOriginal = false))
            case _ => List(new Field(x = xCoord, y = yCoord, v = char.toString, isOriginal = true))
          }
          row = row ::: newField
          xCoord += 1
        }
        yCoord += 1
        rows = rows ::: List(row)
      }
      new Table(content = rows, pen = pen)
    }
    else{
      this
    }
  }
//  delField()
//  transpose()
//  exchange()

  // TODO executeMoves()
  // TODO filterColumnAndRow()
  // TODO filterSubTable()
  // TODO isSolved()
  // TODO isSolvable()
  // TODO solve()
  // TODO createOpSequence()
  override def toString: String = {
    val ret = new StringBuilder()
    for (line <- content) {
      for (element <- line){
        ret.append(element.toString)
      }
      ret.append('\n')
    }
    ret.toString()
  }
}



object test {
  def readTableFromFile(fileName: String): Table = {
    val source = io.Source.fromFile(fileName)
    var rows = List[List[Field]]()
    val pen = new Pen(0,0)
    var yCoord = 0
    for (line <- source.getLines) {
      var xCoord = 0
      var row = List[Field]()
      for (char <- line.toCharArray){
        val newField = char match {
          case 'P' => 
            val pen = new Pen(x = xCoord, y = yCoord)
            List(new Field(x = xCoord, y = yCoord, v = "-", isOriginal = false))

          case '-' => List(new Field(x = xCoord, y = yCoord, v = "-", isOriginal = false))
          case _ => List(new Field(x = xCoord, y = yCoord, v = char.toString, isOriginal = true))
        }
        row = row ::: newField
        xCoord += 1
      }
      yCoord += 1
      rows = rows ::: List(row)
    }
    source.close()
    new Table(content = rows, pen = pen)
  }

  def main(args: Array[String]): Unit = {
    val t1 = readTableFromFile("input/test.tbl")
    print(t1.toString)
    print(t1.writeFieldUsingSlicing(1, 8, '9').toString)
  }
}