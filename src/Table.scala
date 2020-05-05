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
  def writeField(x:Int, y:Int, _val: Char, _overrideValue: Boolean = false): Table = {
    if (_overrideValue){
      var rows = content.slice(0, x)
      var row = content(x).slice(0, y)
      row = row ::: List(new Field(x, y, v = _val.toString, isOriginal = true))
      row = row ::: content(x).slice(y + 1, content(x).length)
      rows = rows ::: List(row) ::: content.slice(x + 1, content.length)
      new Table(rows, pen)
    }
    else if (!content(x)(y).isOriginal) {
      var rows = content.slice(0, x)
      var row = content(x).slice(0, y)
      print(_val.toString)
      row = row ::: List(new Field(x, y, v = _val.toString, isOriginal = false))
      row = row ::: content(x).slice(y + 1, content(x).length)
      rows = rows ::: List(row) ::: content.slice(x + 1, content.length)
      new Table(rows, pen)
    }
    else {
      this
    }
  }
  def delField(x: Int, y:Int): Table = {
    writeField(x, y, _val = '-', _overrideValue = true)
  }
  def transpose(): Table = {
    new Table(content = content.transpose, pen = this.pen)
  }
  def exchange(): Table = {
    val newContent = content.map(line =>{
      line.map(field => {
        field.getVal match{
          case "-" => new Field(field.x, field.y, v = "-", isOriginal = false)
          case _ =>
            if (field.getVal == "9"){
              new Field(field.x, field.y, v = "-", isOriginal = true)
            }
            else{
              new Field(field.x, field.y, v = (9-field.getVal.toInt).toString, isOriginal = true)
            }
        }
      })
    })
    new Table(newContent, this.pen)
  }

  def solve(): Table = {
    def movePenAndIncField(_table: Table): Table = {
      val pen = _table.pen.++()
      val table = new Table(_table.content, pen)
      if (table.isSolved()) return this
      table.content(table.pen.x)(table.pen.y).getVal match {
          // check if valid option
        case "9" => table.writeField(table.pen.x, table.pen.y, '1')
        case "-" => table.writeField(table.pen.x, table.pen.y, '1')
        case _ => table.writeField(table.pen.x, table.pen.y, table.content(table.pen.x)(table.pen.y).getVal.toCharArray()(0))
      }
    }
    @scala.annotation.tailrec
    def _solve(table: Table): Table = {
      var tab = movePenAndIncField(table)
      _solve(tab)
    }
    _solve(this)
  }

  def mapToInts(): List[List[Int]] ={
    this.content.map(line =>{
      line.map(field =>{
        field.getVal match {
          case "-" => 0
          case _ => field.getVal.toInt
        }
      })
    })
  }

  def isSolved(): Boolean = {
    // TODO test
    val intContent = this.mapToInts()
    val transposedIntContent = intContent.transpose
    def checkRows(): Boolean = {
      for (line <- intContent) {
        if (line.contains(0)) return false
      }
      intContent.foreach(line => {
        if (line.distinct.length != 9) return false
      })
      true
    }
    def checkCols(): Boolean = {
      for (line <- transposedIntContent) {
        if (line.contains(0)) return false
      }
      transposedIntContent.foreach(line => {
        if (line.distinct.length != 9) return false
      })
      true
    }
    def checkSquares(): Boolean = {
      //NOTICE: checkSquares DOES NOT check for zeroes, always call rows and cols BEFORE squares
      def getSubMatrix(x:Int,y:Int) : List[List[Int]] = {
        intContent.slice(x, x+3).slice(y, y+3)
      }
      for(x <- List(0, 3, 6);
          y <- List(0, 3, 6)){
        if (getSubMatrix(x, y).distinct.length != 9) return false
      }
      checkCols() && checkRows()
    }
    checkSquares()
    }


  // filterColumnAndRow()
  // filterSubTable()
  // TODO executeMoves()
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
//    print(t1.exchange().toString)
    print(t1.solve())
  }
}