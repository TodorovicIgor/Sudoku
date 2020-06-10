object Main {

  def main(args: Array[String]): Unit = {
    var state = 10
    var tables = List[Table]()
    var currentTable : Table = null
    def cls(): Unit = {
      println("-----------------------------------------")
    }
    def readState(): Unit = {
      val option = scala.io.StdIn.readInt()
      assert(option > -1 && option < 100)
      state = option
    }
    val option10 = "Unesite broj opcije\n" +
      "Učitavanje sudoku tabele iz fajla: 1\n" +
      "Prolazak kroz postojeće tabele: 2\n" +
      "Izlazak iz aplikacije: 0\n" +
      "Vaš izbor: "
    val option1 = "Unesite ime fajla, ekstenzija mora biti .tbl: "
    val option2 = "Unesite redni broj tabele za selektovanje: "
    val option20 = "Pokreni igru sa prikazanom tabelom: 21\n" +
      "Edituj prikazanu tabelu: 22\n" +
      "Vaš izbor: "
    val option21 = "Moguće opcije: 'u', 'd', 'l', 'r', '-', cifre od 1 do 9: "

    while(state>0){
      cls()
      state match{
        case 0 => System.exit(0)
        case 10 =>
          print(option10)
          readState()
        case 1 =>
          print(option1)
          val tableName = scala.io.StdIn.readLine()
          tables = tables ::: List(FileReader.readTableFromFile("input/"+tableName))
          println("Tabela "+tableName+" uspešno učitana, povratak u početni meni")
          Thread.sleep(1000)
          state = 10
        case 2 =>
          if (tables.isEmpty){
            println("Nema ucitanih tabela")
            state = 10
          }
          else {
            for (index <- tables.indices) {
              println("Redni broj: " + index + "\n" + tables(index).toString + "-------------\n")
            }
            print(option2)
            val index = scala.io.StdIn.readLine()
            currentTable = tables(index.toInt)
            state = 20
          }
        case 20 =>
          println(currentTable.toString)
          println(option20)
          readState()
        case 21 =>
          var notSolved = true
          while (notSolved){
            cls()
            println(option21)
            println(currentTable.toString)
            val c = scala.io.StdIn.readChar()
            c match{
              case 'u' => currentTable = new Table(currentTable.content, currentTable.pen.goUp())
              case 'd' => currentTable = new Table(currentTable.content, currentTable.pen.goDown())
              case 'l' => currentTable = new Table(currentTable.content, currentTable.pen.goLeft())
              case 'r' => currentTable = new Table(currentTable.content, currentTable.pen.goRight())
              case _ => currentTable = currentTable.writeField(currentTable.pen.x, currentTable.pen.y, c)
            }
            println(currentTable.isSolved())
            notSolved = !(currentTable.isSolved())
          }
          println(currentTable.toString)
          println("Sudoku je resen!\n" +
            "Povratak u glavni meni")
          state = 10
      }
    }
  }
}
