import java.io.PrintWriter

object Main {

  def main(args: Array[String]): Unit = {
    var state = 10
    var tables = List[Table]()
    var currentTable: Table = null

    def cls(): Unit = {
      println("-----------------------------------------")
    }

    def readState(): Unit = {
      val option = scala.io.StdIn.readInt()
      state = option
    }

    val option10 = "Unesite broj opcije\n" +
      "Učitavanje sudoku tabele iz fajla: 1\n" +
      "Prolazak kroz postojeće tabele: 2\n" +
      "kreiranje nove sekvence poteza: 3\n" +
      "Izlazak iz aplikacije: 0\n" +
      "Vaš izbor: "
    val option1 = "Unesite ime fajla, ekstenzija mora biti .tbl: "
    val option2 = "Unesite redni broj tabele za selektovanje: "
    val option3 = "TODO"
    val option20 = "Pokreni igru sa prikazanom tabelom: 21\n" +
      "Edituj prikazanu tabelu: 22\n" +
      "Vaš izbor: "
    val option21 = "Rešavanje igre\n" +
      "Moguće opcije: 'u', 'd', 'l', 'r', '-', cifre od 1 do 9, '*' za proveru, 'm' za ucitavanje poteza iz fajla, 'q' za izlaz: "
    val option22 = "Editovanje tabele\n" +
      "Moguće opcije: 'u', 'd', 'l', 'r', '-', cifre od 1 do 9, 'm' za ucitavanje poteza iz fajla, 't' za transpoziciju, 'e' za zamenu cifara, 'q' za izlaz: "


    while (state > 0) {
      cls()
      state match {
        case 0 => System.exit(0)
        case 10 =>
          print(option10)
          readState()
        case 1 =>
          print(option1)
          val tableName = scala.io.StdIn.readLine()
          tables = tables ::: List(FileReader.readTableFromFile("input/" + tableName + ".tbl"))
          println("Tabela " + tableName + " uspešno učitana, povratak u početni meni")
          //          Thread.sleep(1000)
          state = 10
        case 2 =>
          if (tables.isEmpty) {
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
        case 3 =>
          println("Unesite sekvence, moguće opcije: 'u', 'd', 'l', 'r', '-', cifre od 1 do 9, 't' za transpoziciju, 'e' za zamenu cifara, 'q' za izlaz: ")
          val moves = new StringBuilder()
          var move = scala.io.StdIn.readLine()
          while (move != "q") {
            moves.append(move + "\n")
            move = scala.io.StdIn.readLine()
          }
          print("Unesite ime sekvence poteza: ")
          val fileName = scala.io.StdIn.readLine()
          new PrintWriter("input/" + fileName + ".mvs") {
            write(moves.toString())
            close()
          }
          state = 10

        case 20 =>
          println(currentTable.toString)
          println(option20)
          readState()
        case 21 =>
          var notSolved = true
          while (notSolved) {
            notSolved = !(currentTable.isSolved())
            cls()
            println(option21)
            println(currentTable.toString)
            val c = scala.io.StdIn.readLine()
            if (c.charAt(0) == '1' ||
              c.charAt(0) == '2' ||
              c.charAt(0) == '3' ||
              c.charAt(0) == '4' ||
              c.charAt(0) == '5' ||
              c.charAt(0) == '6' ||
              c.charAt(0) == '7' ||
              c.charAt(0) == '8' ||
              c.charAt(0) == '9') {
              currentTable = currentTable.writeField(currentTable.pen.x, currentTable.pen.y, c.charAt(0))
            }
            else {
              c match {
                case "u" => currentTable = new Table(currentTable.content, currentTable.pen.goUp())
                case "d" => currentTable = new Table(currentTable.content, currentTable.pen.goDown())
                case "l" => currentTable = new Table(currentTable.content, currentTable.pen.goLeft())
                case "r" => currentTable = new Table(currentTable.content, currentTable.pen.goRight())
                case "-" =>
                  currentTable = currentTable.writeField(currentTable.pen.x, currentTable.pen.y, c.charAt(0))
                case "q" =>
                  state = 10
                  notSolved = false
                case "*" =>
                  if (currentTable.isSolved()) {
                    println("Sudoku je resen!")
                  }
                  else {
                    println("Sudoku nije resen!")
                  }
                case "m" =>
                  print("Unesite ime fajla iz koje se citaju potezi: ")
                  val fileName = scala.io.StdIn.readLine()
                  currentTable = currentTable.executeMoves("input/" + fileName + ".mvs")
                case _ =>
                  currentTable = currentTable.executeMoves("input/" + c + ".mvs")
              }
              if (currentTable.isSolved()) notSolved = false
              //            println(currentTable.isSolved())
            }
          }
          if (currentTable.isSolved()) {
            println(currentTable.toString)
            println("Sudoku je resen!")
          }
          println("Povratak u glavni meni")

        case 22 =>
          var notExit = true
          while (notExit) {
            cls()
            println(option22)
            println(currentTable.toString)
            val c = scala.io.StdIn.readLine()
            if (c.charAt(0) == '1' ||
              c.charAt(0) == '2' ||
              c.charAt(0) == '3' ||
              c.charAt(0) == '4' ||
              c.charAt(0) == '5' ||
              c.charAt(0) == '6' ||
              c.charAt(0) == '7' ||
              c.charAt(0) == '8' ||
              c.charAt(0) == '9') {
              currentTable = currentTable.writeField(currentTable.pen.x, currentTable.pen.y, c.charAt(0), _overrideValue = true)
            }
            else {
              c match {
                case "u" => currentTable = new Table(currentTable.content, currentTable.pen.goUp())
                case "d" => currentTable = new Table(currentTable.content, currentTable.pen.goDown())
                case "l" => currentTable = new Table(currentTable.content, currentTable.pen.goLeft())
                case "r" => currentTable = new Table(currentTable.content, currentTable.pen.goRight())
                case "-" =>
                  currentTable = currentTable.writeField(currentTable.pen.x, currentTable.pen.y, c.charAt(0), _overrideValue = true)
                case "q" =>
                  print("Unesite ime fajla za cuvanje ")
                  val fileName = scala.io.StdIn.readLine()
                  currentTable.saveToFile("input/" + fileName + ".tbl")
                  print("Fajl uspesno sacuvan!\n" +
                    "Povratak u glalvni meni")
                  state = 10
                  notExit = false
                case "*" =>
                  currentTable = currentTable.filterSubMatrix()
                case "+" =>
                  currentTable = currentTable.filterColumnAndRow()
                case "t" =>
                  currentTable = currentTable.transpose()
                case "e" =>
                  currentTable = currentTable.exchange()
                case "m" =>
                  print("Unesite ime fajla iz koje se citaju potezi: ")
                  val fileName = scala.io.StdIn.readLine()
                  currentTable = currentTable.executeMoves("input/" + fileName + ".mvs")
                case _ =>
                  currentTable = currentTable.executeMoves("input/" + c + ".mvs")
              }
            }
          }
      }

    }
    state = 10

  }
}

