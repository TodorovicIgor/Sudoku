class Field(val x: Int, val y: Int, val v: String = "-", val isOriginal: Boolean = false) extends Position {
  require(v.equals("-") || v.toInt<10 && v.toInt>0)
  def getVal: String = {
    v match {
      case "-" => "-"
      case _ => v
    }
  }
  def writeVal(v: String): Field = {
    if (isOriginal) {
      this
    } else {
      new Field(x = this.x, y = this.y, v = v, isOriginal = this.isOriginal)
    }
  }
  override def toString: String = {
    getVal.toString
  }
}

//object test {
//  def main(args: Array[String]): Unit = {
//    val nonMutableField: Field = new Field(0, 0, "3", isOriginal = true)
//    assert(nonMutableField.getVal == 3)
//    val nonMutatedField: Field = nonMutableField.writeVal("0")
//    assert(nonMutatedField.getVal == 3)
//    assert(nonMutableField == nonMutatedField)
//    val mutableField: Field = new Field(0, 0, isOriginal = false)
//    assert(mutableField.getVal == "-")
//    val mutatedField: Field = mutableField.writeVal("9")
//    assert(mutatedField.getVal == 9)
//
//    print("passed")
//  }
//}
