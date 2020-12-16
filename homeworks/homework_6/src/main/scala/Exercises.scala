object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = seq.reverse

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = {
    @scala.annotation.tailrec
    def fibFcn(n: Int, acc1: Int, acc2: Int): Int = n match {
      case 0 => acc1
      case 1 => acc2
      case _ => fibFcn(n - 1, acc2, acc1 + acc2)
        }
    fibFcn(idx, 0 , 1);
  }
  def fibonacci(idx: Int): Seq[Int] = if(idx < 0) Seq(0) else  for( i <- 0 to idx) yield fibonacci4Index(i)

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
                       "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
                       "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
                       "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
                       "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = text.toUpperCase.split("")
    .map( x => if(!MORSE.contains(x)) x else MORSE(x))
    .mkString(" ")


  def wordReverse(text: String): String = text.split("(?=[.!, ])|(?<=[.!, ])").map( x => {
    if(x(0).isUpper) {
      val temp = reverse(x.toLowerCase()).toArray
      temp(0) = temp(0).toUpper
      temp.mkString("")
    }
    else
      reverse(x)
  }).mkString("")

}
