package katas

object Level_8 {
  object WolfInSheepsClothing {
    val title = "A Wolf in Sheeps Clothing"
    val description = "Return the sheep 'behind' the wolf in the array, if the wolf is 'first' return a different string"
    val difficulty = 8

    object MySolution {
      val wolfString = "wolf"
      val goAway = "Pls go away and stop eating my sheep"

      def sheepN(n: Int): String = s"Oi! Sheep number $n! You are about to be eaten by a wolf!"

      def warnTheSheepCheck(queue: Array[String]): String =
        if (queue.last == wolfString) return goAway
        sheepN(queue.reverse.indexOf(wolfString))
    }
    // An early return using queue.last with an If/Else

    object IdealSolution {
      def warnTheSheepCheck(queue: Array[String]): String = {
        queue.reverse.indexOf("wolf") match {
          case 0 => "Pls go away and stop eating my sheep"
          case i => s"Oi! Sheep number ${i}! You are about to be eaten by a wolf!"
        }
      }
    }
  }
  // pattern matching <3, just gorgeous use of scala
}
