package katas

object Level_6 {
  object DeleteMultiplesBeyondN {
    val title = "Delete occurrences of an element if it occurs more than n times"
    val description = "input(List(1,2,3,1,2,1,2,3), 2) => List(1,2,3,1,2,3); input(List(20, 37, 20, 21), 1) => List(20, 37, 21);"
    val difficulty = 6

    object MySolution {
//      def deleteNth(elements: List[Int], maxOccurrences: Int): List[Int] = {
//        // We cannot use flatMap here as we need to access the new Array as it is created
//        elements.flatMap {
//          case (i: Int, index: Int, arr: List[Option[Int]]) if arr.count(_.get == i) < maxOccurrences => Some(i)
//          case _ => None
//        }
//      } // doesnt work
    }

    object IdealSolution {
      def deleteNth(elements: List[Int], max: Int): List[Int] = {
        elements.foldLeft(List[Int]()) {
          case (acc, cur) if acc.count(_ == cur) < max => cur :: acc
          case (acc, _) => acc
        }.reverse
      }
      // foldLeft = reduce; Meaning we can actually access the list during creation
    }
  }

  object Deadfish {
    val title = "Make the Deadfish Swim"
    val description = "parse + execute the commands and return the result"
    val difficulty = 6

    object MySolution {
      def parse(data: String): List[Int] =
      //   foldLeft[types]       (initialValues)(function)
        data.foldLeft[(Int, List[Int])]((0, Nil))((acc, char) => char match {
          // case  => ._1 is the value, ._2 is the list
          case 'i' => (acc._1 + 1, acc._2)
          case 'd' => (acc._1 - 1, acc._2)
          case 's' => (acc._1 * acc._1, acc._2)
          case 'o' => (acc._1, acc._2 :+ acc._1)
          case _ => acc
        })._2 // we return the array at the end using ._2 (second index in the result tuple, the array)
    }
  }

  object ReverseRotate {
    val title = "Reverse Rotate"
    val description = "Read the page, its insane..."
    val difficulty = 6

    def revRot(strng: String, sz: Int): String = {
      if (sz <= 0 || strng.isEmpty || sz > strng.length) return ""
      strng.grouped(sz).toList.filter(_.length == sz).map(chunk => {
        if (isReverse(chunk)) chunk.reverse
        else chunk.slice(1, chunk.length) + chunk(0)
      }).mkString("")
    }

    def isReverse(chunk: String): Boolean = {
      chunk.split("").map(d => d.toInt * d.toInt * d.toInt).sum % 2 == 0
    }
  }
}
