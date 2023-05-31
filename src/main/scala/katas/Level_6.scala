package katas

import scala.collection.immutable.HashMap

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

  object Tribonacci {
    val title = "Tribonacci"
    val description = "Fibonacci, but with the last 3 not last 2"
    val difficulty = 6

//    object MySolution {
      //    def tribonacci[T: Numeric](signature: List[T], n: Int): List[T] =
      //      if (n == 0) return List()
      //      signature match {
      //        case sig if sig.last == 0 => sig.slice(0, n)
      //        case sig => List[T] sig tailRecTrib(n)
      //    }
      //    def tailRecTrib[T: Numeric](i: Int, memo: scala.collection.mutable.Map[Int, T] = scala.collection.mutable.Map()): Int =  {
      //      if (memo.contains(i)) return memo(i)
      //      if (i <= 2) return 1
      //      memo(i) = tailRecTrib(i - 1, memo) + tailRecTrib(i - 2, memo) + tailRecTrib(i - 3, memo)
      //      memo(i)
      //    }
//    }

    object IdealSolution {
      def tribonacci[T: Numeric](signature: List[T], n: Int): List[T] =
        // dear god you got that wrong lewis
        if (n <= 3) signature.take(n) else signature.head +: tribonacci(signature.tail :+ signature.sum, n - 1)
    }

    object AnotherIdealSolution {
      def tribonacci[T: Numeric](signature: List[T], n: Int): List[T] = {
        def recur(result: List[T], acc: Int): List[T] = acc match {
          case 0 => result
          case _ => result.head :: recur(result.tail :+ result.sum, acc - 1)
        }
        recur(signature, n)
      }
    }
  }

  object PermuteAPalindrome {
    val title = "Permute a Palindrome"
    val description = "Given a list find of chars, determine where a palindrome can be made from those chars"
    val difficulty = 6

    object MySolution {
      // cute but it doesnt work does it
      //      def permutePalindrome(input: String): Boolean =
      //      // fold over input, if acc.contains(char) add 1 to value, else add the (char,1) pair
      //        input.foldLeft(Map.empty[Char, Int])((acc, char) => char match {
      //          case (acc, c) if acc.contains(c) => acc + (c -> acc(char) += 1)
      //          case (acc, c) => acc + (c -> 1)
      //        }).map { case (key, value) => (key, value % 2 == 0) }.values.count(_ == false)
      //    }
      def permutePalindrome(input: String): Boolean =
        input.foldLeft(Map.empty[Char, Int]) { (map, char) => {
          map + (char -> (map.getOrElse(char, 0) + 1))
        }}.values.count(_ % 2 == 1) < 2
    }

    object IdealSolution {
      def permutePalindrome(input: String): Boolean =
        // same theory but using groupBy - identity is some magic... needs some research
        input.groupBy(identity).count(_._2.length % 2 == 1) < 2
    }
  }
}
