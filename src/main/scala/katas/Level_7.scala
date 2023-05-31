package katas

import scala.annotation.tailrec

object Level_7 {
  object ReverseWords {
    val title = "Reverse Words"
    val description = "\"This is an example!\" ==> \"sihT si na !elpmaxe\""
    val difficulty = 7

    object MySolution {
      def reverseWords(text: String): String = text.split(" ").map(_.reverse).mkString(" ")
    }

    object IdealSolution {
      def reverseWords(text: String): String = text.split(" ").map(_.reverse).mkString(" ")
    }
    // Good job - perfect best practice
  }

  object Mumbling {
    val title = "Write the Acc function"
    val description = "accum(abcd) => A-Bb-Ccc-Dddd"
    val difficulty = 7

    object MySolution {
      def accum(s: String): String = {
        s.split("").zipWithIndex.map {
          case (x, i) => s"${x.toUpperCase}${x.toLowerCase * i}"
        }.mkString("-")
      }
    }

    object IdealSolution {
      def accum(s: String): String = {
        s.zipWithIndex.map {
          case (c, i) => c.toUpper + c.toLower.toString * i
        }.mkString("-")
      }
    }
    // Good job - perfect best practice
  }

  object GetTheMiddleChar {
    val title = "Get the middle char (or two if len is even)"
    val description = "GetMiddle(test) => es; GetMiddle(testing) => t;"
    val difficulty = 7

    object MySolution {
      def middle(s: String): String =
        if (s.length == 1) return s
        val split = s.split("")
        val mid: Int = 0 + ((s.length) - 1 - 0) / 2
        if (split.length % 2 != 0) s(mid)
        s"${s((s.length / 2) - 1)}${s(s.length / 2)}"
    }
    // Not so good, doesn't actually work well at all

    object IdealSolution {
      @tailrec
      def middle(string: String): String =
        if (string.length <= 2) string
        else middle(string drop 1 dropRight 1)
    }
    // Recursion!!!
    //    - trim the lefts+rights until nothing left but the mid
    //    - Much nicer than my solution

    object NotableThirdSolution {
      def middle(s: String): String = s match {
        case str if str.length == 1 => str
        case str if str.length % 2 != 0 => str((s.length - 1) / 2).toString
        case str => str.slice((s.length - 1) / 2, (s.length - 1) / 2 + 2)
      }
    }
    // Beautiful use of pattern matching to cover all cases
  }

  object ReverseAndCleanWord {
    val title = "Reverse and remove non-alpha-num chars"
    val description = "ultr53o?n => nortlu"
    val difficulty = 7

    object MySolution {
      def reverseLetter(str: String): String = {
        val regex = "([a-zA-Z])"
        str.split("").filter(_.matches(regex)).reverse.mkString("")
      }
    }
    // Not bad - but there is a more concise way to do this

    object IdealSolution {
      def reverseLetter(str: String): String = str.filter(_.isLetter).reverse
    }
    // Damn simple... String is an iterator in scala - use it like one
  }

  object FilteredList {
    val title = "Create new list from mixed list"
    val description = "List(1, 2, 'a', 'b', 3) => List(1, 2, 3)"
    val difficulty = 7

    object MySolution {
      //      def filterList(list: List[Any]): List[Int] = List(list.filter {
      //        case anInt: Int => Some()
      //        case _ => ()
      //      }) // attempted to do what I have done below

      def filterListIntended(items: List[Any]): List[Int] = items.flatMap {
        case i: Int => Some(i)
        case _ => None
      } // we use flatmap here (not map) as we want the Int not Some(Int) [so we flatten it with flatmap]
      // we also use "flatMap { case... }" instead of "flatMap { _ match { case... } }" for brevity
    }

    object IdealSolution {
      def filterList(list: List[Any]): List[Int] = list.collect { case x: Int => x }
    }
    // collect() applies a partial function to each element and returns elements that satisfy the partial.
  }

  object MaximumLengthDifference {
    val title = "Maximum Length Difference"
    val description = "Find biggest difference in length of items in each list"
    val difficulty = 7

    object MySolution {
      def mxdiflg(a1: List[String], a2: List[String]): Int =
        if (a1.isEmpty || a2.isEmpty) return -1
        val lenA1 = a1.map(_.length)
        val lenA2 = a2.map(_.length)
        math.max(lenA1.max - lenA2.min, lenA2.max - lenA1.min)
      // map to lengths, find the biggest of the diff from mins+maxs of a1+a2
    }

    object IdealSolution {
      def mxdiflg(a1: List[String], a2: List[String]): Int =
        (for {
          x <- a1
          y <- a2
        } yield (y.length - x.length).abs) match {
          case Nil => -1
          case l => l.max
        }
      // for comprehension (for _ in a1, for _ in a2) yielding abs of(all x.lengths - all y.lengths)
      // finally, return -1 when for comp fails to return a number, and the biggest item in the list if an 'l' is present

      def mxdiflg2(a1: List[String], a2: List[String]): Int =
        if (a1.isEmpty || a2.isEmpty) -1
        else a1.flatMap(x =>
          a2.map(y =>
            math.abs(x.length - y.length)
          )
        ).max
      // Early return, nested mapping - we make a new list using the values of all diff(xs and ys) and return the max
    }
  }

  object DontGiveMeFive {
    val title = "Dont give me Five"
    val description = "1,9 -> 1,2,3,4,5,6,7,8,9 => result = 8; (9 nums - the 5)"
    val difficulty = 7

    object MySolution {
      def dontGiveMeFive(start: Int, end: Int): Int = {
        Range(start, end + 1, 1).map(_.toString).count(!_.contains("5"))
      }
      // using range, map to string, filter + count using contains
    }

    object IdealSolution {
      def dontGiveMeFive(start: Int, end: Int): Int = {
        (start to end).count(!_.toString.contains("5"))
      }
    }
    // using <Start> to <End> range syntax, convert during contains check as opposed to before
  }

  object SeriesSum {
    val title = "Sum of the first nth series"
    val description: String = "2 --> 1 + 1/4 --> '1.25'"
    val difficulty = 7

    object MySolution {
      def seriesSum(n: Int): String = n match {
        case 0 => "0.00"
        case 1 => "1.00"
        case sum => "%.2f".format(reoccurSum(sum))
      }

      def reoccurSum(n: Int): Double = n match {
        case 0 => 0.0
        case _ => 1.0 / (3.0 * (n - 1) + 1) + reoccurSum(n - 1)
      }
    } // Not a horrible solution, readable - but a bit clumsy

    object IdealSolution {
      def seriesSum(n: Int): String = {
        "%1.2f".format(
          (0 until n).foldLeft(0.0)((acc, x) =>
            acc + 1.0 / (1.0 + (x * 3.0)))
        )
      }
    } // effective and sexy - but seriously split it into more functions
  }

  object SumOfLowestTwo {
    val title = "Sum of the lowest two"
    val description: String = "two lists, find the smallest sum of two ints"
    val difficulty = 7

    object MySolution {
      def sumSmallestTwo(numbers: List[Int]): Int =
        numbers.sorted.slice(0, 2).sum
    } // bang on, but take exists; slice(0, n) == take(n)

    object IdealSolution {
      def sumTwoSmallest(numbers: List[Int]): Int =
        numbers.sorted.take(2).sum
    }
  }

  object NumberSort {
    val title = "Number Sort"
    val description: String = "Sort the numbers in ascending order, handle nulls with an empty array"
    val difficulty = 7

    object MySolution {
      // nums.sorted handles nums.empty, cant pass Nil in due to List[Int] type restriction
      def sol(nums: List[Int]): List[Int] = nums.sorted
    }
  }

  object BinaryAddition {
    val title = "Binary Addition"
    val description: String = "Add two numbers and display the new value in binary."
    val difficulty = 7

    object MySolution {
      def addBinary(a: Int, b: Int): String = (a + b).toBinaryString
    }
    
    object IdealSolution /* possibly? */ {
      def addBinary(a: Int, b: Int): String =
        Integer.toString(a + b, 2)
    }
  }
}
