package rcrs.nosolver

import scala.annotation.tailrec

object GroupGenerator {

  /**
    * Generates List containing all possible "partitions" of passed List toProcess.
    * "Partitions" (Lists containing elements of type T) must be at least minItems and at most maxItems long.
    */
  def generate[T](minItems: Int, maxItems: Int)(toProcess: Iterable[T]): Iterable[List[List[T]]] = {
    def go(l: List[List[List[T]]], item: T): List[List[List[T]]] = {

      // From given list with n items generates n lists, so that first list has
      // item prepended to first sublist, second list to the second sublist etc.
      //
      // List(Nil,List(1)), 2 -> List(List(2), List(1)), List(Nil, List(2, 1))
      def genListsWithItemAdded(item: T)(l: List[List[T]]): List[List[List[T]]] = {
        @tailrec
        def goRec(init: List[List[T]], rest: List[List[T]], acc: List[List[List[T]]]): List[List[List[T]]] = {
          rest match {
            case Nil =>
              acc
            case h :: t =>
              val newList: List[List[T]] = init ++ List(item :: h) ++ t
              goRec(init ++ List(h), t, newList :: acc)
          }
        }

        goRec(Nil, l, Nil)
      }

      val res = l ++
        l.map(List(item) :: _) ++
        l.flatMap(genListsWithItemAdded(item))

      // filter partial result make subsequent steps faster
      res.filter(_.forall(_.size <= maxItems))
    }

    // TODO - generate lazy list (Stream) instead of List?
    toProcess.foldLeft(List(List(Nil: List[T])))(go).filter(_.forall(_.size >= minItems))
  }

  type Part[T] = List[T]
  type PartWithTgt[T,U] = (List[T], Option[U])

  // zip groups with fire permutations
  // - to each group add all fire permutations, produce stream List of tuples group-fire
  def zipWithPermutations[T,U](groupList: Iterable[List[List[T]]])(permList: List[U]): Iterable[List[(List[T], U)]] = {

    def genPermutations(l: List[U])(n: Int): List[List[Option[U]]] = {
      // TODO - memoize?

      val toAdd = n - l.size

      // handle situation when permList.size <= n - add Nones to have n elements
      val perms = (l.map(Some(_)) ++ List.fill(toAdd)(None)).permutations

      // handle situtation when perms.length > n - trim
      perms.map(_.take(n)).toSet.toList
    }

    // with each group zip all permutations
    val groupsWithOptTargets = groupList.flatMap{g =>
      val perms = genPermutations(permList)(g.size)
      perms.map(p => g.zip(p))
    }

    // remove groups with None target
    // TODO - remove duplicates?
    groupsWithOptTargets.map(_.collect{ case (l, Some(t)) => (l, t) })
  }


  /*
  def main(args: Array[String]) {
    val list = (1 to 5).toList
    generate(2, 3)(list).foreach(println(_))
    println("--------------")

    val groups = generate(2, 3)(list)
    val perms = List('a')

    zipWithPermutations(groups)(perms).foreach(println(_))
  }
  */

}
