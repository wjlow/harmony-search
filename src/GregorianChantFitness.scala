object GregorianChantFitness {

  val penalties: (Seq[Int], Seq[Int]) => Int =
    (ms, hs) => {
      val p1 = harmonyLessOrEqualMelody(ms, hs)
      val p2 = consecutive(ms, hs)
      val p3 = startAndEnd(ms, hs)

      ((p1, p2, p3).zipped map (_ + _ + _)).sum
    }

  val dropHeadAndLast: Seq[Int] => Seq[Int] =
    xs => xs.drop(1).dropRight(1)

  val harmonyLessOrEqualMelody: (Seq[Int], Seq[Int]) => Seq[Int] =
    (ms, hs) =>
      ms zip hs map {
        case (m, h) => if (!(h <= m)) 3 else 0
      }

  val consecutive: (Seq[Int], Seq[Int]) => Seq[Int] =
    (ms, hs) => {
      val toIntervals: Seq[Int] => Seq[Int] =
        xs => xs zip xs.tail map {
          case (a, b) => b - a
        }
      toIntervals(ms) zip toIntervals(hs) map {
        case (m, h) => if (m > h) 3 else 0
      }
    }

  val startAndEnd: (Seq[Int], Seq[Int]) => Seq[Int] =
    (ms, hs) => {
      val start = if (ms.head - hs.head != 0) 3 else 0
      val mid = dropHeadAndLast(ms) map (_ => 0)
      val end = if (ms.last - hs.last != 0) 3 else 0
      start +: mid :+ end
    }

  val rankAll: (Seq[Int], Seq[Int]) => Int =
    (ms, hs) => ((ms, hs).zipped map rank).sum

  val rank: (Int, Int) => Int =
    (m, h) =>
      m - h match {
        case 5 => 1
        case 7 => 2
        case 0 => 3
        case 12 => 3
        case 4 => 4
        case 9 => 6
        case 2 => 5
        case 10 => 5
        case _ => 3
      }

  val compute: (Seq[Int], Seq[Int]) => Int =
    (ms, hs) => rankAll(ms, hs) + penalties(ms, hs)

}
