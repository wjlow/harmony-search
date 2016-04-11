object GregorianChantFitness {

  type Score = Int

  private val penalties: (Seq[Int], Seq[Int]) => Score =
    (ms, hs) => {
      val p1 = harmonyLessOrEqualMelody(ms, hs)
      val p2 = consecutive(ms, hs)
      val p3 = startAndEnd(ms, hs)

      ((p1, p2, p3).zipped map (_ + _ + _)).sum
    }

  private val dropHeadAndLast: Seq[Int] => Seq[Int] =
    xs => xs.drop(1).dropRight(1)


  private val harmonyLessOrEqualMelody: (Seq[Int], Seq[Int]) => Seq[Score] =
    (ms, hs) => {
      val penalty = 5
      ms zip hs map {
        case (m, h) => if (!(h <= m)) penalty else 0
      }
    }

  private val consecutive: (Seq[Int], Seq[Int]) => Seq[Score] =
    (ms, hs) => {
      val penalty = 3
      val toIntervals: Seq[Int] => Seq[Int] =
        xs => xs zip xs.tail map {
          case (a, b) => b - a
        }
      toIntervals(ms) zip toIntervals(hs) map {
        case (m, h) => if (m > h) penalty else 0
      }
    }

  private val startAndEnd: (Seq[Int], Seq[Int]) => Seq[Score] =
    (ms, hs) => {
      val penalty = 0
      val start = if (ms.head != hs.head) penalty else 0
      val mid = dropHeadAndLast(ms) map (_ => 0)
      val end = if (ms.last != hs.last) penalty else 0
      start +: mid :+ end
    }

  private val rankAll: (Seq[Int], Seq[Int]) => Score =
    (ms, hs) => ((ms, hs).zipped map rank).sum

  private val rank: (Int, Int) => Score =
    (m, h) =>
      m - h match {
        case 5 => 1
        case 7 => 2
        case 0 => 3
        case 12 => 3
        case 4 => 4
        case 9 => 4
        case 2 => 5
        case 10 => 5
        case _ => 3
      }

  val compute: (Seq[Int], Seq[Int]) => Score =
    (ms, hs) => rankAll(ms, hs) + penalties(ms, hs)

}
