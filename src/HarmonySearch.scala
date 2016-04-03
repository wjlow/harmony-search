import scala.util.Random

object HarmonySearch {

  case class HarmonyVector(harmony: Seq[Int], fitness: Int)

  val melody: Seq[Int] = Seq("C", "D", "E", "F", "G", "G", "A", "A", "G", "F", "D", "E", "G", "G", "A", "B", "C+", "C+", "B", "G", "B", "A", "G", "A", "G", "F", "D", "E") map (Music.pitchClassToInt(_))

  def randomVector: HarmonyVector = {
    val harmony = Seq.fill(melody.size)(Random.nextInt(13))
    HarmonyVector(harmony, GregorianChantFitness.compute(melody, harmony))
  }

  val generateHm: Int => Seq[HarmonyVector] =
    hms => {
      Seq.fill(hms)(randomVector)
    }

  val search: (Int, Int) => HarmonyVector =
    (iterations, hms) => {
      var hm = generateHm(hms)
      for (i <- 0 until iterations) {
        val newVector = randomVector
        val worst = hm maxBy (v => v.fitness)
        if (newVector.fitness < worst.fitness) {
          hm = newVector +: hm.filterNot(vector => vector == worst)
        } else hm
      }
      hm minBy (v => v.fitness)
    }

}
