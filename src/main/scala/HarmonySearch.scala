import scala.util.Random

object HarmonySearch {

   case class HarmonyVector(harmony: Seq[Int], fitness: Int)

   val hmcr = 0.9

   val melody: Seq[Int] = Seq("C", "D", "E", "F", "G", "G", "A", "A", "G", "F", "D", "E", "G", "G", "A", "B", "C+", "C+", "B", "G", "B", "A", "G", "A", "G", "F", "D", "E") map Music.pitchClassToInt

   def randomVector: HarmonyVector = {
     val harmony = Seq.fill(melody.size)(Random.nextInt(13))
     HarmonyVector(harmony, GregorianChantFitness.compute(melody, harmony))
   }

   def generateMutant(hm: Seq[HarmonyVector]): HarmonyVector = {
       var mutant: Seq[Int] = Nil
       for (i <- melody.indices) {
         val randomlyChosenHarmony = hm(Random.nextInt(hm.size))
         val pitchShiftedNote = randomlyChosenHarmony.harmony(i) + Random.shuffle(Seq(-1, 1)).head
         mutant :+= pitchShiftedNote
       }
       HarmonyVector(mutant, GregorianChantFitness.compute(melody, mutant))
     }

   case class BestHarmonyVector(harmony: Seq[String], fitness: Int)

   val search: (Int, Int) => BestHarmonyVector =
     (iterations, hms) => {
       val generateInitialHm: Int => Seq[HarmonyVector] = hms => Seq.fill(hms)(randomVector)

       var hm = generateInitialHm(hms)
       for (i <- 0 until iterations) {
         val newVector = generateMutant(hm)
         val worst = hm maxBy (v => v.fitness)
         if (newVector.fitness < worst.fitness) {
           hm = newVector +: hm.filterNot(vector => vector == worst)
         } else hm
       }
       val best: HarmonyVector = hm minBy (v => v.fitness)
       val bestReadable = best.harmony map (note => {
         val swapped = Music.pitchClassIntMap map (_.swap)
         swapped(note)
       })
       BestHarmonyVector(bestReadable, best.fitness)
     }

 }
