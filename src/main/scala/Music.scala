// Cloned from https://github.com/wjlow/harmony-korine for convenience

case class Note(pitchClass: String, octave: Int)

object Music {

  val scales = Map(
    "major" -> List(2, 2, 1, 2, 2, 2, 1),
    "minor" -> List(2, 1, 2, 2, 1, 2, 2)
  )

  def harmony(note: String, octave: Int, scale: String, degree: Int): Note = {
    val notesInScale: Seq[Note] = generateScale(scales.get(scale).get, Note(note, octave))
    notesInScale(degree - 1)
  }

  def generateScale(scaleSteps: Seq[Int], root: Note): Seq[Note] = {
    scaleSteps.scanLeft(absPitch(root))(_ + _).map(abs => pitch(abs))
  }

  def transpose(note: Note, step: Int): Note = {
    pitch(absPitch(note) + step)
  }

  def absPitch(note: Note): Int = {
    12 * note.octave + pitchClassToInt(note.pitchClass)
  }

  def pitchClassToInt(pitchClass: String): Int = {
    pitchClassIntMap(pitchClass)
  }

  val pitchClassIntMap = Map(
    "C" -> 0,
    "Cs" -> 1,
    "D" -> 2,
    "Ds" -> 3,
    "E" -> 4,
    "F" -> 5,
    "Fs" -> 6,
    "G" -> 7,
    "Gs" -> 8,
    "A" -> 9,
    "As" -> 10,
    "B" -> 11,
    "C+" -> 12
  )

  def pitch(absPitch: Int): Note = {
    val pitch = absPitch % 12
    val octave = absPitch / 12
    Note(List("C", "Cs", "D", "Ds", "E", "F", "Fs", "G", "Gs", "A", "As", "B")(pitch), octave)
  }

}