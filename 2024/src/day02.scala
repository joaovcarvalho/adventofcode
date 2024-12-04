import scala.io.Source._
import scala.compiletime.ops.boolean

object Day02Params {
    val InputTextFile = "day02.txt"
    val Separator = " "
}

def areLevelsSafe(levels: List[Int]): Boolean =
    val differences = levels.zipWithIndex.dropRight(1).map { case (level, i) => level - levels(i + 1)}
    val isSafe = differences.map { diff => math.abs(diff) > 0 && math.abs(diff) < 4 }.reduce(_ && _) && (differences.forall(_ > 0) || differences.forall(_ < 0))
    isSafe
end areLevelsSafe

class Report(val levels: List[Int]) {
  def isSafe(): Boolean = 
    var isSafe = areLevelsSafe(levels)

    // If report is unsafe, we can try removing one level and chcecking again
    if (!isSafe) {
      var removedIndex = 0
      while(!isSafe && removedIndex <= levels.length - 1) {
        val newLevels = levels.zipWithIndex.filter { (level, i) => i != removedIndex }.map(_._1)
        val newLevelsSafe = areLevelsSafe(newLevels)
        if (newLevelsSafe) {
          isSafe = newLevelsSafe
        }

        removedIndex = removedIndex + 1
      }
    }

    isSafe
  end isSafe

  override def toString(): String = 
    s"Report levels: $levels - Is it safe? ${isSafe()}"
}

@main
def hello(): Unit =
  val lines = fromFile(Day02Params.InputTextFile).getLines
  val levelsLists = lines.map(
    _.split(Day02Params.Separator).map(_.trim.toInt).toList
  ).toList

  val reports = levelsLists
    .map { levels => Report(levels=levels)}

  reports.map(println)

  val (safe, unsafe) = reports
    .partition(_.isSafe())

  val howManySafeReports = safe.length

  println(s"Safe reports: $howManySafeReports")