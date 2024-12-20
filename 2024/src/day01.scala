import scala.io.Source._

object Day01Params {
    val InputTextFile = "day01.txt"
    val Separator = "  "
}

def getListByIndex(index: Int, groupedBy: Map[Int, List[(Int, Int)]]) = {
    groupedBy.get(index).match {
        case None => List()
        case Some(value) => value.map(_._1)
    }.sorted 
}

@main
def day01(): Unit =
  val lines = fromFile(Day01Params.InputTextFile).getLines
  val values = lines.map(
    _.split(Day01Params.Separator).map(_.trim.toInt).zipWithIndex
  ).toList

  val groupedBy = values.flatten.groupBy(_._2)

  val leftList = getListByIndex(0, groupedBy)
  val rightList = getListByIndex(1, groupedBy)

  assert(rightList.length == leftList.length)

  val differences = leftList.zip(rightList).map{ elem => math.abs(elem._1 - elem._2) }
  val sumOfDifferences = differences.sum()
  println(s"Sum of differences: $sumOfDifferences")

  val frequencies = rightList.groupBy{ elem => elem }.view.mapValues { values => values.length }

  val similarities = leftList.map { elem => frequencies.getOrElse(elem, 0) * elem }
  val sumOfSimilarities = similarities.sum()
  println(s"Sum of similarities: $sumOfSimilarities")