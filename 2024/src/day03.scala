import scala.io.Source._
import scala.util.matching.Regex

val operationPattern: Regex = raw"(?<operator>mul|don\'t|do)\(([0-9]*),*([0-9]*)\)".r

object Day03Params {
    val InputTextFile = "day03.txt"
}

enum OperationType:
    case Mul, Do, Dont

case class Operation(operator: OperationType, args: List[Integer]) {
    def result(): Int = {
        if(operator == OperationType.Mul){
            args.reduce(_ * _)
        } else {
            throw Error("Operation does not support result")
        }
    }
}

@main
def day03(): Unit =
    val inputText = fromFile(Day03Params.InputTextFile).getLines().reduce(_ ++ _)
    val matches = operationPattern.findAllIn(inputText).matchData
    val operations = matches.map { m  => m.group("operator") match {
        case "mul" => Operation(operator = OperationType.Mul, args=List(m.group(2).toInt, m.group(3).toInt))
        case "do" => Operation(operator = OperationType.Do, args=List())
        case "don\'t" => Operation(operator = OperationType.Dont, args=List())
    }}.toList

    var mulEnabled = true
    var total = 0
    for (op <- operations) {
        val operationIsMul = op.operator == OperationType.Mul
        if (mulEnabled && operationIsMul) {
            total = total + op.result()
        }

        if (op.operator == OperationType.Do) {
            mulEnabled = true
        }

        if (op.operator == OperationType.Dont) {
            mulEnabled = false
        }
    }
    println(s"Result ${total}")