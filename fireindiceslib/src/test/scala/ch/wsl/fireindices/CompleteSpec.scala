package ch.wsl.fireindices

import ch.wsl.fireindices.app.{FireCalculator, SimpleApp, Timer}
import ch.wsl.fireindices.functions.Utils
import ch.wsl.fireindices.model.FCRow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import scala.util.Try

class CompleteSpec extends AnyFlatSpec with Matchers {
  val simpleApp = new SimpleApp {}

  def readCsv(csv:Seq[String]):Seq[FCRow] = {
      val header = csv.head.split(",").toSeq
      val data = csv.tail.map(_.split(",").toSeq)

      data.map{ d =>
        FCRow(Utils.solarDate2Long(d.head),header.tail.zip(d.tail).map{ case (variable,value) => variable -> Try(value.toDouble).getOrElse(Double.NaN) }.toMap)
      }

  }

  "Complete" should "calulate incremental indexes" in {

    val header = Seq("DateYYYYMMDD","T","H","U","P")
    val rows = Seq(
        FCRow(Utils.solarDate2Long("19890901","yyyyMMdd"),Map("T" -> 17.63, "H" -> 58.2, "U" -> 1.041, "P" -> 0.02)),
        FCRow(Utils.solarDate2Long("19890902","yyyyMMdd"),Map("T" -> 15.89, "H" -> 74.4, "U" -> 1.287, "P" -> 0.61)),
        FCRow(Utils.solarDate2Long("19890903","yyyyMMdd"),Map("T" -> 14.74, "H" -> 63.8, "U" -> 2.719, "P" -> 0.79)),
        FCRow(Utils.solarDate2Long("19890904","yyyyMMdd"),Map("T" -> 14.4, "H" -> 61, "U" -> 3.843, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19890905","yyyyMMdd"),Map("T" -> 14.72, "H" -> 63.3, "U" -> 2.812, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19890906","yyyyMMdd"),Map("T" -> 14.83, "H" -> 64.3, "U" -> 1.692, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19890907","yyyyMMdd"),Map("T" -> 15.69, "H" -> 65.4, "U" -> 1.227, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19890908","yyyyMMdd"),Map("T" -> 17.01, "H" -> 61.5, "U" -> 0.807, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19890909","yyyyMMdd"),Map("T" -> 17.04, "H" -> 71.2, "U" -> 0.727, "P" -> 0.25)),
        FCRow(Utils.solarDate2Long("19890910","yyyyMMdd"),Map("T" -> 18.16, "H" -> 62, "U" -> 1.122, "P" -> 0.18)),
        FCRow(Utils.solarDate2Long("19890911","yyyyMMdd"),Map("T" -> 16.55, "H" -> 80.4, "U" -> 1.466, "P" -> 4.18)),
        FCRow(Utils.solarDate2Long("19890912","yyyyMMdd"),Map("T" -> 17.43, "H" -> 70.4, "U" -> 1.067, "P" -> 0.18)),
        FCRow(Utils.solarDate2Long("19890913","yyyyMMdd"),Map("T" -> 14.59, "H" -> 82, "U" -> 1.396, "P" -> 1.8)),
        FCRow(Utils.solarDate2Long("19890914","yyyyMMdd"),Map("T" -> 15.2, "H" -> 77.9, "U" -> 1.207, "P" -> 3.72)),
        FCRow(Utils.solarDate2Long("19890915","yyyyMMdd"),Map("T" -> 16.62, "H" -> 68.6, "U" -> 1.342, "P" -> 0.02)),
        FCRow(Utils.solarDate2Long("19890916","yyyyMMdd"),Map("T" -> 17.13, "H" -> 67.4, "U" -> 0.851, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19890917","yyyyMMdd"),Map("T" -> 18.52, "H" -> 65, "U" -> 1.377, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19890918","yyyyMMdd"),Map("T" -> 18.78, "H" -> 68.9, "U" -> 1.104, "P" -> 0.05)),
        FCRow(Utils.solarDate2Long("19890919","yyyyMMdd"),Map("T" -> 16.78, "H" -> 81.7, "U" -> 0.986, "P" -> 0.52)),
        FCRow(Utils.solarDate2Long("19890920","yyyyMMdd"),Map("T" -> 17.54, "H" -> 75.9, "U" -> 0.896, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19890921","yyyyMMdd"),Map("T" -> 18.89, "H" -> 70.7, "U" -> 0.695, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19890922","yyyyMMdd"),Map("T" -> 19.47, "H" -> 65.3, "U" -> 0.97, "P" -> 0.06)),
        FCRow(Utils.solarDate2Long("19890923","yyyyMMdd"),Map("T" -> 15.65, "H" -> 84.9, "U" -> 1.199, "P" -> 20.79)),
        FCRow(Utils.solarDate2Long("19890924","yyyyMMdd"),Map("T" -> 15.28, "H" -> 76.8, "U" -> 0.876, "P" -> 0.01)),
        FCRow(Utils.solarDate2Long("19890925","yyyyMMdd"),Map("T" -> 15.43, "H" -> 70.3, "U" -> 1.387, "P" -> 0.03)),
        FCRow(Utils.solarDate2Long("19890926","yyyyMMdd"),Map("T" -> 14.52, "H" -> 64.2, "U" -> 3.419, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19890927","yyyyMMdd"),Map("T" -> 14.31, "H" -> 67, "U" -> 1.436, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19890928","yyyyMMdd"),Map("T" -> 12.84, "H" -> 52.5, "U" -> 5.496, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19890929","yyyyMMdd"),Map("T" -> 12.44, "H" -> 63.7, "U" -> 6.735, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19890930","yyyyMMdd"),Map("T" -> 12.65, "H" -> 55.8, "U" -> 4.11, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891001","yyyyMMdd"),Map("T" -> 12.76, "H" -> 62.2, "U" -> 2.221, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891002","yyyyMMdd"),Map("T" -> 12.62, "H" -> 69.7, "U" -> 1.839, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891003","yyyyMMdd"),Map("T" -> 12.42, "H" -> 60.4, "U" -> 3.354, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891004","yyyyMMdd"),Map("T" -> 11.06, "H" -> 70, "U" -> 1.593, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891005","yyyyMMdd"),Map("T" -> 11.59, "H" -> 74.2, "U" -> 0.928, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891006","yyyyMMdd"),Map("T" -> 11.84, "H" -> 77, "U" -> 1.105, "P" -> 0.13)),
        FCRow(Utils.solarDate2Long("19891007","yyyyMMdd"),Map("T" -> 11.04, "H" -> 79.1, "U" -> 1.873, "P" -> 10.06)),
        FCRow(Utils.solarDate2Long("19891008","yyyyMMdd"),Map("T" -> 9.4, "H" -> 75.4, "U" -> 2.701, "P" -> 11.39)),
        FCRow(Utils.solarDate2Long("19891009","yyyyMMdd"),Map("T" -> 10.08, "H" -> 65.1, "U" -> 2.428, "P" -> 0.19)),
        FCRow(Utils.solarDate2Long("19891010","yyyyMMdd"),Map("T" -> 10.09, "H" -> 70, "U" -> 1.504, "P" -> 0.29)),
        FCRow(Utils.solarDate2Long("19891011","yyyyMMdd"),Map("T" -> 8.6, "H" -> 65.1, "U" -> 1.371, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891012","yyyyMMdd"),Map("T" -> 10.28, "H" -> 72, "U" -> 0.75, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891013","yyyyMMdd"),Map("T" -> 10.37, "H" -> 71.5, "U" -> 1.054, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891014","yyyyMMdd"),Map("T" -> 11.83, "H" -> 66.3, "U" -> 1.542, "P" -> 0.04)),
        FCRow(Utils.solarDate2Long("19891015","yyyyMMdd"),Map("T" -> 11.29, "H" -> 58.6, "U" -> 1.609, "P" -> 0.1)),
        FCRow(Utils.solarDate2Long("19891016","yyyyMMdd"),Map("T" -> 8.08, "H" -> 76.7, "U" -> 0.695, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891017","yyyyMMdd"),Map("T" -> 8.48, "H" -> 77.1, "U" -> 0.729, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891018","yyyyMMdd"),Map("T" -> 9.9, "H" -> 76.5, "U" -> 0.932, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891019","yyyyMMdd"),Map("T" -> 12.29, "H" -> 69.7, "U" -> 1.288, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891020","yyyyMMdd"),Map("T" -> 12.62, "H" -> 72.9, "U" -> 1.803, "P" -> 0.21)),
        FCRow(Utils.solarDate2Long("19891021","yyyyMMdd"),Map("T" -> 16.31, "H" -> 59.9, "U" -> 1.961, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891022","yyyyMMdd"),Map("T" -> 13.48, "H" -> 75.8, "U" -> 1.385, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891023","yyyyMMdd"),Map("T" -> 12.72, "H" -> 78, "U" -> 1.13, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891024","yyyyMMdd"),Map("T" -> 12.04, "H" -> 80.4, "U" -> 0.606, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891025","yyyyMMdd"),Map("T" -> 11.8, "H" -> 77.4, "U" -> 0.475, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891026","yyyyMMdd"),Map("T" -> 11.57, "H" -> 74.8, "U" -> 0.672, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891027","yyyyMMdd"),Map("T" -> 10.9, "H" -> 73.7, "U" -> 0.897, "P" -> 0)),
        FCRow(Utils.solarDate2Long("19891028","yyyyMMdd"),Map("T" -> 9.34, "H" -> 91.4, "U" -> 1.565, "P" -> 13.88)),
        FCRow(Utils.solarDate2Long("19891029","yyyyMMdd"),Map("T" -> 13.02, "H" -> 75.7, "U" -> 2.52, "P" -> 14.42)),
        FCRow(Utils.solarDate2Long("19891030","yyyyMMdd"),Map("T" -> 14.5, "H" -> 73, "U" -> 1.846, "P" -> 1.28))

    )



    val parameters:Map[String,Double] = Map(
      "I" -> 55.124855136716,
      "MeanAnnualRain" ->  1822.9522571819418,
      "Altitude" -> 300,
    )


    val (report,out)  = FireCalculator.calculate(header,parameters,rows)


    val consoleOut = readCsv(Source.fromURL(getClass.getResource("/calculate_result.csv")).getLines().toSeq)

    out.foreach{ o =>
        consoleOut.find(_.date == o.date) match {
            case Some(co) => {
                o.variables.foreach{ case (variable,value) =>
                    co.variables.get(variable) match {
                        case Some(v) if Math.abs(v - value) < 0.000000000001 || (v.isNaN && value.isNaN)  => ()
                        case Some(v) if v != value => fail(s"$v was not equal to $value for variable $variable at date ${o.date}")
                        case None if value.isNaN => ()
                        case None => fail(s"Didn't found variable $variable in console output")
                    }
                }
            }
            case None => fail(s"Didn't found value in console output for date: ${o.date}")
        }
    }


  }

}
