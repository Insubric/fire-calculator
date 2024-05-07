package ch.wsl.fireindices

import ch.wsl.fireindices.app.{FireCalculator, SimpleApp, Timer}
import ch.wsl.fireindices.fixtures.MeteoData
import ch.wsl.fireindices.functions.Utils
import ch.wsl.fireindices.model.FCRow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import scala.util.Try

class CalculateSpec extends AnyFlatSpec with Matchers {

  def readCsv(csv:Seq[String]):Seq[FCRow] = {
    val header = csv.head.split(",").toSeq
    val data = csv.tail.map(_.split(",").toSeq)

    data.map{ d =>
      FCRow(Utils.solarDate2Long(d.head),header.tail.zip(d.tail).map{ case (variable,value) => variable -> Try(value.toDouble).getOrElse(Double.NaN) }.toMap)
    }

  }

  "Calculate" should "calulate indexes" in {




    val (report,out)  = FireCalculator.calculate(MeteoData.header,MeteoData.parameters,MeteoData.rows)


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
