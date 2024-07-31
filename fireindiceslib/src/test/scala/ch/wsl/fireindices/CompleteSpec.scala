package ch.wsl.fireindices

import ch.wsl.fireindices.app.{FireCalculator, SimpleApp, Timer}
import ch.wsl.fireindices.fixtures.{CSVReader, MeteoData}
import ch.wsl.fireindices.functions.Utils
import ch.wsl.fireindices.model.FCRow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, LocalDate}
import scala.Predef.println
import scala.io.Source
import scala.util.Try

class CompleteSpec extends AnyFlatSpec with Matchers {



  "Complete" should "calulate incremental indexes" in {


      val (report,outCalculated)  = FireCalculator.calculate(MeteoData.header,MeteoData.parameters,MeteoData.rows)

      val inToComplete = outCalculated.init ++ Seq(outCalculated.last.copy(variables = outCalculated.last.variables.map{ case (k,v) =>
            val r = if(MeteoData.header.contains(k)) v else Double.NaN
          (k,r)
      }))

      val headers = Seq(MeteoData.header.head) ++ outCalculated.head.variables.keys


      val (reportComplete,out)  = FireCalculator.complete(headers,MeteoData.parameters,inToComplete)

      println(reportComplete.formatLog)

    scala.Double

      out.foreach{ o =>
          outCalculated.find(_.date == o.date) match {
              case Some(co) => {
                  o.variables.foreach{ case (variable,v1) =>
                      co.variables.get(variable) match {
                          case Some(v2) if Math.abs(v2 - v1) < 0.000000000001 || (java.lang.Double.isNaN(v2) && java.lang.Double.isNaN(v1))  => ()
                          case Some(v2) if v2 != v1 => fail(s"$v2 was not equal to $v1 for variable $variable at date ${o.date}")
                          case None if java.lang.Double.isNaN(v1) => ()
                          case None => fail(s"Didn't found variable $variable in console output")
                      }
                  }
              }
              case None => fail(s"Didn't found value in console output for date: ${o.date}")
          }
      }


  }

    it should "be completed for 2  rows" in {



        val (report,outCalculated)  = FireCalculator.calculate(MeteoData.header,MeteoData.parameters,MeteoData.rows.take(1))

        val headers = Seq(MeteoData.header.head) ++ outCalculated.head.variables.keys

        val in = outCalculated ++ MeteoData.rows.drop(1).take(1)
        val (reportComplete,out)  = FireCalculator.complete(headers,MeteoData.parameters,in)

        out


    }

  it should "calculate all indices" in {
    val (headers,data) = CSVReader.readCsv(Source.fromURL(getClass.getResource("/complete-2024-01-01.csv")).getLines().toSeq)
    val parameters  = Seq(("Altitude",204.8),("Latitude",46.167484),("MeanAnnualRain",1731.5955),("I",52.881645))
    val (log,r) = FireCalculator.complete(headers,parameters.toMap,data)

    Math.abs(r.last.variables("FWI")  - 0.009161104) < 0.000001

  }

}
