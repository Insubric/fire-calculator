package ch.wsl.fireindices

import ch.wsl.fireindices.app.{FireCalculator, SimpleApp, Timer}
import ch.wsl.fireindices.fixtures.MeteoData
import ch.wsl.fireindices.functions.Utils
import ch.wsl.fireindices.model.FCRow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import scala.util.Try

class CompleteSpec extends AnyFlatSpec with Matchers {


  "Complete" should "calulate incremental indexes" in {


      val (report,outCalculated)  = FireCalculator.calculate(MeteoData.header,MeteoData.parameters,MeteoData.rows)

      val inToComplete = outCalculated.init ++ Seq(outCalculated.last.copy(variables = outCalculated.last.variables.map{ case (k,v) =>
        k -> {if(MeteoData.header.contains(k)) v else Double.NaN }
      }))

      val headers = Seq(MeteoData.header.head) ++ outCalculated.head.variables.keys


      val (reportComplete,out)  = FireCalculator.complete(headers,MeteoData.parameters,inToComplete)


      out.foreach{ o =>
          outCalculated.find(_.date == o.date) match {
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

    it should "be completed for 2  rows" in {



        val (report,outCalculated)  = FireCalculator.calculate(MeteoData.header,MeteoData.parameters,MeteoData.rows.take(1))

        val headers = Seq(MeteoData.header.head) ++ outCalculated.head.variables.keys

        val in = outCalculated ++ MeteoData.rows.drop(1).take(1)
        val (reportComplete,out)  = FireCalculator.complete(headers,MeteoData.parameters,in)

        out


    }

}
