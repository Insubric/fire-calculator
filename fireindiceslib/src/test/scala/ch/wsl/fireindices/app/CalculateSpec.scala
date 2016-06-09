
package ch.wsl.fireindices.app

import org.scalatest._
import org.scalatest.matchers._

import java.io.File
import java.util.Date
import java.text.SimpleDateFormat
import ch.wsl.fireindices.app._
import ch.wsl.fireindices.functions._
import ch.wsl.fireindices.log.ReportLog
import ch.wsl.fireindices.metadata._
import scala.collection.immutable.HashSet
import scala.io.Source

class CalculateSpec(dummyArgumentToStopAutomaticExecution:Any) extends WordSpec with Matchers {
//class CalculateSpec extends WordSpec with Matchers {
  val parameters = new Parameters
  parameters.completeWithDefaults
  
  Variable.load
  //Variable.getByAbbrCaseInsensitive("sonwcover") //only to make work the following

  

  "A SimpleApp to calculate " when{
    val elab = new SimpleApp4CSV
    val path = "./fireindiceslib/testdata/DATA_sample_2.csv"
    val rep = new ReportLog(path)
    
    "processing" should {
        val currentFile = new File(path)

        "find the file "+currentFile.getName() in {
            currentFile.exists shouldBe true
        }
        "recognize the date header" in {
            elab.readHeadersFile(currentFile, rep) shouldBe true
        }
        "recognize 9 columns" in {
            elab.headers.size shouldBe 9
        }
        "find problems during check" in {
            elab.readDataFile(currentFile, parameters, Some("20000101"),Some("20020101"))
            elab.check(rep) shouldBe false
        }
        "expect to calculate 65 variables " in{
          //elab.stillToCalculate.size shouldBe === 53
          elab.dc.toCalculate.size shouldBe 65 
        }
        "remain no more variables to calculate" in {
            
            elab.calculateFile(parameters,rep)
            //elab.calculate(parameters,rep,FWI::Nil)
                        //elab.replaceNulls(Seq(FWI,ISI,BUI,FFMC,DMC,DC,BaumgartnerDanger,Ifa))
                        //elab.calculate(parameters,rep)
//            println(rep.toString)
            elab.writeLog(rep, true, true)
            //rep.toString.lines.size shouldBe === 51+22
            elab.dc.toCalculate.size shouldBe 0
        }
//        "print json descritpion of variables" in {
//          println(Variable.variables.map(_.descJSON).mkString("\n"))
//        }
    }
  }
}


//class FlSpec extends WordSpec with ShouldMatchers {
//
//    "Snowcover as abbreviation" should {
//      "exist" in {
//        Variable.getByAbbrCaseInsensitive("sonwcover") shouldBe === Variable.SnowCover
//      }
//    }
//}
