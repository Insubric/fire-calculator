
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

class ReplaceSpec(dummyArgumentToStopAutomaticExecution:Any)  extends WordSpec with Matchers {
  val parameters = new Parameters
  parameters.completeWithDefaults
  
  Variable.load

  parameters += new Parameter(Variable.getByAbbr("I"),55.124855136716)             //extracted from the calculations in test on calculate
  parameters += new Parameter(Variable.getByAbbr("MeanAnnualRain"),1822.9522571819418)    //extracted from the calculations in test on calculate
  
  "A SimpleApp to replace" when{
    val elab = new SimpleApp4CSV
    val path = "./fireindiceslib/testdata/DATA_sample_2_toReplace.csv"
    val rep = new ReportLog(path)
    
    "processing" should {
        val currentFile = new File(path)

        "find the file "+currentFile.getName() in {
            currentFile.exists shouldBe true
        }
        "recognize the date header" in {
            elab.readHeadersFile(currentFile, rep) shouldBe true
        }
        "recognize 72 columns" in {
            elab.headers.size shouldBe 72
        }
        "replace 233 days" in {
            elab.readDataFile(currentFile, parameters, Some("20000101"),Some("20020101"))
            //println(elab.dc.dss.map(_._2.variable.abbr).mkString(" "))
            
            
//            val sdf =  new SimpleDateFormat("yyyy-MM-dd")
            elab.replaceFile(parameters, 233, null,(ch.wsl.fireindices.metadata.Date::T::Tmax::Tmin::Tdew::H::U::P::SnowCover::Nil).toSeq, rep, printOnlyLast=false)
//            elab.replace(parameters, sdf.parse("2001-05-15"), (ch.wsl.fireindices.metadata.Date::T::Tmax::Tmin::Tdew::H::U::P::SnowCover::Nil).toSeq, rep, printOnlyLast=false)
//            elab.replace(parameters, sdf.parse("2001-05-15"), null, rep, printOnlyLast=false)
        
//            println(rep.toString)
            elab.writeLog(rep, true, true)
            elab.dc.toComplete.size shouldBe 12   //not calculated because of snowcover
        }
    }
  }
}

