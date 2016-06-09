
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


class CompleteContinuousSpec(dummyArgumentToStopAutomaticExecution:Any)  extends WordSpec with Matchers {
  val parameters = new Parameters
  parameters.completeWithDefaults
  
  Variable.load

  parameters += new Parameter(Variable.getByAbbr("I"),55.124855136716)             //extracted from the calculations in test on calculate
  parameters += new Parameter(Variable.getByAbbr("MeanAnnualRain"),1822.9522571819418)    //extracted from the calculations in test on calculate
  
  "A SimpleApp to complete continuous" when{
    val elab = new SimpleApp4CSV
    val path = "./fireindiceslib/testdata/DATA_sample_2_toCompleteContinuous.csv"
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
        "compute" in {
            elab.readDataFile(currentFile, parameters, Some("20000101"),Some("20020101"))
            //println(elab.dc.dss.map(_._2.variable.abbr).mkString(" "))
	    val odc = elab.dc.cloneAll           
            for (ds <- elab.dc.dss.map(_._2)){
				if ((ch.wsl.fireindices.metadata.Date::T::Tmax::Tmin::Tdew::H::U::P::SnowCover::Nil).contains(ds.variable)){
					ds.values = ds.values.slice(0,501)
				} else {
					ds.values = ds.values.slice(0,500):::Null.getNullOfType(ds.values(0))::Nil
				}				
			}
			
			
			
            for (i<- Range(1,100)){
                    elab.dc.complete()
                    for (ds <- elab.dc.dss.map(_._2)){
                            if ((ch.wsl.fireindices.metadata.Date::T::Tmax::Tmin::Tdew::H::U::D::P::SnowCover::Nil).contains(ds.variable)){
                                    ds.values = ds.values:::odc.dss(ds.variable).values(500+i)::Nil    //copy values from cloned dataset (original)
                            } else {
                                    ds.values = ds.values:::Double.NaN::Nil
                            }				
                    }
            }
			
			
            elab.completeFile(parameters, rep, printOnlyLast=false)
            //elab.complete(parameters,rep,FWI::Nil)
//            println(rep.toString)
             elab.writeLog(rep, true, true)
            println(elab.dc.toComplete.map(_.abbr).mkString("  "))
            elab.dc.toComplete.size shouldBe 0
        }
    }
  }
}

