/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package ch.wsl.fireindices.app

import org.scalatest.Matchers
import org.scalatest.WordSpec
import scala.io.Source

//class ResultsSpec extends WordSpec with Matchers {
class ResultsSpec(dummyArgumentToStopAutomaticExecution:Any)  extends WordSpec with Matchers {

  
  def getLine(filename:String, line_nr:Int):String = {
    val bufferedSource = Source.fromFile(filename)
    val line = bufferedSource.getLines.toList(line_nr)
    bufferedSource.close
    
    //trunc the fields to 10 char (avoid rounding differences)
    line.split(",").map(s => s.substring(0, math.min(9, s.length-1))).mkString(",")
//    line
  }
  
  def remove_column(csv_string:String, index:Int)={
    val s = csv_string.split(",").toList
    (s.take(index):::s.takeRight(s.length-index-1)).mkString(",")
  }
  
  val path = "./fireindiceslib/testdata/"
  
  "The results on the 1 August 2001 of CALCULATE Data_sample_2_RESULT.csv " when{
    
    val CALCULATE = getLine(path + "DATA_sample_2_RESULT.csv", 580-1)
      
    "compared to COMPLETE DATA_sample_2_toComplete_RESULT.csv" should {
        val COMPLETE = getLine(path + "DATA_sample_2_toComplete_RESULT.csv", 2-1)

        "be the same" in {
            COMPLETE shouldEqual CALCULATE
//            remove_column(COMPLETE, 2) shouldEqual CALCULATE
        }
    }
    "compared to COMPLETECONTINUOUS DATA_sample_2_toCompleteContinuous_RESULT.csv" should {
        val COMPLETECONTINUOUS = getLine(path + "DATA_sample_2_toCompleteContinuous_RESULT.csv", 580-1)

        "be the same" in {
            COMPLETECONTINUOUS shouldEqual CALCULATE
        }
    }
    "compared to COMPLETE DATA_sample_2_toReplace_RESULT.csv" should {
        val REPLACE = getLine(path + "DATA_sample_2_toReplace_RESULT.csv", 580-1)

        "be the same" in {
            REPLACE shouldEqual CALCULATE
        }
    }
  }
}

