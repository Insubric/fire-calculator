/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package ch.wsl.fireindices


import org.scalatest._
import org.scalatest.matchers._

import java.io.File
import java.util.Date
import java.text.SimpleDateFormat
import ch.wsl.fireindices.app._

import ch.wsl.fireindices.functions._
import ch.wsl.fireindices.metadata._
import scala.collection.immutable.HashSet

class FunctionsSpec extends WordSpec with Matchers {

  "lastRainsum " when{

    val a = List(0,2.0,0,0,0,0,0)
    val b = List(0,2.0,0,Double.NaN,0,0,0)
    val c = List(0,0.2,0,0,0.4,0,0)
    val d = List(0,Double.NaN,0,0,2.4,0,0,0)
    
    "processing " + a should {
        "give 2" in {
            ListFunctions.applyFunctionPrevLastRainSum(Functions.lastRainSum,0.0,
                                                       a,0.0).last should be (2)
        }
    }
    "processing " + b should {
        "give Double.NaN" in {
            assert(ListFunctions.applyFunctionPrevLastRainSum(Functions.lastRainSum,0.0,
                                                       b,0.0).last.equals(Double.NaN) )
        }
    }
    "processing " + c should {
        "give 0.4" in {
            ListFunctions.applyFunctionPrevLastRainSum(Functions.lastRainSum,0.0,
                                                       c,0.0).last should be (0.4)
        }
    }
    "processing " + d should {
        "give 2.4" in {
            ListFunctions.applyFunctionPrevLastRainSum(Functions.lastRainSum,0.0,
                                                       d,0.0).last should be (2.4)
        }
    }
  }
}
