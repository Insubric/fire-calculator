
package ch.wsl.fireindices.metadata

/**
 * Implementation of a class to hold a single value (parameter), with a definition (variable).
 * Time is defined by the start date (in millisec.) and the interval (timestep also in millisec.).
 * It has a method to check the data for range, according to the variable definition.
 *
 **/
import ch.wsl.fireindices.functions.Utils
import ch.wsl.fireindices.log.DataLog
import ch.wsl.fireindices.log.ParamLog

class Parameter(override val variable:Variable,var value:Double, override val log: DataLog, override val notes:String = "") 
  extends Data(variable, log, notes){

  
  
  def this(variable:Variable, value:Double) = {
    this(variable, value, DataLog(variable.abbr), "")
  }
  def this(variable:Variable, value:Double, notes:String) = {
    this(variable, value, DataLog(variable.abbr), notes)
  }
  /**
   * clones the DataSerie
   *
   * @return            Parameter
   */
  override def clone:Parameter =  new Parameter(variable, value, log, notes)
	  

  /**
   * return a string with a report of Parameter object
   *
   * @return         String
   */
   def getCheckReport(dateFormat:String):String = {
     if (variable.outOfRange(value))  variable.abbr + " is out of range (value = "+ value.toString +" min = "+ variable.min +"  max = "+ variable.max +")\n"
     else ""
   }
   
   def printValue:String ={
      if(variable.c.toString.equals("long")) 
        Utils.solarDate2String(value.asInstanceOf[Long],"dd-MMM") 
      else 
        value.toString   
  }
  
  def toParamLog = ParamLog(variable.abbr, printValue)
}



//object Parameter{
//		
//
//  
//	//implicit def ParameterAny2ParameterDouble(v: Parameter):Parameter= v.asInstanceOf[Parameter]
//	//implicit def ParameterAny2ParameterLong(v: Parameter):Parameter[Long]= v.asInstanceOf[Parameter[Long]]
//	//implicit def ParameterAny2ParameterInt(v: Parameter):Parameter[Int]= v.asInstanceOf[Parameter[Int]]
//
//}
