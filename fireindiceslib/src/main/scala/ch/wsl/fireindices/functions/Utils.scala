package ch.wsl.fireindices.functions

import ch.wsl.fireindices.metadata.Null
import java.text.SimpleDateFormat
import java.util.Date

/**
* Contains conversion functions (for units and dates) and other utilities.
*
**/
object Utils {

  /**
  * find the interval index in which the data falls
  * (returns an index form 1 to length(thresholds)+1
  *
  * @param          thresholds      Seq of thresholds
  * @param          value           value to situate
  * @return         index from 1 to length(thresholds)+1
  */
  def intervalIndex (thresholds:Seq[Double],value:Double):Long = {
    val i=thresholds.indexWhere(_>value)
    if (i== -1) thresholds.length+1
    else i+1
  }

  /**
  * Converts Celsius to Fahrenheit
  *
  * @param          celsius value
  * @return         fahrenheit value
  */
  def C2F (celValue:Double):Double = (celValue*9.0/5.0)+32.0

  /**
  * Converts Fahrenheit to Celsius
  *
  * @param          fahrenheit value
  * @return         celsius value
  */
  def F2C (farValue:Double):Double = (farValue+32)*5.0/9.0
  /**
  * Converts KMH to MPH
  *
  * @param          KMH value
  * @return         MPH value
  */
  def kmh2mph (kmhValue:Double):Double = kmhValue/1.609344
  /**
  * Converts MPH to KMH
  *
  * @param          MPH value
  * @return         KMH value
  */
  def mph2kmh (mphValue:Double):Double = mphValue*1.609344
  /**
  * Converts mm to inches
  *
  * @param          mm value
  * @return         inches value
  */
  def mm2inch (mmValue:Double):Double = mmValue/25.4
  /**
  * Converts inches to mm
  *
  * @param          inches value
  * @return         mm value
  */
  def inch2mm (inchValue:Double):Double = inchValue*25.4
  /**
  * Converts decimal degrees to radians
  *
  * @param          decimal degrees value
  * @return         radians value
  */
  def deg2rad (decdegValue:Double):Double = decdegValue * (math.Pi/180.0)
  /**
  * Converts radians to decimal degrees
  *
  * @param          radians value
  * @return         decimal degrees value
  */
  def rad2deg (radValue:Double):Double = radValue * (180.0/math.Pi)
  /**
  *Converts kPa to mmH2O
  *
  *@param          kPa value
  *@return         mm of water alue
  */
  def kPa2mmH2O (kPaValue:Double):Double = kPaValue / 0.00980665
  /**
  * Converts kPa to mmH2O
  *
  * @param          kPa value
  * @return         mm of mercury value
  */
  def kPa2mmHg (kPaValue:Double):Double = kPaValue * 7.500616827042



  
  /**
   * Converts a date-string to a date-long(millisec) excluding legal time changes
   * 
   *
   *
   * @param   date   date String of format "format"
   * @param   format format of the date-string
   * @return         date in millisec
   */
  def solarDate2Long(date:String, format:String ="yyyyMMdd"):Long={
    val sdf = new SimpleDateFormat(format)
    val tmp = sdf.parse(date)
    if (sdf.getTimeZone.inDaylightTime(tmp))
      tmp.getTime + sdf.getTimeZone.getDSTSavings
    else
      tmp.getTime
  }
  
  /**
   * Converts a date to a date-long(millisec) excluding legal time changes
   * 
   *
   *
   * @param   date   date
   * @return         date in millisec
   */
  def solarDate2Long(date:Date /*,format:String ="yyyyMMdd"*/):Long={
    val sdf = new SimpleDateFormat("yyyyMMdd")
    if (sdf.getTimeZone.inDaylightTime(date))              //correct for daylight saving time
      date.getTime + sdf.getTimeZone.getDSTSavings
    else
      date.getTime
  }
  
  /**
   * Converts a date-long(millisec) to a date-string excluding legal time changes
   * 
   *
   *
   * @param   date   date in millisec
   * @param   format format of the date-string
   * @return         date String of format "format"
   */
  def solarDate2String(date:Long,format:String="yyyyMMdd"):String={
    val sdf = new SimpleDateFormat(format)
    val tmp = if (sdf.getTimeZone.inDaylightTime(new Date(date)))
                  date - sdf.getTimeZone.getDSTSavings
               else
                  date
    sdf.format(new Date(tmp))
  }
  
  /**
   * Converts a date to a date-string excluding legal time changes
   * 
   *
   *
   * @param   date   date
   * @param   format format of the date-string
   * @return         date String of format "format"
   */
  def solarJDate2String(date:Date,format:String="yyyyMMdd"):String={
    val sdf = new SimpleDateFormat(format)
    sdf.format(date)
  }
  
  /**
   * Functions return number of days since beginning of the year
   * 
   *
   *
   * @param  date  date
   * @return       number of days since beginning of the year
   */
  def solarDate2Julian(date:Long):Int={
    solarDate2String(date,"DDD").toInt
  }
  
  /**
   * if input is a list returns the head of this list
   * 
   *
   *
   * @param  x  Any object
   * @return    if input object is a list returns the head, else returns the object
   */
  def toHead(x:Any):Any ={
    if (x.isInstanceOf[List[Any]]) asInstanceOf[List[Any]].head
     else x
  }
  
  /**
   * if input is a list returns the tail of this list
   * 
   *
   *
   * @param  x  Any object
   * @return    if input object is a list returns the tail, else returns the object
   */
  def toTail(x:Any):Any ={
    if (x.isInstanceOf[List[Any]]) asInstanceOf[List[Any]].tail
     else x
  }
 
  
  def crop(value:Double, min:Double, max:Double)=
    math.max(min, math.min(max, value))
    
}
