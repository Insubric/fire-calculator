
package ch.wsl.fireindices.metadata

import ch.wsl.fireindices.log.DataLog
import com.typesafe.scalalogging.LazyLogging
import java.sql.ResultSet

import ch.wsl.fireindices.functions.Utils._
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.NumericRange

/**
 * Implementation of a class holding data series (time series), with a definition (variable).
 * Time is defined by the start date (in millisec.) and the interval (timestep also in millisec.).
 * It has several methods to get the date serie, slice the data and to check the data
 * for missing values or outliers (according to the variable definition).
 *
 **/
class DataSerie(override val variable:Serie, var start:Long, var interval:Long, var values:List[Double], override val log:DataLog, override val notes:String = "") 
//class DataSerie(override val variable:Serie, var start:Long, var interval:Long, var values:List[Double], val log:DataSerieLog , override val notes:String = "") 
    extends Data(variable, log, notes)
    with LazyLogging{
   
  var outOfRangeMap = new LinkedHashMap[Long,Double]
  var gapMap = new LinkedHashMap[Long,Long]
  val NotAvailableInput:String = ""
  
  /**
   * DataSerie Constructor
   *
   * @param  name   Serie[T] > Name of the variable
   * @param  dates  List[Long] > list of dates range of variable data
   * @param  values list of variable values
   * @return        DataSerie
   */
  def this(name:Serie, dates:List[Long], values:List[Double], log:DataLog, notes:String)={
    this(name, dates.head, dates.tail.head-dates.head, values, log, notes)
  }
  def this(name:Serie, dates:List[Long], values:List[Double], log:DataLog)={
    this(name, dates.head, dates.tail.head-dates.head, values, log, "")
  }
  def this(name:Serie, dates:List[Long], values:List[Double], notes:String)={
    this(name, dates.head, dates.tail.head-dates.head, values, DataLog(name.abbr), notes)
  }
  def this(name:Serie, dates:List[Long], values:List[Double])={
    this(name, dates.head, dates.tail.head-dates.head, values, DataLog(name.abbr), "")
  }
  def this(name:Serie, start:Long, interval:Long, values:List[Double], notes:String)={
    this(name, start, interval, values, DataLog(name.abbr), notes)
  }
  def this(name:Serie, start:Long, interval:Long, values:List[Double])={
    this(name, start, interval, values, DataLog(name.abbr), "")
  }
  
  /**
   * number of values in the DataSerie
   *
   * @return            Int
   */
  def length = this.values.length  
  
  /**
   * clones the DataSerie
   *
   * @return            DataSerie
   */
  override def clone:DataSerie =  variable.createDataSerie( start, interval, values.head::values.tail , log, notes)
	  
  
  /**
   * Slices a DataSerie on a given Date
   *
   * @param dateFormat  format of the date
   * @param condition   the given date from where to slice
   * @return            List[T]
   */
  def sliceData(dateFormat:String, condition:String):List[Double]={
    val sliced = new ListBuffer[Double]
    for (i <- 0 until values.length){
      if (solarDate2String(getDate(i),dateFormat)==condition) sliced+=values(i)
    }
    sliced.toList
  }
  
//  /**
//   **
//   *
//   * @param       
//   * @return      
//   */
//  def aggregateTo(dateFormat:String):List[T]={
//    val results = new ListBuffer[T]
//    val groupBy = getDates.map(Utils.solarDate2String(_,dateFormat)).asInstanceOf[List[String]]
//    val zz = values.zip(groupBy)
//    for (key <- uniques(groupBy)){
//      val selectedValues = zz.filter(_._2==key).map(_._1).asInstanceOf[List[Double]]
//      results += (selectedValues.sum / selectedValues.length).asInstanceOf[T]
//    }
//    results.toList
//  }
  
  /**
   *Returns aggregations of values according to temporal aggregates and a 
   *given function (e.g. max, min, sum)
   *
   * @param         dateFormat   date format 
   * @param         f            aggregation function
   * @return        
   */
  def aggregate(dateFormat:String, f: (List[Double])=>Double):List[Double]={
    val results = new ListBuffer[Double]
    val groupBy = getDates.map(solarDate2String(_,dateFormat)).asInstanceOf[List[String]]
    val zz = values.zip(groupBy)
    for (key <- uniques(groupBy)){
      val selectedValues = zz.filter(_._2==key).map(_._1).asInstanceOf[List[Double]]
      results += f(selectedValues)
//      results += (selectedValues.sum / selectedValues.length).asInstanceOf[T]
    }
    results.toList
  }
  
   
  /**
   *Returns the means of values according to temporal aggregates
   *
   * @param      dateFormat   date format     
   * @return        
   */
  def mean(dateFormat:String):List[Double]={
//    def mean(x:List[Double]):Double = x.sum/x.length
//    aggregate(dateFormat, mean(_))
    aggregate(dateFormat, x=> (x.sum/x.length))
  } 
  

  
  /**
   *Returns a new data serie with time aggregation according to a 
   *given function (e.g. max, min, sum).
   *disclaimer: this is working only for days, has to be adjusted for periods of different lengths (e.g. months)
   *
   * @param         period   string holding the period : "week", "month". "year" 
   * @param         f            aggregation function
   * @return        
   */
  def aggregateDs(period:String, f: (List[Double])=>Double):DataSerie={   //TODO to fix (for non regular intervals)
    period match {
      case "day" => new DataSerie(variable, start, 1L, aggregate("yyyyMMdd",f), log)
//      case "week" => new DataSerie(variable, start, 7L, aggregate("yyyyww",f), log)
//      case "month" => new DataSerie(variable, start, 0L, aggregate("yyyyMM",f), log)
//      case "year" => new DataSerie(variable, start, 0L, aggregate("yyyy",f), log)
      case _ => null
    }  
  }
  
  /**
   * Returns a part of the DataSerie of given indices
   *
   * @param  indices List[Int] > given indices
   * @return         List[T] >
   */
  def apply(indices: List[Int]):List[Double] ={
    if (indices.isEmpty) Nil
    else apply(indices.head)::apply(indices.tail)
  }
  
  /**
   * Returns a specific data from the DataSerie at given index
   *
   * @param  index  the given index
   * @return        T
   */
  def apply(index: Int):Double ={
    values(index)
  }
  
  
    /**
   * Returns the last data from the DataSerie 
   *
   * @param  nr   number of data to be returned
   * @return      List[T]
   */
  def last(nr: Int = 1):List[Double] ={
    slice(length-nr,length)
  }
  
  /**
   * Returns a new DataSerie with only the indicated last values
   *
   * @param  nr   number of data to be returned
   * @return      List[T]
   */
  def lastDs(nr: Int = 1):DataSerie ={
//    variable.createDataSerie( start+interval * Math.max(0, length -nr), interval, last(nr), notes)
    variable.createDataSerie( this.getDate(Math.max(0,this.length-nr)), interval, last(nr), log, notes)
  }
  
    /**
   * Returns the before-last data from the DataSerie 
   *
   * 
   * @return      T
   */
  def prev: Double ={
    apply(length-2)
  }
  
  
  /**
   * Returns the date (in millisec) of a specific data index
   *
   * @param  index  the given index
   * @return        the date in millisec
   */
  def getDate(index: Int):Long ={
    start + interval * index
  }
  
  /**
   * -reverse a list of Dates
   *
   * @param  n    amount of date in the list
   * @return      List[Long] > reversed dates (millisec)
   */
  private def getReverseDates(n: Int):List[Long] ={
    if (n<0) Nil
    else getDate(n)::getReverseDates(n-1)
  }
  
  /**
   * Gets the list of dates of the DataSerie
   *
   * @return         List[Long] > list of dates (millisec)
   */
  def getDates:List[Long] ={
    NumericRange(start,start+interval*length,interval).toList
  }
  
  /**
   * Gets the index at given date
   *
   * @param   date   date in millisec
   * @return         the index
   */
  def getIndex (date:Long):Int = {
    ((date - start)/interval).asInstanceOf[Int]-1
  }

  /**
   * Slices a portion of the DataSerie
   *
   * @param  from      index from where to slice (0-based)
   * @param  until     index until where to slice (not included)
   * @return           portion of the DataSerie
   */
  def slice(from:Int,until:Int):List[Double]={
   values.slice(from,until)
  }
  
  /**
   * Slices a portion of the DataSerie
   *
   * @param  dateStart date from where to slice
   * @param  dateEnd   date until where to slice
   * @return           portion of the DataSerie
   */
  def slice(dateStart:Long,dateEnd:Long):List[Double]={
//    apply(NumericRange(getIndex(dateStart),getIndex(dateEnd),1).toList)
    slice(getIndex(dateStart),getIndex(dateEnd))
  }
  
  /**
   * Returns a list as consecutive unique values (identical consecutive values
   * are reduced to 1 value).
   *
   * @param    in  input list
   * @return       output list   
   */
  def uniques[A](in:List[A]):List[A]={
    if (in.isEmpty) Nil
    else in.head::uniques(in.partition(_==in.head)._2)
  }
  
  /**
   * Inserts a new value at the beginning of the DataSerie
   *
   * @param   v   T > the new value to insert
   * @return      List[T]
   */
  def insertOneRow(v:Double)={  //at the beginning
    values = v::values
  }
  
  /**
   * Appends a new value at the end of the DataSerie
   *
   * @param   v   T > the new value to append
   * @return      List[T]
   */
  def appendOneRow(v:Double)={  //at the end
    values = values:::(v::Nil)
  }
  
  
  /**
   * Reads and inserts a value of a specific field in the DataSerie
   *
   * @param  results  ResultSet > the ResultSet containing all fields and value
   * @param  field    the header of the fiel to read
   * @return          List[T]
   */
  def readField(results:ResultSet, field:String) = {

     val str = results.getString(field)

     insertOneRow({if (str==NotAvailableInput) Double.NaN 
                   else try{str.toDouble} catch {case e:Exception => Double.NaN}})
  }
  
  /**
   * Reads and inserts a value of a specific field in the DataSerie
   *
   * @param  results  ResultSet > the ResultSet containing all fields and value
   * @param  field    the header of the fiel to read
   * @return          List[T]
   */
  def readField2(results:ResultSet, field:String) = {

     val str = results.getString(field)

     appendOneRow({if (str==NotAvailableInput) Double.NaN 
                   else try{str.toDouble} catch {case e:Exception => Double.NaN}})
  }
  
  
  /**
   * Checks all values of the Dataserie (if are in the range of variable min&max and if there are gaps)
   * updating outOfRangeMap and gapMap
   *
   * @return     Unit
   */
  def checkValues={
    val date = getDates
    var currDate:Long = 0L
    var currGap :Long = 0L

    def addGapToMap ={
      gapMap += (currDate -> currGap)
      currGap = 0
    }
    //*******************************************************
    //check gaps and range
    for (i <- 0 until values.length){
      if (variable.isNull(values(i))){
        if (currGap==0) currDate=date(i)
        currGap+=1
        if (i==(values.length-1)){   //if value is the last then already add it to gapMap
          addGapToMap
        }
      }
      else if (!(variable.isNull(values(i)))&&currGap!=0 ){  //add it to gapMap only when non null value appear again
        addGapToMap
      }
      //check values out of range
      if (variable.outOfRange(values(i))) {
         outOfRangeMap += (date(i) -> values(i))
         //substitute out of range with nulls
         val (before,after)=values.splitAt(i)
         values = before:::(Null.getNullOfType(variable.min)::after.tail)
       }
    }
  }
  
  /**
   * Returns a string whit a report on out of range values an gaps (for logs)
   *
   * @return       String
   */
  def getCheckReport(dateFormat:String):String={
       var stringOut = ""

       if (!gapMap.isEmpty){
         stringOut+=variable.abbr+" gaps"+"\n"
         gapMap.foreach{y => stringOut+="  "+solarDate2String(y._1, dateFormat)+"-->"+y._2+"\n"}
         stringOut+="\n"
       }
       if (!outOfRangeMap.isEmpty){
         stringOut+=variable.abbr+" out of range values(min = "+ variable.min +"  max = "+ variable.max +")"+"\n"
         outOfRangeMap.foreach{y => stringOut+="  "+solarDate2String(y._1, dateFormat)+"-->"+y._2+"\n"}
         stringOut+="\n"
       }

       return stringOut
   }
   
  /**
   * Returns if the last value of the serie is missing
   *
   * @return       Boolean
   */
  def missingLastValue:Boolean={
     
      //values.endsWith(variable.getNull::Nil)
      variable.isNull(values(length-1))
    
   }
   
  /**
   * Updates the last value in the data with the given one
   *
   * @return       Boolean
   */
  def updateLast(last: Double)={
     this.values = this.values.slice(0, this.length-1):::last::Nil 
     this
   } 
   
  /**
   * Updates the last value in the data and notes from another DatsaSerie
   *
   * @return       Boolean
   */
  def updateLastAndNotes(ds: DataSerie):DataSerie={	  
	  if (variable==ds.variable){
		 this.updateLast(ds.values.last)             //this needs to be done, since the iteration in DataCollection.complete is not aware of canges in Data collection during cycle
		 this.variable.createDataSerie(this.start, this.interval, this.values, ds.log, ds.notes)
	  } else{
		  logger.warn("different "+ this.variable.abbr +"  "+ds.variable.abbr)
		 null
	  }
   } 
   
   def getStringDataSerie: StringDataSerie = {
	   new StringDataSerie(this.variable.abbr, this.start, this.interval,
			this.values.map(this.variable.format(_))) 
   }
   
    /**
   * Prints the basic information of the DataSerie
   *
   * @return       String
   */
   def printMe={
     this.variable.abbr +"  start "+ solarDate2String(this.start) + "   nrdata "+ this.length +
     "    notes " + this.notes
   } 
}

