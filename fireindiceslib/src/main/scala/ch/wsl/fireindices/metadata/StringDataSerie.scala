
package ch.wsl.fireindices.metadata
import java.sql.ResultSet

import ch.wsl.fireindices.functions.Utils
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.MutableList
import scala.collection.immutable.NumericRange

/**
 * Implementation of a class holding data series (time series), with a definition (variable).
 * Time is defined by the start date (in millisec.) and the interval (timestep also in millisec.).
 * It has several methods to get the date serie, slice the data and to check the data
 * for missing values or outliers (according to the variable definition).
 *
 **/
class StringDataSerie(val name:String,var start:Long,var interval:Long, var values:List[String]) {

   val NotAvailableInput:String = ""
  
  /**
   * DataSerie Constructor
   *
   * @param  name   Serie[T] > Name of the variable
   * @param  dates  List[Long] > list of dates range of variable data
   * @param  values list of variable values
   * @return        DataSerie
   */
  def this(name:String,dates:List[Long],values:List[String])={
    this(name,dates.head,dates.tail.head-dates.head,values)
  }
  
  /**
   * number of values in the DataSerie
   *
   * @return            Int
   */
  def length = this.values.length  
  
	  
  
  /**
   * Slices a DataSerie on a given Date
   *
   * @param dateFormat  format of the date
   * @param condition   the given date from where to slice
   * @return            List[T]
   */
  def sliceData(dateFormat:String, condition:String):List[String]={
    val sliced = new MutableList[String]
    for (i <- 0 until values.length){
      if (Utils.solarDate2String(getDate(i),dateFormat)==condition) sliced+=values(i)
    }
    sliced.toList
  }
  
    /**
   * Crops the StringDataSerie to the desired time span
   *
   * @param  start     date from where to slice
   * @param  length    number of data to keep
   * @return           nothing
   */
  def crop(start:Long, length:Long):StringDataSerie={
      this.start = start
      val startIndex = getIndex(start)
      this.values = this.values.slice(startIndex, startIndex + length.toInt +1)
      
      this
  }
  
    
  /**
   * Returns a part of the DataSerie of given indices
   *
   * @param  indices List[Int] > given indices
   * @return         List[T] >
   */
  def apply(indices: List[Int]):List[String] ={
    if (indices.isEmpty) Nil
    else apply(indices.head)::apply(indices.tail)
  }
  
  /**
   * Returns a specific data from the DataSerie at given index
   *
   * @param  index  the given index
   * @return        T
   */
  def apply(index: Int):String ={
    values(index)
  }
  
  
    /**
   * Returns the last data from the DataSerie 
   *
   * @param  nr   number of data to be returned
   * @return      List[T]
   */
  def last(nr: Int = 1):List[String] ={
    slice(length-nr,length)
  }
  
  
    /**
   * Returns a new DataSerie with only the indicated last values
   *
   * @param  nr   number of data to be returned
   * @return      List[T]
   */
  def lastDs(nr: Int = 1):StringDataSerie ={
	new StringDataSerie(this.name, this.start+this.interval * Math.max(0, this.length -nr), this.interval, last(nr))  
  }
  
    /**
   * Returns the before-last data from the DataSerie 
   *
   * 
   * @return      T
   */
  def prev: String ={
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
  def slice(from:Int,until:Int):List[String]={
   values.slice(from,until)
  }
  
  /**
   * Slices a portion of the DataSerie
   *
   * @param  dateStart date from where to slice
   * @param  dateEnd   date until where to slice
   * @return           portion of the DataSerie
   */
  def slice(dateStart:Long,dateEnd:Long):List[String]={
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
  def insertOneRow(v:String)={  //at the beginning
    values = v::values
  }
  
  /**
   * Reads and inserts a value of a specific field in the DataSerie
   *
   * @param  results  ResultSet > the ResultSet containing all fields and value
   * @param  field    the header of the fiel to read
   * @return          List[T]
   */
  def readField(results:ResultSet, field:String) = {

     insertOneRow(results.getString(field))
  }
  

}

