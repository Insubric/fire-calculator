/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package ch.wsl.fireindices.metadata



/**
 * Trait for variables which are time series (DataSerie).
 * Contains some factory methods for DataSerie.
 **/
import ch.wsl.fireindices.log.DataLog

trait Serie extends Variable {
//class Serie (override val name:String, override val abbr:String, override val unit:String, 
//             override val min:Double, override val max:Double, override val c: Class[_ <: AnyVal], 
//             val requiredInterval:Long) extends Variable(name, abbr, unit, min, max, c){
  
  /**
   * DataSerie constructor
   *
   * @param  start    date of the first value (millisec)
   * @param  interval interval between each values (millisec)
   * @param  values   the values
   * @param  notes    a string containing notes on the DataSeries
   * @return          DataSerie
   */
  def createDataSerie(start:Long, interval:Long, values:List[Double], log: DataLog, notes:String = ""):DataSerie={
     //min match{
      //case x:Double =>new DataSerie(this,start,interval,values,notes)
      //case x:Long =>new DataSerie(this,start,interval,values,notes)
      //case x:Int =>new DataSerie(this,start,interval,values,notes)
      ////case x:Any =>new DataSerie(this,start,interval,values.asInstanceOf[List[T]],notes)
      //case _ =>new DataSerie(this,start,interval,values,notes)
      new DataSerie(this, start, interval, values, log, notes)
    }
  
  /**
   * DataSerie constructor
   *
   * @param  dates    list of dates (millisec)
   * @param  values   the values
   * @param  notes    a string containing notes on the DataSeries
   * @return          DataSerie
   */
  def createDataSerieFromDates(dates:List[Long],values:List[Double], log: DataLog, notes:String = ""):DataSerie= 
		new DataSerie(this,dates.head,dates.tail.head-dates.head, values, log, notes)
              
  def createDataSerieFromDates(dates:List[Long],values:List[Double]):DataSerie= 
		new DataSerie(this,dates.head,dates.tail.head-dates.head, values)
	////if (dates.size >1){
		//min match{
		  //case x:Double =>new DataSerie(this,dates.head,dates.tail.head-dates.head,values, notes)
		  //case x:Long =>new DataSerie(this,dates.head,dates.tail.head-dates.head,values, notes)
		  //case x:Int =>new DataSerie(this,dates.head,dates.tail.head-dates.head,values, notes)
		  ////case x:Any =>new DataSerie(this,dates.head,dates.tail.head-dates.head,values.asInstanceOf[List[T]], notes)
		  //case _ =>new DataSerie(this,dates.head,dates.tail.head-dates.head,values.asInstanceOf[List[Double]], notes)
		//}
	////}else{
		  ////case x:Double =>new DataSerie(this,dates.head,dates.tail.head-dates.head,values, notes)
		  ////case x:Long =>new DataSerie(this,dates.head,dates.tail.head-dates.head,values, notes)
		  ////case x:Int =>new DataSerie(this,dates.head,dates.tail.head-dates.head,values, notes)
		  ////case x:Any =>new DataSerie(this,dates.head,dates.tail.head-dates.head,values.asInstanceOf[List[T]], notes)		
	////}
}