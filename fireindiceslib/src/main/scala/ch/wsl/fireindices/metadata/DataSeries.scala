
package ch.wsl.fireindices.metadata

import ch.wsl.fireindices.log.DataLog
import ch.wsl.fireindices.metadata.Variable._

import scala.collection.generic._

/**
 * Collection of DataSerie objects.
 *
 **/


class DataSeries(that: TraversableOnce[DataSerie]) extends KeySet[Serie,DataSerie](that){
									//with Map[Serie,DataSerie] 
									//with MapLike[Serie,DataSerie, DataSeries]{
  
  /**
   * simple constructor 
   *
   */ 
   def this() = this(Nil) 
  
  /**
   * return a new key for the DataCollection KeySet
   *
   * @param          DataSerie > the new DataSerie to enter
   * @return         Variable > the key of the data
   */
  def buildKey(ds: DataSerie)= ds.variable

  def apply(abbr: String):DataSerie={
    for (v <- this.keySet){
      if (abbr==v.abbr) return this(v)
    }
    return null
  }
  
  def getCaseInsensitive(abbr: String):DataSerie={
    for (v <- this.keySet){
      if (abbr.toLowerCase==v.abbr.toLowerCase) return this(v)
    }
    return null
  }
  
  /**
   * return a DataSerie with the dates
   * 
   * @return         Date DataSerie
   */
  def dateDs:DataSerie= {
	  if (this.size==0) {
		  new DataSerie(Date, 0, 0, Nil)
	  }else{
		  val fds = this.head._2  //get first serie
                  if (fds.interval < msecInOneDay)
                    new DataSerie(DateTime, fds.start, fds.interval, fds.getDates.map(_.toDouble))
                  else if (fds.interval == msecInOneDay)
                    new DataSerie(Date, fds.start, fds.interval, fds.getDates.map(_.toDouble))
                  else
                    new DataSerie(Date, fds.start, fds.interval, fds.getDates.map(_.toDouble))
	  }
  }
  
  /**
   * Returns a DataSeries with the order specified in Variable.series
   *
   * @return     DataSeries
   */
  def ordered: DataSeries = new DataSeries(Variable.series.intersect(this.keySet.toSeq).map(this(_)))

  
  /**
   * clones all DataSerie s
   *
   * @return            DataSeries
   */
  def cloneAll:DataSeries =  new DataSeries(this.valuesIterator.map(_.clone))
 
}
