

package ch.wsl.fireindices.metadata

import scala.collection.mutable.MapLike
import scala.collection.mutable.HashTable
import scala.collection.mutable.LinkedEntry
import scala.collection.generic._

/**
 * Collection of Data objects (DataSerie or Parameter).
 *
 **/
 

class DataCollection(that: TraversableOnce[Data]) extends KeySet[Variable, Data](that){
																//with Map[Variable,Data] 
																//with MapLike[Variable,Data, DataCollection]{

  
  /**
   * simple constructor 
   *
   */ 
   def this() = this(Nil) 
  
  
  
  /**
   * returns a new key for the DataCollection KeySet
   *
   * @param          Data > the new data to enter
   * @return         Variable > the key of the data
   */
  def buildKey(data: Data)= data.variable
 
 
  /**
   * clones the DataSerie
   *
   * @return            DataSerie
   */
  def cloneAll:DataCollection =  {
	  val d = new DataCollection
	  for (data <- this.valuesIterator)  {
		 if( data.isInstanceOf[DataSerie]) d+= data.asInstanceOf[DataSerie].clone
		 else d+= data.asInstanceOf[Parameter].clone
	  }
	  d
  }  
 
  /**
   * Returns a DataCollection with the order specified in Variable.values
   *
   * @return     DataCollection
   */
  def ordered: DataCollection ={
    val ordered = Variable.variables.intersect(this.keySet.toSeq)
    new DataCollection(ordered.map(this(_)))
  }

  /**
   * Returns the DataSeries from the DataCollection
   *
   * @return     DataSeries
   */
  def dss: DataSeries ={
    new DataSeries(this.filterKeys(_.isInstanceOf[Serie]).valuesIterator.asInstanceOf[TraversableOnce[DataSerie]])
  }
  
  /**
   * Returns the Parameters from the DataCollection
   * 
   * @return     Parameters
   */
  def pars: Parameters ={
    new Parameters(this.filterKeys(!_.isInstanceOf[Serie]).valuesIterator.asInstanceOf[TraversableOnce[Parameter]])
  }
  
  /**
   * Removes the Dataseries from the DataCollection
   *
   * @return         Unit
   */
  def removeDss ={
    this --= this.keys.filter(_.isInstanceOf[Serie])

  }
  
  /**
   * Removes the Parameters from the DataCollection
   *
   * @return         Unit
   */
  def removePars ={
    this --=  this.keys.filter(!_.isInstanceOf[Serie])
  }
  
  /**
   * Removes the Parameters who are not availables from the DataCollection
   *
   * @return    Unit
   */
  def removeNotAvailablePars ={
    this --=  this.keys.filter(x=> !x.isInstanceOf[Serie] && (x == null || x.equals(Double.NaN)))
  }

  /**
   * return a List of Variables which could be calculated with the given variables
   *
   * @return     List[Variable]
   */
  def calculables =Variable.calculables.filter(_.canCalculate(this))
  
  /**
   * return a List of Variables with Serie which could be calculated with the given variables
   *
   * @return     List[Variable with Serie]
   */
  def calculableSeries = calculables.filter(_.isInstanceOf[Serie])

  /**
   * return a Sequence of Variables with Calculable which already exists in the collection
   * (either as input or already calculated)
   *
   * @return     Seq[Variable]
   */
  def calculated: List[Variable with Calculable] ={
    calculables.intersect(this.keySet.toSeq)
  }
  
 /**
   * return a Sequence of Series with Calculable which already exists in the collection
   * (either as input or already calculated)
   *
   * @return     Seq[Variable]
   */
  def calculatedSeries: List[Serie with Calculable] = calculated.filter(_.isInstanceOf[Serie]).asInstanceOf[List[Serie with Calculable]]

  
   /**
   * return a Sequence of Variables with Serie which already exists in the collection
   * but for which the last value has to be calculated
   *
   * @return     Seq[Variable with Serie]
   */
  def toComplete ={
	calculatedSeries.filter(x => this.dss(x.asInstanceOf[Serie]).missingLastValue)
  }

  /**
   * return a Sequence of Variables  which do not exists in the collection
   * (either as input or already calulated)
   *
   * @return     List[Variable]
   */
  def toCalculate ={
     calculables.filterNot(this.keySet.contains(_)).toSeq
  }
  
  
  
  /**
   * return a DataSerie with the dates
   * 
   * @return         Date DataSerie
   */
  def dateDs:DataSerie= {
	  dss.dateDs
  }
  
  
  /**
   * calculate the given variables and necessary/possible intermediate ones
   * 
   * @return         Date DataSerie
   */
  def calculate(vars: Seq[Variable with Calculable] = this.toCalculate)= {
      for (variable <- vars) this += variable.calculate(this)
  }
 
   /**
   * complete the given variables and necessary/possible intermediate ones
   * 
   * @return         Date DataSerie
   */
  def complete(vars: Seq[Serie with Calculable] = this.toComplete)= {
      for (variable <- vars)   
        if (this.dss(variable).missingLastValue)  this.update(variable,variable.complete(this))    
  }
     
   /**
   * complete the given variables and necessary/possible intermediate ones
   * 
   * @return         Date DataSerie
   */
  def forceComplete(vars: Seq[Serie with Calculable] = this.toComplete)= {
      for (variable <- vars)  {
        this.dss(variable).values = this.dss(variable).values.take(this.dss(variable).values.length-1):::Double.NaN::Nil
        this.update(variable,variable.complete(this))
      } 
  }
  
}

