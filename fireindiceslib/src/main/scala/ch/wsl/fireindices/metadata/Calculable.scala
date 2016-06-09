
package ch.wsl.fireindices.metadata


/**
 * Trait for variables that can be calculated.
 * Contains some utility functions to check or search for input parameters.
 **/
trait Calculable{
  val in = new VariableChooser
  
  /**
   * check if a Set[Variable] contains this variable
   *
   * @param  variables  Set[Variable]> the set of variables
   * @return            Boolean
   */
  def hasInputs(variables:Set[Variable]):Boolean = {
    if (in.size==0) false
    else in.hasInputs(variables)
  }
  
  /**
   * check if a DataCollection contains this variable
   *
   * @param  dss  DataCollection > the DataCollection
   * @return      Boolean
   */
  def hasInputs(dss:DataCollection):Boolean = {
    in.hasInputs(dss.keySet.toSet)
  }
  
  /**
   * check if the variable is calculable with the given set of variables
   *
   * @param  variables  Set[Variable]> the set of variables
   * @return      Boolean
   */
  def canCalculate(variables:Set[Variable]):Boolean = {
    if (in.size==0) false
    else in.canCalculate(variables)
  }
  
  /**
   * check if the variable is calculable with the given DataCollection 
   *
   * @param  dss  DataCollection > the DataCollection
   * @return      Boolean
   */
  def canCalculate(dc:DataCollection):Boolean = {
    canCalculate(dc.keySet.toSet)
  }
  
  /**
   * check if a Sequence of abbreviation contain this variable abbreviation
   *
   * @param  abbreviations  Seq[String] > Sequence of abbreviation
   * @return                Boolean
   */
  def hasInputsFromAbbr(abbreviations:Seq[String]):Boolean = {
    in.hasInputs(abbreviations.map(Variable.getByAbbr(_)).toSet)
  }
  
  /**
   * choose a group of variables in a DataMapper set from a Datacollection
   *
   * @param  dss  DataCollection > the data collection
   * @return      VariableDataMap
   */
  def chooseVariablesAndCalculate(dc:DataCollection):DataMapper={
    val v= in.chooseVariables(dc.keys.toSet)
    v.values.filterNot(dc.contains(_)).foreach(dc += _.asInstanceOf[Variable with Calculable].calculate(dc))

    new DataMapper(dc, v)
  }
  
  /**
   * choose a group of variables in a DataMapper set from a Datacollection
   *
   * @param  dss  DataCollection > the data collection
   * @return      VariableDataMap
   */
  def chooseVariablesAndComplete(dc:DataCollection):DataMapper={
    val v= in.chooseVariables(dc.keys.toSet)
    //v.values.toSeq.intersect(dc.toComplete).foreach(_.asInstanceOf[Variable with Calculable].complete(dc))
    v.values.toSeq.intersect(dc.toComplete).foreach(dc +=  _.asInstanceOf[Variable with Calculable].complete(dc))

    new DataMapper(dc, v)
  }
  
  /**
   * choose a group of DataSeries in a DataMapper set from a Datacollection
   *
   * @param  dss  DataCollection > the data collection
   * @return      VariablePairMap
   */
  def chooseSeries(dc:DataCollection):DataMapper={
    chooseVariablesAndCalculate(dc).dssDM
  }
  
  /**
   * choose a group of Parameters in a DataMapper set from a Datacollection
   *
   * @param  dss  DataCollection > the data collection
   * @return      VariablePairMap
   */
  def chooseParameters(dc:DataCollection):DataMapper={
    chooseVariablesAndCalculate(dc).parsDM
  }

  
  /**
   * calculate the variable DataSerie (abstract to override in each variable)
   *
   * @param   dss  DataCollection > the data collection
   * @return       Data
   */
  def calculate(dc:DataCollection):Data
  
   /**
   * calculate the last value of the variable DataSerie (abstract to override in each variable)
   *
   * @param   dss  DataCollection > the data collection
   * @return       Data
   */
  def complete(dc:DataCollection):Data
  
  /**
   * prints the required variables
   *
   * @return      String
   */
  def printRequiredVariables={
    if (in.size>0) "("+in.keys.mkString(", ")+")"
    else ""
  }
  
  /**
   * prints the required variables
   *
   * @return      String
   */
  def printRequiredVariablesJSON={
    
      if (in.size>0) "["+in.keys.map(_.abbr.mkString("\"","", "\"")).mkString(", ")+"]"
      else "[]"

  }
}

