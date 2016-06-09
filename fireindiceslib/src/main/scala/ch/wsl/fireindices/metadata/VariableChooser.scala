
package ch.wsl.fireindices.metadata

/**
 * Collection of Variable sequences, storing the order of variable selection for computations.
 * Has methods to check for available variables if it contains the required variables or if
 * it can compute them.
 *
 **/
class VariableChooser extends KeySet[Variable, Seq[Variable]]{

  
  /**
   * build key for new Variable in KeySet
   *
   * @param  seq   Seq[Variable] > a sequence of variables
   * @return       Variable >
   */
  def buildKey(seq:Seq[Variable]):Variable = seq.head

  
  /**
   * check if Set of Variables contains inputs necessary to calculate
   *
   * @param  availableVariables  Set[Variable]> the set containing variables
   * @return                     Boolean
   */
  def hasInputs(availableVariables:Set[Variable]):Boolean={
    var canCalc = true
    for(seq <- this.values){
      var hasVariable = false
      for (el <- seq)
        if (availableVariables.contains(el)) hasVariable=true
      if (hasVariable==false) canCalc = false
    }
    canCalc
  }
  
  /**
   * check if Set of Variables contains inputs necessary to calculate,
   * by looking also if a specified input can be calculated
   *
   * @param  availableVariables  Set[Variable]> the set containing variables
   * @return                     Boolean
   */
  def canCalculate(availableVariables:Set[Variable]):Boolean={
    var canCalc = true
    for(seq <- this.values){
      var hasVariable = false
      for (el <- seq)
        if (availableVariables.contains(el)) hasVariable=true
        else if (el.isInstanceOf[Variable with Calculable]){
          if (el.asInstanceOf[Variable with Calculable].canCalculate(availableVariables)) hasVariable=true
        }
      if (hasVariable==false) canCalc = false
    }
    canCalc
  }
  
  /**
   * return Variable if this variable is available or calculable
   *
   * @param  variable            Variable > Varaible needed
   * @param  availableVariables  Set[Variable] > set of Variable availables
   * @return                     Variable
   */
  def chooseCalculableVariable(variable:Variable, availableVariables:Set[Variable]):Variable={
    var selected:Variable = null
    if (this.contains(variable))
      for (el <- this(variable))
        if (selected==null)
          if (availableVariables.contains(el)) selected=el.asInstanceOf[Variable]
          else if (el.isInstanceOf[Variable with Calculable]){
            if (el.asInstanceOf[Variable with Calculable].canCalculate(availableVariables)) selected=el.asInstanceOf[Variable]
      }
    selected
  }
  
  /**
   * return Variable if this variable is available
   *
   * @param  variable            Variable > Varaible needed
   * @param  availableVariables  Set[Variable] > set of Variable availables
   * @return                     Variable
   */
  def chooseVariable(variable:Variable, availableVariables:Set[Variable]):Variable={
    var selected:Variable = null
    if (this.contains(variable))
      for (el <- this(variable))
        if (selected==null && availableVariables.contains(el)) selected=el.asInstanceOf[Variable]
    selected
  }
  
  /**
   * return VariablePairs containing all variables necessary to calculate another variable
   *
   * @param  availableVariables  Set[Variable] > set of Variable availables
   * @return                     VariablePairs
   */
  def chooseVariables(availableVariables:Set[Variable]):VariablePairs={
      val m = new VariablePairs
      m ++= this.mapValues(x => chooseCalculableVariable(x.head,availableVariables))
  }
  
  /**
   * return VariablePairs containing all variables necessary to calculate another variable
   *
   * @param  availableVariables  Set[Variable] > set of Variable availables
   * @return                     VariablePairs
   */
  def chooseSeries(availableVariables:Set[Variable]):VariablePairs={
    chooseVariables(availableVariables).filterKeys(_.isInstanceOf[Serie]).toMap[Variable,Variable].asInstanceOf[VariablePairs]
  }
  
  /**
   * return VariablePairs containing all parameters necessary to calculate another variable
   *
   * @param  availableVariables  Set[Variable] > set of Variable availables
   * @return                     VariablePairs
   */
  def chooseParameters(availableVariables:Set[Variable]):VariablePairs={
    chooseVariables(availableVariables).filterKeys(!_.isInstanceOf[Serie]).toMap[Variable,Variable].asInstanceOf[VariablePairs]
  }
}
