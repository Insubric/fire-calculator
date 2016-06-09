/*
 * currently not used
 * 
 */

package ch.wsl.fireindices.metadata

import scala.collection.mutable.HashMap

/**
 * Not used. To be completed.
 *
 **/
class FormulaChooser extends HashMap[Any, VariableChooser]{   //Any is for a  partially applied function
  
  /**
   * Check if a variable is calculable or not
   *
   * @param          Set[Variable] > all variables
   * @return         Boolean > result of check
   */
  def canCalculate(variables:Set[Variable])={            //check if it can calculate at least one formula
    this.values.map(_.canCalculate(variables)).exists(_==true)
  }

}
