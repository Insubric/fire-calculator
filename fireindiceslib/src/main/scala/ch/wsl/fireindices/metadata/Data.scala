

package ch.wsl.fireindices.metadata

/**
 * Core class (abstract) to hold data, with a definition (variable).
 *
 **/
import ch.wsl.fireindices.log.DataLog

abstract class Data(val variable: Variable, val log: DataLog, val notes:String = "") {
 
  /**
   * clones the Data
   *
   * @return            Data
   */
   //def clone:Data
	  
  
  /**
   * Returns a string with a report of Data object (abstract)
   *
   * @return         String
   */
  def getCheckReport(dateFormat:String):String
  
  def logWithNotes = log.addNotes(notes)
}
