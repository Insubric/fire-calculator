package ch.wsl.fireindices.app

import java.io.File
import java.io.FileWriter
import java.sql.ResultSet
import java.text.SimpleDateFormat
import java.util.Date
import ch.wsl.fireindices.metadata.StringDataSerie
import ch.wsl.fireindices.metadata.Variable
import ch.wsl.fireindices.functions.Utils
import ch.wsl.fireindices.metadata.DataSerie
import ch.wsl.fireindices.metadata.Null


/**
 * Collection of utillities for the application
 *
 */
object AppUtils{

  val NotAvailableInput:String = ""

  
  /**
   * Read and return a field for a given ResultSet
   *
   * @param  results ResultSet > ResultSet containing fields
   * @param  field   Field Name
   * @param  v       Variable > only to get field type [T]
   * @return         T > data of type [T]
   */
  def readField[T](results:ResultSet, field:String, v:Variable):T = {

     val a=v.c.toString match{
         case  "double"  => {if (results.getString(field)==NotAvailableInput) Null.Double
                                      else results.getDouble(field)}
         case  "long"  => {if (results.getString(field)==NotAvailableInput) Null.Long
                                      else results.getLong(field)}
         case  "int"  => {if (results.getString(field)==NotAvailableInput) Null.Int
                                      else results.getInt(field)}
         case _ => null
     }
     a.asInstanceOf[T]
  }
  
  /**
   * Get the first date in the input file
   *
   * @param  inputFile the input file
   * @param  dtVar     date time variable
   * @return           the first date
   */
  def getFirstDate(inputFile:File, dtVar: Variable):Date = {

        Class.forName("org.relique.jdbc.csv.CsvDriver")
        val conn: java.sql.Connection  = java.sql.DriverManager.getConnection("jdbc:relique:csv:" + inputFile.getParent)
        val stmt: java.sql.Statement  = conn.createStatement()
        val results: java.sql.ResultSet = stmt.executeQuery("SELECT "+ dtVar.abbr+" from "+inputFile.getName.substring(0, inputFile.getName.length-4))
        results.next
        val Date = new SimpleDateFormat(dtVar.unit).parse(results.getString(dtVar.abbr))

        results.close()
        stmt.close()
        conn.close()

        //Calculate Effective Average
        return Date

  }
  
  /**
   * Get the last date in the input file
   *
   * @param  inputFile the input file
   * @param  dtVar     date time variable 
   * @return           the last date
   */
  def getLastDate(inputFile:File, dtVar: Variable):Date = {
        var dateStr = ""
        Class.forName("org.relique.jdbc.csv.CsvDriver")
        val conn: java.sql.Connection  = java.sql.DriverManager.getConnection("jdbc:relique:csv:" + inputFile.getParent)
        val stmt: java.sql.Statement  = conn.createStatement()
        val results: java.sql.ResultSet = stmt.executeQuery("SELECT "+ dtVar.abbr+" from "+inputFile.getName.substring(0, inputFile.getName.length-4))
        while (results.next) {
          dateStr = results.getString(dtVar.abbr)
        } 
//        results.last   //not allowed fo forward only recordsets
        val date = 
        results.close()
        stmt.close()
        conn.close()

        //Calculate Effective Average
        return new SimpleDateFormat(dtVar.unit).parse(dateStr)
  }

  /**
   * print all the table (List[Dataserie]) in a file
   *
   * @param  table     List[DataSerie] > the table
   * @param  sep       a string to separate each field
   * @param  fileout   FileWriter > File where you want to print
   * @param  withHead  Boolean > to print or not with headers
   * @return           Unit
   */
  def printTable (dateDs:DataSerie, table:List[StringDataSerie], sep:String, fileout:FileWriter, withHead:Boolean):Unit = {
 
        val dates =  table.head.getDates  //get dates from first serie of data
     
        if (withHead) fileout.write(dateDs.variable.abbr + sep + table.map(_.name).mkString(sep) + "\n" )

        val newTable = (dates.map(Utils.solarDate2String(_, dateDs.variable.unit))::
                        table.map(_.values)).transpose
                
        fileout.write(newTable.map(_.mkString(sep)).mkString("\n"))
        
  }
}
