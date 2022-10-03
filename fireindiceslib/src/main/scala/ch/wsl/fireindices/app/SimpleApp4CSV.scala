package ch.wsl.fireindices.app

import ch.wsl.fireindices.app.AppUtils._
import ch.wsl.fireindices.functions.Utils
import ch.wsl.fireindices.log.ReportLog
import ch.wsl.fireindices.metadata.Calculable
import ch.wsl.fireindices.metadata.DataCollection
import ch.wsl.fireindices.metadata.DataSerie
import ch.wsl.fireindices.metadata.DataSeries
import ch.wsl.fireindices.metadata.Parameters
import ch.wsl.fireindices.metadata.Serie
import ch.wsl.fireindices.metadata.Variable
import ch.wsl.fireindices.metadata._
import ch.wsl.fireindices.ImplicitConversions._

import com.typesafe.scalalogging.LazyLogging
import java.io.File
import java.sql._
import scala.collection.mutable.LinkedHashMap



/**
 * Simple class (without user interface) which contains the methods to read data
 * from a csv file, check it, calculate all possibles indices (calculable variable)
 * and save a file with the results and a file with the logs
 *
 * This can easily be used in a UI application to include the data processing logic.
 *
 */
class SimpleApp4CSV  extends SimpleApp4DB () {

  var inputFile:File = null

  Variable.load       //this initializes the Variable object (lazy) and the inner lists (values, ...)
  
  /**
   * Reads the header of a file and returns
   * -true if date column is found and all other column recognised
   * -false if not all column have been recognised
   *
   * @param  inputFile the input file
   * @return           the headers String
   */
  def readHeadersFile(inputFile:File, report:ReportLog = new ReportLog):Boolean={
     try{
      // load the driver into memory
      Class.forName("org.relique.jdbc.csv.CsvDriver")

      // create a connection. The first command line parameter is assumed to
      //  be the directory in which the .csv files are held
      val conn:Connection = DriverManager.getConnection("jdbc:relique:csv:" + inputFile.getParent)
      // create a Statement object to execute the query with
      val stmt:Statement = conn.createStatement()
      // Select the ID and NAME columns from sample.csv
      val rs:ResultSet = stmt.executeQuery("SELECT * FROM "+inputFile.getName.substring(0, inputFile.getName.length-4))


     readHeaders(rs, report)   

      //clean up
      rs.close()
      stmt.close()
      conn.close()
    }
    catch{
      case e:Exception => {
                            logger.error("ERROR: opening file connection  => " + e +"\n" + e.getStackTrace.map(_.toString).mkString("\n"))
//                            report.append("Problems while reading headers")
      } 
    }
    dt != null //& (headers.length==(nrcols-1))
  }
  
  
  /**
   * read all the recognized data from the input file
   *
   * @param  inputFile the input file
   * @param  settings  Parameters > the parameters (to check if all data are needed)
   * @param  dateSTART a string with first date of needed data
   * @param  dateEND   a string with last date of needed data
   * @param  dtformat  String for SimpleDateFormat to parse the dates
   * @return           a string with a report of the reading routine (for logs)
   */
  def readDataFile(inputFile:File, settings:Parameters,                        // [A] is just to help the compiler for Variable[types]
                  dateSTART:Option[String]=None,
                  dateEND:Option[String]=None, dtformat:String = dt.unit):String={
 
    this.inputFile = inputFile 
    logFileName = new File(inputFile.getParent+"/"+inputFile.getName.substring(0, inputFile.getName.length-4)+"_LOG.txt")
    jsonlogFileName = new File(inputFile.getParent+"/"+inputFile.getName.substring(0, inputFile.getName.length-4)+"_LOG.json")
    try{
        // load the driver into memory
        Class.forName("org.relique.jdbc.csv.CsvDriver")
        // create a connection. The first command line parameter is assumed to
        //  be the directory in which the .csv files are held
        val conn:Connection = DriverManager.getConnection("jdbc:relique:csv:" + inputFile.getParent)
        // create a Statement object to execute the query with
        val stmt:Statement = conn.createStatement()
        // Select the ID and NAME columns from sample.csv
        val rs:ResultSet = stmt.executeQuery("SELECT * FROM "+inputFile.getName.substring(0, inputFile.getName.length-4))

        readData(rs, settings, dateSTART, dateEND, dtformat)

        //clean up
        rs.close()
        stmt.close()
        conn.close()
        
    }catch{
        case e:Exception => logger.error("ERROR: opening file connection => " + e.getStackTrace.map(_.toString).mkString("\n"))  
    }
     ""           
  }


  /**
   * calculate all the possible variables & indices from inputs choice
   *
   * @param  Settings Parameter > all parameters needed for calculation
   * @return          report of calculation (for logs)
   */
  def calculateFile(settings:Parameters, report: ReportLog=new ReportLog(), vars:Seq[Variable with Calculable]=null):File={

    calculate(settings, report, vars)


    var outFilePath=""
    val outputTABLE:List[StringDataSerie] =
      dcAliens.values.map(_.crop(dc.dss.head._2.start, dc.dss.head._2.values.length)).toList :::
        dc.dss.ordered.map(_._2.getStringDataSerie).toList


    outFilePath = inputFile.getParent+"/"+inputFile.getName.substring(0, inputFile.getName.length-4)+"_RESULT.csv"
    val fileout = new java.io.FileWriter(outFilePath)

    try{

      printTable(DsDate, outputTABLE, ",",fileout,true)

    }catch{
      case e:Exception => logger.error("ERROR: DATA OUTPUT FILE=> " + e.getStackTrace.map(_.toString).mkString("\n"))
    }finally{
      fileout.close
    }

    return new File(outFilePath)
  }

  
  /**
   * complete all the possible variables & indices from inputs choice
   *
   * @param  Settings Parameter > all parameters needed for calculation
   * @return          report of calculation (for logs)
   */
  def completeFile(settings:Parameters, report: ReportLog=new ReportLog, vars2complete:Seq[Serie with Calculable]=null, vars2calculate:Seq[Serie with Calculable]=null, printOnlyLast:Boolean = true):File={

    calculate(settings, report, vars2calculate)
//    if (vars2calculate!= null) calculate(settings, report, vars2calculate)
    complete(settings, report, vars2complete, printOnlyLast)

    var outFilePath=""

    
    val outputTABLE = if (printOnlyLast) {
//                            dcAliens.map(_._2.lastDs()).toList ::: dc.dss.ordered.map(x => x._2.lastDs().getStringDataSerie).toList

                            dcAliens.map(_._2.lastDs()).toList ::: 
                            dc.dss.ordered.map(x => x._2.lastDs().getStringDataSerie).toList
                      }else{
//                            dcAliens.values.toList ::: dc.dss.ordered.map(_._2.getStringDataSerie).toList
                            
                            dcAliens.values.map(_.crop(dc.dss.head._2.start, dc.dss.head._2.values.length)).toList ::: 
                            dc.dss.ordered.map(_._2.getStringDataSerie).toList
                        
                      }
		
    outFilePath = inputFile.getParent+"/"+inputFile.getName.substring(0, inputFile.getName.length-4)+"_RESULT.csv"
    val fileout = new java.io.FileWriter(outFilePath)
    
    try{
      printTable(DsDate, outputTABLE, ",",fileout,true)
      
    }catch{
      case e:Exception => logger.error("ERROR: DATA OUTPUT FILE=> " + e.getStackTrace.map(_.toString).mkString("\n"))
    }finally{
      fileout.close
    }

   
    return new File(outFilePath)
  }

  /**
   * replace the already calculated values with freshly calculated (with complete) 
   * (to be used for few cases, since it is slow beacuse it internally uses complete) 
   *
   * @param  Settings Parameter > all parameters needed for calculation
   * @return          report of calculation (for logs)
   */
  def replaceFile(settings:Parameters, nr2replace: Int, vars:Seq[Serie with Calculable]=null, varsToSkeep:Seq[Serie]=null,
                  vars2calculate:Seq[Serie with Calculable]=null, report: ReportLog=new ReportLog, printOnlyLast:Boolean = true):File={

    calculate(settings, report, vars2calculate)

    replace(settings, nr2replace, vars, varsToSkeep, report)
         
    val outFilePath = inputFile.getParent+"/"+inputFile.getName.substring(0, inputFile.getName.length-4)+"_RESULT.csv"
    
    val outputTABLE = if (printOnlyLast) {
//                              dcAliens.map(_._2.lastDs()).toList ::: dc.dss.ordered.map(x => x._2.lastDs().getStringDataSerie).toList
                                dcAliens.map(_._2.lastDs()).toList ::: 
                                dc.dss.ordered.map(x => x._2.lastDs().getStringDataSerie).toList
                      }else{
//                                dcAliens.values.toList ::: dc.dss.ordered.map(_._2.getStringDataSerie).toList
                                dcAliens.values.map(_.crop(dc.dss.head._2.start, dc.dss.head._2.values.length)).toList ::: 
                                dc.dss.ordered.map(_._2.getStringDataSerie).toList
                      }
		
    val fileout = new java.io.FileWriter(outFilePath)
    
    try{
      printTable(DsDate, outputTABLE, ",",fileout,true)

    }catch{
      case e:Exception => logger.error("ERROR: DATA OUTPUT FILE=> " + e.getStackTrace.map(_.toString).mkString("\n"))
    }finally{
      fileout.close
    }


    return new File(outFilePath)
  }

}
