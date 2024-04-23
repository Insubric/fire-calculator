package ch.wsl.fireindices.app

import ch.wsl.fireindices.functions.Utils
import ch.wsl.fireindices.model.{FCHeader, FCRow}

import java.util
import scala.collection.mutable.ListBuffer
import scala.util.Try
//import ch.wsl.fireindices.functions.Lambda
import ch.wsl.fireindices.log.CheckLog
import ch.wsl.fireindices.log.DataLog
import ch.wsl.fireindices.log.HeadersLog
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
 * from a recordset, check it, calculate all possibles indices (calculable variable)
 * and save a file with the results and a file with the logs
 *
 * This can easily be used in other applications to include the data processing logic.
 *
 */
abstract class SimpleApp4DB extends LazyLogging  with SimpleApp {

  var logFileName:File = null
  var jsonlogFileName:File = null
  var DsDate = new DataSerie(ch.wsl.fireindices.metadata.Date, 0L::0L::Nil, 0D::0D::Nil, 
                             DataLog(ch.wsl.fireindices.metadata.Date.abbr))

  var dcAliens = new LinkedHashMap[String, StringDataSerie]    //holds the columns not recognized
  var stillToCalculate:Seq[Variable] = null   //used only for the tests and calculated report
  var stillToComplete:Seq[Variable] = null   //used only for the tests and completed report
  var stillToCalculatePars:Seq[Variable] = null   //used only for the tests and calculated report
  
  Variable.load       //this initializes the Variable object (lazy) and the inner lists (values, ...)
  
  /**
   * Reads the header of a file and returns
   * -true if date column is found and all other column recognised
   * -false if not all column have been recognised
   *
   * @param  rs        the input ResultSet
   * @return           boolean if date column exists
   */
  def readHeaders(rs:ResultSet, report:ReportLog = new ReportLog):Option[FCHeader]={

    try{

      val columns = for (i <- 1 to rs.getMetaData.getColumnCount) yield rs.getMetaData.getColumnName(i)

      val h = setHeaders(columns,report)

      h

    }
    catch{
      case e:Exception => {
                            logger.error("ERROR: while reading headers  => " + e +"\n" + e.getStackTrace.map(_.toString).mkString("\n"))
//                            report.append("Problems while reading headers")
        None
      }
    }

  }
  

  /**
   * read all the recognized data from the input file
   *
   * @param  rs        the input RecordSet
   * @param  settings  Parameters > the parameters (to check if all data are needed)
   * @param  dateSTART a string with first date of needed data
   * @param  dateEND   a string with last date of needed data
   * @param  dtformat  String for SimpleDateFormat to parse the dates
   * @return           a string with a report of the reading routine (for logs)
   */
  def readData(headers:FCHeader,rs:ResultSet, settings:Parameters,
                  dateSTART:Option[String]=None,
                  dateEND:Option[String]=None):DataCollection={

    val sdt = if (dateSTART.isDefined) Utils.solarDate2Long(dateSTART.get, headers.serie.unit) else 0
    val edt = if (dateEND.isDefined) Utils.solarDate2Long(dateEND.get, headers.serie.unit) else Long.MaxValue


    //initial lists
    var xDATE:ListBuffer[Long] = new ListBuffer
    var xMap = new DataSeries
    val data = ListBuffer[FCRow]()

    try{
      headers.defined.foreach(x => {
          val v=Variable.getByAbbrCaseInsensitive(x).asInstanceOf[Serie]
          xMap += new DataSerie(v, 0, 0, v.getEmptyList) 
        })
      headers.undefined.foreach(x => {
          val sds = new StringDataSerie(x, 0, 0, List[String]())               
          dcAliens.put(x, sds )
        })

      
      while (rs.next())
      {
        val d=Utils.solarDate2Long(rs.getString(headers.serie.abbr), headers.serie.unit)
        if (d>=sdt && d<=edt){
          xDATE += d
          val variables = for (colName <- headers.defined) yield {
            colName -> Try(rs.getString(colName).toDouble).getOrElse(Double.NaN)
          }
          data.addOne(FCRow(d,variables.toMap))
          for (colName <- headers.undefined){
            dcAliens(colName).readField(rs, colName)
          }
        }
      }
    }
    catch{
      case e:Exception => logger.error("ERROR: while READING INPUT ResultSet => " + e + "\n"+ e.getStackTrace.map(_.toString).mkString("\n"))
    }

    val listDate = xDATE.toList

    //Creating DataSeries
    DsDate = new DataSerie(headers.serie, listDate, listDate)

	for (colName <- headers.undefined){
		dcAliens(colName) = new StringDataSerie(colName, listDate,dcAliens(colName).values.reverse)
	}

    setData(headers,data.toSeq,settings)
  }


  

  



  def writeLog(log:ReportLog, text:Boolean=true, json:Boolean=false)={
    try{
//      val logPath = logPath //+"/"+"_LOG.txt"
      if (text) {
        val logW = new java.io.FileWriter(logFileName)
        logW.write(log.formatLog)
        logW.close
      }
      if (json){
        val logWj = new java.io.FileWriter(jsonlogFileName)
//        logWj.write(log.formatJson)
        logWj.write(log.formatLog)
        logWj.close
      }
    }
    catch{
      case e:Exception => logger.error("ERROR writing LOG OUTPUT FILES=> " + e)
    }
  }
}
