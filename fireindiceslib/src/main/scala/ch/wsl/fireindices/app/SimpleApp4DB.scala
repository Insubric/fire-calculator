package ch.wsl.fireindices.app

import ch.wsl.fireindices.functions.Utils

import java.util
import scala.collection.mutable.ListBuffer
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
class SimpleApp4DB extends LazyLogging {

  var logFileName:File = null
  var jsonlogFileName:File = null
  var nullDateList = 0D::Nil
  var headers = new ListBuffer[String]
  var otherHeaders = new ListBuffer[String]   //holds the header of the columns not recognized
  var DsDate = new DataSerie(ch.wsl.fireindices.metadata.Date, 0L::0L::Nil, 0D::0D::Nil, 
                             DataLog(ch.wsl.fireindices.metadata.Date.abbr))
  var dc = new DataCollection
  var dcAliens = new LinkedHashMap[String, StringDataSerie]    //holds the columns not recognized
  var dt:Serie = null                                    //holds variable of date time column 
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
  def readHeaders(rs:ResultSet, report:ReportLog = new ReportLog):Boolean={
    
    var nrcols:Int=0

    try{
 
      for (i <- 1 to rs.getMetaData.getColumnCount){
        val colName = rs.getMetaData.getColumnName(i)
        if (Variable.dateExistsByAbbr(colName)) 
            dt = Variable.getByAbbrCaseInsensitive(colName).asInstanceOf[Serie]
        else {
          if (Variable.existsByAbbrCaseInsensitive(colName))
              headers += colName
          else
              otherHeaders += colName
        }
      }
      nrcols=rs.getMetaData.getColumnCount
      report.headers = new HeadersLog(nrcols, headers.toList,
                                      if (dt != null) dt.abbr else "")

    }
    catch{
      case e:Exception => {
                            logger.error("ERROR: while reading headers  => " + e +"\n" + e.getStackTrace.map(_.toString).mkString("\n"))
//                            report.append("Problems while reading headers")
      }
    }
    dt != null //& (headers.length==(nrcols-1))
  }
  
  /**
   * return a report with the headers recognized for the calculation
   *
   * @param  colNumber number of headers contained
   * @return           the headers report string
   */
  def headersReport(colNumber:Int):HeadersLog={

//    val readColumns = (headers:\"")(_ +"  "+_)
//    val fractionRecognizedColumns = " (" + headers.length + " of " +
//        (colNumber-1) +")"
//    return "Recognized columns"+fractionRecognizedColumns+": " + readColumns +"\n"
//    
    new HeadersLog(colNumber, headers.toList)
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
  def readData(rs:ResultSet, settings:Parameters,                        
                  dateSTART:Option[String]=None,
                  dateEND:Option[String]=None, dtformat:String = dt.unit):String={

    val sdt = if (dateSTART.isDefined) Utils.solarDate2Long(dateSTART.get, dtformat) else 0
    val edt = if (dateEND.isDefined) Utils.solarDate2Long(dateEND.get, dtformat) else Long.MaxValue

    dc.removePars
    dc ++= settings

    //initial lists
    var xDATE:ListBuffer[Long] = new ListBuffer
    var xMap = new DataSeries 
 
    try{
      headers.foreach(x => {
          val v=Variable.getByAbbrCaseInsensitive(x).asInstanceOf[Serie]
          xMap += new DataSerie(v, 0, 0, v.getEmptyList) 
        })
      otherHeaders.foreach(x => {
          val sds = new StringDataSerie(x, 0, 0, List[String]())               
          dcAliens.put(x, sds )
        })
      
      
      while (rs.next())
      {
        val d=Utils.solarDate2Long(rs.getString(dt.abbr), dt.unit)
        if (d>=sdt && d<=edt){
          xDATE += d
          for (colName <- headers){
            val v=Variable.getByAbbrCaseInsensitive(colName).asInstanceOf[Serie]
            xMap(v).readField(rs, colName)
          }
          for (colName <- otherHeaders){			  
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
    DsDate = new DataSerie(dt, listDate, listDate)
    for (colName <- headers){
      val dsv=xMap.getCaseInsensitive(colName).asInstanceOf[DataSerie]
      dc += dsv.variable.createDataSerieFromDates(listDate, dsv.values.reverse)//.toList)
      
    }
    
	for (colName <- otherHeaders){
		dcAliens(colName) = new StringDataSerie(colName, listDate,dcAliens(colName).values.reverse)
	}

    stillToCalculate = dc.toCalculate    //used only for the tests and to keep track of variables that have been calculated
    stillToComplete = dc.toComplete    //used only for the tests and to keep track of variables that have been calculated
    ""
  }


  /**
   * return a String with a list of calculables variables
   *
   * @return     String
   */
  def calculableReport:String={

    val s="Can calculate: "+dc.calculables.mkString(",")
    s
  }
  
  /**
   * return a string with the already calculated variables
   * 
   * @return    String
   */
  def calculatedReport:String={

    "Calculated variables\n" +
//    dc.ordered.filter(y => stillToCalculate.contains(y._1)).values.map(x => "\t" + x.variable.abbr+": "+x.notes).mkString("\n") +"\n"
    dc.ordered.filter(y => stillToCalculate.contains(y._1)).values.map(x => "\"" + x.variable.abbr+"\": {"+x.notes).mkString("{", "},\n", "},") +"\n"
  }
  
  /**
   * return a string with the variables where only the last value has to be calculated
   *}
   * @return    String
   */
  def completedReport:String={

    "Completed variables\n" +
    dc.ordered.filter(y => stillToComplete.contains(y._1)).filterNot(x => dc.toComplete.contains(x._1)).values.map(x => "\t" + x.variable.abbr+": "+x.notes).mkString("\n") +"\n"
  }
  
  
  /**
   * perform a check on the input variables and return a report
   *
   * @return     report check
   */
  def check(report:ReportLog = new ReportLog):Boolean={

    dc.dss.values.foreach(_.checkValues)
    val problems = dc.values.map(_.getCheckReport(DsDate.variable.unit)).filterNot(_.size==0)

    val isOk = if (problems.size>0) {
                  report.check = CheckLog("\t"+problems.mkString("\n").replaceAll("\n", "\n\t"))
                  false
                }else{
                  report.check = CheckLog("")
                  true
                }  
    isOk
  }
  
  /**
   * calculate all the possible variables & indices from inputs choice
   *
   * @param  Settings Parameter > all parameters needed for calculation
   * @return          report of calculation (for logs)
   */
//  def calculate(settings:Parameters, report: StringBuffer=new StringBuffer, vars:Seq[Variable with Calculable]=null):DataCollection={
  def calculate(settings:Parameters, report: ReportLog=new ReportLog(), vars:Seq[Variable with Calculable]=null):DataCollection={

    dc.removePars
    dc ++= settings

    try{
      if (vars==null)
        dc.calculate()
      else
        dc.calculate(vars)
			  
    }catch{
      case e:Exception => {
        logger.error("ERROR: while reading headers  => " + e +"\n" + e.getStackTrace.map(_.toString).mkString("\n"))
        //                            report.append("Problems while reading headers")
      }
    }
    
    val calcSeries = dc.ordered.filter(y => stillToCalculate.contains(y._1)).values
    val calcPars = new Parameters(dc.ordered.pars.filter(x => stillToCalculate.contains(x._1)).values).values
    report.series_calculated = calcSeries.map(_.logWithNotes).toList
    report.parameters_calculated = calcPars.map(_.logWithNotes).toList
    report.parameters_given  = settings.ordered.values.map(_.toParamLog).toList
    dc
  }
  
  /**
   * complete all the possible variables & indices from inputs choice
   *
   * @param  Settings Parameter > all parameters needed for calculation
   * @return          report of calculation (for logs)
   */
  def complete(settings:Parameters, report: ReportLog=new ReportLog, vars:Seq[Serie with Calculable]=null, printOnlyLast:Boolean = true):DataCollection={

    dc.removePars
    dc ++= settings

    try{
		if (vars==null)
			dc.complete()
		else
			dc.complete(vars)
      
    }catch{
      case e:Exception => logger.error(e + "\n"+ e.getStackTrace.map(_.toString).mkString("\n"))
    }

    val complSeries = dc.ordered.filter(y => stillToComplete.contains(y._1)).filterNot(x => dc.toComplete.contains(x._1)).values
    report.series_completed = complSeries.map(_.logWithNotes).toList.asInstanceOf[List[DataLog]]
    if (report.parameters_given.length ==0)   report.parameters_given  = settings.ordered.values.map(_.toParamLog).toList
    dc
  }
  
  
  
   /**
   * replace the already calculated values with null values 
   *
   * @param  Settings Parameter > all parameters needed for calculation
   * @return          report of calculation (for logs)
   */
  def setToNull(settings:Parameters, nr2replace: Int, vars:Seq[Serie with Calculable]=null, varsToSkip:Seq[Serie]=null, report: ReportLog=new ReportLog):DataCollection={

    dc.removePars
    dc ++= settings
    var outFilePath=""
    
    val ix = dc.dateDs.length - nr2replace



    val varsToCalc:Seq[Serie] = {
          if (vars==null) {

                if (varsToSkip==null) {
                    dc.dss.filter(_._1.isInstanceOf[Calculable]).map(_._1).toSeq
                }else {
                    dc.dss.map(_._1).toSeq.diff(varsToSkip).filter(_.isInstanceOf[Calculable])
                }
          } else {
                vars         
          }
    }
//    logger.debug(varsToCalc.map(_.abbr).mkString(" - "))
    dc.dss.filter(x=>varsToCalc.contains(x._1)).map(_._2).foreach(x =>  x.values = x.values.take(ix):::(List.fill(nr2replace)(Double.NaN) ))  //eliminate values to replace
    
//    logger.debug(Utils.solarDate2String(dc.dss.dateDs.getDate(ix)) +"   nrdata "+ dc.dss(H).length)
    
    stillToComplete = dc.toComplete
    
    dc    
  }
  
    
   /**
   * replace the already calculated values with freshly calculated (with complete) 
   * (to be used for few cases, since it is slow beacuse it internally uses complete) 
   *
   * @param  Settings Parameter > all parameters needed for calculation
   * @return          report of calculation (for logs)
   */
  def replace(settings:Parameters, nr2replace: Int, vars:Seq[Serie with Calculable]=null, varsToSkip:Seq[Serie]=null, report: ReportLog=new ReportLog):DataCollection={
    
        setToNull(settings, nr2replace, vars, varsToSkip, report)
        val ix = dc.dateDs.length - nr2replace
        val varsToCalc:Seq[Serie] = {
                if (vars==null) {
                      if (varsToSkip==null) {
                        dc.dss.filter(_._1.isInstanceOf[Calculable]).map(_._1).toSeq
                       }else{
                        dc.dss.map(_._1).toSeq.diff(varsToSkip).filter(_.isInstanceOf[Calculable])
                       }
                }else{
                      vars
                }
        }
        val copydc = dc.cloneAll   
        copydc.dss.map(_._2).foreach(x =>  x.values = x.values.take(ix))  //take only until the last complete row


        for (i <- Range(ix, dc.dss.dateDs.length)){

          for (ds <- copydc.dss.map(_._2)){                     //add data from dc for the next day
                  if (varsToCalc.contains(ds.variable)){
                          ds.values = ds.values:::Double.NaN::Nil
                  } else {                  
                          ds.values = ds.values:::dc.dss(ds.variable).values(i)::Nil
                  }				
          }

          try{
             copydc.complete(varsToCalc.map(_.asInstanceOf[Serie with Calculable]))
    //         copydc.forceComplete()
    //         copydc.complete()
          }catch{
            case e:Exception => logger.error(e + "\n"+ e.getStackTrace.map(_.toString).mkString("\n"))
          }     

        }

        dc = copydc
        
        val complSeries = dc.ordered.filter(y => stillToComplete.contains(y._1)).filterNot(x => dc.toComplete.contains(x._1)).values
        report.series_replaced = complSeries.map(_.logWithNotes).toList
        report.n_replaced = nr2replace
        if (report.parameters_given.length==0) report.parameters_given  = settings.ordered.values.map(_.toParamLog).toList
        dc
  }
//   /**
//   * replace the already calculated values with freshly calculated (with complete) 
//   * (to be used for few cases, since it is slow beacuse it internally uses complete) 
//   *
//   * @param  Settings Parameter > all parameters needed for calculation
//   * @return          report of calculation (for logs)
//   */
//  def replaceBoosted(settings:Parameters, nr2replace: Int, varsToSkip:Seq[Serie]=null, report: StringBuffer=new StringBuffer):DataCollection={
//    
//        setToNull(settings, nr2replace, varsToSkip, report)
//        val ix = dc.dateDs.length - nr2replace
//        val varsToCalc:Seq[Serie] = if (varsToSkip==null) {
//                          dc.dss.filter(_._1.isInstanceOf[Calculable]).map(_._1).toSeq
//                         }else{
//                          dc.dss.map(_._1).toSeq.diff(varsToSkip).filter(_.isInstanceOf[Calculable])
//                         }
//                      
//        val copydc = dc.cloneAll   
//        copydc.dss.map(_._2).foreach(x =>  x.values = x.values.takeRight(ix+1))  //take only from the last complete row
//
//    //    logger.debug(Utils.solarDate2String(copydc.dss.dateDs.getDate(ix)) +"   nrdata "+ copydc.dss(H).length)
//
//        copydc.dss.filter(v => varsToCalc.contains(v._1)).map(_._2.)
//        
//
//        for (i <- 2 to ix+1 ){
//
//
//          try{
//             copydc.complete(varsToCalc.map(_.asInstanceOf[Serie with Calculable]))
//    //         copydc.forceComplete()
//    //         copydc.complete()
//          }catch{
//            case e:Exception => e + "\n"+ e.printStackTrace
//          }     
//
//        }
//
//        dc = copydc
//        report.append(completedReport+"\n")
//        report.append("Parameters given \n" + settings.ordered.print("\t") + "\n")
//        dc
//  }
  

  /**
   * replace null values with zero for the given series
   *
   * @param  vars  > series to replace      
   */  
  def replaceNulls(vars:Seq[Serie]) = {
    for (variable <- vars)
      dc.dss(variable).values = dc.dss(variable).values.map(x => if (x.equals(Double.NaN)) 0.0 else x)
  }

//  
//  /**
//   * calculate the output of a MaxEnt model based on a lambda file string 
//   * (on the last value)
//   *
//   * @param  lambda           > String with the content of a lambda file
//   * @return          MaxEnt model output
//   */  
//  def calculateLambda(lambda:String):Double = {
//	  Lambda(lambda, dc.dss).result
//	  
//  }
//  /**
//   * calculate the output of a MaxEnt model based on a lambda file string
//   *
//   * @param  lambda           > String with the content of a lambda file
//   * @param  indexBeforeLast  > Int indicating the index of the row to be taken 
//   *                            (nr from last,  last=1)
//   * @return          MaxEnt model output
//   */  
//  def calculateLambda(lambda:String, indexBeforeLast:Int):Double = {
//	  Lambda(lambda, dc.dss, indexBeforeLast).result
//	  
//  }
//  /**
//   * calculate the output of a MaxEnt model based on a lambda file string on 
//   * a range of last values
//   *
//   * @param  lambda           > String with the content of a lambda file
//   * @param  indexBeforeLast  > Int indicating the index of the row to be taken 
//   *                            (nr from last,  last=1)
//   * @return          MaxEnt model output
//   */  
//  def calculateLambdas(lambda:String, indexBeforeLast:Int):List[Double] = {
//	  Range(indexBeforeLast, 0, -1).toList.map(i => Lambda(lambda, dc.dss, i).result)
//	  
//  }
  
  
  

  
  /**
   * print log file of the reading, check, calculation routines
   *
   * @param  log    a String containing all logs
   * @return        Unit
   */
//  def printLog(log:String)={
//    try{
////      val logPath = logPath //+"/"+"_LOG.txt"
//      val logW = new java.io.FileWriter(logFileName)
//      logW.write(log)
//      logW.close
//    }
//    catch{
//      case e:Exception => logger.error("ERROR printing LOG OUTPUT FILE=> " + e)
//    }
//  }
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
