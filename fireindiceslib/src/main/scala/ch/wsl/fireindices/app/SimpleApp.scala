package ch.wsl.fireindices.app

import ch.wsl.fireindices.functions.Utils
import ch.wsl.fireindices.log.{CheckLog, DataLog, HeadersLog, ReportLog}
import ch.wsl.fireindices.metadata.{Calculable, DataCollection, DataSerie, DataSeries, Parameters, Serie, StringDataSerie, Variable}
import ch.wsl.fireindices.model.{FCHeader, FCRow}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable.ListBuffer



trait SimpleApp extends LazyLogging {
  def setHeaders(headers:Seq[String],report: ReportLog) = {

    val _headers = ListBuffer[String]()
    val otherHeaders = ListBuffer[String]()
    var dateColumn = Option.empty[String]

    headers.foreach { colName =>
      if (Variable.dateExistsByAbbr(colName))
        dateColumn = Some(colName)
      else {
        if (Variable.existsByAbbrCaseInsensitive(colName))
          _headers += colName
        else
          otherHeaders += colName
      }
    }

    report.headers = new HeadersLog(headers.length, headers.toList, dateColumn.toString)


    dateColumn.map(dc => FCHeader(dc,_headers.toSeq,otherHeaders.toSeq))
  }

  def setData(headers: FCHeader, data: Seq[FCRow], settings: Parameters): DataCollection = {

    val dc = new DataCollection()
    dc ++= settings

    //initial lists
    val xMap = new DataSeries


    headers.defined.foreach(x => {
      val v = Variable.getByAbbrCaseInsensitive(x).asInstanceOf[Serie]
      xMap += new DataSerie(v, 0, 0, v.getEmptyList)
    })


    data.foreach { row =>
      for (colName <- headers.defined) {
        val v = Variable.getByAbbrCaseInsensitive(colName).asInstanceOf[Serie]
        xMap(v).insertOneRow(row.variables(colName))
      }

    }


    //Creating DataSeries
    for (colName <- headers.defined) {
      val dsv = xMap.getCaseInsensitive(colName).asInstanceOf[DataSerie]
      dc += dsv.variable.createDataSerieFromDates(data.map(_.date).toList, dsv.values.reverse) //.toList)

    }

    dc
  }



  /**
   * calculate all the possible variables & indices from inputs choice
   *
   * @param  Settings Parameter > all parameters needed for calculation
   * @return          report of calculation (for logs)
   */
  //  def calculate(settings:Parameters, report: StringBuffer=new StringBuffer, vars:Seq[Variable with Calculable]=null):DataCollection={
  def calculate(dc:DataCollection,settings:Parameters, report: ReportLog=new ReportLog(), vars:Seq[Variable with Calculable]=null):DataCollection={

    dc.removePars
    dc ++= settings

    try{
      if (vars==null)
        dc.calculate()
      else
        dc.calculate(vars)

    }catch{
      case e:Exception => {
        logger.error("ERROR: while calculate  => " + e +"\n" + e.getStackTrace.map(_.toString).mkString("\n") )
        logger.error(s""" Data collection: $dc """)
        //                            report.append("Problems while reading headers")
      }
    }

    val calcSeries = dc.ordered.filter(y => dc.toCalculate.contains(y._1)).values
    val calcPars = new Parameters(dc.ordered.pars.filter(x => dc.toCalculate.contains(x._1)).values).values
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
  def complete(dc:DataCollection,settings:Parameters, report: ReportLog=new ReportLog, vars:Seq[Serie with Calculable]=null, printOnlyLast:Boolean = true):DataCollection={

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

    val complSeries = dc.ordered.filter(y => dc.toComplete.contains(y._1)).filterNot(x => dc.toComplete.contains(x._1)).values
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
  def setToNull(dc:DataCollection,settings:Parameters, nr2replace: Int, vars:Seq[Serie with Calculable]=null, varsToSkip:Seq[Serie]=null, report: ReportLog=new ReportLog):DataCollection={

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

    dc
  }


  /**
   * replace the already calculated values with freshly calculated (with complete)
   * (to be used for few cases, since it is slow beacuse it internally uses complete)
   *
   * @param  Settings Parameter > all parameters needed for calculation
   * @return          report of calculation (for logs)
   */
  def replace(_dc:DataCollection,settings:Parameters, nr2replace: Int, vars:Seq[Serie with Calculable]=null, varsToSkip:Seq[Serie]=null, report: ReportLog=new ReportLog):DataCollection={

    val dc = setToNull(_dc,settings, nr2replace, vars, varsToSkip, report)
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


    val complSeries = copydc.ordered.filter(y => copydc.toComplete.contains(y._1)).filterNot(x => dc.toComplete.contains(x._1)).values
    report.series_replaced = complSeries.map(_.logWithNotes).toList
    report.n_replaced = nr2replace
    if (report.parameters_given.length==0) report.parameters_given  = settings.ordered.values.map(_.toParamLog).toList
    copydc
  }

  /**
   * perform a check on the input variables and return a report
   *
   * @return     report check
   */
  def check(dc:DataCollection, headers: FCHeader, report:ReportLog = new ReportLog):Boolean={

    dc.dss.values.foreach(_.checkValues)
    val problems = dc.values.map(_.getCheckReport(headers.serie.unit)).filter(_.nonEmpty)

    val isOk = if (problems.nonEmpty) {
      report.check = CheckLog("\t"+problems.mkString("\n").replaceAll("\n", "\n\t"))
      false
    }else{
      report.check = CheckLog("")
      true
    }
    isOk
  }



}
