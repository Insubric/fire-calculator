package ch.wsl.fireindices.app

import ch.wsl.fireindices.log.ReportLog
import ch.wsl.fireindices.metadata.{DataSerie, Parameter, Parameters, Variable}
import ch.wsl.fireindices.model.FCRow

object FireCalculator {

  private val app = new SimpleApp {}

  def calculate(headers:Seq[String],parameters:Map[String,Double],data:Seq[FCRow]) = {
    val report = ReportLog()
    val params = new Parameters()

    parameters.foreach{ case (variable,value) =>
      params.addIfNotNull(Variable.getByAbbrCaseInsensitive(variable),value)
    }

    params.completeWithDefaults

    val result =  Timer.mesure() {
      val _headers = app.setHeaders(headers, report)
      val _data = app.setData(_headers.get, data, params)
      app.calculate(_data, params, report)
    }
    val out = result.map{ r =>
      r._2 match {
        case ds:DataSerie =>  ds.values.map( x => r._1.abbr -> x)
        case _ => Seq()
      }
    }

    val results = out.toSeq.filter(_.nonEmpty).transpose

    val rows = data.zip(results).map{case (d,r) => FCRow(d.date,r.toMap)}


    (report,rows)
  }

}
