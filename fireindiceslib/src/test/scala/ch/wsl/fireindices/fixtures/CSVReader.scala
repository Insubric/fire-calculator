package ch.wsl.fireindices.fixtures

import ch.wsl.fireindices.functions.Utils
import ch.wsl.fireindices.model.FCRow

import scala.util.Try

object CSVReader {
  def readCsv(csv:Seq[String]):(Seq[String],Seq[FCRow]) = {
    val header = csv.head.split(",").toSeq.map(_.replaceAll("\"",""))
    val data = csv.tail.map(_.split(",").toSeq)


    val r = data.map{ d =>
      FCRow(Utils.solarDate2Long(d.head),header.tail.zipWithIndex.map{ case (variable,i) => (variable,Try(d.tail.lift(i).get.toDouble).getOrElse(Double.NaN)) }.toMap)
    }

    (header,r)

  }
}
