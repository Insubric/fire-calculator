/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package ch.wsl.fireindices.log

//import net.liftweb.json._
//import net.liftweb.json.Serialization.{read, write}
//import spray.json._


//object MyJsonProtocol extends DefaultJsonProtocol {
//  implicit val paramLogFormat = jsonFormat2(ParamLog)
//  implicit val dataLogFormat = jsonFormat5(DataLog)
//  implicit val headersLogFormat = jsonFormat3(HeadersLog)
//  implicit val checkLogFormat = jsonFormat1(CheckLog)
//  implicit val reportLogFormat = jsonFormat8(ReportLog)
//}


case class ParamLog(name:String,
                    value:String){
  
  def format = name + " = " + value
}

case class DataLog (name:String, 
                    required_series: List[String]=List(), 
                    used_series: List[String]=List(),
                    used_parameters: List[ParamLog]=List(),
                    notes:String = ""
){
  
  def addNotes(s:String) = this.copy(notes=s)
  
  def format = name + ": (" + required_series.mkString(",")+ ")->(" +
                              used_series.mkString(",")+ ")" +
                              (if (used_parameters.size>0) 
                                "["+ used_parameters.map(x => x.name +"="+x.value).mkString(",") +"]"
                               else "") + " " +
                              notes
                    } 

case class HeadersLog (read_columns: Int=0, 
                       recognized_columns: List[String]=List(),
                       date_column: String=""){
  
  def format = (if (date_column.length > 0) "Date column exists: " + date_column
              else "Date column do not exist!") + "\n" +
              "Recognised columns (" + recognized_columns.size +" of "+ read_columns +
                "): " + recognized_columns.mkString(" ")
  
} 
case class CheckLog (check: String=""){
  
  def format = "Check" + (if (check.length > 0) " (some problems):\n " + check
                          else " .. OK") 
  
} 


case class ReportLog(var source: String="",
                      var headers: HeadersLog=new HeadersLog,
                      var read: String="",
                      var check: CheckLog = CheckLog(),
                      var series_calculated: List[DataLog]=List(),
                      var series_completed: List[DataLog]=List(),
                      var series_replaced: List[DataLog]=List(),
                      var n_replaced: Int =0,
                      var parameters_default: List[String]=List(),
                      var parameters_calculated: List[DataLog]=List(),
                      var parameters_given: List[ParamLog]=List()
){
//  implicit val formats = Serialization.formats(NoTypeHints)
//// import MyJsonProtocol._
  
  def formatCalculateLog={
    val r = new StringBuilder
     r ++= "Calculated variables\n"
     r ++= series_calculated.map("\t" + _.format).mkString("\n")
     r ++= "\n\n"
     r ++= "Completed variables\n"
     r ++= series_completed.map("\t" + _.format).mkString("\n")
     r ++= s"Replaced variables ($n_replaced)\n"
     r ++= series_replaced.map("\t" + _.format).mkString("\n")
     r ++= "\n\n"
     r ++= "Parameters calculated\n"
     r ++= parameters_calculated.map("\t" + _.format).mkString("\n")
     r ++= "\n\n"
     r ++= "Parameters given\n"
     r ++= parameters_given.map("\t" + _.format).mkString("\n")
     
     r.toString   
    
  }
  def formatLog={
     val r = new StringBuilder
     
     r ++= "source: " + source
     r ++= "\n\n"
     r ++= headers.format
     r ++= "\n\n"
     r ++= "Reading data " + read
     r ++= "\n\n"
     r ++= check.format
     r ++= "\n\n"
     r ++= formatCalculateLog
     
     r.toString
  }
  
//def formatJson = pretty(render(parse(write(this))))
////def formatJson = this.toJson.prettyPrint
  
  
}