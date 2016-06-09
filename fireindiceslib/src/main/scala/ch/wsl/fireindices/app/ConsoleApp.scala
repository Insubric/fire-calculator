package ch.wsl.fireindices.app

import org.clapper.argot._

import ch.wsl.fireindices.log.ReportLog
import ch.wsl.fireindices.metadata._
import ch.wsl.fireindices.app._
import ch.wsl.fireindices.functions._
import com.typesafe.scalalogging.LazyLogging
import java.io.File
import java.text.SimpleDateFormat
import scala.collection.mutable.HashMap
import ch.wsl.fireindices._



//to test console complete 
//run --addparam --complete --nocheck --I_ 55.124855136716  --MeanAnnualRain 1822.9522571819418 ./testdata/DATA_sample_2_toComplete.csv
//run --addparam --complete --replace 1 --nocheck --I_ 55.124855136716  --MeanAnnualRain 1822.9522571819418 ./testdata/DATA_sample_2_toComplete.csv    //should give warning
//run --addparam --replace 1 --nocheck --I_ 55.124855136716  --MeanAnnualRain 1822.9522571819418 ./testdata/DATA_sample_2_toComplete.csv
//run --addparam --replace 10 --nocheck --I_ 55.124855136716  --MeanAnnualRain 1822.9522571819418 ./testdata/DATA_sample_2_toReplace.csv

//run --addparam -i Risico_dffm,Risico_WindEffect,Risico_V,Risico_FI ./testdata/DATA_sample_h_2.csv
//run --variableinfo  ./testdata/DATA_sample_h_2.csv
//run --addparam   ./testdata/DATA_sample_h_2.csv
//run --addparam  --jsonlog ./testdata/DATA_sample_h_2.csv


object ConsoleApp extends LazyLogging {
  import org.clapper.argot.ArgotConverters._

  val parser = new ArgotParser(
        "fireindiceslib",
        compactUsage = true,
        outputWidth = 120, 
        preUsage=Some("Fire weather indices calculator: Version "+BuildInfo.version+" Copyright (c) " +
                      "2010-2016, WSL Swiss Federal Reserach Insititute, boris.pezzatti@wsl.ch \n\n"+
                      "Written in scala, with following open source libraries:\n"+
                      "\tcsvjdbc.jar\n"+
                      "\targot.jar\n"+
                      "and compacted with proguard."),
        postUsage=Some("\nExample of usage:\n\t"+
                       " generic:  java -Xmx512m -Xss10m -cp fireindicesui.jar ch.wsl.fireindices.app.ConsoleApp --Altitude 300 --fileparam  --addparam  -s 20000201 -e 20040101  ./DATA_sample.csv\n\t"+
                       " linux:    bash console.sh --Altitude 300 --fileparam  --addparam  -s 20000201 -e 20040101  ./DATA_sample.csv\n\t"+
                       " windows:  fireindicesconsole.exe --Altitude 300 --fileparam  --addparam  -s 20000201 -e 20040101  ./DATA_sample.csv"),
        sortUsage=false
  )

  val nocheck = parser.flag[Boolean]("nocheck",false,"Do not check for missing data or outliers.")
  val nocalc = parser.flag[Boolean]("nocalc",false,"Do not calculate fire weather indices.")
  val complete = parser.flag[Boolean](List("c", "complete"),"Calculate only the missing values in the last row.")  //this calculates only the last row
  val replace = parser.option[Int](List("r", "replace"), "<n>", "Replace last n rows with new calculated data.\nWarning: if no indices parameter is specified all\ncalculable variables will be replaced.")  //this replaces the last n rows
  val variableinfo = parser.flag[Boolean](List("v", "variableinfo"),"Information for variables.")

  val addPar = parser.flag[Boolean](List("p", "addparam"),"Complete missing parameters with default values.")
  val filePar = parser.flag[Boolean](List("f", "fileparam"),"Import parameters from file *_PAR.txt.")

  val sdt = parser.option[String](List("s","startdate"),"<yyyymmdd> or <yyyymmddhhmm>", "Start date for the elaboration.")
  val edt = parser.option[String](List("e","enddate"),"<yyyymmdd> or <yyyymmddhhmm>", "End date for the elaboration.")

  val indices = parser.option[String](List("i","indices"),"<csv String>", "List of indices to be calculated.")
  val Risico = parser.flag[Boolean](List("R","Risico"),"Calculate only Risico indices.")

  val jsonlog = parser.flag[Boolean]("jsonlog",false,"Write also a log in Json format.")

  val parIn = new HashMap[String,SingleValueOption[_]]
  for (v <- Variable.parameters){
     val abbr = if (v.abbr.length==1) v.abbr+"_" else v.abbr   // to put style POSIX even if parameter abbr is of length 1  (e.g.  I )
     val opt = v.c.toString match {
                 case "double"  => parser.option[Double](abbr,"<"+v.unit+">" , v.name)
                 case "long"    => v match {   //in case of dates read it as string
                     case FireSeasonStart  =>parser.option[String](abbr,"<"+v.unit+">" , v.name)
                     case FireSeasonEnd    =>parser.option[String](abbr,"<"+v.unit+">" , v.name)
                     case M68VegCorrStep3Start=>parser.option[String](abbr,"<"+v.unit+">" , v.name)
                     case M68VegCorrStep3End=>parser.option[String](abbr,"<"+v.unit+">" , v.name)
                     case XbirchLeaves   =>parser.option[String](abbr,"<"+v.unit+">" , v.name)
                     case XrobiniaBlossom =>parser.option[String](abbr,"<"+v.unit+">" , v.name)
                     case XsnowcoverStart=>parser.option[String](abbr,"<"+v.unit+">" , v.name)
                     case XsnowcoverEnd   =>parser.option[String](abbr,"<"+v.unit+">" , v.name)
                     case _ => parser.option[Long]  (abbr,"<"+v.unit+">" , v.name)
                 }        
                 case "int"     => parser.option[Int]   (abbr,"<"+v.unit+">" , v.name)
                 case _         => parser.option[String](abbr,"<"+v.unit+">" , v.name)
               }
     parIn += (v.abbr -> opt)
  }

  val input = parser.parameter[File]("input", "Input file to read. ", false){
        (s, opt) =>

        val file = new File(s)
        if (! file.exists)
            parser.usage("Input file \"" + s + "\" does not exist.")

        file
  }


  def main(args: Array[String]){    

        try{
            parser.parse(args)
            
            if (variableinfo.value.getOrElse(false)){
                println(Variable.variables.map(_.descJSON).mkString("\n"))
            }else{
            
                val ix =  if (Risico.value.getOrElse(false)) 
                            Risico_dffm::Risico_WindEffect::Risico_V::Risico_FI::Nil
                          else
                            indices.value.getOrElse("").split(",").toList.map(Variable.getByAbbrCaseInsensitive(_))

                val parameters = new Parameters

                //add paramters from file
                if (filePar.value.getOrElse(false)) {
                  val parFile = new File(input.value.getOrElse(null).getPath.take(input.value.getOrElse(null).getPath.size-4)+"_PAR.txt")
                  if (parFile.exists) parameters.read(parFile)
                }

                //add parameters from command line
                for ((str,opt)<-parIn.filterNot(_ equals None)){
                  val abbr = if (opt.names(0).length==2 & opt.names(0).tail=="_") opt.names(0).head.toString else opt.names(0)  // to read also the POSIX with 1 char
                  val v=Variable.getByAbbrCaseInsensitive(abbr)
                  if (opt.value.isDefined) {
                      val value = opt.value.get
                      parameters.addIfNotNull(v.asInstanceOf[Variable],v match {
                            case FireSeasonStart  => Utils.solarDate2Long(value.toString,"dd.MM")
                            case FireSeasonEnd    => Utils.solarDate2Long(value.toString,"dd.MM")
                            case M68VegCorrStep3Start=> Utils.solarDate2Long(value.toString,"dd.MM")
                            case M68VegCorrStep3End=> Utils.solarDate2Long(value.toString,"dd.MM")
                            case XbirchLeaves   => Utils.solarDate2Long(value.toString,"dd.MM")
                            case XrobiniaBlossom => Utils.solarDate2Long(value.toString,"dd.MM")
                            case XsnowcoverStart=> Utils.solarDate2Long(value.toString,"dd.MM")
                            case XsnowcoverEnd   => Utils.solarDate2Long(value.toString,"dd.MM")
                            case _ => value.toString.toDouble //asInstanceOf[Double]
                     })
                  }
                }

                
//                val completionStr = if (addPar.value.getOrElse(false)) "Added default parameters: "+parameters.completeWithDefaults+"\n"
//                                    else ""


                val elab=new SimpleApp4CSV
                val log = new ReportLog()
                
                //add default parameters
                log.parameters_default = if (addPar.value.getOrElse(false)) parameters.completeWithDefaults
                                         else List()
            

                elab.readHeadersFile(input.value.getOrElse(null),log)
                elab.readDataFile(input.value.getOrElse(null),parameters,sdt.value,edt.value)
                if (!nocheck.value.getOrElse(false)) elab.check(log)

                if (complete.value.getOrElse(false) && replace.value.getOrElse(0)>0) 
                  logger.warn("Both complete and replace options have been specified. Replace option will be ignored.")

                if (!nocalc.value.getOrElse(false))
                        if (!complete.value.getOrElse(false))
                              if (replace.value.getOrElse(0)<1)
                                    if (ix(0)==null) elab.calculateFile(parameters, log)   
                                    else elab.calculateFile(parameters, log, ix.asInstanceOf[Seq[Variable with Calculable]])
                              else
                                    if (ix(0)==null) elab.replaceFile(parameters, replace.value.get, null, null, log, false)   
                                    else elab.replaceFile(parameters, replace.value.get, ix.asInstanceOf[Seq[Serie with Calculable]], null, log, false)

                        else 
                                if (ix(0)==null) elab.completeFile(parameters, log)
                                else elab.completeFile(parameters, log, ix.asInstanceOf[Seq[Serie with Calculable]])


                elab.writeLog(log, true, jsonlog.value.getOrElse(false))
            } 

        }catch{
            case e: ArgotUsageException => {
                if (variableinfo.value.getOrElse(false)){
                  println(Variable.variables.map(_.descJSON).mkString("\n"))
                }else{
                  logger.error(e.message)
                }
            }
        }
    }


}
