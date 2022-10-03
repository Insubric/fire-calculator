package ch.wsl.fireindices.app

import org.rogach.scallop._
import ch.wsl.fireindices.log.ReportLog
import ch.wsl.fireindices.metadata._
import ch.wsl.fireindices.app._
import ch.wsl.fireindices.functions._
import com.typesafe.scalalogging.LazyLogging

import java.io.File
import java.text.SimpleDateFormat
import scala.collection.mutable.HashMap
import ch.wsl.fireindices._
import org.rogach.scallop.exceptions.ScallopException

//to test console complete 
//run --addparam --complete --nocheck --I_ 55.124855136716  --MeanAnnualRain 1822.9522571819418 ./testdata/DATA_sample_2_toComplete.csv
//run --addparam --complete --replace 1 --nocheck --I_ 55.124855136716  --MeanAnnualRain 1822.9522571819418 ./testdata/DATA_sample_2_toComplete.csv    //should give warning
//run --addparam --replace 1 --nocheck --I_ 55.124855136716  --MeanAnnualRain 1822.9522571819418 ./testdata/DATA_sample_2_toComplete.csv
//run --addparam --replace 10 --nocheck --I_ 55.124855136716  --MeanAnnualRain 1822.9522571819418 ./testdata/DATA_sample_2_toReplace.csv

//run --addparam -i Risico_dffm,Risico_WindEffect,Risico_V,Risico_FI ./testdata/DATA_sample_h_2.csv
//run --variableinfo  ./testdata/DATA_sample_h_2.csv
//run --addparam   ./testdata/DATA_sample_h_2.csv
//run --addparam  --jsonlog ./testdata/DATA_sample_h_2.csv



class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version("Fire weather indices calculator: Version "+ BuildInfo.version+" Copyright (c) " +
    "2010-2016, WSL Swiss Federal Reserach Insititute, boris.pezzatti@wsl.ch \n\n"+
    "Written in scala, with following open source libraries:\n"+
    "\tcsvjdbc\n"+
    "\tscallop\n"+
    "and compacted with proguard.")
  banner("""Usage: test [OPTION]... [tree|palm] [OPTION]... [tree-name]
           |test is an awesome program, which does something funny
           |Options:
           |""".stripMargin)
  footer("\nExample of usage:\n\t"+
    " generic:  java -Xmx512m -Xss10m -cp fireindicesui.jar ch.wsl.fireindices.app.ConsoleApp --Altitude 300 --fileparam  --addparam  -s 20000201 -e 20040101  ./DATA_sample.csv\n\t"+
    " linux:    bash console.sh --Altitude 300 --fileparam  --addparam  -s 20000201 -e 20040101  ./DATA_sample.csv\n\t"+
    " windows:  fireindicesconsole.exe --Altitude 300 --fileparam  --addparam  -s 20000201 -e 20040101  ./DATA_sample.csv")


  val nocheck = opt[Boolean]("nocheck", default=Option(false), descr ="Do not check for missing data or outliers.")
  val nocalc = opt[Boolean]("nocalc", default=Option(false), descr ="Do not calculate fire weather indices.")
  val complete = opt[Boolean](name="complete", 'c', default = Option(false), descr ="Calculate only the missing values in the last row.")  //this calculates only the last row
  val replace = opt[Int]("replace", 'r', default = Option(0), descr ="Replace last n rows with new calculated data.\nWarning: if no indices parameter is specified all\ncalculable variables will be replaced.")  //this replaces the last n rows
  val variableinfo = opt[Boolean]("variableinfo", 'v', descr ="Information for variables.")

  val addPar = opt[Boolean]("addparam", 'p', descr ="Complete missing parameters with default values.")
  val filePar = opt[Boolean]("fileparam",'f', descr ="Import parameters from file *_PAR.txt.")

  val sdt = opt[String]("startdate",'s',descr ="Start date for the elaboration <yyyymmdd> or <yyyymmddhhmm>.")
  val edt = opt[String]("enddate",'e',descr ="End date for the elaboration <yyyymmdd> or <yyyymmddhhmm>.")

  val indices = opt[String]("indices", 'i',descr ="List of indices to be calculated <csv String>.")
  val calculateMissingIndices = opt[String]("calculatemissing", 'm', descr ="List of missing indices to be calculated <csv String>. To be used with complete/replace.")
  val onlyLast = opt[Boolean]("onlyLast",'o',descr ="Output only last line.")

  val Risico = opt[Boolean]("Risico",'R',descr ="Calculate only Risico indices.")

  val jsonlog = opt[Boolean]("jsonlog", 'j', default=Option(false),descr ="Write also a log in Json format.")

//  val parIn = props[_](name = 'P', "Input parameters")


  var parIn = new HashMap[String, ScallopOption[_]]
  
  for (v <- Variable.parameters){
    val abbr = if (v.abbr.length==1) v.abbr+"_" else v.abbr   // to put style POSIX even if parameter abbr is of length 1  (e.g.  I )
    val myopt = v.c.toString match {
      case "double"  => opt[Double](abbr, descr=v.name +" <"+v.unit+">" )
      case "long"    => v match {   //in case of dates read it as string
        case FireSeasonStart  =>opt[String](abbr, descr=v.name +" <"+v.unit+">")
        case FireSeasonEnd    =>opt[String](abbr,descr=v.name +" <"+v.unit+">")
        case M68VegCorrStep3Start=>opt[String](abbr,descr=v.name +" <"+v.unit+">")
        case M68VegCorrStep3End=>opt[String](abbr,descr=v.name +" <"+v.unit+">")
        case XbirchLeaves   =>opt[String](abbr,descr=v.name +" <"+v.unit+">")
        case XrobiniaBlossom =>opt[String](abbr,descr=v.name +" <"+v.unit+">")
        case XsnowcoverStart=>opt[String](abbr,descr=v.name +" <"+v.unit+">")
        case XsnowcoverEnd   =>opt[String](abbr,descr=v.name +" <"+v.unit+">")
        case _ => opt[Long]  (abbr,descr=v.name +" <"+v.unit+">")
      }
      case "int"     => opt[Int]   (abbr,descr=v.name +" <"+v.unit+">")
      case _         => opt[String](abbr,descr=v.name +" <"+v.unit+">")
    }
    parIn += (v.abbr -> myopt)
  }

  val input = trailArg[File]("input", descr ="Input file to read. ", required=false)
//    validate =  (x) =>
//
//      val file = new File(x)
//      if (! file.exists)
//        parser.usage("Input file \"" + s + "\" does not exist.")
//
//      file
//  )
  validateFileExists(input)
  verify()
}



object ConsoleApp extends LazyLogging {

  def main(args: Array[String]){    

        try{
          val a = new Conf(args)

          if (a.variableinfo()){
                println(Variable.variables.map(_.descJSON).mkString("\n"))
            }else{
            
              val ix =  if (a.Risico())
                          Risico_dffm::Risico_WindEffect::Risico_V::Risico_FI::Nil
                        else
                          a.indices.getOrElse("").split(",").toList.map(Variable.getByAbbrCaseInsensitive(_)).filterNot(_ ==null)

              val mix =  if (a.Risico())
                Risico_dffm::Risico_WindEffect::Risico_V::Risico_FI::Nil
              else
                a.calculateMissingIndices.getOrElse("").split(",").toList.map(Variable.getByAbbrCaseInsensitive(_)).filterNot(_ ==null)

              val parameters = new Parameters

                //add paramters from file
                if (a.filePar()) {
                  val parFile = new File(a.input().getPath.take(a.input().getPath.size-4)+"_PAR.txt")
                  if (parFile.exists) parameters.read(parFile)
                }

                //add parameters from command line
                for ((str,myopt)<- a.parIn){//asInstanceOf[HashMap[String, ScallopOption[_]]].filter((k,v) => v.toOption.isDefined)){
                  val abbr = if (myopt.name.length==2 & myopt.name.tail=="_") myopt.name.head.toString else myopt.name  // to read also the POSIX with 1 char
                  val v=Variable.getByAbbrCaseInsensitive(abbr)
                  if (myopt.toOption.isDefined) {
                      val value = myopt()
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


                val elab = new SimpleApp4CSV
                val log  = new ReportLog()
                
                //add default parameters
                log.parameters_default = if (a.addPar()) parameters.completeWithDefaults
                                         else List()
            

                elab.readHeadersFile(a.input(),log)
                elab.readDataFile(a.input(),parameters, a.sdt.toOption, a.edt.toOption)
                if (!a.nocheck()) elab.check(log)

                if (a.complete() && a.replace()>0)
                  logger.warn("Both complete and replace options have been specified. Replace option will be ignored.")

                if (!a.nocalc())
                        if (!a.complete() & a.replace()<1) {

                          if (ix.size==0) elab.calculateFile(parameters, log)
                          else elab.calculateFile(parameters, log, ix.asInstanceOf[Seq[Variable with Calculable]])

                        }else{

                          val vars2calculate = if (mix.size==0) null else mix.asInstanceOf[Seq[Serie with Calculable]]

                          if (a.complete()) {

                            if (ix.size==0) elab.completeFile(parameters, log, null , vars2calculate, a.onlyLast())
                            else elab.completeFile(parameters, log, ix.asInstanceOf[Seq[Serie with Calculable]], vars2calculate, a.onlyLast())

                          }else{
                            //replace
                            if (ix.size==0) elab.replaceFile(parameters, a.replace(), null, vars2calculate, vars2calculate, log,  a.onlyLast())
                            else elab.replaceFile(parameters, a.replace(), ix.asInstanceOf[Seq[Serie with Calculable]], null, vars2calculate, log, a.onlyLast())

                          }
                        }


              elab.writeLog(log, true, a.jsonlog())
            } 

        }catch{
            case e: Exception => {
//                if (variableinfo.value.getOrElse(false)){
//                  println(Variable.variables.map(_.descJSON).mkString("\n"))
//                }else{
                  logger.error(e.getMessage)
//                }
            }
        }
    }


}
