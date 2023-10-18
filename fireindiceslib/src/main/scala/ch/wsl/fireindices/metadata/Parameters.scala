

package ch.wsl.fireindices.metadata


import ch.wsl.fireindices.log.DataLog
import java.io.File
import java.text.NumberFormat
import ch.wsl.fireindices.functions.Utils
import scala.collection.mutable.HashMap
import scala.collection.generic._
import scala.collection.mutable.ListBuffer

/**
 * Collection of Parameter objects.
 *
 **/
class Parameters(that: IterableOnce[Parameter]) extends KeySet[Variable,Parameter](that) {
									//with Map[Variable,Parameter] 
									//with MapLike[Variable,Parameter, Parameters]{
 
  import Parameters._
  
  /**
   * simple constructor 
   *
   */ 
   def this() = this(Nil) 
   
   
  /**
   * builds a key to insert a new Parameter in the KeySet
   *
   * @param  par  Parameter[Any] > the new parameter
   * @return      Variable[Any] >
   */
  def buildKey(par: Parameter)= par.variable
  
  /**
   * add a value to Parameters object if it's not null
   *
   * @param  name  Variable > the variable for key building
   * @param  value A > the new value
   * @return       Parameters
   */
  def addIfNotNull(name: Variable, value: Double)={
    if (!value.equals(Double.NaN)) this += new Parameter(name, value, DataLog(name.abbr))
  }
  
  /**
   * add a value in String format to Parameters object if it's not null
   *
   * @param  name  Variable > the variable for key building & for determinate the type
   * @param  value the new value
   * @return       Parameters
   */
  def addIfNotNull_Txt(name: Variable, txt: String)={
    if (txt != null && !txt.equals("") && txt!=Null.getNullVal)
      this += new Parameter(name,name.toValue(txt), DataLog(name.abbr))
  }
//  /**
//   * add a value in String format to Parameters object if it's not null
//   *
//   * @param  name  Variable > the variable for key building & for determinate the type
//   * @param  value the new value
//   * @return       Parameters
//   */
//  def addIfNotNull_TxtA(name: Variable, txt: String)={
//    if (txt != null && !txt.equals("") && txt!=Null.getNullVal)
//      this += new Parameter(name,name.toValue(txt))
//
////    val d:Double = NumberFormat.getInstance.parse(txt).doubleValue
//////    val doubleTxt:String = d.toString
////    if (txt != null && !txt.equals("") && txt!=Null.getNullVal) {
////      val value = name.dataManifest.toString match{
////                                  case "Double"=>d
////                                  case "Long"  =>d.toLong
////                                  case "Int"   =>d.toInt
////                                  case _       => d
////                               }
////      this += new Parameter[A](name,value.asInstanceOf[A] )
////    }
//
//  }

  /**
   * Returns a Parameters with the order specified in Variable.parameters
   *
   * @return     Parameters
   */
  def ordered: Parameters ={
    val ordered = Variable.parameters.intersect(this.keySet.toSeq)
    new Parameters(ordered.map(this(_)))
  }
  
  /**
   * read the parameters values from a file
   *
   * @param  file    parameter file
   * 
   */
  def read(file:File):Boolean ={
    import resource._

    for(input <- managed(io.Source.fromFile(file))) {

        for (line <- io.Source.fromFile(file).getLines){
          val s=line.split(" ").filter(_.size>0)
          if (s.length>0){
            val v=Variable.getByAbbrCaseInsensitive(s(0))
            if (v!=null)
              try{
                val value = v match {
                   case FireSeasonStart  => Utils.solarDate2Long(s(1),"dd.MM")
                   case FireSeasonEnd    => Utils.solarDate2Long(s(1),"dd.MM")
                   case M68VegCorrStep3Start=> Utils.solarDate2Long(s(1),"dd.MM")
                   case M68VegCorrStep3End=> Utils.solarDate2Long(s(1),"dd.MM")
                   case XbirchLeaves   => Utils.solarDate2Long(s(1),"dd.MM")
                   case XrobiniaBlossom => Utils.solarDate2Long(s(1),"dd.MM")
                   case XsnowcoverStart=> Utils.solarDate2Long(s(1),"dd.MM")
                   case XsnowcoverEnd   => Utils.solarDate2Long(s(1),"dd.MM")
                   case _ => s(1)
                }
                v match{
                  case Climate => this.addIfNotNull(v.asInstanceOf[Variable] , value.toString.toInt)  //I
                  case FireSeasonStart  =>this.addIfNotNull(v.asInstanceOf[Variable] , value.toString.toLong)
                  case FireSeasonEnd    =>this.addIfNotNull(v.asInstanceOf[Variable] , value.toString.toLong)
                  case M68VegCorrStep3Start=>this.addIfNotNull(v.asInstanceOf[Variable] ,value.toString.toLong)
                  case M68VegCorrStep3End=>this.addIfNotNull(v.asInstanceOf[Variable] ,value.toString.toLong)
                  case XbirchLeaves   =>this.addIfNotNull(v.asInstanceOf[Variable] ,value.toString.toLong)
                  case XrobiniaBlossom =>this.addIfNotNull(v.asInstanceOf[Variable] ,value.toString.toLong)
                  case XsnowcoverStart=>this.addIfNotNull(v.asInstanceOf[Variable] ,value.toString.toLong)
                  case XsnowcoverEnd   =>this.addIfNotNull(v.asInstanceOf[Variable] ,value.toString.toLong)
                  case _ =>       this.addIfNotNull_Txt(v,value.toString)
                }
              } catch {
                  case e:Throwable =>     //do nothing
              }
          }
        }
    }
    file.exists
  }

  /**
   * add the default values for the missing parameters
   *
   * @return         String with the list of paramters added
   */
  def completeWithDefaults:List[String] ={
    val missing = defaults.filterKeys(!this.keySet.contains(_))
    var toPrint=new ListBuffer[String]
    missing.foreach((kv)=>{this.addIfNotNull(kv._1.asInstanceOf[Variable],kv._2)
                           toPrint += kv._1.abbr})
    toPrint.toList
  }


  /**
   * clones all Parameter s
   *
   * @return            Parameters
   */
  def cloneAll:Parameters =   new Parameters(this.valuesIterator.map(_.clone))


  /**
   * return a String with all parameters contained in Parameters object
   *
   * @param  prefix  a prefix for all parameters printed
   * @return         String
   */
  def print(prefix:String=""):String ={
    this.values.map((x) => prefix + x.variable.abbr +" = "+
        (if(x.variable.c.toString.equals("long")) Utils.solarDate2String(x.value.asInstanceOf[Long],"dd-MMM") else x.value)
        ).mkString("\n")+ "\n"
  }
}


object Parameters {
  
  val defaults = new HashMap[Variable, Double]
  defaults += (Altitude -> 273.0,
                Latitude -> 46.0,
                RainyDayThreshold -> 1.27,
                Rs_a -> 0.25,
                Rs_b -> 0.50,
                Krs -> 0.16,
                Albedo -> 0.25,
                RainyWeekThreshold -> 30.0,
                FFMCstart -> 85.0,
                DMCstart -> 6.0,
                DCstart -> 15.0,
                SDMCstart -> 12.0,
                Climate -> 3,
                FireSeasonStart ->  Utils.solarDate2Long("15.02","dd.MM"),
                FireSeasonEnd ->  Utils.solarDate2Long("30.09","dd.MM"),
                M68VegCorrStep3Start ->  Utils.solarDate2Long("15.08","dd.MM"),
                M68VegCorrStep3End ->  Utils.solarDate2Long("01.09","dd.MM"),
                XbirchLeaves ->  Utils.solarDate2Long("01.04","dd.MM"),
                XrobiniaBlossom ->  Utils.solarDate2Long("01.06","dd.MM"),
                XsnowcoverStart ->  Utils.solarDate2Long("01.01","dd.MM"),
                XsnowcoverEnd ->  Utils.solarDate2Long("01.01","dd.MM"),
                Aspect -> 180.0,
                Slope -> 0.0,
                Risico_v0 -> 140,
                Risico_d0 -> 1.0,
                Risico_d1 -> 3.0,
                Risico_T0 -> 6.0,
                Risico_sat -> 40,
                Risico_hhv -> 21000.0,
                Risico_humidity -> 45.0
) 
}
