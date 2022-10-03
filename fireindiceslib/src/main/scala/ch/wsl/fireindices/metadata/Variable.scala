

package ch.wsl.fireindices.metadata

import ch.wsl.fireindices.functions.Functions
import ch.wsl.fireindices.functions.Utils
import ch.wsl.fireindices.ImplicitConversions._

import ch.wsl.fireindices.log.DataLog
import ch.wsl.fireindices.log.ParamLog






/**
 * Base class for a variable definition (metadata). Contains some utility functions.
 **/
 /* sealed */    /* abstract */  /* case */
 class Variable(val name:String, val abbr:String, val unit:String, val min:Double, val max:Double, val c: Class[_ <: AnyVal]){

  /**
   * return the value of type T from a given String
   *
   * @param    txt   string format of value
   * @return         T
   */
  def toValue(txt: String):Double={
	  if (txt=="" || txt==null) Double.NaN
	  else txt.toDouble
  }

  /**
   * check if value is Null
   *
   * @param   value  T > the input value
   * @return         Boolean
   */
  def isNull(value: Double):Boolean={
    value.equals(Double.NaN)
  }

  /**
   * return an empty List of specific variable type T
   *
   * @return     List[T]
   */
  def getEmptyList={
	List[Double]()
  }

  
  /**
   * check is variable value is out of range
   *
   * @param  value  T > the value to check
   * @return        Boolean
   */
  def outOfRange(value:Double)={
//    (!isNull(value)&&(value<min || value>max))
    (!isNull(value)  &&  
      (!isNull(min)&&(value<min)) || (!isNull(max)&& (value>max)))
  }
  
  /**
   * parse a value to a corresponding String
   *
   * @param  value  T > the value to parse
   * @return        String
   */
  def format(x:Double):String={

	if (isNull(x)) ""
    else c.toString match{
		  case "double" => x.toString //"%.2f".format(x)
		  case "long" => x.toLong.toString
		  case "int" => x.toInt.toString
		  case _ => x.toString
		}
   }
   
  def descJSON:String ={
    s"""{
      "abbr":"${abbr}",
      "name":"${name}",
      "unit":"${unit}",
      "min":"${min}",
      "max":"${max}",
      "class":"${c.toString}",
      "serie":"${isInstanceOf[Serie]}",
      "calculable":"${isInstanceOf[Calculable]}",
      "required_variables":${this match {
            case x:Calculable => x.printRequiredVariablesJSON
            case _ => "[]"}}
    }"""
  }
	  
  /**
   * Gets all notes from a group of DataSeries and Parameters and return them in a String
   *
   * @param  selectedVariables  VariableDataMap > the group of variables
   * @return                    String
   */
  def getLog(selectedVariables:DataMapper):DataLog={
    val s=selectedVariables.dssDM
    val p=selectedVariables.parsDM
    
    DataLog(abbr, 
            (if (s.size>0) s.map(_._1.abbr).toList else List()),  //do this since .keys extractor do not always maintain order
            (if (s.size>0) s.map(_._2.abbr).toList else List()),
//            (if (s.size>0) s.keys.map(_.abbr).toList else List()),
//            (if (s.size>0) s.values.map(_.abbr).toList else List()),
            (if (p.size>0) 
              p.dc.pars.values.map(x => ParamLog(x.variable.abbr,
                       (if(x.variable.c.toString.equals("long")) 
                          Utils.solarDate2String(x.value,"dd-MMM") 
                       else 
                          x.value.toString)
            )).toList
            else List())
            
    )
  }	
}





/**
 * Contains the list of variables (case objects), to be used in a enum style.
 * Each variable should be added to the values.
 * Contains some methods to retrieve the variables (or subsets) or check their existence.
 **/
object Variable{
  val variables:List[Variable] = List(Altitude,Latitude,Slope,Aspect,Rs_a,Rs_b,Krs,Albedo,RainyDayThreshold,RainyWeekThreshold,FFMCstart,DMCstart,DCstart,
                                      Risico_v0, Risico_d0, Risico_d1, Risico_sat, Risico_T0, /*Risico_dT,*/ Risico_hhv, Risico_humidity,
                                      FireSeasonStart,FireSeasonEnd,M68VegCorrStep3Start,M68VegCorrStep3End,XsnowcoverStart,XsnowcoverEnd,XbirchLeaves,XrobiniaBlossom,MeanAnnualRain,
                                      Date,DateTime,
                                      T,Tmax,Tmin,T12,T13,T15,Tdew,Tdew12,Tdew15,H,Hmax,Hmin,H12,H13,H15,U,Umax,U12,U13,U15,D,P,P12,P13,P15,PDur,
                                      N,Cc,ns,Ra,Rs,Rs_fromT,NetRad,
                                      Climate,SnowCover,RobiniaBlossom,BirchLeaves,PC,
                                      DaysSinceRain,AgeRainEvent_2_20,WeekRain,RainSum,lastRainSum,lastRainSum_2_20,
                                      VPD,VPD12,VPD13,
                                      Angstroem,Nesterov,Munger,
                                      EMC,EMCfa,EMC24,
                                      FFWI,FFWImod,
                                      KBDI,KBDISI,
                                      Sharples,FMI,
                                      Baumgartner,BaumgartnerDanger,
                                      PETpen,PETthorn,PETthorn_camargo,PETthorn_pereira,I,
                                      res,OrieuxDanger,res_surf,I87,
                                      RN,
                                      M68,pM68,M68dwd,pM68dwd,
                                      FFDI,DFnoble,DFgriffith,DFgriffithAdj,
                                      FWI,ISI,BUI,FFMC,DMC,DC,
                                      FWI_lat,BUI_lat,DMC_lat,DC_lat,
                                      Risico_dffm, Risico_WindEffect, Risico_V, Risico_FI,
                                      IREPI,
                                      MC1,MC10,MC100,MC1000,
                                      Ifa)
  val timelines:List[Variable] = List(Date,DateTime)   
  
  val inputs:List[Variable] = List(T,T12,T13,T15,Tmin,Tmax,P,P12,P13,P15,U,U12,U13,U15,D,H,H12,H13,H15,Hmin,Hmax,Cc,ns,Rs)
  val optionalInputs:List[Variable] = List(SnowCover,BirchLeaves,RobiniaBlossom,PC,PDur)
  val parameters:List[Variable] = variables.filterNot(_.isInstanceOf[Serie])
  val series:List[Serie] = variables.filter(_.isInstanceOf[Serie]).asInstanceOf[List[Serie]]
  val calculables:List[Variable with Calculable] = variables.filter(_.isInstanceOf[Variable with Calculable]).asInstanceOf[List[Variable with Calculable]]
  def calculables(dc:DataCollection) = dc.dateDs
  val subdaily_calculables:List[Serie] = List(VPD12,VPD13,Risico_dffm, Risico_WindEffect, Risico_V, Risico_FI)
        
  val msecInOneYear = 31622400000L
  val msecInOneHour = 1000*60*60
  val msecInOneDay = msecInOneHour*24

    

  def load = {variables.length + timelines.length + inputs.length + optionalInputs.length + 
              parameters.length + series.length + calculables.length}
  
  /**
   * Gets a variable by his abbreviation
   *
   * @param  abbreviation  the abbreviation
   * @return               Variable
   */
  def getByAbbr(abbreviation: String): Variable={
    val l = variables.filter(_.abbr.equals(abbreviation))
    if (l.length>0) l(0).asInstanceOf[Variable] else null
  }
  
  /**
   * Gets a variable by his abbreviation non-casesensitive
   *
   * @param  abbreviation  the abbreviation
   * @return               Variable
   */
  def getByAbbrCaseInsensitive(abbreviation: String): Variable={
    val l = variables.filter(_.abbr.toLowerCase.equals(abbreviation.toLowerCase))
    if (l.length>0) l(0) else null
  }
  
  /**
   * Checks if a variable exist by his abbreviation
   *
   * @param  abbreviation  the abbreviation
   * @return               Variable
   */
  def existsByAbbr(abbreviation: String): Boolean ={
    //if (getByAbbr(abbreviation)==null) false else true
    variables.exists(_.abbr.toLowerCase.equals(abbreviation.toLowerCase))
  }
  /**
   * Checks if a date variable exist by his abbreviation
   *
   * @param  abbreviation  the abbreviation
   * @return               Variable
   */
  def dateExistsByAbbr(abbreviation: String): Boolean ={
    //if (getByAbbr(abbreviation)==null) false else true
    timelines.exists(_.abbr.toLowerCase.equals(abbreviation.toLowerCase))
  }
  
  /**
   * Checks if a variable exist by his abbreviation non-casesensitive
   *
   * @param  abbreviation  the abbreviation
   * @return               Variable
   */
  def existsByAbbrCaseInsensitive(abbreviation: String): Boolean ={
    getByAbbrCaseInsensitive(abbreviation)!=null
  }

  
  /**
   * Gets all notes from a group of DataSeries and Parameters and return them in a String
   *
   * @param  selectedVariables  VariableDataMap > the group of variables
   * @return                    String
   */
  def getNotesOld(selectedVariables:DataMapper)={
    val s=selectedVariables.dssDM
    val p=selectedVariables.parsDM
    val str=
		(if (s.size>0)"("+s.keys.mkString(",")+")->("+s.values.mkString(",")+")" else "")+
		(if (p.size>0) "["+p.dc.pars.values.map(x => x.variable.abbr + " = " +
		  (if(x.variable.c.toString.equals("long")) Utils.solarDate2String(x.value,"dd-MMM") else x.value)
			).mkString(",")+"]" else "")

	str
  }

  /**
   * Gets all notes from a group of DataSeries and Parameters and return them in a String
   *
   * @param  selectedVariables  VariableDataMap > the group of variables
   * @return                    String
   */
  def getNotes(selectedVariables:DataMapper)={
    val str=
		""" "required_series":"""+ getRequiredSeries(selectedVariables) +",\n" +
                """ "used_series":"""+ getUsedSeries(selectedVariables) +",\n" +
		""" "used_parameters":"""+ getUsedParameters(selectedVariables) + "\n"

	str
  }

  def getRequiredSeries(selectedVariables:DataMapper)={
    val s=selectedVariables.dssDM
    
    "[" + ( if (s.size>0) s.keys.map(_.abbr.mkString("\"","","\"")).mkString(",")else "") +"]" 
  }
  
  def getUsedSeries(selectedVariables:DataMapper)={
    val s=selectedVariables.dssDM
    
    "[" + (if (s.size>0) s.values.map(_.abbr.mkString("\"","","\"")).mkString(",") else "") +"]" 
  }
  
  def getUsedParameters(selectedVariables:DataMapper)={
    val p=selectedVariables.parsDM
    
    "{"+(if (p.size>0) 
            p.dc.pars.values.map(x => "\""+ x.variable.abbr + "\":\"" +
		  (if(x.variable.c.toString.equals("long")) Utils.solarDate2String(x.value,"dd-MMM") 
                   else x.value) + "\""
            ).mkString(",\n") 
         else "") +
    "}"
  }
}
