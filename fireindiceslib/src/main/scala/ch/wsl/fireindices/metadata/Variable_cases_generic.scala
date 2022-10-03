package ch.wsl.fireindices.metadata

import ch.wsl.fireindices.functions.Functions
import ch.wsl.fireindices.functions.ListFunctions
import ch.wsl.fireindices.functions.Utils
import ch.wsl.fireindices.ImplicitConversions._
import ch.wsl.fireindices.log.DataLog

import java.util
import scala.collection.mutable.ListBuffer

  //using unit field for the java date format
  case object Date extends Variable("Date","DateYYYYMMDD","yyyyMMdd",-Variable.msecInOneYear*100,Variable.msecInOneYear*100, classOf[Long]) with Serie
  case object DateTime extends Variable("DateTime","DateYYYYMMDDHHMI","yyyyMMddHHmm",-Variable.msecInOneYear*100,Variable.msecInOneYear*100, classOf[Long]) with Serie

  case object SnowCover extends Variable("Snow cover","SnowCover","0/1",0,1, classOf[Int]) with Serie with Calculable{
    in += XsnowcoverStart::Nil
    in += XsnowcoverEnd::Nil
    
    /**
     * create snowcover DataSerie from parameters beginSnow endSnow
     *
     * @param  dates      Seq[Long] > list of all dates
     * @param  beginSnow  date of snovcover start (millisec)
     * @param  endSnow    date of snovcover end (millisec)
     * @param  notes      notes on DataSerie creation
     * @return            DataSerie
     */
     
    def calculate(dates:Seq[Long], beginSnow:Long, endSnow:Long, log:DataLog, notes:String=""):DataSerie={
      var values = new ListBuffer[Int]
      val beginS = Utils.solarDate2String(beginSnow,"MMdd").toLong
      val endS = Utils.solarDate2String(endSnow,"MMdd").toLong
      for (i <- 0 until dates.length){
        val MMdd = Utils.solarDate2String(dates(i),"MMdd").toLong
        values += ( if (beginS==endS) 0
                    else if (beginS<endS) {if (MMdd<beginS||MMdd>=endS) 0 else 1}
                    else {if (MMdd>=beginS&&MMdd<=1231 || MMdd>=101&&MMdd<endS)  1 else  0})
      }
      createDataSerie(dates(0),(dates.last-dates.head)/dates.length,values.toList, log, notes)
    }
    
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(dss.dateDs.values,v.par(XsnowcoverStart).value,v.par(XsnowcoverEnd).value, this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.lastDs().getDates,v.par(XsnowcoverStart).value,v.par(XsnowcoverEnd).value, this.getLog(v)))
    }
  }
  case object PC extends Variable("Phenological coefficient","PC","100/200",100,200, classOf[Long]) with Serie with Calculable{
    in += res::Nil

    /**
     * create phenological coefficient DataSerie, according to Carrega (1988)
     *
     * @param  r          Orieux soil water reserve
     * @param  notes      notes on DataSerie creation
     * @return            DataSerie
     */
    def calculate(res:DataSerie, log:DataLog, notes:String=""):DataSerie={
      var values = new ListBuffer[Int]
      val dates = res.getDates
      for (i <- 0 until dates.length){
        val MMdd = Utils.solarDate2String(dates(i),"MMdd").toLong
        values += (if (MMdd< 321 || (MMdd>=621 && MMdd<921) || MMdd>=1221) {   //winter or summer
                      if (res(i)<50) 200 else 100
                   }else{             //spring and automn
                     100
                   })
      }
      createDataSerie(dates(0),(dates.last-dates.head)/dates.length, values.toList, log, notes)
    }

    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(res), this.getLog(v))
    }

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(res,1), this.getLog(v)))
    }
  }
  case object BirchLeaves extends Variable("Phenological phase first birch leaves","BirchLeaves","0/1",0,1, classOf[Int]) with Serie with Calculable{
    in += XbirchLeaves::Nil
    
    /**
     * create BirchLeaves DataSerie
     *
     * @param  dates        Seq[Long] > list of all dates
     * @param  birchLeaves  date when birch put first leaves (millisec)
     * @param  notes        notes on DataSerie creation
     * @return              DataSerie
     */
    def calculate(dates:Seq[Long], birchLeaves:Long, log:DataLog, notes:String=""):DataSerie={
      var values = new ListBuffer[Int]
      for (i <- 0 until dates.length){                    //TODO optimize
        val MMdd = Utils.solarDate2String(dates(i),"MMdd")
        values += (if (MMdd==Utils.solarDate2String(birchLeaves,"MMdd"))  1 else  0)
      }
      //createDataSerieFromDates(dates.toList,values.toList, log, notes)
      createDataSerie(dates(0), Variable.msecInOneDay,values.toList, log, notes)        //it is interval = 1 day
    }
    
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(dss.dateDs.values,v.par(XbirchLeaves).value, this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.lastDs().getDates,v.par(XbirchLeaves).value, this.getLog(v)))
    }
  }
  case object RobiniaBlossom extends Variable("Phenological phase robinia blossom","RobiniaBlossom","0/1",0,1, classOf[Int]) with Serie with Calculable{
    in += XrobiniaBlossom::Nil
    
    /**
     * create RobiniaBlossom DataSerie
     *
     * @param  dates           Seq[Long] > list of all dates
     * @param  robiniaBlossom  date when robinia put first flowers (millisec)
     * @param  notes           notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(dates:Seq[Long], robiniaBlossom:Long, log:DataLog, notes:String=""):DataSerie={
      var values = new ListBuffer[Int]
      for (i <- 0 until dates.length){                    //TODO optimize
        val MMdd = Utils.solarDate2String(dates(i),"MMdd")
        values += (if (MMdd==Utils.solarDate2String(robiniaBlossom,"MMdd"))  1 else  0)
      }
      //createDataSerieFromDates(dates.toList,values.toList, log, notes)
      createDataSerie(dates(0), Variable.msecInOneDay,values.toList.map(_.toDouble), log, notes)        //it is interval = 1 day
    }
    
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(dss.dateDs.values,v.par(XrobiniaBlossom).value, this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.lastDs().getDates,v.par(XrobiniaBlossom).value, this.getLog(v)))
    }
  }
  case object Tmax extends Variable("Maximal temperature","Tmax","°C",-50,50, classOf[Double]) with Serie
  case object Tmin extends Variable("Minimal temperature","Tmin","°C",-50,50, classOf[Double]) with Serie
  case object T extends Variable("Temperature average","T","°C",-50,50, classOf[Double]) with Serie
  case object T12 extends Variable("Temperature at 12:00","T12","°C",-50,50, classOf[Double]) with Serie
  case object T13 extends Variable("Temperature at 13:00","T13","°C",-50,50, classOf[Double]) with Serie
  case object T15 extends Variable("Temperature at 15:00","T15","°C",-50,50, classOf[Double]) with Serie
  case object Tdew extends Variable("Dewpoint temperature","Tdew","°C",-50,50, classOf[Double]) with Serie with Calculable{
    in += (T::T12::Nil)
    in += (H::H12::Nil)
    
    /**
     * Calculate dewpoint temperature DataSerie
     *
     * @param  T      temperature DataSerie
     * @param  H      relative humidity DataSerie
     * @param  notes  some notes on DataSerie creation
     * @return        DataSerie
     */
    def calculate(T:DataSerie,H:DataSerie, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(T.start,T.interval,ListFunctions.applyFunction(Functions.Tdew,T.values,H.values), log, notes)
    }
    
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(T),v.ds(H), this.getLog(v))
    }
       

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(T,1),v.ds(H,1), this.getLog(v)))
    }
  }
  case object Tdew12 extends Variable("Dewpoint temperature at 12:00","Tdew12","°C",-50,50, classOf[Double]) with Serie with Calculable{
    in += (T12::T13::T15::T::Nil)
    in += (H12::H13::H15::H::Nil)
    
    /**
     * Calculate dewpoint temperature DataSerie
     *
     * @param  T12    temperature at 12:00 DataSerie
     * @param  H12    relative humidity at 12:00 DataSerie
     * @param  notes  some notes on DataSerie creation
     * @return        DataSerie
     */
   def calculate(T12:DataSerie,H12:DataSerie, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(T12.start,T12.interval,ListFunctions.applyFunction(Functions.Tdew,T12.values,H12.values), log, notes)
    }
    
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(T12),v.ds(H12),this.getLog(v))
    }
 

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(T12,1),v.ds(H12,1), this.getLog(v)))
    }
  }
  case object Tdew15 extends Variable("Dewpoint temperature at 15:00","Tdew15","°C",-50,50, classOf[Double]) with Serie with Calculable{
    in += (T15::T13::T12::T::Nil)
    in += (H15::H13::H12::H::Nil)

    /**
     * Calculate dewpoint temperature DataSerie
     *
     * @param  T15    temperature at 15:00 DataSerie
     * @param  H15    relative humidity at 15:00 DataSerie
     * @param  notes  some notes on DataSerie creation
     * @return        DataSerie
     */
    def calculate(T15:DataSerie,H15:DataSerie, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(T15.start,T15.interval,ListFunctions.applyFunction(Functions.Tdew,T15.values,H15.values), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(T15),v.ds(H15), this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(T15,1),v.ds(H15,1), this.getLog(v)))
    }
  }
  case object H extends Variable("Relative humidity","H","%",0,100, classOf[Double]) with Serie
  case object H12 extends Variable("Relative humidity at 12:00","H12","%",0,100, classOf[Double]) with Serie
  case object H13 extends Variable("Relative humidity at 13:00","H13","%",0,100, classOf[Double]) with Serie
  case object H15 extends Variable("Relative humidity at 15:00","H15","%",0,100, classOf[Double]) with Serie
  case object Hmax extends Variable("Maximum Relative humidity","Hmax","%",0,100, classOf[Double]) with Serie
  case object Hmin extends Variable("Minimum Relative humidity","Hmin","%",0,100, classOf[Double]) with Serie
  case object U extends Variable("Windspeed average","U","m/s",0,80, classOf[Double]) with Serie
  case object U12 extends Variable("Windspeed at 12:00","U12","m/s",0,80, classOf[Double]) with Serie
  case object U13 extends Variable("Windspeed at 13:00","U13","m/s",0,80, classOf[Double]) with Serie
  case object U15 extends Variable("Windspeed at 15:00","U15","m/s",0,80, classOf[Double]) with Serie
  case object Umax extends Variable("Maximum Windspeed","Umax","m/s",0,80, classOf[Double]) with Serie
  case object D extends Variable("Winddirection average","D","Azimuth[0-360]",0,360, classOf[Double]) with Serie
  case object P extends Variable("Rainfall sum at 06:00","P","mm/day",0,400, classOf[Double]) with Serie
  case object P12 extends Variable("Rainfall sum at 12:00","P12","mm/day",0,400, classOf[Double]) with Serie
  case object P13 extends Variable("Rainfall sum at 13:00","P13","mm/day",0,400, classOf[Double]) with Serie
  case object P15 extends Variable("Rainfall sum at 15:00","P15","mm/day",0,400, classOf[Double]) with Serie
  case object PDur extends Variable("Duration of rainfall in a day","PDur","h",0.0,400.0, classOf[Double]) with Serie with Calculable{
    in += P::Nil
    in += Climate::Nil

    /**
     * Calculate Precipitation duration in a day DataSerie
     *
     * @param  P             rainfall [mm]
     * @param  climate       climate class (Deeming et al.1977)
     * @return               PDur
     */
    def calculate(P:DataSerie,climate:Int, log:DataLog, notes:String=""):DataSerie={
      //createDataSerieFromDates(P.getDates,ListFunctions.applyFunction_k(Functions.PDur,P.values,climate),"climate = "+climate+", "+notes)
      createDataSerie(P.getDates(0), Variable.msecInOneDay, ListFunctions.applyFunction_k(Functions.PDur,P.values,climate), log, notes)   //it is interval = 1 day
   }



    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(P),v.par(Climate).value, this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(P,1),v.par(Climate).value, this.getLog(v)))
    }
  }
  case object Cc extends Variable("Cloud cover fraction","Cc","ratio",0,1, classOf[Double]) with Serie
    
  case object Ra extends Variable("Extraterrestrail radiation","Ra","MJ/m2/d",0.0,10000.0, classOf[Double]) with Serie with Calculable{
    in += Latitude::Nil

    /**
     * Calculate Extraterrestrail radiation
     *
     * @param  Latitude             rainfall [mm]
     * @return                      Ra
     */
    def calculate(dates:List[Long], Latitude:Double, log:DataLog, notes:String=""):DataSerie={
      //createDataSerieFromDates(P.getDates,ListFunctions.applyFunction_k(Functions.PDur,P.values,climate),"climate = "+climate+", "+notes)
      createDataSerie(dates.head, Variable.msecInOneDay, ListFunctions.applyFunction_k(Functions.Ra, dates, Latitude), log, notes)   //it is interval = 1 day
    }

    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(dss.dateDs.values, v.par(Latitude).value, this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.lastDs().getDates, v.par(Latitude).value, this.getLog(v)))
    }
  }
  
  case object Rs extends Variable("Solar radiation","Rs","MJ/m2/d",0.0,10000.0, classOf[Double]) with Serie with Calculable{
//    in += Latitude::Nil
    in += Ra::Nil
    in += N::Nil
    in += ns::Nil
    in += Rs_a::Nil
    in += Rs_b::Nil

    /**
     * Calculate Solar radiation
     *
     * @param  Ra             Extraterrestrail radiation
     * @param  N              Daylight hours FAO
     * @param  ns             Actual duration of sunshine
     * @param  Rs_a
     * @param  Rs_b
     * @return                Rs
     */
    def calculate(Ra:DataSerie, n:DataSerie, N:DataSerie, a:Double=0.25, b:Double=0.5, log:DataLog, notes:String=""):DataSerie={
      //createDataSerieFromDates(P.getDates,ListFunctions.applyFunction_k(Functions.PDur,P.values,climate),"climate = "+climate+", "+notes)
      createDataSerie(Ra.start, Variable.msecInOneDay, ListFunctions.applyFunction_kk(Functions.Rs_fromSunshine, Ra.values, n.values, N.values, a, b), log, "a = "+a+"b = "+b+", "+notes)   //it is interval = 1 day
    }

    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(Ra), v.ds(ns), v.ds(N), v.par(Rs_a).value, v.par(Rs_b).value, this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate( v.ds(Ra,1), v.ds(ns,1), v.ds(N,1), v.par(Rs_a).value, v.par(Rs_b).value, this.getLog(v)))
    }
  }
  case object Rs_fromT extends Variable("Solar radiation calculated from Tmin and Tmax","Rs_fromT","MJ/m2/d",0.0,10000.0, classOf[Double]) with Serie with Calculable{
    in += Ra::Nil
    in += Tmin::Nil
    in += Tmax::Nil
    in += Krs::Nil

    /**
     * Calculate Precipitation duration in a day DataSerie
     *
     * @param  P             rainfall [mm]
     * @param  climate       climate class (Deeming et al.1977)
     * @return               PDur
     */
    def calculate(Ra:DataSerie, Tmax:DataSerie, Tmin:DataSerie, krs:Double=0.16, log:DataLog, notes:String=""):DataSerie={
      //createDataSerieFromDates(P.getDates,ListFunctions.applyFunction_k(Functions.PDur,P.values,climate),"climate = "+climate+", "+notes)
      createDataSerie(Ra.start, Variable.msecInOneDay, ListFunctions.applyFunction_k(Functions.Rs_fromT,Ra.values, Tmax.values, Tmin.values, krs), log, notes)   //it is interval = 1 day
    }

    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(Ra), v.ds(Tmax), v.ds(Tmin), v.par(Krs).value, this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(Ra,1), v.ds(Tmax,1), v.ds(Tmin,1), v.par(Krs).value, this.getLog(v)))
    }
  }
  case object NetRad extends Variable("Net radiation","NetRad","MJ/m2/d",0.0,10000.0, classOf[Double]) with Serie with Calculable{
    in += Rs::Rs_fromT::Nil
    in += Ra::Nil
    in += H::Nil
    in += Tmax::Nil
    in += Tmin::Nil
    in += Altitude::Nil
    in += Albedo::Nil

    /**
     * Calculate NetRadiation
     *
     * @return               NetRad
     */
    def calculate(Rs:DataSerie, Ra:DataSerie, H:DataSerie, Tmax:DataSerie, Tmin:DataSerie, Altitude:Double, Albedo:Double=0.20, log:DataLog, notes:String=""):DataSerie={
      //createDataSerieFromDates(P.getDates,ListFunctions.applyFunction_k(Functions.PDur,P.values,climate),"climate = "+climate+", "+notes)
      createDataSerie(Rs.start, Variable.msecInOneDay, ListFunctions.applyFunction_kk(Functions.Rnet,Rs.values, Ra.values, H.values, Tmax.values, Tmin.values, Altitude, Albedo), log, notes)   //it is interval = 1 day
    }

    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(Rs), v.ds(Ra), v.ds(H), v.ds(Tmax), v.ds(Tmin), v.par(Altitude).value, v.par(Albedo).value, this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(Rs,1), v.ds(Ra,1), v.ds(H,1), v.ds(Tmax,1), v.ds(Tmin,1), v.par(Altitude).value, v.par(Albedo).value, this.getLog(v)))
    }
  }
  
  case object ns extends Variable("Actual duration of sunshine","ns","h",0,24, classOf[Double]) with Serie
  
  case object N extends Variable("Daylight hours FAO","N","h",0,24, classOf[Double]) with Serie with Calculable{
    in += Latitude::Nil
    
    /**
     * Calculate DayLightHours DataSerie
     *
     * @param  dates             dates
     * @param  latitude          latitude
     * @return                   DataSerie
     */
    def calculate(dates:List[Long],latitude:Double, log:DataLog, notes:String=""):DataSerie={
      //createDataSerieFromDates(dates,ListFunctions.applyFunction_k(Functions.DaylightHoursFAO,dates,latitude),"latitude = "+latitude+", "+notes)
	  createDataSerie(dates(0), Variable.msecInOneDay, ListFunctions.applyFunction_k(Functions.DaylightHoursFAO,dates,latitude), log, notes)     //it is interval = 1 day
    }
    
    

    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(dss.dateDs.values,v.par(Latitude).value, this.getLog(v))
    }   
    

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.lastDs().getDates,v.par(Latitude).value, this.getLog(v)))
    }
  }
  case object DaysSinceRain extends Variable("Days since last rainfall","DaysSinceRain","day",0,365, classOf[Long]) with Serie with Calculable{
    in += (P::P12::Nil)
    in += RainyDayThreshold::Nil

    /**
     * Calculate DaysSinceRain DataSerie
     *
     * @param  prev              previous value of DaysSinceRain
     * @param  rainyDayThreshold threshold to determinate if it's a rainy day
     * @param  P                 precipitations Dataserie
     * @param  notes             some notes on DataSerie creation
     * @return                   DataSerie
     */
    def calculate(prev:Double,rainyDayThreshold:Double,P:DataSerie, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(P.start,P.interval,ListFunctions.applyFunctionPrevThreshold(Functions.DaysSinceRain,prev,rainyDayThreshold,P.values), log, "start = "+prev+", threshold = "+rainyDayThreshold+", "+notes)
    }

    /**
     * launch calculate on DataCollection with a previous val
     *
     * @param   prev previous value of index
     * @param   dss  DataCollection > all DataSeries
     * @return       DataSerie
     */
    def calculate(prev:Double,dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(prev,v.par(RainyDayThreshold).value,v.ds(P), this.getLog(v))
    }

    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.prev.asInstanceOf[Double],v.par(RainyDayThreshold).value,v.ds(P,1), this.getLog(v)))
    }
  }
  case object WeekRain extends Variable("One week rainfall","WeekRain","mm/week",0,1000, classOf[Double]) with Serie with Calculable{
    in += (P::P12::P13::P15::Nil)
    
    /**
     * Calculate WeekRain DataSerie
     *
     * @param  P                 precipitations Dataserie
     * @param  notes             some notes on DataSerie creation
     * @return                   DataSerie
     */
    def calculate(P:DataSerie, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(P.start,P.interval,ListFunctions.weeklySum(P.values), log, notes)
    }

    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(P), this.getLog(v))
    }

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(P,8), this.getLog(v)))
    }
  }
  case object RainSum extends Variable("Sum of consecutive rainfall","RainSum","mm",0,1000, classOf[Double]) with Serie with Calculable{
    in += (P::P12::P13::P15::Nil)
    
    /**
     * Calculates RainSum DataSerie
     *
     * @param  prev              previous value of SumPrevRain
     * @param  P                 precipitations Dataserie
     * @param  notes             some notes on DataSerie creation
     * @return                   DataSerie
     */
    def calculate[T](prev:Double,P:DataSerie, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(P.start,P.interval,ListFunctions.applyFunctionPrev(Functions.RainSum,prev,P.values), log, "start = "+prev+", "+notes)
    }
    
    /**
     * launch calculate on DataCollection with a previous val
     *
     * @param   prev previous value of index
     * @param   dss  DataCollection > all DataSeries
     * @return       DataSerie
     */
    def calculate(prev:Double,dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(prev,v.ds(P), this.getLog(v))
    }
    
    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.prev.asInstanceOf[Double],v.ds(P,1), this.getLog(v)))
    }
  }
  case object lastRainSum extends Variable("Sum of last consecutive rainfall","lastRainSum","mm",0,1000, classOf[Double]) with Serie with Calculable{
    in += (P::P12::P13::P15::Nil)

    /**
     * Calculates RainSum DataSerie
     *
     * @param  prev              previous value of RainSum
     * @param  P                 precipitations Dataserie
     * @param  P_1               previous day precipitation (initialization)
     * @param  notes             some notes on DataSerie creation
     * @return                   DataSerie
     */
    def calculate[T](prev:Double,P:DataSerie,P_1:Double, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(P.start,P.interval,ListFunctions.applyFunctionPrevLastRainSum(Functions.lastRainSum,prev,P.values,P_1), log, "start = "+prev +", P_1 = "+P_1+", "+notes)
    }

    /**
     * launch calculate on DataCollection with a previous val
     *
     * @param   prev previous value of index
     * @param   dss  DataCollection > all DataSeries
     * @return       DataSerie
     */
    def calculate(prev:Double,dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(prev,v.ds(P),0, this.getLog(v))
    }


    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.prev.asInstanceOf[Double], v.ds(P,90),0, this.getLog(v)))
    }
  }
  case object lastRainSum_2_20 extends Variable("Sum of last significant rainfall event (days > 2mm in past 20 days)","lastRainSum_2_20","mm",0,1000, classOf[Double]) with Serie with Calculable{
    in += (P::P12::P13::P15::Nil)


    def calculate[T](P:DataSerie, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(P.start,P.interval,ListFunctions.lastRainSum_withThreshold_20days(P.values, 2.0, 20)._1, log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(P), this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(P,21), this.getLog(v)))
    }
  }
  case object AgeRainEvent_2_20 extends Variable("Age of last significant rainfall event (days > 2mm in past 20 days)(day with max rain in event)","AgeRainEvent_2_20","days",0,1000, classOf[Double]) with Serie with Calculable{
    in += (P::P12::P13::P15::Nil)


    def calculate[T](P:DataSerie, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(P.start, P.interval, ListFunctions.lastRainSum_withThreshold_20days(P.values, 2.0, 20)._2, log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(P), this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(P,20), this.getLog(v)))
    }
  }
  
  case object VPD extends Variable("Vapour pressure deficit value","VPD","kPa",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (H::Nil)
    in += (Tmax::Nil)
    in += (Tmin::Nil)
    
    /**
     * Calculate VPD DataSerie
     *
     * @param  H       relative humidity Dataserie
     * @param  Tmax    maximal temperature DataSerie
     * @param  Tmin    minimal temperature DataSerie
     * @param  notes   some notes on DataSerie creation
     * @return         DataSerie
     */
    def calculate(H:DataSerie,Tmax:DataSerie,Tmin:DataSerie, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(Tmax.start,Tmax.interval,ListFunctions.applyFunction(Functions.VPD,H.values,Tmax.values,Tmin.values), log, notes)
    }
    
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(H),v.ds(Tmax),v.ds(Tmin), this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(H,1),v.ds(Tmax,1),v.ds(Tmin,1), this.getLog(v)))
    }
  }
  case object VPD12 extends Variable("Vapour pressure deficit value at 12:00","VPD12","kPa",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T12::T13::T15::T::Tmax::Nil)
    in += (H12::H13::H15::H::Nil)
    
    /**
     * Calculate VPD DataSerie
     *
     * @param  H13     relative humidity Dataserie
     * @param  T13     maximal temperature DataSerie
     * @param  notes   some notes on DataSerie creation
     * @return         DataSerie
     */
    def calculate(H12:DataSerie,T12:DataSerie, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(T12.start,T12.interval,ListFunctions.applyFunction(Functions.VPD,H12.values,T12.values), log, notes)
    }
    
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(H12),v.ds(T12), this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(H12,1),v.ds(T12,1), this.getLog(v)))
    }
  }
  case object VPD13 extends Variable("Vapour pressure deficit value at 13:00","VPD13","kPa",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T13::T12::T15::T::Tmax::Nil)
    in += (H13::H12::H15::H::Nil)
    
    /**
     * Calculate VPD DataSerie
     *
     * @param  H13     relative humidity Dataserie
     * @param  T13     maximal temperature DataSerie
     * @param  notes   some notes on DataSerie creation
     * @return         DataSerie
     */
    def calculate(H13:DataSerie,T13:DataSerie, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(T13.start,T13.interval,ListFunctions.applyFunction(Functions.VPD,H13.values,T13.values), log, notes)
    }
    
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(H13),v.ds(T13), this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(H13,1),v.ds(T13,1), this.getLog(v)))
    }
  }
  case object PETthorn extends Variable("Potential evapotranspiration value Thorntwaite formula","PETthorn","mm/day",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += T::T12::T13::T15::Nil
    in += I::Nil
    in += N::Nil
    
    /**
     * Calculate PETthorn DataSerie
     *
     * @param  T             temperature DataSerie
     * @param  DaylightHour  daylight hours
     * @param  I             heat index 
     * @param  notes         some notes on DataSerie creation
     * @return               DataSerie
     */
    def calculate(T:DataSerie,daylightHour:DataSerie,I:Double, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(T.start,T.interval,ListFunctions.applyFunction_k(Functions.PETthorn,T.values,daylightHour.values,I), log, notes)
    }
    
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(T),v.ds(N),v.par(I).value, this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(T,1),v.ds(N,1),v.par(I).value, this.getLog(v)))
    }
  }
  case object PETthorn_camargo extends Variable("Potential evapotranspiration value Thorntwaite formula, with Tef of Camargo et al (1999)","PETthorn_camargo","mm/day",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += Tmin::Nil
    in += Tmax::Nil
    in += I::Nil
    in += N::Nil

    /**
     * Calculate PETthorn DataSerie
     *
     * @param  T             temperature DataSerie
     * @param  DaylightHour  daylight hours
     * @param  I             heat index
     * @param  notes         some notes on DataSerie creation
     * @return               DataSerie
     */
    def calculate(Tmin:DataSerie,Tmax:DataSerie,daylightHour:DataSerie,I:Double, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(Tmin.start,Tmin.interval,ListFunctions.applyFunction_k(Functions.PETthorn_camargo,Tmin.values,Tmax.values,daylightHour.values,I), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(Tmin),v.ds(Tmax),v.ds(N),v.par(I).value, this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(Tmin,1),v.ds(Tmax,1),v.ds(N,1),v.par(I).value, this.getLog(v)))
    }
  }
  case object PETthorn_pereira extends Variable("Potential evapotranspiration value Thorntwaite formula, with Tef of Pereira and Pruitt (2004)","PETthorn_pereira","mm/day",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += Tmin::Nil
    in += Tmax::Nil
    in += I::Nil
    in += N::Nil

    /**
     * Calculate PETthorn DataSerie
     *
     * @param  T             temperature DataSerie
     * @param  DaylightHour  daylight hours
     * @param  I             heat index
     * @param  notes         some notes on DataSerie creation
     * @return               DataSerie
     */
    def calculate(Tmin:DataSerie,Tmax:DataSerie,daylightHour:DataSerie,I:Double, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(Tmin.start,Tmin.interval,ListFunctions.applyFunction_k(Functions.PETthorn_pereira,Tmin.values,Tmax.values,daylightHour.values,I), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(Tmin),v.ds(Tmax),v.ds(N),v.par(I).value, this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(Tmin,1),v.ds(Tmax,1),v.ds(N,1),v.par(I).value, this.getLog(v)))
    }
  }
  case object PETpen extends Variable("Potential evapotranspiration value Penmann formula","PETpen","mm/day",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (NetRad::Nil)
    in += (T::Nil)
    in += (U::Nil)
    in += (VPD::Nil)
    in += Altitude::Nil

    /**
     * Calculate PETpen DataSerie
     *
     * @param  NetRad  Net radiation Dataserie
     * @param  T       temperature DataSerie
     * @param  U       wind speed DataSerie
     * @param  VPD     Vapor pressur deficit DataSerie
     * @param  notes   some notes on DataSerie creation
     * @return         DataSerie
     */
    def calculate(NetRad:DataSerie,T:DataSerie,U:DataSerie,VPD:DataSerie,altitude:Double, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(T.start,T.interval,ListFunctions.applyFunction_k(Functions.PETpen,NetRad.values,T.values,U.values,VPD.values,altitude), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(NetRad),v.ds(T),v.ds(U),v.ds(VPD),v.par(Altitude).value, this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(NetRad,1),v.ds(T,1),v.ds(U,1),v.ds(VPD,1),v.par(Altitude).value, this.getLog(v)))
    }
  }
  case object EMC extends Variable("Equilibrium moisture content","EMC","%",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T::T12::Tmax::T13::T15::Nil)
    in += (H::H12::H13::H15::Nil)

    /**
     * Calculate EMC index DataSerie
     *
     * @param  T       temperature DataSerie
     * @param  H       relative humidity DataSerie
     * @param  notes   some notes on DataSerie creation
     * @return         DataSerie
     */
    def calculate(T:DataSerie,H:DataSerie, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(T.start,T.interval,ListFunctions.applyFunction(Functions.EMC,T.values,H.values), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(T),v.ds(H), this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(T,1),v.ds(H,1), this.getLog(v)))
    }
  }
  case object EMCfa extends Variable("Equilibrium moisture content at fuel-atmosphere interface","EMCfa","%",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T::T12::Tmax::T13::T15::Nil)
    in += (H::H12::H13::H15::Nil)
    in += (Cc::Nil)

    /**
     * Calculate EMCfa index DataSerie
     *
     * @param  T       temperature DataSerie
     * @param  H       relative humidity DataSerie
     * @param  Cc      cloud cover fraction DataSerie
     * @param  notes   some notes on DataSerie creation
     * @return         EMCfa
     */
    def calculate(T:DataSerie,H:DataSerie,Cc:DataSerie, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(T.start,T.interval,ListFunctions.applyFunction(Functions.EMCfa,T.values,H.values,Cc.values), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(T),v.ds(H),v.ds(Cc), this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(T,1),v.ds(H,1),v.ds(Cc,1), this.getLog(v)))
    }
  }
  case object EMC24 extends Variable("Weighted 24-hour average EMC","EMC24","%",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (Tmin::Nil)
    in += (Tmax::Nil)
    in += (Hmin::Nil)
    in += (Hmax::Nil)
    in += (N::Nil)

    /**
     * Computes weighted 24-hour average EMC
     * (Brandshaw et al. 1983)
     *
     * @param    Tmin   minimum Temperature
     * @param    Tmax   maximum Temperature
     * @param    Hmin   minimum Relative Humidity
     * @param    Hmax   maximum Relative Humidity
     * @param    N      daylength [hours]
     * @return          weighted 24-hour average Equilibrium moisture content
     */
    def calculate(Tmin:DataSerie,Tmax:DataSerie,Hmin:DataSerie,Hmax:DataSerie,N:DataSerie, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(Tmin.start,Tmin.interval,ListFunctions.applyFunction(Functions.EMC24,Tmin.values,Tmax.values,Hmin.values,Hmax.values,N.values), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(Tmin),v.ds(Tmax),v.ds(Hmin),v.ds(Hmax),v.ds(N), this.getLog(v))
    }
  

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(Tmin,1),v.ds(Tmax,1),v.ds(Hmin,1),v.ds(Hmax,1),v.ds(N,1), this.getLog(v)))
    }
  }
//  case object NetRad extends Variable("Net Radiation","NetRad","--",-100000,100000, classOf[Double]) with Serie with Calculable{
//    in += H::Nil
//    in += Tmax::Nil
//    in += Tmin::Nil
//    in += Latitude::Nil
//    in += Altitude::Nil
//    in += Krs::Nil
//    in += Albedo::Nil
//    
//    /**
//     * Calculate Net radiation DataSerie
//     *
//     * @param  H               relative humidity DataSerie
//     * @param  Tmax            maximal temperature DataSerie
//     * @param  Tmin            minimal temperature DataSerie
//     * @param  latitude        latitude of measurement location
//     * @param  altitude        altitude of measurement location
//     * @param  krs             krs correction factor
//     * @param  albedo          albedo  correction factor
//     * @param  notes           some notes on DataSerie creation
//     * @return                 DataSerie
//     */
//    def calculate(H:DataSerie,Tmax:DataSerie,Tmin:DataSerie,latitude:Double,altitude:Double,krs:Double=0.16,albedo:Double=0.20, log:DataLog, notes:String=""):DataSerie={
//      createDataSerie(H.start,H.interval,ListFunctions.applyFunction_kkkk(Functions.Rnet,H.getDates,H.values,Tmax.values,Tmin.values,latitude,altitude,krs,albedo), log, notes)
//    }
//    
//    def calculate(dss:DataCollection):DataSerie={
//      val v=chooseVariablesAndCalculate(dss)
//      calculate(v.ds(H),v.ds(Tmax),v.ds(Tmin),v.par(Latitude).value,v.par(Altitude).value,v.par(Krs).value,v.par(Albedo).value, this.getLog(v))
//    }
//    
//    def complete(dss:DataCollection):DataSerie={
//      val v=chooseVariablesAndComplete(dss)
//      val me=dss.dss(this)
//      
//      me.updateLastAndNotes(calculate(v.ds(H,1),v.ds(Tmax,1),v.ds(Tmin,1),v.par(Latitude).value,v.par(Altitude).value,v.par(Krs).value,v.par(Albedo).value, this.getLog(v)))
//    }
//  }
