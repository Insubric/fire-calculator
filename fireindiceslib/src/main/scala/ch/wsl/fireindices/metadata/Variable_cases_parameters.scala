package ch.wsl.fireindices.metadata

import ch.wsl.fireindices.functions.Functions
import ch.wsl.fireindices.ImplicitConversions._
import ch.wsl.fireindices.log.DataLog


  case object Altitude extends Variable("Altitude","Altitude","masl",0,10000, classOf[Double])
  case object Aspect extends Variable("Aspect","Aspect","Azimuth[0-360]",0,360, classOf[Double])
  case object Slope extends Variable("Slope","Slope","°[0-90]",0,90, classOf[Double])
  case object Latitude extends Variable("Latitude","Latitude","°",-90,90, classOf[Double])
  case object Albedo extends Variable("Albedo","Albedo","--",0.20,0.25, classOf[Double])  //0.20
  case object Krs extends Variable("Krs","Krs","--",0.16,0.19, classOf[Double])  //0.16
  case object Rs_a extends Variable("Fraction of extraterrestrial radiation reaching the earth on overcast days","Rs_a","--",0.0,1.0, classOf[Double])  //0.25
  case object Rs_b extends Variable("Part of the Fraction of extraterrestrial radiation reaching the earth on clear days","Rs_b","--",0.0,1.0, classOf[Double])  //0.50
  case object RainyDayThreshold extends Variable("Rain threshold (daily)","RainyDayThreshold","mm",0,10, classOf[Double])//0.05 * 25.4
  
  //used for KBDI, KBDISI and IREPI
  case object RainyWeekThreshold extends Variable("Minimum weekly rainfall for soil saturation","RainyWeekThreshold","",0,1000, classOf[Double])//120.0 //mm
  
  case object FFMCstart extends Variable("Initial FFMC","FFMCstart","--",0,100, classOf[Double]) //85
  case object DMCstart extends Variable("Initial DMC","DMCstart","--",0,100, classOf[Double])  //6
  case object DCstart extends Variable("Initial  DC","DCstart","--",0,100, classOf[Double])  //15
  case object SDMCstart extends Variable("Initial SDMC","SDMCstart","--",0,100, classOf[Double])  //12
  case object FireSeasonStart extends Variable("Begin of the fire season","FireSeasonStart","day-month",-3600000L,31622400000L, classOf[Long])   // Utils.solarDateToLong("0215", "MMdd")
  case object FireSeasonEnd extends Variable("End of the fire season","FireSeasonEnd","day-month",-3600000L,31622400000L, classOf[Long])  //Utils.solarDateToLong("0930", "MMdd")
  case object M68VegCorrStep3Start extends Variable("Begin of Step 3","M68VegCorrStep3Start","--",-3600000L,31622400000L, classOf[Long])  // Utils.solarDateToLong("0810", "MMdd")
  case object M68VegCorrStep3End extends Variable("End of Step 3","M68VegCorrStep3End","--",-3600000L,31622400000L, classOf[Long])  // Utils.solarDateToLong("0901", "MMdd")
  case object MeanAnnualRain extends Variable("Mean annual rain","MeanAnnualRain","mm/year",0,20000, classOf[Double]) with Calculable {
    in += P::P12::P13::P15::Nil
    
    /**
     * calculate mean annual rainfall from the rainfall amount DataSerie
     * (if the dataserie has missing values, it skips them, but calculate only if 
     * the ratio of missing values is belove 20 %)
     *
     * @param  P  DataSerie > the rainfall DataSerie
     * @return    Double
     */
    def calculate(P:DataSerie, notes:String=""):Double={
      val ratioMissing:Double = P.values.filter(_.isNaN).length / P.values.length

      if (ratioMissing < 0.20)
        Functions.meanAnnualRain(P.values)
      else
        Double.NaN
    }
    
    def calculate(dss:DataCollection):Parameter={
      val v=chooseVariablesAndCalculate(dss)
      new Parameter(MeanAnnualRain, calculate(v.ds(P)), MeanAnnualRain.getLog(v))
    }
    
    /**
     * not possible to complete on DataCollection
     *
     * @param   dss  DataCollection > all DataSeries
     * @return       Parameter
     */
    def complete(dss:DataCollection):Parameter={
      throw new Exception("it is not possible to calculate MeanAnnualRain on a day by day basis")
    }
    
  }
  case object I extends Variable("Heat index","I","",0,20000, classOf[Double]) with Calculable {
    in += T::Nil

    /**
     * calculate the heat index from the temperature DataSerie
     *
     * @param  T  temperature DataSerie
     * @return    Double
     */
    def calculate(T:DataSerie, log: DataLog, notes:String=""):Parameter={
      var ind = 0.0
      for (month <- List("01","02","03","04","05","06","07","08","09","10","11","12")){
        val monthlyData = T.sliceData("MM",month)
        ind += math.pow(math.max(0,monthlyData.filterNot(_.isNaN).sum/monthlyData.filterNot(_.isNaN).length)/5, 1.514)    //TODO verify if only for max(0,T)
      }
      new Parameter(I,ind, log, notes)
    }

    def calculate(dss:DataCollection):Parameter={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(T), I.getLog(v))
    }

    /**
     * not possible to complete on DataCollection
     *
     * @param   dss  DataCollection > all DataSeries
     * @return       Parameter
     */
    def complete(dss:DataCollection):Parameter={
      throw new Exception("it is not possible to calculate I on a day by day basis")
    }
  }
    
  case object XsnowcoverStart extends Variable("XsnowcoverStart","XsnowcoverStart","day-month",-3600000L,31622400000L, classOf[Long])
  case object XsnowcoverEnd extends Variable("XsnowcoverEnd","XsnowcoverEnd","day-month",-3600000L,31622400000L, classOf[Long])
  case object XbirchLeaves extends Variable("XbirchLeaves","XbirchLeaves","day-month",-3600000L,31622400000L, classOf[Long])
  case object XrobiniaBlossom extends Variable("XrobiniaBlossom","XrobiniaBlossom","day-month",-3600000L,31622400000L, classOf[Long])
  case object Climate extends Variable("Climate class (Deeming et al. 1977)","Climate","1-4",1,4, classOf[Int])

  case object Risico_v0 extends Variable("initial speed","Risico_v0","m/h",0.0,1000.0, classOf[Double])  
  case object Risico_d0 extends Variable("fuel density in litter","Risico_d0","kg/m^2",0.0,1000.0, classOf[Double])  
  case object Risico_d1 extends Variable("live fuel density","Risico_d1","kg/m^2",0.0,1000.0, classOf[Double])  
  case object Risico_T0 extends Variable("response time","Risico_T0","h",0.0,100.0, classOf[Double])  
  case object Risico_sat extends Variable("saturation humidity","Risico_sat","%",0.0,100.0, classOf[Double])  
  case object Risico_hhv extends Variable("higher heating value","Risico_hhv","kJ/kg",0.0,1000000.0, classOf[Double])  
  case object Risico_humidity extends Variable("live fuel humidity","Risico_humidity","-",0.0,100.0, classOf[Double])  //todo verify unit
//  case object Risico_dT extends Variable("risico calculation step","Risico_dT","h",0.0,100.0, classOf[Double])  
  

//	v0: velocità iniziale
	//	d0: densità del combustibile nella lettiera
	//	d1: densità del combustibile vivo
	//	T0: tempo di risposta
	//	sat: umidità di saturazione
	//	hhv: higher heating value
	//	humidity: umidità del combustibile vivo