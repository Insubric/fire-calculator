package ch.wsl.fireindices.metadata

import ch.wsl.fireindices.functions.Functions
import ch.wsl.fireindices.functions.ListFunctions
import ch.wsl.fireindices.functions.Utils
import ch.wsl.fireindices.ImplicitConversions._

import ch.wsl.fireindices.log.DataLog
import scala.collection.mutable.ListBuffer


  case object Nesterov extends Variable("Nesterov index value","Nesterov","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T15::T12::T::Nil)
    in += (Tdew15::Tdew12::Tdew::Nil)
    in += (P15::P12::P::Nil)
    
    /**
     * Calculate Nesterov index DataSerie
     *
     * @param  prev    previous value of Nesterov index
     * @param  T15     temperature at 15:00 DataSerie
     * @param  Tdew15  dewpoint temperature at 15:00 DataSerie
     * @param  P       precipitations Dataserie
     * @param  notes   some notes on DataSerie creation
     * @return         DataSerie
     */
    def calculate(prev:Double,T15:DataSerie,Tdew15:DataSerie,P:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(T15.start,T15.interval,ListFunctions.applyFunctionPrev(Functions.Nesterov,prev,T15.values,Tdew15.values,P.values), log, "start = "+prev+", "+notes)
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
      calculate(prev,v.ds(T15),v.ds(Tdew15),v.ds(P15), this.getLog(v))
    }
   
    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }
     
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.prev.asInstanceOf[Double],v.ds(T15,1),v.ds(Tdew15,1),v.ds(P15,1), this.getLog(v)))
    }
  }

  case object Angstroem extends Variable("Angstroem index value","Angstroem","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T13::T12::T::Nil)
    in += (H13::H12::H::Nil)
    
    /**
     * Calculate PETpen DataSerie
     *
     * @param  T       temperature DataSerie
     * @param  H       relative humidity DataSerie
     * @param  notes   some notes on DataSerie creation
     * @return         DataSerie
     */
    def calculate(T13:DataSerie,H13:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(T13.start,T13.interval,ListFunctions.applyFunction(Functions.Angstroem,T13.values,H13.values), log, notes)
    }
   
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(T13),v.ds(H13), this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(T13,1),v.ds(H13,1), this.getLog(v)))
    }
  }
  case object Munger extends Variable("Munger index value","Munger","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (P::P12::P13::P15::Nil)
    
    /**
     * Calculate Munger index DataSerie
     *
     * @param  prev    previous value of Munger Indedx
     * @param  P       precipitation DataSerie
     * @param  notes   some notes on DataSerie creation
     * @return         DataSerie
     */
    def calculate(prev:Double,P:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(P.start,P.interval,ListFunctions.applyFunctionPrev(Functions.Munger,prev,P.values), log,"start = "+prev+" "+notes)
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
  case object MC1 extends Variable("1-hour timelag fuel moisture","MC1","%",0,100, classOf[Double]) with Serie with Calculable{
    in += (EMCfa::Nil)

    /**
     * Calculate MC1 index DataSerie
     *
     * @param  EMCfa   Equilibrium moisture content at fuel-atmosphere interface
     * @param  notes   some notes on DataSerie creation
     * @return         MC1
     */
    def calculate(EMCfa:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(EMCfa.start,EMCfa.interval, EMCfa.values.map(Functions.MC1(_)), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(EMCfa), this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(EMCfa,1), this.getLog(v)))
    }
  }
  case object MC10 extends Variable("10-hour timelag fuel moisture","MC10","%",0,100, classOf[Double]) with Serie with Calculable{
    in += (EMCfa::Nil)

    /**
     * Calculate MC10 index DataSerie
     *
     * @param  EMCfa   Equilibrium moisture content at fuel-atmosphere interface
     * @param  notes   some notes on DataSerie creation
     * @return         MC10
     */
    def calculate(EMCfa:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(EMCfa.start,EMCfa.interval, EMCfa.values.map(Functions.MC10(_)), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(EMCfa), this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(EMCfa,1), this.getLog(v)))
    }
  }
  case object MC100 extends Variable("100-hour timelag fuel moisture","MC100","%",0,100, classOf[Double]) with Serie with Calculable{
    in += (EMC24::Nil)
    in += (PDur::Nil)

    /**
     * Calculate MC100 index DataSerie
     *
     * @param  EMC24   Weighted 24-hour average EMC
     * @param  PDur    duration of precipitation [h]
     * @param  notes   some notes on DataSerie creation
     * @return         MC100
     */
    def calculate(prev:Double,EMC24:DataSerie,PDur:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(EMC24.start,EMC24.interval,ListFunctions.applyFunctionPrev(Functions.MC100,prev,EMC24.values,PDur.values), log, "start = "+prev+"  "+notes)
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
      calculate(prev ,v.ds(EMC24),v.ds(PDur), this.getLog(v))
    }
    
    /**
     * launch calculate on DataCollection (30% initial value)
     *
     * @param   dss  DataCollection > all DataSeries
     * @return       DataSerie
     */
    def calculate(dss:DataCollection):DataSerie={
      calculate(30.0 ,dss)   //30 % initial value
    }
     

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.prev.asInstanceOf[Double],v.ds(EMC24,1),v.ds(PDur,1), this.getLog(v)))
    }
  }
  case object MC1000 extends Variable("1000-hour timelag fuel moisture","MC1000","%",0,100, classOf[Double]) with Serie with Calculable{
    in += (EMC24::Nil)
    in += (PDur::Nil)

    /**
     * Calculate MC1000 index DataSerie
     *
     * @param  EMC24   Weighted 24-hour average EMC
     * @param  PDur    duration of precipitation [h]
     * @param  notes   some notes on DataSerie creation
     * @return         MC1000
     */
    def calculate(EMC24:DataSerie,PDur:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(EMC24.start,EMC24.interval,Functions.MC1000(EMC24.values,PDur.values), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(EMC24),v.ds(PDur), this.getLog(v))   //30 % initial value
    }
    
 

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(EMC24,1),v.ds(PDur,1), this.getLog(v)))   //30 % initial value
    }
  }

  case object FFWI extends Variable("FFWI index value","FFWI","--",-100000,100000, classOf[Double]) with Serie with Calculable{
//    in += (T::T12::Tmax::T13::Nil)
//    in += (H::H12::Hmax::H13::Nil)
    in += (EMC::Nil)
    in += (U::U12::U13::Nil)
    
    /**
     * Calculate FFWI index DataSerie
     *
     * @param  EMC     equilibrium moisture content DataSerie
     * @param  U       wind speed DataSerie
     * @param  notes   some notes on DataSerie creation
     * @return         DataSerie
     */
    def calculate(EMC:DataSerie,U:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(EMC.start,EMC.interval,ListFunctions.applyFunction(Functions.FFWI,EMC.values,U.values), log, notes)
    }
   
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(EMC),v.ds(U), this.getLog(v))
    }    
    
   /**
     * launch complete on DataCollection
     *
     * @param   dss  DataCollection > all DataSeries
     * @return       DataSerie
     */
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(EMC,1),v.ds(U,1), this.getLog(v)))
    }
  }
  case object FFWImod extends Variable("modified FFWI index value","FFWImod","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (KBDI::Nil)
    in += (FFWI::Nil)

    /**
     * Calculate modified FFWI index DataSerie
     *
     * @param  KBDI    KBDI index [inch/100]
     * @param  FFWI    Fosberg index
     * @param  notes   some notes on DataSerie creation
     * @return         DataSerie
     */
    def calculate(KBDI:DataSerie,FFWI:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(KBDI.start,KBDI.interval,ListFunctions.applyFunction(Functions.FFWImod,KBDI.values,FFWI.values), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(KBDI),v.ds(FFWI), this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(KBDI,1),v.ds(FFWI,1), this.getLog(v)))
    }
  }
  case object KBDI extends Variable("KBDI index value","KBDI","inches/100",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (Tmax::T15::T13::T12::T::Nil)
    in += (P::P12::P13::P15::Nil)
    in += MeanAnnualRain::Nil
    in += RainyWeekThreshold::Nil
    in += RainSum::Nil
    in += WeekRain::Nil
    in += RainyDayThreshold::Nil
    
    /**
     * Calculate KBDI index DataSerie (inches values)
     *
     * @param  prev           previous value of KBDI Indedx
     * @param  threshold      threshold to initalize KBDI index at zero
     * @param  meanAnnualRain mean annual rainfall parameter
     * @param  Tmax           maximal temperature DataSerie
     * @param  SumPrevRain    consecutive rainfall sum DataSerie
     * @param  P              precipitation DataSerie
     * @param  notes          some notes on DataSerie creation
     * @return                DataSerie
     */
    def calculate(prev:Double,threshold:Double,meanAnnualRain:Double,Tmax:DataSerie,SumPrevRain:DataSerie,P:DataSerie,WeekRain:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(Tmax.start,Tmax.interval,ListFunctions.applyFunctionPrevThreshold(Functions.KBDI,prev,threshold,meanAnnualRain,Tmax.values,SumPrevRain.values,P.values,WeekRain.values), log, "start = "+prev+" "+notes)
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
      calculate(prev,v.par(RainyWeekThreshold).value,v.par(MeanAnnualRain).value,v.ds(Tmax),v.ds(RainSum),v.ds(P),v.ds(WeekRain), this.getLog(v))
    }
   
    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }
 
 
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.prev.asInstanceOf[Double],v.par(RainyWeekThreshold).value,v.par(MeanAnnualRain).value,v.ds(Tmax,1),v.ds(RainSum,1),v.ds(P,1),v.ds(WeekRain,1), this.getLog(v)))
    }
  }
  case object KBDISI extends Variable("KBDISI index value","KBDISI","mm",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (Tmax::T15::T13::T12::T::Nil)
    in += (P::P12::P13::P15::Nil)
    in += MeanAnnualRain::Nil
    in += RainyWeekThreshold::Nil
    in += RainSum::Nil
    in += WeekRain::Nil
    in += RainyDayThreshold::Nil
    
    /**
     * Calculate KBDISI index DataSerie (metrical values)
     *
     * @param  prev           previous value of KBDISI Indedx
     * @param  PweekThreshold threshold to initalize KBDISI index at zero
     * @param  meanAnnualRain mean annual rainfall parameter
     * @param  Tmax           maximal temperature DataSerie
     * @param  SumPrevRain    consecutive rainfall sum DataSerie
     * @param  P              precipitation DataSerie
     * @param  notes          some notes on DataSerie creation
     * @return                DataSerie
     */
    def calculate(prev:Double,PweekThreshold:Double,Py_avg:Double,Tmax:DataSerie,SumPrevRain:DataSerie,P:DataSerie,WeekRain:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(Tmax.start,Tmax.interval,ListFunctions.applyFunctionPrevThreshold(Functions.KBDI_SI,prev,PweekThreshold,Py_avg,Tmax.values,SumPrevRain.values,P.values,WeekRain.values), log, "start = "+prev+" "+notes)
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
      calculate(prev,v.par(RainyWeekThreshold).value,v.par(MeanAnnualRain).value,v.ds(Tmax),v.ds(RainSum),v.ds(P),v.ds(WeekRain), this.getLog(v))
    }
   
    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.prev.asInstanceOf[Double],v.par(RainyWeekThreshold).value,v.par(MeanAnnualRain).value,v.ds(Tmax,1),v.ds(RainSum,1),v.ds(P,1),v.ds(WeekRain,1), this.getLog(v)))
    }
  }
  case object Sharples extends Variable("Sharples index value","Sharples","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T::T12::T13::T15::Tmax::Nil)
    in += (H::H12::H13::H15::Nil)
    in += (U::U12::U13::U15::Umax::Nil)
    
    /**
     * Calculate Sharples index DataSerie
     *
     * @param  T       temperature DataSerie
     * @param  H       relative humidity DataSerie
     * @param  U       wind speed DataSerie
     * @param  notes   some notes on DataSerie creation
     * @return         DataSerie
     */
    def calculate(T:DataSerie,H:DataSerie,U:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(T.start,T.interval,ListFunctions.applyFunction(Functions.Sharples,T.values,H.values,U.values), log, notes)
    }
   
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(T),v.ds(H),v.ds(U), this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(T,1),v.ds(H,1),v.ds(U,1), this.getLog(v)))
    }
  }

  case object FMI extends Variable("Sharples fuel moisture index","FMI","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T::T12::T13::T15::Tmax::Nil)
    in += (H::H12::H13::H15::Nil)
    
    /**
     * Calculate FMI index DataSerie
     *
     * @param  T       temperature DataSerie
     * @param  H       relative humidity DataSerie
     * @param  notes   some notes on DataSerie creation
     * @return         DataSerie
     */
    def calculate(T:DataSerie,H:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(T.start,T.interval,ListFunctions.applyFunction(Functions.FMI,T.values,H.values), log, notes)
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
case object OrieuxDanger extends Variable("Orieux Danger classes","OrieuxDanger","",0,3, classOf[Double]) with Serie with Calculable{
    in += res::Nil
    in += U::U12::U13::U15::Nil

    /**
     * Calculate OrieuxDanger DataSerie
     *
     * @param  r               soil reserve DataSerie [mm]
     * @param  U               wind speed DataSerie [m/s
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(r:DataSerie,U:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(r.start,r.interval,ListFunctions.applyFunction(Functions.OrieuxDanger,r.values,U.values), log, notes)
    }

    /**
     * launch calculate on DataCollection
     *
     * @param   prev previous value of index
     * @param   dss  DataCollection > all DataSeries
     * @return       DataSerie
     */
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(res),v.ds(U), this.getLog(v))
    }

    /**
     * launch complete on DataCollection
     *
     * @param   prev previous value of index
     * @param   dss  DataCollection > all DataSeries
     * @return       DataSerie
     */
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(res,1),v.ds(U,1), this.getLog(v)))
    }  
  }
  case object res extends Variable("soil water reserve (Orieux Index)","res","mm",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += P::P12::P13::P15::Nil
    in += PETthorn::Nil
    in += WeekRain::Nil
    in += RainyWeekThreshold::Nil

    /**
     * Calculate soil water reserve of Orieux index DataSerie [mm]
     *
     * @param  prev            previous value of Orieux index
     * @param  PweekThreshold  weekly rain threshol to initialize index
     * @param  P               precipitations
     * @param  PETthorn        potential evapotranspiration after Thorntwaite
     * @param  WeekRain        weekly rain sum to initialize index
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(prev:Double,PweekThreshold:Double,P:DataSerie,PETthorn:DataSerie,WeekRain:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(P.start,P.interval,ListFunctions.applyFunctionPrevThreshold(Functions.r,prev,PweekThreshold,P.values,PETthorn.values,WeekRain.values), log, "start = "+prev+",PweekThreshold = "+PweekThreshold+" "+notes)
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
      calculate(prev,v.par(RainyWeekThreshold).value,v.ds(P),v.ds(PETthorn),v.ds(WeekRain), this.getLog(v))
    }


    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.prev.asInstanceOf[Double], v.par(RainyWeekThreshold).value,v.ds(P,1),v.ds(PETthorn,1),v.ds(WeekRain,1), this.getLog(v)))
   }

  }
  case object res_surf extends Variable("surface soil water reserve (Carrega 1988)","res_surf","mm",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += P::P12::P13::P15::Nil
    in += PETthorn::Nil
    in += RainSum::Nil

    /**
     * Calculate rs (surface soil water reserve) DataSerie (Carrega 1988)
     *
     * @param    prev             previous'day rs  DataSerie
     * @param    P                rainfall [mm] DataSerie
     * @param    PETth            potential evapotranspiration after Thornthwaite DataSerie
     * @param    RainSum          sum of continuous rain DataSerie
     * @param    notes            some notes on DataSerie creation
     * @return                    rs DataSerie
     */
    def calculate(prev:Double,P:DataSerie,PETthorn:DataSerie,RainSum:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(P.start,P.interval,ListFunctions.applyFunctionPrev(Functions.rs,prev,P.values,PETthorn.values,RainSum.values), log, "start = "+prev+" "+notes)
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
      calculate(prev,v.ds(P),v.ds(PETthorn),v.ds(RainSum), this.getLog(v))
    }


    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }
 
 
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.prev.asInstanceOf[Double] ,v.ds(P,1),v.ds(PETthorn,1),v.ds(RainSum,1), this.getLog(v)))
    }
  }
  case object I87 extends Variable("I87 index (Carrega 1988)","I87","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += T::Nil
    in += H::Nil
    in += U::Nil
    in += P::Nil
    in += res::Nil
    in += res_surf::Nil
    in += PC::Nil

    /**
     * Calculate I87 index DataSerie (Carrega 1988)
     *
     * @param    T             air temperature
     * @param    H             air humidity [%]
     * @param    U             wind speed [m/s]
     * @param    P             rainfall [mm]
     * @param    r             Orieux soil water reserve [mm]
     * @param    rs            Carrega surface soil water reserve [mm]
     * @param    PC            phenological coefficient
     * @param    notes         some notes on DataSerie creation
     * @return                 I87 DataSerie
     */
    def calculate(T:DataSerie,H:DataSerie,U:DataSerie,P:DataSerie,r:DataSerie,rs:DataSerie,PC:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(T.start,T.interval,ListFunctions.applyFunction(Functions.I87,T.values,H.values,U.values,P.values,r.values,rs.values,PC.values.map(_.toInt)), log, notes)
    }

    /**
     * launch calculate on DataCollection
     *
     * @param   dss  all DataSeries
     * @return       DataSerie
     */
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)

      calculate(v.ds(T),v.ds(H),v.ds(U),v.ds(P),v.ds(res),v.ds(res_surf),v.ds(PC), this.getLog(v))
    }
    
    /**
     * launch complete on DataCollection
     *
     * @param   dss  all DataSeries
     * @return       DataSerie
     */
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(T,1),v.ds(H,1),v.ds(U,1),v.ds(P,1),v.ds(res,1),v.ds(res_surf,1),v.ds(PC,1), this.getLog(v)))
    }

  }
  case object pM68 extends Variable("pM68 index value","pM68","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T13::T12::T::Nil)
    in += (VPD13::VPD12::VPD::Nil)
//    in += (H13::H12::Hmax::H::Nil)
    in += (P13::P12::P::Nil)
    in += (SnowCover::Nil)
    in += FireSeasonStart::Nil
    in += FireSeasonEnd::Nil
    
    /**
     * Calculate pM68 index DataSerie (M68 precipitation correction)
     *
     * @param  prev            previous value of pM68
     * @param  T13             temperature at 13:00 DataSerie
     * @param  VPD13           vapour pressure deficit at 13:00 DataSerie
     * @param  P13             precipitation sum at 13:00 DataSerie
     * @param  P_1             precipitations of day before
     * @param  P_2             precipitations of 2 days before
     * @param  P_3             precipitations of 3 days before
     * @param  snowcover       DataSerie > snowcover DataSerie
     * @param  snowcover_1     snowcover of day before
     * @param  snowcover_2     snowcover of 2 days before
     * @param  beginFireSeason date of beginning of the fire season (millisec)
     * @param  endFireSeason   date of ending of the fire season (millisec)
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(prev:Double,T13:DataSerie,VPD13:DataSerie,P13:DataSerie,P_1:Double,P_2:Double,P_3:Double,snowcover:DataSerie, snowcover_1:Int, snowcover_2:Int,startFireSeason:Long,endFireSeason:Long, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(T13.start,T13.interval,ListFunctions.applyFunctionPrev(Functions.pM68_vpd,prev,T13.values,VPD13.values,T13.getDates,P13.values,P_1,P_2,P_3,snowcover.values.map(_.toInt),snowcover_1,snowcover_2,startFireSeason,endFireSeason), log, "start = "+prev+" "+notes)
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
      
      calculate(prev,v.ds(T13),v.ds(VPD13),v.ds(P13),Double.NaN,Double.NaN,Double.NaN,v.ds(SnowCover),Null.Int,Null.Int,v.par(FireSeasonStart).value,v.par(FireSeasonEnd).value, this.getLog(v))
    }
   
    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val p13=v.ds(P13,4).values
      val sc=v.ds(SnowCover,3).values
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.prev.asInstanceOf[Double], v.ds(T13,1),v.ds(VPD13,1),v.ds(P13,1),p13(2),p13(1),p13(0),v.ds(SnowCover,1),sc(1),sc(0),v.par(FireSeasonStart).value,v.par(FireSeasonEnd).value, this.getLog(v)))
    }
  }

  case object pM68dwd extends Variable("pM68 index value (DWD modification)","pM68dwd","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T13::T12::T15::T::Nil)
    in += (H13::H12::H15::H::Nil)
    in += (P13::P12::P15::P::Nil)
    in += (SnowCover::Nil)
    in += FireSeasonStart::Nil
    in += FireSeasonEnd::Nil
    
    /**
     * Calculate pM68dwd index DataSerie (M68 precipitation correction)
     *
     * @param  prev            previous value of pM68
     * @param  T13             temperature at 13:00 DataSerie
     * @param  H13             relative humidity at 13:00 DataSerie
     * @param  P13             precipitation sum at 13:00 DataSerie
     * @param  P_1             precipitations of day before
     * @param  P_2             precipitations of 2 days before
     * @param  P_3             precipitations of 3 days before
     * @param  snowcover       DataSerie > snowcover DataSerie
     * @param  snowcover_1     snowcover of day before
     * @param  snowcover_2     snowcover of 2 days before
     * @param  beginFireSeason date of beginning of the fire season (millisec)
     * @param  endFireSeason   date of ending of the fire season (millisec)
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(prev:Double,T13:DataSerie,H13:DataSerie,P13:DataSerie,P_1:Double,P_2:Double,P_3:Double,snowcover:DataSerie,snowcover_1:Int,snowcover_2:Int,startFireSeason:Long,endFireSeason:Long, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(T13.start,T13.interval,ListFunctions.applyFunctionPrev(Functions.pM68dwd,prev,T13.values,H13.values,T13.getDates,P13.values,P_1,P_2,P_3,snowcover.values.map(_.toInt),snowcover_1,snowcover_2,startFireSeason,endFireSeason), log, "start = "+prev+" "+notes)
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
      calculate(prev,v.ds(T13),v.ds(H13),v.ds(P13),Double.NaN,Double.NaN,Double.NaN,v.ds(SnowCover),Null.Int,Null.Int,v.par(FireSeasonStart).value,v.par(FireSeasonEnd).value, this.getLog(v))
    }
   
    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val p13=v.ds(P13,4).values
      val sc=v.ds(SnowCover,3).values
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.prev.asInstanceOf[Double], v.ds(T13,1),v.ds(H13,1),v.ds(P13,1),p13(2),p13(1),p13(0),v.ds(SnowCover,1),sc(1),sc(0),v.par(FireSeasonStart).value,v.par(FireSeasonEnd).value, this.getLog(v)))
    }
  }
  case object M68 extends Variable("M68 index value","M68","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (pM68::Nil)
    in += (P::P12::P13::P15::Nil)
    in += BirchLeaves::Nil
    in += RobiniaBlossom::Nil
    in += M68VegCorrStep3Start::Nil
    in += M68VegCorrStep3End::Nil

    
    /**
     * Calculate M68 index DataSerie (M68 vegetation correction)
     *
     * @param  pM68              pM68 DataSerie
     * @param  P                 precipitations DataSerie
     * @param  birchPhase        DataSerie > birch leaves phenological phase DataSerie
     * @param  robiniaPhase      DataSerie > robinia blossom phenological phase DataSerie
     * @param  vegCorrStep3Start first date for cegetation correction(millisec)
     * @param  vegCorrStep3end   second date for cegetation correction(millisec)
     * @param  notes             some notes on DataSerie creation
     * @return                   DataSerie
     */
    def calculate(pM68:DataSerie,P:DataSerie,birchPhase:DataSerie,robiniaPhase:DataSerie,vegCorrStep3Start:Long,vegCorrStep3End:Long, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(pM68.start,pM68.interval,Functions.M68(pM68.values,pM68.getDates,P.values,birchPhase.values,robiniaPhase.values,vegCorrStep3Start,vegCorrStep3End), log, notes)
    }
   
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)

      calculate(v.ds(pM68),v.ds(P),v.ds(BirchLeaves),v.ds(RobiniaBlossom),v.par(M68VegCorrStep3Start).value,v.par(M68VegCorrStep3End).value, this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(pM68,366),v.ds(P,366),v.ds(BirchLeaves,366),v.ds(RobiniaBlossom,366),v.par(M68VegCorrStep3Start).value,v.par(M68VegCorrStep3End).value, this.getLog(v)))
    }
   }
  case object M68dwd extends Variable("M68 index value (DWD modification)","M68dwd","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (pM68dwd::Nil)
    in += (P::P12::P13::P15::Nil)
    in += BirchLeaves::Nil
    in += RobiniaBlossom::Nil
    in += M68VegCorrStep3Start::Nil
    in += M68VegCorrStep3End::Nil

    
    /**
     * Calculate M68 index DataSerie (M68 vegetation correction)
     *
     * @param  pM68              pM68 DataSerie
     * @param  P                 precipitations DataSerie
     * @param  birchPhase        DataSerie > birch leaves phenological phase DataSerie
     * @param  robiniaPhase      DataSerie > robinia blossom phenological phase DataSerie
     * @param  vegCorrStep3Start first date for cegetation correction(millisec)
     * @param  vegCorrStep3end   second date for cegetation correction(millisec)
     * @param  notes             some notes on DataSerie creation
     * @return                   DataSerie
     */
    def calculate(pM68dwd:DataSerie,P:DataSerie,birchPhase:DataSerie,robiniaPhase:DataSerie,vegCorrStep3Start:Long,vegCorrStep3End:Long, log:DataLog, notes:String=""):DataSerie={
      createDataSerie(pM68dwd.start,pM68dwd.interval,Functions.M68(pM68dwd.values,pM68dwd.getDates,P.values,birchPhase.values,robiniaPhase.values,vegCorrStep3Start,vegCorrStep3End), log, notes)
    }
   
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(pM68dwd),v.ds(P),v.ds(BirchLeaves),v.ds(RobiniaBlossom),v.par(M68VegCorrStep3Start).value,v.par(M68VegCorrStep3End).value, this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(pM68dwd,366),v.ds(P,366),v.ds(BirchLeaves,366),v.ds(RobiniaBlossom,366),v.par(M68VegCorrStep3Start).value,v.par(M68VegCorrStep3End).value, this.getLog(v)))
    }  
  }
  case object DFnoble extends Variable("Drought factor (Noble 1980)","DFnoble","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (lastRainSum::Nil)
    in += (DaysSinceRain::Nil)
    in += (KBDISI::Nil)

    /**
     * Calculate DF  DataSerie
     *
     * @param  lastRainSum     last consecutive rainfall sum DataSerie 
     * @param  DaysSinceRain   consecutive days wihtout rain DataSerie
     * @param  KBDISI          KBDISI index DataSerie
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
//    def calculate(lastRainSum:DataSerie,DaysSinceRain:DataSerie,KBDISI:DataSerie, log: DataLog, notes:String=""):DataSerie={
    def calculate(lastRainSum:DataSerie,DaysSinceRain:DataSerie,KBDISI:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(lastRainSum.start,lastRainSum.interval,ListFunctions.applyFunction(Functions.DFnoble,lastRainSum.values,DaysSinceRain.values,KBDISI.values), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
     val v=chooseVariablesAndCalculate(dss)

      calculate(v.ds(lastRainSum),v.ds(DaysSinceRain),v.ds(KBDISI), this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(lastRainSum,1),v.ds(DaysSinceRain,1),v.ds(KBDISI,1), this.getLog(v)))
    }
  }

  case object DFgriffith extends Variable("Drought factor (Griffith 1999)","DFgriffith","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (lastRainSum_2_20::lastRainSum::Nil)
//    in += (P::Nil)
    in += (AgeRainEvent_2_20::DaysSinceRain::Nil)
    in += (KBDISI::Nil)

    /**
     * Calculate DF  DataSerie
     *
     * @param  lastRainSum     sum of the last consecutive rain event (days above 2mm) in the past 20 days
     * @param  ageRainEvent    age of rain event [days] (Finkele 2006)
     * @param  KBDISI          KBDISI index DataSerie
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(lastRainSum:DataSerie,ageRainEvent:DataSerie,KBDISI:DataSerie, log: DataLog, notes:String=""):DataSerie={
//    def calculate(P:DataSerie,DaysSinceRain:DataSerie,KBDISI:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(lastRainSum.start,lastRainSum.interval,ListFunctions.applyFunction(Functions.DFgriffith,lastRainSum.values,ageRainEvent.values,KBDISI.values), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)

      calculate(v.ds(lastRainSum_2_20),v.ds(AgeRainEvent_2_20),v.ds(KBDISI), this.getLog(v))
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(lastRainSum_2_20),v.ds(AgeRainEvent_2_20),v.ds(KBDISI), this.getLog(v)))
    }
  }
  case object DFgriffithAdj extends Variable("Drought factor (Finkele et al.2006)","DFgriffithAdj","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (lastRainSum_2_20::lastRainSum::Nil)
//    in += (P::Nil)
    in += (AgeRainEvent_2_20::DaysSinceRain::Nil)
    in += (KBDISI::Nil)

    /**
     * Calculate DF  DataSerie
     *
     * @param  lastRainSum     sum of the last consecutive rain event (days above 2mm) in the past 20 days
     * @param  ageRainEvent    age of rain event [days] (Finkele 2006)
     * @param  KBDISI          KBDISI index DataSerie
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(lastRainSum:DataSerie,ageRainEvent:DataSerie,KBDISI:DataSerie, log: DataLog, notes:String=""):DataSerie={
//    def calculate(P:DataSerie,DaysSinceRain:DataSerie,KBDISI:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(lastRainSum.start,lastRainSum.interval,ListFunctions.applyFunction(Functions.DFgriffithAdj,lastRainSum.values,ageRainEvent.values,KBDISI.values), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)

      calculate(v.ds(lastRainSum_2_20),v.ds(AgeRainEvent_2_20),v.ds(KBDISI), this.getLog(v))
    }
    
 
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(lastRainSum_2_20,1),v.ds(AgeRainEvent_2_20,1),v.ds(KBDISI,1), this.getLog(v)))
    }
  }
  case object FFDI extends Variable("MacArthur5 index value","FFDI","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (DFgriffithAdj::DFgriffith::DFnoble::Nil)
    in += (T15::T13::T12::T::Nil)
    in += (H15::H13::H12::H::Nil)
    in += (U15::U13::U12::U::Nil)
    
    /**
     * Calculate FFDI index DataSerie, with Drought factor according to Griffith 1999
     *
     * @param  DF              drougth factor DataSerie
     * @param  Tmax            maximal temperature DataSerie
     * @param  H               relative humidity DataSerie
     * @param  U               wind speed DataSerie
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(DF:DataSerie,T15:DataSerie,H15:DataSerie,U15:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(DF.start,DF.interval,ListFunctions.applyFunction(Functions.FFDI,DF.values,T15.values,H15.values,U15.values), log, notes)
    }
   
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)

      calculate(v.ds(DFgriffithAdj),v.ds(T15),v.ds(H15),v.ds(U15), this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(DFgriffithAdj,1),v.ds(T15,1),v.ds(H15,1),v.ds(U15,1), this.getLog(v)))
    }
  }
  case object FFMC extends Variable("Fine fuel moisture code","FFMC","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T12::T13::T15::T::Nil)
    in += (H12::H13::H15::H::Nil)
    in += (U12::U13::U15::U::Nil)
    in += (P12::P13::P15::P::Nil)
    in += (SnowCover::Nil)
    in += FFMCstart::Nil
    
    /**
     * Calculate FFMC index DataSerie
     *
     * @param  start           to initialize index (generally 85)
     * @param  prev            previous value of FFMC index
     * @param  P12             precipitations at noon DataSerie
     * @param  T12             temperature at noon DataSerie
     * @param  H12             relative humidity at noon DataSerie
     * @param  U12             wind speed at noon DataSerie
     * @param  snowcover       DataSerie > snowcover DataSerie
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(start:Double=85.0,prev:Double,P12:DataSerie,T12:DataSerie,H12:DataSerie,U12:DataSerie,SnowCover:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(P12.start,P12.interval,ListFunctions.applyFunctionPrev_k(Functions.FFMC,prev,P12.values,T12.values,H12.values,U12.values,SnowCover.values.map(_.toInt),start), log, "start = "+prev+" "+notes)
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

      calculate(v.par(FFMCstart).value,prev,v.ds(P12),v.ds(T12),v.ds(H12),v.ds(U12),v.ds(SnowCover), this.getLog(v))
    }
   
    def calculate(dss:DataCollection):DataSerie={
      //calculate(dss.pars(FFMCstart).value, dss)
      calculate(Double.NaN,dss)
    }


    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.par(FFMCstart).value, me.prev.asInstanceOf[Double], v.ds(P12,1),v.ds(T12,1),v.ds(H12,1),v.ds(U12,1),v.ds(SnowCover,1), this.getLog(v)))
    }

    
  }
  case object DMC extends Variable("Duff moisture code","DMC","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T12::T13::T15::T::Nil)
    in += (H12::H13::H15::H::Nil)
    in += (P12::P13::P15::P::Nil)
    in += (SnowCover::Nil)
    in += DMCstart::Nil
    //in += Latitude::Nil   #currently not used, since also DC should be adapted for daylight and no standard definition exists
    
    /**
     * Calculate DMC index DataSerie
     *
     * @param  start           to initialize index (generally 6)
     * @param  prev            previous value of DMC index
     * @param  P12             precipitations at noon DataSerie
     * @param  T12             temperature at noon DataSerie
     * @param  U12             wind speed at noon DataSerie
     * @param  snowcover       DataSerie > snowcover DataSerie
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(start:Double=6,prev:Double,P12:DataSerie,T12:DataSerie,H12:DataSerie,SnowCover:DataSerie,latitude:Double=Double.NaN, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(P12.start,P12.interval,ListFunctions.applyFunctionPrev_kk(Functions.DMC,prev,P12.getDates,P12.values,T12.values,H12.values,SnowCover.values.map(_.toInt),latitude,start), log, "start = "+prev+" "+notes)
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
      //calculate(v.par(DMCstart).value,prev,v.ds(P12),v.ds(T12),v.ds(H12),v.ds(SnowCover), v.par(Latitude).value, this.getLog(v))
      calculate(v.par(DMCstart).value,prev,v.ds(P12),v.ds(T12),v.ds(H12),v.ds(SnowCover), Double.NaN, this.getLog(v))
    }
   
    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      //calculate(v.par(DMCstart).value, me.prev.asInstanceOf[Double] ,v.ds(P12,1),v.ds(T12,1),v.ds(H12,1),v.ds(SnowCover,1), v.par(Latitude).value, this.getLog(v))
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.par(DMCstart).value, me.prev.asInstanceOf[Double] ,v.ds(P12,1),v.ds(T12,1),v.ds(H12,1),v.ds(SnowCover,1), Double.NaN, this.getLog(v)))
    }  
  }
  case object DMC_lat extends Variable("Duff moisture code adapted for latitude","DMC_lat","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T12::T13::T15::T::Nil)
    in += (H12::H13::H15::H::Nil)
    in += (P12::P13::P15::P::Nil)
    in += (SnowCover::Nil)
    in += DMCstart::Nil
    in += Latitude::Nil  
    
    /**
     * Calculate DMC index DataSerie
     *
     * @param  start           to initialize index (generally 6)
     * @param  prev            previous value of DMC index
     * @param  P12             precipitations at noon DataSerie
     * @param  T12             temperature at noon DataSerie
     * @param  U12             wind speed at noon DataSerie
     * @param  snowcover       DataSerie > snowcover DataSerie
     * @param  latitude        latitude of measurement location to calculate the daylight factor 
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(start:Double=6,prev:Double,P12:DataSerie,T12:DataSerie,H12:DataSerie,SnowCover:DataSerie,latitude:Double=Double.NaN, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(P12.start,P12.interval,ListFunctions.applyFunctionPrev_kk(Functions.DMC,prev,P12.getDates,P12.values,T12.values,H12.values,SnowCover.values.map(_.toInt),latitude,start), log, "start = "+prev+" "+notes)
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
      calculate(v.par(DMCstart).value,prev,v.ds(P12),v.ds(T12),v.ds(H12),v.ds(SnowCover), v.par(Latitude).value, this.getLog(v))
    }
   
    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.par(DMCstart).value, me.prev.asInstanceOf[Double] ,v.ds(P12,1),v.ds(T12,1),v.ds(H12,1),v.ds(SnowCover,1), v.par(Latitude).value, this.getLog(v)))
    }  
  }
  case object DC extends Variable("Drought code","DC","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T12::T13::T15::T::Nil)
    in += (P12::P13::P15::P::Nil)
    in += (SnowCover::Nil)
    in += DCstart::Nil
    //in += Latitude::Nil   #currently not used, since also DC should be adapted for daylight and no standard definition exists
    
    /**
     * Calculate DC index DataSerie
     *
     * @param  start           to initialize index (generally 15)
     * @param  prev            previous value of DC index
     * @param  P12             precipitations at noon DataSerie
     * @param  T12             temperature at noon DataSerie
     * @param  snowcover       DataSerie > snowcover DataSerie
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(start:Double=15,prev:Double,P12:DataSerie,T12:DataSerie,SnowCover:DataSerie,latitude:Double=Double.NaN, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(P12.start,P12.interval,ListFunctions.applyFunctionPrev_kk(Functions.DC,prev,P12.getDates,P12.values,T12.values,SnowCover.values.map(_.toInt),latitude,start), log, "start = "+prev+" "+notes)
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
      calculate(v.par(DCstart).value,prev,v.ds(P12),v.ds(T12),v.ds(SnowCover), Double.NaN, this.getLog(v))
    }
   
    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }
    
 
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.par(DCstart).value, me.prev.asInstanceOf[Double]  ,v.ds(P12,1),v.ds(T12,1),v.ds(SnowCover,1), Double.NaN, this.getLog(v)))
    }
  }
  
  case object DC_lat extends Variable("Drought code adapted for latitude","DC_lat","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T12::T13::T15::T::Nil)
    in += (P12::P13::P15::P::Nil)
    in += (SnowCover::Nil)
    in += DCstart::Nil
    in += Latitude::Nil  
    
    /**
     * Calculate DC index DataSerie
     *
     * @param  start           to initialize index (generally 15)
     * @param  prev            previous value of DC index
     * @param  P12             precipitations at noon DataSerie
     * @param  T12             temperature at noon DataSerie
     * @param  snowcover       DataSerie > snowcover DataSerie
     * @param  latitude        latitude of measurement location to calculate the daylight factor 
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(start:Double=15,prev:Double,P12:DataSerie,T12:DataSerie,SnowCover:DataSerie,latitude:Double=Double.NaN, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(P12.start,P12.interval,ListFunctions.applyFunctionPrev_kk(Functions.DC,prev,P12.getDates,P12.values,T12.values,SnowCover.values.map(_.toInt),latitude,start), log, "start = "+prev+" "+notes)
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
      calculate(v.par(DCstart).value,prev,v.ds(P12),v.ds(T12),v.ds(SnowCover), v.par(Latitude).value, this.getLog(v))
    }
   
    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }
    
 
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.par(DCstart).value, me.prev.asInstanceOf[Double]  ,v.ds(P12,1),v.ds(T12,1),v.ds(SnowCover,1), v.par(Latitude).value, this.getLog(v)))
    }
  }
  case object ISI extends Variable("Initial spread index","ISI","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (FFMC::Nil)
    in += (U12::U13::U15::U::Nil)
    
    /**
     * Calculate ISI index DataSerie
     *
     * @param  FFMC            FFMC DataSerie
     * @param  U12             wind speed at noon DataSerie
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(FFMC:DataSerie,U12:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(FFMC.start,FFMC.interval,ListFunctions.applyFunction(Functions.ISI,FFMC.values,U12.values), log, notes)
    }
   
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(FFMC),v.ds(U12), this.getLog(v))
    }
    
 
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(FFMC,1),v.ds(U12,1), this.getLog(v)))
    }   
  }
  case object BUI extends Variable("Buildup index","BUI","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (DMC::Nil)
    in += (DC::Nil)
    
    /**
     * Calculate BUI index DataSerie
     *
     * @param  DMC             DMC DataSerie
     * @param  DC              DC DataSerie
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(DMC:DataSerie,DC:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(DMC.start,DMC.interval,ListFunctions.applyFunction(Functions.BUI,DMC.values,DC.values), log, notes)
    }
   
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(DMC),v.ds(DC), this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(DMC,1),v.ds(DC,1), this.getLog(v)))
    }
  }
  
  case object BUI_lat extends Variable("Buildup index adapted for latitude","BUI_lat","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (DMC_lat::Nil)
    in += (DC_lat::Nil)
    
    /**
     * Calculate BUI index DataSerie
     *
     * @param  DMC_lat         DMC_lat DataSerie
     * @param  DC_lat          DC_lat DataSerie
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(DMC_lat:DataSerie,DC_lat:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(DMC_lat.start,DMC_lat.interval,ListFunctions.applyFunction(Functions.BUI,DMC_lat.values,DC_lat.values), log, notes)
    }
   
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(DMC_lat),v.ds(DC_lat), this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(DMC_lat,1),v.ds(DC_lat,1), this.getLog(v)))
    }
  }
  
  case object FWI extends Variable("Fire weather index","FWI","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (BUI::Nil)
    in += (ISI::Nil)
    
    /**
     * Calculate FWI index DataSerie
     *
     * @param  BUI             BUI DataSerie
     * @param  ISI             ISI DataSerie
     * @param  U12             wind speed at noon DataSerie
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(BUI:DataSerie,ISI:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(BUI.start,BUI.interval,ListFunctions.applyFunction(Functions.FWI,BUI.values,ISI.values), log, notes)
    }
   
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
  
      calculate(v.ds(BUI),v.ds(ISI), this.getLog(v))
    }
    
  
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(BUI,1),v.ds(ISI,1), this.getLog(v)))
    }
  }
  
  case object FWI_lat extends Variable("Fire weather index adapted for latitude","FWI_lat","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (BUI_lat::Nil)
    in += (ISI::Nil)
    
    /**
     * Calculate FWI index DataSerie
     *
     * @param  BUI             BUI DataSerie
     * @param  ISI             ISI DataSerie
     * @param  U12             wind speed at noon DataSerie
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(BUI_lat:DataSerie,ISI:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(BUI_lat.start,BUI_lat.interval,ListFunctions.applyFunction(Functions.FWI,BUI_lat.values,ISI.values), log, notes)
    }
   
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
  
      calculate(v.ds(BUI_lat),v.ds(ISI), this.getLog(v))
    }
    
  
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(BUI_lat,1),v.ds(ISI,1), this.getLog(v)))
    }
  }
  
  case object Baumgartner extends Variable("Baumgartner index","Baumgartner","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (P::P12::P13::P15::Nil)
    in += (PETpen::Nil)
    
    /**
     * Calculate Baumgartner index DataSerie
     *
     * @param  PETpen          PETpen DataSerie
     * @param  P               precipitations DataSerie
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(PETpen:DataSerie,P:DataSerie, log: DataLog, notes:String=""):DataSerie={
      val FivePrecDaysPETpen = ListFunctions.runningSumPos(5,5,PETpen.values)
      val FivePrecDaysRain = ListFunctions.runningSumPos(5,5,P.values)
      createDataSerie(PETpen.start,PETpen.interval,ListFunctions.applyFunction(Functions.Baumgartner,FivePrecDaysPETpen,FivePrecDaysRain), log, notes)
    }
   
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(PETpen),v.ds(P), this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(PETpen,7),v.ds(P,7), this.getLog(v)))
    }
  }
  case object BaumgartnerDanger extends Variable("Baumgartner Danger classes","BaumgartnerDanger","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (Baumgartner::Nil)
    
    /**
     * Calculate Baumgartner Danger classes DataSerie
     *
     * @param  Baumgartner     Baumgartner Index
     * @param  notes           some notes on DataSerie creation
     * @return                 DataSerie
     */
    def calculate(Baumgartner:DataSerie, log: DataLog, notes:String=""):DataSerie={
      val months = Baumgartner.getDates.map(Utils.solarDate2String(_,"MM").toInt)
      createDataSerie(Baumgartner.start,Baumgartner.interval,ListFunctions.applyFunction(Functions.BaumgartnerDanger,Baumgartner.values,months), log, notes)
    }
   
    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(Baumgartner), this.getLog(v))
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(Baumgartner,1), this.getLog(v)))
    }
    
  }
  case object RN extends Variable("Numerical risk index","RN","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T::Nil)
    in += (Tdew::Nil)
    in += (U::Nil)
    in += (Cc::Nil)
    in += (res::Nil)

    /**
     * Calculate Numerical risk index DataSerie
     *
     * @param  T          air temperature  DataSerie
     * @param  Tdew       dewpoint temperature DataSerie
     * @param  U          wind [m/s] DataSerie
     * @param  Cc         cloud cover [fraction] DataSerie
     * @param  r          soil water reserve [mm] DataSerie
     * @return            numerical risk index DataSerie
     */
    def calculate(T:DataSerie,Tdew:DataSerie,U:DataSerie,Cc:DataSerie,r:DataSerie, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(T.start,T.interval,ListFunctions.applyFunction(Functions.RN,T.values,Tdew.values,U.values,Cc.values,r.values,T.getDates), log, notes)
    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(T),v.ds(Tdew),v.ds(U),v.ds(Cc),v.ds(res), this.getLog(v))
    }
    
  
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(T,1),v.ds(Tdew,1),v.ds(U,1),v.ds(Cc,1),v.ds(res,1), this.getLog(v)))
    }
  }
  
  case object Ifa extends Variable("Ifa index (Portuguese Index)","Ifa","--",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T12::T13::T15::T::Tmax::Nil)
    in += (Tdew12::Tdew::Tdew15::Nil)
    in += (P12::P::P13::P15::Nil)
    in += (U12::U::U13::U15::Nil)   //according to MEGAFires project U12 first
    in += (FireSeasonStart::Nil)
    in += (FireSeasonEnd::Nil)

    /**
     * Calculate Ifa index DataSerie
     *
     * @param  T12              air temperature at noon DataSerie
     * @param  Tdew12           dewpoint temperature at noon DataSerie
     * @param  P12              rainfall [mm] DataSerie (cut at noon)
     * @param  U12              windspeed [m/s] DataSerie at noon
     * @param  FireSeasonStart  start of fire season [msec]
     * @param  FireSeasonEnd    end of fire season [msec]
     * @return                  Ifa index DataSerie
     */
    def calculate(T12:DataSerie,Tdew12:DataSerie,P12:DataSerie,U12:DataSerie,FireSeasonStart:Long,FireSeasonEnd:Long, log: DataLog, notes:String=""):DataSerie={

      createDataSerie(T12.start,T12.interval,
                      Functions.Ifa(T12.getDates,ListFunctions.applyFunction(
                                    Functions.IG,T12.values,Tdew12.values),P12.values,U12.values,FireSeasonStart,FireSeasonEnd), log, notes)

    }


    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(T12),v.ds(Tdew12),v.ds(P12),v.ds(U12),v.par(FireSeasonStart).value,v.par(FireSeasonEnd).value, this.getLog(v))
    }

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(T12,366),v.ds(Tdew12,366),v.ds(P12,366),v.ds(U12,366),v.par(FireSeasonStart).value,v.par(FireSeasonEnd).value, this.getLog(v)))
    }
  }
  
  case object Risico_dffm extends Variable("dead fuel humidity","Risico_dffm","mass-%",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (T::Nil)
    in += (H::Nil)
    in += (U::Nil)
    in += (P::Nil)
    in += (Risico_sat::Nil)
    in += (Risico_T0::Nil)
//    in += (Risico_dT::Nil)

    /**
     * Calculate Risico_WindEffect DataSerie
     *
     * @param  T12              air temperature at noon DataSerie
     * @return                  Ifa index DataSerie
     */
    def calculate(prev:Double,T:DataSerie,H:DataSerie,U:DataSerie,P:DataSerie,Risico_sat:Double, Risico_T0:Double, /*Risico_dT:Double,*/ RainThreshold:Double=0.0, log: DataLog, notes:String=""):DataSerie={
      createDataSerie(T.start,T.interval,ListFunctions.applyFunctionPrev_kkkk(Functions.Dffm, prev,
                                                                              T.values,H.values,U.values,P.values,
                                                                              Risico_sat,Risico_T0, 
                                                                              T.interval / 1000.0 / 60.0 / 60.0, 
//                                                                              T.interval / Variable.msecInOneHour.toDouble, 
                                                                              RainThreshold), log, notes)

    }


    def calculate(prev:Double, dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(prev, v.ds(T),v.ds(H),v.ds(U),v.ds(P),v.par(Risico_sat).value,v.par(Risico_T0).value, /*v.par(Risico_dT).value,*/ 0.0, this.getLog(v))
    }
    
    def calculate(dss:DataCollection):DataSerie={
      calculate(Double.NaN,dss)
    }
    
    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(me.prev,v.ds(T,1),v.ds(H,1),v.ds(U,1),v.ds(P,1),v.par(Risico_sat).value,v.par(Risico_T0).value, /*v.par(Risico_dT).value,*/ 0.0, this.getLog(v)))
    }
  }
  
  case object Risico_WindEffect extends Variable("wind effect on fire propagation speed","Risico_WindEffect","-",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (U::Nil)
    in += (D::Nil)
    in += (Slope::Nil)
    in += (Aspect::Nil)

    /**
     * Calculate Risico_WindEffect DataSerie
     *
     * @param  T12              air temperature at noon DataSerie
     * @return                  Ifa index DataSerie
     */
    def calculate(U:DataSerie,D:DataSerie,Slope:Double, Aspect:Double, log: DataLog, notes:String=""):DataSerie={

      createDataSerie(U.start,U.interval,ListFunctions.applyFunction_kk(Functions.WindEffect,
                                                                        U.values,D.values, Slope, Aspect), log, notes)
    }

    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(U),v.ds(D),v.par(Slope).value,v.par(Aspect).value, this.getLog(v))
    }

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(U,1),v.ds(D,1),v.par(Slope).value,v.par(Aspect).value, this.getLog(v)))
    }
  }
  
  case object Risico_V extends Variable("fire propagation speed","Risico_V","m/h",-100000,100000, classOf[Double]) with Serie with Calculable{
    in += (Risico_dffm::Nil)
    in += (Risico_WindEffect::Nil)
    in += (SnowCover::Nil)
    in += (Risico_v0::Nil)
    in += (Risico_d0::Nil)
    in += (Risico_d1::Nil)
    in += (Slope::Nil)  


    /**
     * Calculate Ifa index DataSerie
     *
     * @param  T12              air temperature at noon DataSerie
     * @param  Tdew12           dewpoint temperature at noon DataSerie
     * @param  P                rainfall [mm] DataSerie
     * @param  U                windspeed [m/s] DataSerie
     * @param  FireSeasonStart  start of fire season [msec]
     * @param  FireSeasonEnd    end of fire season [msec]
     * @return                  Ifa index DataSerie
     */
    def calculate(Risico_dffm:DataSerie,Risico_WindEffect:DataSerie,SnowCover:DataSerie,Risico_v0:Double,Risico_d0:Double, Risico_d1:Double, Slope:Double, log: DataLog, notes:String=""):DataSerie={

      createDataSerie(Risico_dffm.start,Risico_dffm.interval,ListFunctions.applyFunction_kkkk(Functions.V,
                                                                                              Risico_dffm.values,Risico_WindEffect.values,SnowCover.values,
                                                                                              Risico_v0,Risico_d0,Risico_d1,Slope), log, notes)
    }

    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(Risico_dffm),v.ds(Risico_WindEffect),v.ds(SnowCover),v.par(Risico_v0).value,v.par(Risico_d0).value, v.par(Risico_d1).value, v.par(Slope).value, this.getLog(v))
    }

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(Risico_dffm,1),v.ds(Risico_WindEffect,1),v.ds(SnowCover,1),v.par(Risico_v0).value,v.par(Risico_d0).value, v.par(Risico_d1).value, v.par(Slope).value, this.getLog(v)))
    }
  }
  
  case object Risico_FI extends Variable("fireline intensity","Risico_FI","kW/m",0,10000, classOf[Double]) with Serie with Calculable{
    in += (Risico_dffm::Nil)
    in += (Risico_V::Nil)
    in += (Risico_d0::Nil)
    in += (Risico_d1::Nil)
    in += (Risico_hhv::Nil)  
    in += (Risico_humidity::Nil)  

    /**
     * Calculate Ifa index DataSerie
     *
     * @param  T12              air temperature at noon DataSerie
     * @param  Tdew12           dewpoint temperature at noon DataSerie
     * @param  P                rainfall [mm] DataSerie
     * @param  U                windspeed [m/s] DataSerie
     * @param  FireSeasonStart  start of fire season [msec]
     * @param  FireSeasonEnd    end of fire season [msec]
     * @return                  Ifa index DataSerie
     */
    def calculate(Risico_dffm:DataSerie,Risico_V:DataSerie,Risico_d0:Double, Risico_d1:Double, Risico_hhv:Double, Risico_humidity:Double, log: DataLog, notes:String=""):DataSerie={

      createDataSerie(Risico_dffm.start,Risico_dffm.interval,ListFunctions.applyFunction_kkkk(Functions.FI,
                                                                                              Risico_dffm.values,Risico_V.values,
                                                                                              Risico_d0,Risico_d1,Risico_hhv,Risico_humidity), log, notes)
    }

    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(Risico_dffm),v.ds(Risico_V),v.par(Risico_d0).value,v.par(Risico_d1).value,v.par(Risico_hhv).value,v.par(Risico_humidity).value, this.getLog(v))
    }

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(Risico_dffm,1),v.ds(Risico_V,1),v.par(Risico_d0).value,v.par(Risico_d1).value,v.par(Risico_hhv).value,v.par(Risico_humidity).value, this.getLog(v)))
    }
  }
  
  case object IREPI extends Variable("IREPI","IREPI","--",0,10000, classOf[Double]) with Serie with Calculable{
    in += (P::Nil)
    in += (PETpen::Nil)
    in += RainyWeekThreshold::Nil
    in += WeekRain::Nil

    /**
     * Calculate Ifa index DataSerie
     *
     * @param  T12              air temperature at noon DataSerie
     * @param  Tdew12           dewpoint temperature at noon DataSerie
     * @param  P                rainfall [mm] DataSerie
     * @param  U                windspeed [m/s] DataSerie
     * @param  FireSeasonStart  start of fire season [msec]
     * @param  FireSeasonEnd    end of fire season [msec]
     * @return                  Ifa index DataSerie
     */

    def calculate(P:DataSerie,PETpen:DataSerie, PweekThreshold:Double,WeekRain:DataSerie, log: DataLog, notes:String=""):DataSerie={
      import ListFunctions.IREPIconst._
      createDataSerie(P.start, P.interval, ListFunctions.IREPI(P.values, PETpen.values, soilWaterAtSaturation, PweekThreshold, WeekRain.values), log, notes)
    }

    def calculate(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndCalculate(dss)
      calculate(v.ds(P),v.ds(PETpen), v.par(RainyWeekThreshold).value, v.ds(WeekRain), this.getLog(v))
    }

    def complete(dss:DataCollection):DataSerie={
      val v=chooseVariablesAndComplete(dss)
      val me=dss.dss(this)
      me.updateLastAndNotes(calculate(v.ds(P),v.ds(PETpen), v.par(RainyWeekThreshold).value, v.ds(WeekRain), this.getLog(v)))  //by omitting the number of rows will recalculate all
    }
  }
  