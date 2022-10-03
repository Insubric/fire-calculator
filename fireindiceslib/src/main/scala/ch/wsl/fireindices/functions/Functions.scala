package ch.wsl.fireindices.functions

import java.io.File
import scala.math._
import ch.wsl.fireindices.metadata.Null
import ch.wsl.fireindices.functions.Utils._
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable.ListBuffer

/**
 * Contains all the core functions for the calculation of the indices,
 * using single values as inputs.
 **/
object Functions extends LazyLogging{

  
  /**
   * Computes Dewpoint Temperature, according to Allen et al.(1998)
   * 
   * The day temperature average is expressed in  and
   * the relative humidity is expressed in 
   *
   * @param    T     Temperature [°C]
   * @param    H     Relative Humidity [%]
   * @return         Dewpoint Temperature [°C]
   */
  def Tdew(T:Double, H:Double):Double = {
//    val a = 611.21
//    val b = 17.502
//    val c = 240.97
//    val es = a*math.exp(b*T/(c + T))
//    val e  = H*es/100
//    return (c*math.log(e/a))/(b-math.log(e/a))

    val lnea = math.log(Ea(T,H))
    return  (116.91 + 237.3 * lnea)/(16.78 - lnea)
  }
  
  /**
   * Computes effective temperature (Camargo et al. 1999)
   *
   * The day temperature average is expressed in [°C] and
   * the relative humidity is expressed in [%]
   *
   * @param    Tmax     maximum Temperature
   * @param    Tmin     minimum Temperature
   * @return            effective Temperature
   */
  def Tef(Tmin:Double, Tmax:Double):Double = {
    val k = 0.72
    0.5 * k *(3.0*Tmax - Tmin)
  }
  /**
   * Computes effective temperature (Pereira and Pruitt 2004)
   *
   * The day temperature average is expressed in [°C] and
   * the relative humidity is expressed in [%]
   *
   * @param    Tef              effective Temperature
   * @param    daylightHours    daylightHours
   * @return                    modified effective Temperature
   */
  def Tef2(Tmin:Double, Tmax:Double, daylightHours:Double):Double = {
    val t=Tef(Tmin,Tmax) * daylightHours / (daylightHours-24)
    math.min(Tmax, math.max((Tmax+Tmin)/2,t))     //Tavg <= t  <= Tmax
  }
  /**
   * Approximates precipitation duration in a day [h]
   * (Bradshaw 1983, p.14)
   *
   *
   * @param    P              rainfall [mm]
   * @param    Climate        climate class (Deeming et al. 1977) [1-4]
   * @return                  precipitation duration in a day [h]
   */
  def PDur(P:Double, Climate:Int):Double = {

    if (P>0.0) 
      math.min(8.0, (Utils.mm2inch(P) + 0.02) / (if (Climate<=2) 0.25 else 0.05 ))
    else 0

  }

  /**
   * Computes soil water reserve for the Orieux index.
   * Initialisation condition has been added similar to KBDI index
   *
   * @param    prev             previous'day Orieux index
   * @param    PweekThreshold   weekly rain threshold to initialize index [mm]
   * @param    P                rainfall [mm]
   * @param    PETth            potential evapotranspiration after Thorntwaite
   * @param    WeeklyRain       sum of rain over last 7 days
   * @return                    soil water reserve of the Orieux index [mm]
   */
  def r(prev:Double, PweekThreshold:Double, P:Double,PETth:Double, WeeklyRain:Double):Double = {
    val rmax = 150.0 //mm
    if (Null.is(prev) && WeeklyRain>=PweekThreshold) {   //initialisation
        150.0
    }else {
        math.min(rmax, prev + P - PETth*prev/rmax)
    }

  }
  
    /**
   * Computes Orieux index (soil water reserve).
   * 
   * @param    r              soil water reserve [mm]
   * @param    U              wind speed [m/s]
   * @return                  Orieux index (1 usual, 2high, 3 very high fire danger)
   */
  def OrieuxDanger(r:Double, U:Double):Double = {
    if (Null.is(r) || Null.is(U)) Double.NaN
    else if (r>=100) 0
    else if (r>=50) {if (U> (40/3.6)) 2 else 1}
    else if (r>=30) {if (U> (40/3.6)) 3 else if (U>=(20/3.6)) 2 else 1}
    else {if (U> (40/3.6)) 3 else if (U>=(20/3.6)) 2 else 1}
  }
  
  
  /**
   * Computes the surface soil water reserve, according to Carrega 1988.
   * The water balance has been used.
   * 
   * Dew has been omitted.
   *
   * @param    prev             previous'day rs 
   * @param    P                rainfall [mm]
   * @param    PETth            potential evapotranspiration after Thorntwaite
   * @param    RainSum          sum of continuous rain
   * @return                    Orieux index
   */
  def rs(prev:Double, P:Double,PETth:Double, RainSum:Double):Double = {
    val rsmax = 10.0 //mm
    if (Null.is(prev) && RainSum>=rsmax) {   //initialisation
        10.0
    }else {
        math.max(1,math.min(rsmax, prev + P - PETth*prev/rsmax))
    }

  }
  /**
   * Computes the I87 fire danger index, according to Carrega 1988.
   *
   * Dew has been omitted.
   *
   * @param    T             air temperature
   * @param    H             air humidity [%]
   * @param    U             wind speed [m/s]
   * @param    P             rainfall [mm]
   * @param    r             Orieux soil water reserve [mm]
   * @param    rs            Carrega surface soil water reserve [mm]
   * @param    PC            phenological coefficient
   * @return                 I87 index
   */
  def I87(T:Double, H:Double, U:Double, P:Double, r:Double,rs:Double,PC:Int):Double = {

    1/(math.sqrt(r)*rs)*math.max(10,T) *U / H * PC
  }

  /**
   * Computes potential evapotranspiration after Thorntwaite [mm/day]
   *
   * @param    T                temperature
   * @param    daylightHour     daylightHour
   * @param    I                heat index
   * @return                    potential evapotranspiration after Thorntwaite [mm/day]
   */
  def PETthorn(T:Double,daylightHour:Double, I:Double):Double = {
    val a=6.75 * math.pow(10,-7) * math.pow(I,3)- 7.71 * math.pow(10,-5) * math.pow(I,2)+ 0.0179 * I + 0.492

    if (T<=26) 16.0 *daylightHour/360.0*math.pow(10.0*math.max(0.0,T)/I, a)
    else daylightHour/360.0*(-415.85 + 32.24 * T - 0.43 * math.pow(T,2))
  }
  /**
   * Computes potential evapotranspiration after Thorntwaite [mm/day] with Camargo et al. 1999 correction
   *
   * @param    Tmin             minimum daily temperature
   * @param    Tmax             maximum daily temperature
   * @param    daylightHour     daylightHour
   * @param    I                heat index
   * @return                    potential evapotranspiration after Thorntwaite [mm/day]
   */
  def PETthorn_camargo(Tmin:Double,Tmax:Double,daylightHour:Double, I:Double):Double = {
    PETthorn(Tef(Tmin,Tmax),daylightHour,I)
  }
  /**
   * Computes potential evapotranspiration after Thorntwaite [mm/day] with Pereira and Pruitt 2004 correction
   *
   * @param    Tef              effective temperature after Camargo et al 1999
   * @param    daylightHour     daylightHour
   * @param    I                heat index
   * @return                    potential evapotranspiration after Thorntwaite [mm/day]
   */
  def PETthorn_pereira(Tmin:Double,Tmax:Double,daylightHour:Double, I:Double):Double = {
    PETthorn(Tef2(Tmin,Tmax,daylightHour),daylightHour,I)
  }
  /**
   * Computes heatIndex I  (for PET Thorntwaite)
   *
   * @param    Tm               monthly mean temperatures (12 values)
   * @return                    heat index
   */
  def I(Tm:Seq[Double]):Double = {
     Tm.map(x => math.pow(x/5, 1.514)).sum                
  }

  /**
   * Computes DayLightHoursFAO
   * 
   * 
   *
   * @param todayDate   Date in millisec
   * @param latitude    Latitude of location
   * @return            Number of daylight hours FAO
   */
  def DaylightHoursFAO(todayDate:Long, latitude:Double):Double = {
    val J = Utils.solarDate2Julian(todayDate)      //Julian date
    val delta = 0.409*math.sin((2*math.Pi*J/365)-1.39)    //
    val phi = deg2rad(latitude)                       //convert decimaldegrees to radians
    val ws = math.acos(-1*math.tan(phi)*math.tan(delta))  //sunset hour angle [rad]
    return ws*(24/math.Pi)
  }
  
  /**
   * Computes DayLightHoursNFDRS
   * 
   * 
   *
   * @param todayDate   Date in millisec
   * @param latitude    Latitude of location
   * @return            Number of daylight hours NFDRS
   */
  def DaylightHoursNFDRS(todayDate:Long, latitude:Double):Double = {
    val J = Utils.solarDate2Julian(todayDate)                //Julian date
    val delta = 0.41008*math.sin((J-82)-0.01745)                    //
    val phi = latitude*0.01745                                      //convert decimaldegrees to radians
    return 24*(1-math.acos(math.tan(phi)*math.tan(delta))/math.Pi)
  }
  
  /**
   * Computes equilibrium moisture content (Simard 1968)
   * 
   * @param    T   Temperature  [°C]
   * @param    H   Relative Humidity [%]
   * @return       Equilibrium moisture content
   */
  def EMC(T:Double,H:Double):Double = {
    val TempF = C2F(T)
    if (H<=10) 0.03229+0.281073*H-0.000578*H*TempF
    else if (H>10 & H<=50) 2.22749+0.160107*H-0.01478*TempF
    else 21.0606+0.005565*math.pow(H, 2)-0.00035*H*TempF-0.483199*H
  }

  /**
   * Computes equilibrium moisture content at the fuel-atmosphere interface
   * (Bradshaw et al. 1983)
   *
   * @param    T   Temperature
   * @param    H   Relative Humidity
   * @param    Cc  Cloud cover fraction [ratio]
   * @return       Equilibrium moisture content
   */
  def EMCfa(T:Double,H:Double,Cc:Double):Double = {
    val Tfa = F2C(C2F(T)+ (if (Cc<= 0.1)      25
                           else if (Cc<= 0.5) 19    //TODO verify if 0.5 or 0.6; not correctly specified  in Bradshaw
                           else if (Cc<= 0.9) 12
                           else                5))
    val Hfa = H * (if (Cc<= 0.1)      0.75
                   else if (Cc<= 0.5) 0.83    //TODO verify if 0.5 or 0.6; not correctly specified  in Bradshaw
                   else if (Cc<= 0.9) 0.91
                   else               1.00)
    
    EMC(Tfa,Hfa)
  }

  /**
   * Computes weighted 24-hour average EMC
   * (Bradshaw et al. 1983)
   *
   * @param    Tmin   minimum Temperature
   * @param    Tmax   maximum Temperature
   * @param    Hmin   minimum Relative Humidity
   * @param    Hmax   maximum Relative Humidity
   * @param    N      daylength [hours]
   * @return          weighted 24-hour average Equilibrium moisture content
   */
  def EMC24(Tmin:Double,Tmax:Double,Hmin:Double,Hmax:Double,N:Double):Double = {
    val EMCmin = EMC(Tmin,Hmax)
    val EMCmax = EMC(Tmax,Hmin)

    (N * EMCmin + (24.0 - N)* EMCmax) / 24
  }
  
  /**
   * Computes 1-hour timelag fuel moisture
   * (Bradshaw et al. 1983)
   *
   * @param    EMCfa   equilibrium moisture content at the fuel-atmosphere interface
   * @return           1-hour timelag fuel moisture [%]
   */
  def MC1(EMCfa:Double):Double = {
    1.03 * EMCfa
  }

  /**
   * Computes 10-hour timelag fuel moisture
   * (Bradshaw et al. 1983)
   *
   * @param    EMCfa   equilibrium moisture content at the fuel-atmosphere interface
   * @return           10-hour timelag fuel moisture [%]
   */
  def MC10(EMCfa:Double):Double = {
    1.28 * EMCfa
  }

  /**
   * Computes 100-hour timelag fuel moisture
   * (Bradshaw et al. 1983)
   *
   * @param    prev   previous'day value
   * @param    EMC24  weighted 24-hour average Equilibrium moisture content
   * @param    PDur   precipitation duration [hours]
   * @return          100-hour timelag fuel moisture [%]
   */
  def MC100(prev:Double,EMC24:Double, PDur:Double):Double = {
    val D = ((24.0-PDur)*EMC24 + PDur*(0.5*PDur+41))/24

    prev + (D-prev)*(1.0-0.87*math.exp(-0.24))
  }

  /**
   * Computes weighted 24-hour average moisture condition for 1000-hour timelag model
   * (Bradshaw et al. 1983)
   *
   * @param    start  first day value
   * @param    EMC24  weighted 24-hour average Equilibrium moisture content
   * @param    PDur   precipitation duration [hours]
   * @return          1000-hour timelag fuel moisture [%]
   */
  def D1000(EMC24:Double, PDur:Double):Double = {

     ((24.0-PDur)*EMC24 + PDur*(2.7*PDur+76.0))/24
  }
  /**
   * Computes 1000-hour timelag fuel moisture
   * (Bradshaw et al. 1983)
   *
   * @param    start  first day value
   * @param    EMC24  weighted 24-hour average Equilibrium moisture content
   * @param    PDur     precipitation duration [hours]
   * @return          1000-hour timelag fuel moisture [%]
   */
  def MC1000(EMC24:List[Double], PDur:List[Double]):List[Double] = {
    val start = 30.0  //start value in %
    var out = new ListBuffer[Double]

    val D = ListFunctions.applyFunction(D1000,EMC24,PDur)
    val Dm = ListFunctions.weeklySum(D).map(_/7)

    for (i <- 0 until Dm.length){
      val prev = if (i<7) start else out(i-1)
      out += prev + (Dm(i)-prev)*(1.0-0.82*math.exp(-0.168))
    }
    out.toList
  }

  /**
   * Computes saturation vapour pressure, according to Allen et al.(1998), eq.11
   * 
   * @param   T    Temperature [C]
   * @return       saturation vapour pressure [kPa]
   */
  def Es(T:Double):Double = {
    0.6108*math.exp(17.27*T/(T+237.3))
  }
  
  /**
   * Computes the slope of the saturation vapour pressure curve, according to Allen et al.(1998), eq.13
   * 
   * 
   *
   * @param   T    Temperature [C]
   * @return       slope of the saturation vapour pressure curve [kPa/C]
   */
  def slope_Es_curve(T:Double):Double = {
    4098.0*Es(T) / pow(T+237.3,2)
  }

  /**
   * Computes actual vapour pressure, according to Allen et al.(1998)
   *
   *
   *
   * @param   T    temperature [C]
   * @param   H    relative humidity [%]
   * @return       actual vapour pressure [kPa]
   */
  def Ea(T:Double,H:Double):Double = {
    Es(T)*H/100
  }
  
  /**
   * Computes vapor pressure deficit
   * 
   * 
   *
   * @param   H    Relative Humidity [%]
   * @param   T    Temperature [C]
   * @return       Vapor Pressure Deficit [kPa]
   */
  def VPD(H:Double,T:Double):Double = {
//    val es = Es(T)
//    val ea = es*H/100                                                                                           //actual vapor pressure
//    return es-ea
    Es(T)*(1-H/100)
  }
  
  /**
   * Computes vapor pressure deficit using Tmax and Tmin, according to Allen et al.(1998), eq.12
   * 
   * 
   *
   * @param   H    Relative Humidity
   * @param   Tmax Maximal Temperature
   * @param   Tmin Minimal Temperature
   * @return       Vapor Pressure Deficit
   */
  def VPD(H:Double,Tmax:Double,Tmin:Double):Double = {
//    val Es = 0.5*(0.6108*math.exp(17.27*Tmax/(Tmax+237.3))+0.6108*math.exp(17.27*Tmin/(Tmin+237.3))) //vapor saturation pressure
//    val Ea = Es*H/100                                                                                           //actual vapor pressure
//    return Es-Ea
    (VPD(H,Tmax)+VPD(H,Tmin))/2
  }
  
  /**
   * Computes Nesterov index value
   *
   * @param  prev         previous day Nesterov Value
   * @param  T15          temperature at 15:00
   * @param  Tdew15       dewpoint temperature at 15:00
   * @param  P            daily rainfall amount [mm]
   * @return              Nesterov index value
   */
  def Nesterov(prev: Double, T15: Double, Tdew15: Double, P: Double):Double = {
    val RainTreshold:Double = 3.0
    return if (P > RainTreshold) 0 else prev + (T15-Tdew15)*T15
  }
  
  /**
   * Computes Munger index value
   * 
   *
   * @param  prev         previous day Munger Value
   * @param  P            daily rainfall amount [mm]
   * @return              Munger index value
   */
  def Munger(prev:Double, P: Double):Double = {
    val RainTreshold:Double = 1.27
    
    if (P>= RainTreshold ) 
        0
    else
        if (Null.is(prev)) 
            Double.NaN 
        else 
           0.5*math.pow(math.sqrt(2*prev)+1,2)

  }
  
  /**
   * Computes the number of consecutive days without rainfall (or below a threshold)
   * 
   *
   * @param  prev            previous number of consecutive days without rainfall [-]
   * @param  P               daily rainfall amount [mm]
   * @return                 consecutive days without rainfall
   */
  def DaysSinceRain(prev:Double, RainTreshold:Double, P:Double):Double = {
    val thr = if (Null.is(RainTreshold)) 0.0 else RainTreshold
    if (Null.is(P)) Double.NaN
    else if (P>thr) 0
    else prev+1
  }
  
  /**
   * Computes Angstroem index value
   * 
   *
   * @param  T      Temperature
   * @param  H      Relative Humidity
   * @return        Angstroem index value
   */
  def Angstroem(T13:Double, H13: Double):Double = {
    return (H13/20)+((27-T13)/10)
  }
  
  /**
   * Computes Fosberg index value
   * 
   *
   * @param  T    Temperature average [°C]
   * @param  H    Relative humidity [%]
   * @param  U    Wind speed average [m/s]
   * @return      Fosberg index value
   */
  def FFWI(T:Double, H: Double, U:Double):Double = {
    val WNDSPDmph = kmh2mph(U*3.6)  //convert [m/s] into [mph]
    val m = EMC(H,T)                  //equilibrium moisture content
    val n = 1-(2*m/30)+(1.5*math.pow((m/30),2))-(0.5*math.pow((m/30),3))
    return (n*math.sqrt(1+math.pow(WNDSPDmph,2)))/0.3002
  }
  /**
   * Computes Fosberg index value
   *
   *
   * @param  T    Temperature average [°C]
   * @param  H    Relative humidity [%]
   * @param  U    Wind speed average [m/s]
   * @return      Fosberg index value
   */
  def FFWI(EMC: Double, U:Double):Double = {
    val WNDSPDmph = kmh2mph(U*3.6)  //convert [m/s] into [mph]
    val n = 1-(2*EMC/30)+(1.5*math.pow((EMC/30),2))-(0.5*math.pow((EMC/30),3))
    return (n*math.sqrt(1+math.pow(WNDSPDmph,2)))/0.3002
  }

  /**
   * Computes the modified Fosberg index value
   *
   * @param  KBDI   KBDI [inch/100]
   * @param  FFWI   Fosberg index value (original)
   * @return        modified Fosberg index value
   */
  def FFWImod(KBDISI:Double, FFWI: Double):Double = {
    val FAF = 0.000002 * math.pow(KBDISI / 100/ 2.54 * 10 ,2) + 0.72   //fuel availability factor
    FAF * FFWI
  }
  /**
   * Calculate the mean annual rainfall
   *
   * @param  inputFile  File with all daily rainfall
   * @return            Mean Annual Rainfall
   */
  def meanAnnualRain(inputFile:File):Double = {
        var Counter = 0;
        var AVGRain = 0.0;

        try {
            Class.forName("org.relique.jdbc.csv.CsvDriver")
            val conn: java.sql.Connection  = java.sql.DriverManager.getConnection("jdbc:relique:csv:" + inputFile.getParent)
            val stmt: java.sql.Statement  = conn.createStatement()
            val results: java.sql.ResultSet = stmt.executeQuery("SELECT P from "+inputFile.getName.substring(0, inputFile.getName.length-4))
            while (results.next()) {
               Counter += 1;
               AVGRain = AVGRain + results.getDouble("P");
            }
            results.close();
            stmt.close();
            conn.close();

            //Calculate Effective Average
            return AVGRain/Counter*365.25;

        } catch {
            case e:Exception => logger.error("Oops-> There is a problem calculating mean annual rainfall \n" + e + "\n");
            return 0.0;
        }
  }
  
  /**
   * Calculate the mean annual rainfall, not considering missing values
   * 
   * @param  P  sequence with daily rainfalls
   * @return    Mean Annual Rainfall
   */
  def meanAnnualRain(P:Seq[Double]):Double = {  
    
    val tmp = P.filterNot(Null.is(_)) 
    tmp.sum / tmp.length * 365.25
  }

  
  /**
   * Computes KBDI index value
   * 
   *
   * @param  prev           previous day KBDI Value
   * @param  PweekThreshold weekly rain threshold to initialize index
   * @param  Tmax           Maximal temperature [°C]
   * @param  RainSum        sum of consecutive rainfall [mm]
   * @param  P              Rainfall amount [mm]
   * @param  AnnualRainAVG  Annual rainfall average [mm]
   * @return                KBDI index value
   */
  def KBDI(prev:Double, PweekThreshold:Double, AnnualRainAVG:Double, Tmax:Double, RainSum:Double, P:Double, WeeklyRain:Double):Double = {
    val T = C2F(Tmax)                    //convert [°C] to [°F]
    val RainAVGinches = mm2inch(AnnualRainAVG)  //convert [mm] to [inches]
    val DeltaTime = 1
    val RainTreshold = 0.2*25.4             //convert [inches] to [mm]
    val netRain = math.max(0, P - math.max(0, RainTreshold-(RainSum-P)))

//    val Q = if (prev.isNaN && WeeklyRain>=PweekThreshold) {
    val Q = if (WeeklyRain>=PweekThreshold) {
              0.0
            }
            else {
              math.max(0, prev-netRain/25.4*100)
            }

    return Q + math.max(0,(800-Q)*(0.968*math.exp(0.0486 * T)-8.30)*DeltaTime/(1+10.88*math.exp(-0.0441 * RainAVGinches))*math.pow(10, -3))
  }
  
  /**
   * Computes KBDISI index value
   * 
   *
   * @param  prev          previous day KBDI Value
   * @param  PweekThreshold weekly rain threshold to initialize index
   * @param  Tmax          Maximal temperature [°C]
   * @param  SumPrevRain   sum of consecutive rainfall [mm]
   * @param  P             Rainfall amount [mm]
   * @param  AnnualRainAVG Annual rainfall average [mm]
   * @return               KBDISI index value
   */
  def KBDI_SI(prev:Double, PweekThreshold:Double, AnnualRainAVG:Double, Tmax:Double, SumPrevRain:Double, P:Double, WeeklyRain:Double):Double = {
    val DeltaTime = 1
    val RainTreshold = 0.2*25.4             //convert [inches] to [mm]
    val netRain = math.max(0, P - math.max(0, RainTreshold-(SumPrevRain-P)))

    val Q = if (WeeklyRain>=PweekThreshold) {
              0.0
            }
            else {
              math.max(0, prev-netRain)
            }
    
    return Q + math.max(0,(203.2-Q)*(0.968*math.exp(0.0875*Tmax+1.5552)-8.30)*DeltaTime/(1+10.88*math.exp(-0.001736 * AnnualRainAVG))*math.pow(10, -3))
  }
  
  /**
   * Computes amount (sum) of consecutive rainfall.
   * 
   *
   * @param  prev         previous day sum of consecutive rainfall [°C]
   * @param  P            Rainfall amount [%]
   * @return              amount of consecutive rainfall
   */
  def RainSum(prev:Double, P:Double):Double = {
    if (P==0) 0
    else prev+P
  }

  /**
   * Computes amount (sum) of the last consecutive rainfall. Compared to RainSum,
   * it retains the value over the dry periods (P=0)
   *
   *
   * @param  prev         previous day sum of consecutive rainfall [mm]
   * @param  P            Rainfall amount [mm]
   * @param  P_1          previous day's Rainfall amount [mm]
   * @return              amount of last consecutive rainfall [mm]
   */
  def lastRainSum(prev:Double,P:Double,P_1:Double):Double = {
    if (Null.is(P) || Null.is(P_1)) return Double.NaN 
    if (P_1==0.0 && P>0.0) P               //first day
    else prev+P
  }
  
 /**
   * Computes amount (sum) of the last significant rainfall event.
   * The rainfall event is defined as consecutive days with rainfall above a threshold 
   * (2 mm in Finkele 2006).
   * 
   * To be used for the calculation of the DF (Drought Factor) according to Griffith and Finkele.
   *
   *
   * @param  prev                   previous day sum of consecutive rainfall [mm]
   * @param  P                      Rainfall amount [mm]
   * @param  P_1                    previous day's Rainfall amount [mm]
   * @param  rainfallEventThreshold threshold daily rain to consider a rain evnet
   * @return                        amount of last significant rainfall event [mm]
   */
  def lastRainSum_withThreshold(prev:Double, P:Double, P_1:Double, rainfallEventThreshold:Double=2.00):Double = {
    if (Null.is(P) || Null.is(P_1)) return Double.NaN 
    if (P_1<rainfallEventThreshold && P>=rainfallEventThreshold) P              //first day
    else prev + math.max(0.0, P-rainfallEventThreshold)
  }

  /**
   * Rainfall event age, defined as the number of days since the day with 
   * the largest daily rainfall within the rain event (Finkele 2006, ref to Sullivan 2001).
   * The rainfall event is defined as consecutive days with rainfall above a threshold 
   * (2 mm in Finkele 2006).
   * 
   * To be used for the calculation of the DF (Drought Factor) according to Griffith and Finkele.
   *
   * @param  prev                   previous rainfall event age [days]
   * @param  prevMaxP:Double        previous maximum rainfall [mm]
   * @param  P                      Rainfall amount [mm]
   * @param  P_1                    previous day's Rainfall amount [mm]
   * @param  rainfallEventThreshold threshold daily rain to consider a rain evnet
   * @return                        rainfall event age [days]
   */
  def rainEventAge_withThreshold(prev:Long, prevMaxP:Double, P:Double, P_1:Double, rainfallEventThreshold:Double=2.00):Long = {
    if (Null.is(P) || Null.is(P_1)) return Null.Long 
    if (P_1<rainfallEventThreshold && P>=rainfallEventThreshold) 0           //first day
    else if (P>=rainfallEventThreshold && P>=prevMaxP) 0                    //higher P in the rain event
    else prev + 1
  }
  
  /**
   * Computes FMI index value
   * 
   *
   * @param  T       Temperature average [°C]
   * @param  H       Relative humidity [%]
   * @return         FMI index value
   */
  def FMI(T:Double , H:Double):Double = {
    return 10-0.25*(T-H)
  }
  
  /**
   * Computes Sharples index value
   * 
   *
   * @param  T       Temperature average [°C]
   * @param  H       Relative humidity [%]
   * @param  U       Wind speed average [m/s]
   * @return         Sharples index value
   */
  def Sharples(T:Double , H:Double, U:Double):Double = {
    val WindThreshold:Double = 1  //   km/h
    return math.max(WindThreshold,U*3.6)/FMI(T,H)
  }
  
  /**
   * Computes pM68 Value (precipitation corrected with Snowcover field)
   * 
   *
   * @param  prev            previous value of pM68
   * @param  T13             Temperature at 13:00
   * @param  H13             Relative humidity at 13:00
   * @param  currentDate     Current Date (millisec)
   * @param  P13             current day ranifall sum at 13:00
   * @param  P_1             previous day rainfall
   * @param  P_2             2 days ago rainfall
   * @param  P_3             3 days ago rainfall
   * @param  snowcover       current day snowcover (0/1)
   * @param  snowcover_1     previous day snowcover (0/1)
   * @param  snowcover_2     2 days ag snowcover (0/1)
   * @param startFireSeason  Date start of fireseason (millisec)
   * @param endFireSeason    Date end of fireseason (millisec)
   * @return                 pM68 rain/snow correction value
   */
  def pM68(prev:Double, T13:Double,H13:Double, currentDate:Long, P13:Double, P_1:Double, P_2:Double, P_3:Double, snowcover:Int, snowcover_1:Int, snowcover_2:Int, startFireSeason:Long, endFireSeason:Long):Double = {
    if (Null.is(snowcover)||Null.is(snowcover_1)||Null.is(snowcover_2)) return Double.NaN
    else{
      val vpd = VPD(H13,T13)
      val DeltaE = kPa2mmHg(vpd)            //conversion from kPa to mmHg
      val MMdd = Utils.solarDate2String(currentDate).substring(4, 8).toLong
      val start = Utils.solarDate2String(startFireSeason).substring(4, 8).toLong
      val end = Utils.solarDate2String(endFireSeason).substring(4, 8).toLong
      val k1 = if ((P13>=1 && P13<5)||snowcover==1) 0.5
               else if ((P13>=5 && P13<10)||(snowcover==1 && snowcover_1==1)) 0.25
               else if ((P13>=10)||(snowcover==1 && snowcover_1==1 && snowcover_2==1)) 0.0
               else 1.0  //if(P<1)

      val k2 = if ((P13>=20)||(snowcover==1 && snowcover_1==1 && snowcover_2==1)) 0.0
               else if (P13<20 && (P_1>=20 || P_2>=20 || P_3>=20)) 0.5
               else 1   //if (P<20 && P1<20 && P2<20 && P3<20)

      if (MMdd>=start && MMdd<=end) return k1 * prev + math.max(0,k2*(T13+10)*DeltaE)
      else return 0
    }
  }
  
  /**
   * Computes pM68 Value (precipitation corrected with Snowcover field)
   * 
   *
   * @param  prev            previous value of pM68
   * @param  T13             Temperature at 13:00
   * @param  VPD13           Vapour pressure deficit at 13:00
   * @param  currentDate     Current Date (millisec)
   * @param  P13             current day ranifall sum at 13:00
   * @param  P_1             previous day rainfall
   * @param  P_2             2 days ago rainfall
   * @param  P_3             3 days ago rainfall
   * @param  snowcover       current day snowcover (0/1)
   * @param  snowcover_1     previous day snowcover (0/1)
   * @param  snowcover_2     2 days ag snowcover (0/1)
   * @param startFireSeason  Date start of fireseason (millisec)
   * @param endFireSeason    Date end of fireseason (millisec)
   * @return                 pM68 rain/snow correction value
   */
  def pM68_vpd(prev:Double, T13:Double,VPD13:Double, currentDate:Long, P13:Double, P13_1:Double, P13_2:Double, P13_3:Double, snowcover:Int, snowcover_1:Int, snowcover_2:Int, startFireSeason:Long, endFireSeason:Long):Double = {
    if (Null.is(snowcover)||Null.is(snowcover_1)||Null.is(snowcover_2)) return Double.NaN
    else{
      val DeltaE = kPa2mmHg(VPD13)            //conversion from kPa to mmHg
      val MMdd = Utils.solarDate2String(currentDate).substring(4, 8).toLong
      val start = Utils.solarDate2String(startFireSeason).substring(4, 8).toLong
      val end = Utils.solarDate2String(endFireSeason).substring(4, 8).toLong
      val k1 = if ((P13>=1 && P13<5)||snowcover==1) 0.5
               else if ((P13>=5 && P13<10)||(snowcover==1 && snowcover_1==1)) 0.25
               else if ((P13>=10)||(snowcover==1 && snowcover_1==1 && snowcover_2==1)) 0.0
               else 1.0  //if(P<1)

      val k2 = if ((P13>=20)||(snowcover==1 && snowcover_1==1 && snowcover_2==1)) 0.0
               else if (P13<20 && (P13_1>=20 || P13_2>=20 || P13_3>=20)) 0.5
               else 1   //if (P<20 && P1<20 && P2<20 && P3<20)

      if (MMdd>=start && MMdd<=end) return k1 * prev + math.max(0,k2*(T13+10)*DeltaE)
      else return 0
    }
  }
  
  /**
   * Computes pM68dwd Value (precipitation corrected with Snowcover field)
   * 
   *
   * @param  prev           previous value of pM68
   * @param  T13            Temperature at 13:00
   * @param  H13            Relative humidity at 13:00
   * @param  currentDate    Current Date (millisec)
   * @param  P13            current day ranifall sum at 13:00
   * @param  P_1            previous day rainfall
   * @param  P_2            2 days ago rainfall
   * @param  P_3            3 days ago rainfall
   * @param  snowcover      current day snowcover (0/1)
   * @param  snowcover_1    previous day snowcover (0/1)
   * @param  snowcover_2    2 days ag snowcover (0/1)
   * @param startFireSeason Date start of fireseason (millisec)
   * @param endFireSeason   Date end of fireseason (millisec)
   * @return                pM68 rain/snow correction value
   */
  def pM68dwd(prev:Double, T13:Double, H13:Double, currentDate:Long, P:Double, P_1:Double, P_2:Double, P_3:Double, snowcover:Int, snowcover_1:Int, snowcover_2:Int, startFireSeason:Long, endFireSeason:Long):Double = {
    if (Null.is(snowcover)||Null.is(snowcover_1)||Null.is(snowcover_2)) return Double.NaN
    else{
      val vpd = VPD(H13,T13)
      val DeltaE = vpd*10           //conversion from kPa to hPa
      val MMdd = Utils.solarDate2String(currentDate).substring(4, 8).toLong
      val start = Utils.solarDate2String(startFireSeason).substring(4, 8).toLong
      val end = Utils.solarDate2String(endFireSeason).substring(4, 8).toLong
      val k1 = if ((P>=1 && P<5)||snowcover==1) 0.5
               else if ((P>=5 && P<10)||(snowcover==1 && snowcover_1==1)) 0.25
               else if ((P>=10)||(snowcover==1 && snowcover_1==1 && snowcover_2==1)) 0.0
               else 1.0  //if(P<1)

      val k2 = if ((P>=20)||(snowcover==1 && snowcover_1==1 && snowcover_2==1)) 0.0
               else if (P<20 && (P_1>=20 || P_2>=20 || P_3>=20)) 0.5
               else 1   //if (P<20 && P1<20 && P2<20 && P3<20)

      val k4 = if (H13<=26) 10.0
               else if (H13<=66) 20.0
               else 30.0

      if (MMdd>=start && MMdd<=end) return k1 * prev + math.max(0,k2*(T13+k4)/10*DeltaE)
      else return 0
    }
  }
  
  /**
   * Computes M68 Value (precipitation & vegetation corrected) omitted phenological phases
   * 
   *
   * @param  pM68              list of pM68 values
   * @param  currentDate       list of dates (millisec)
   * @param  P                 lis of rainfall values
   * @param  birchPhase        date start of birch leaves phenological phase (millisec)
   * @param  robiniaPhase      date start of robinia blossom phenological phase (millisec)
   * @param  vegCorrStep3Start first date for vegetation correction (millisec)
   * @param  vegCorrStep3End   second date for vegetation correction (millisec)
   * @return                   M68 vegetation correction value
   */
  def M68(pM68:List[Double],currentDate:List[Long],P:List[Double],birchPhase:Long, robiniaPhase:Long, vegCorrStep3Start:Long, vegCorrStep3End:Long):List[Double] = {
    val birch = Utils.solarDate2String(birchPhase,"MMdd").toLong
    val robinia = Utils.solarDate2String(robiniaPhase,"MMdd").toLong
    val vegCorrStart = Utils.solarDate2String(vegCorrStep3Start,"MMdd").toLong
    val vegCorrEnd = Utils.solarDate2String(vegCorrStep3End,"MMdd").toLong
    var tmpCheck1 = false
    var tmpCheck2 = false
    var k3 = 0.0
    var resultList = new ListBuffer[Double]

    for (i <- 0 until currentDate.length) {
      val MMdd = Utils.solarDate2String(currentDate(i)).substring(4, 8).toLong

      //First occurence of rain after robinia blossom
      if (MMdd==101) tmpCheck1 = false
      else if ((MMdd>robinia && P(i)>=5)) tmpCheck1 =  true
      //First occurence of rain after 14 august
      if (MMdd==101) tmpCheck2 = false
      else if ((MMdd>=vegCorrStart && P(i)>=5)||MMdd>=vegCorrEnd) tmpCheck2 = true

      k3 = if (MMdd >= 101 && MMdd < birch) 3
           else if (MMdd >= birch && !tmpCheck1) 2
           else if (tmpCheck1 && !tmpCheck2) 1
           else 0.5

      resultList += k3 * pM68(i)
    }
    return resultList.toList
  }
  
  /**
   * Computes M68 Value (precipitation & vegetation corrected with fields for phenological phases)
   * 
   *
   * @param  pM68              list of pM68 values
   * @param  currentDate       list of dates (millisec)
   * @param  P                 lis of rainfall values
   * @param  birchPhase        list of birch leaves phenological phase (0/1)
   * @param  robiniaPhase      list of robinia blossom phenological phase (0/1)
   * @param  vegCorrStep3Start first date for vegetation correction (millisec)
   * @param  vegCorrStep3End   second date for vegetation correction (millisec)
   * @return                   M68 vegetation correction value
   */
  def M68(pM68:List[Double],currentDate:List[Long],P:List[Double],birchPhase:List[Int], robiniaPhase:List[Int], vegCorrStep3Start:Long, vegCorrStep3End:Long):List[Double] = {
    val vegCorrStart = Utils.solarDate2String(vegCorrStep3Start,"MMdd").toLong
    val vegCorrEnd = Utils.solarDate2String(vegCorrStep3End,"MMdd").toLong
    var tmpCheck1 = false
    var tmpCheck2 = false
    var k3 = 0.0
    var resultList = new ListBuffer[Double]
    var birchOk = false
    var robiniaOk = false

    for (i <- 0 until currentDate.length) {
      if (Null.is(birchPhase(i))||Null.is(robiniaPhase(i))) resultList += Double.NaN
      else{
        val MMdd = Utils.solarDate2String(currentDate(i)).substring(4, 8).toLong
		      
        if (birchPhase(i)==1) birchOk = true
        if (robiniaPhase(i)==1) robiniaOk = true

	    //First occurence of rain after robinia blossom
        if (MMdd==101) {                 //initialization at year begin
            tmpCheck1 = false
            birchOk=false
            robiniaOk =false
        }else if (robiniaOk && P(i)>=5) tmpCheck1 =  true          

        //First occurence of rain after 14 august
        if (MMdd==101) tmpCheck2 = false   //initialization at year begin
        else if ((MMdd>=vegCorrStart && P(i)>=5)||MMdd>=vegCorrEnd) tmpCheck2 = true



        k3 = if (MMdd >= 101 && !birchOk) 3
             else if (birchOk && !tmpCheck1) 2  //(birchPhase(i)==1 && !tmpCheck1) 2
             else if (tmpCheck1 && !tmpCheck2) 1
             else 0.5

        resultList += k3 * pM68(i)
      }
    }
    return resultList.toList
  }
  
  /**
   * Computes Drought Factor after Noble 1980
   *
   * @param   lastRainSum       cumulative sum of last consecutive rainfall            
   * @param   daysSinceRainfall consecutive days without rainfall
   * @param   KBDISI            current KBDISI value [mm]
   * @return                    FFDI index value
   */
  def DFnoble(lastRainSum:Double, daysSinceRainfall:Double, KBDISI:Double):Double = {
      math.min(10, (0.191*(KBDISI+104)*math.pow((daysSinceRainfall+1),1.5))/(3.52*math.pow(daysSinceRainfall+1,1.5)+lastRainSum-1))
  }
  /**
   * Computes Drought Factor after Griffith 1999
   * The rainfall event is defined as consecutive days with rainfall above a threshold
   * (2 mm inFinkele 2006).
   *
   * @param   lastRainSum       cumulative sum of last rain event
   * @param   rainEventAge      rain event age (number of days since the day with the largest daily rainfall
   *amount within the rain event, Finkele 2006)
   * @param   KBDISI            current KBDISI value [mm]
   * @return                    FFDI index value
   */
//  def DF(lastRainSum:Double,daysSinceRainfall:Double,KBDISI:Double):Double = {
  def DFgriffith(lastRainSum:Double,rainEventAge:Double,KBDISI:Double):Double = {

    val x = if (lastRainSum<2)  1
            else if (rainEventAge==0)  math.pow(0.8,1.3) / (math.pow(0.8,1.3)+lastRainSum-2)
            else math.pow(rainEventAge,1.3) / (math.pow(rainEventAge,1.3)+lastRainSum-2)

    math.min(10,10.5*(1-math.exp(-(KBDISI+30)/40))*(41*math.pow(x,2)+x)/(40*math.pow(x,2)+x+1))
  }
  /**
   * Computes Drought Factor after Griffith 1999 and adjusted (Finkele et al. 2006)
   * The rainfall event is defined as consecutive days with rainfall above a threshold
   * (2 mm inFinkele 2006).
   *
   * @param   lastRainSum       cumulative sum of last  rain event
   * @param   rainEventAge      rain event age (number of days since the day with the largest daily rainfall
   *amount within the rain event, Finkele 2006)
   * @param   KBDISI            current KBDISI value [mm]
   * @return                    FFDI index value
   */
//  def DF(lastRainSum:Double,daysSinceRainfall:Double,KBDISI:Double):Double = {
  def DFgriffithAdj(lastRainSum:Double,rainEventAge:Double,KBDISI:Double):Double = {

    val x = if (lastRainSum<2)  1
            else if (rainEventAge==0)  math.pow(0.8,1.3) / (math.pow(0.8,1.3)+lastRainSum-2)
            else math.pow(rainEventAge,1.3) / (math.pow(rainEventAge,1.3)+lastRainSum-2)
    
    val xlim = if (KBDISI<20) 1/(1+0.1135 * KBDISI)
               else 75/ (270.525 - 1.267*KBDISI)

    val x1 = math.min(x,xlim)
    math.min(10,10.5*(1-math.exp(-(KBDISI+30)/40))*(41*math.pow(x1,2)+x1)/(40*math.pow(x1,2)+x1+1))
  }

  /**
   * Computes MacArthur 5 index (FFDI)
   *
   * @param   DF                Drought Factor
   * @param   T15               air temperaure at 15:00
   * @param   H15               relative humidity at 15:00
   * @param   U15               Wind speed at 15:00 [m/s]
   * @return                    FFDI index value
   */
  def FFDI(DF:Double,T15:Double,H15:Double,U15:Double):Double = {
      val Ukmh = U15*3.6   //Converts m/s to km/h
      return 2.0*math.exp(-0.45+0.987*math.log(DF)-0.0345*H15+0.0338*T15+0.0234*Ukmh)
  }
  
  /**
   * Computes Extraterrestrial radiation
   * 
   *
   * @param  todayDate date of current day
   * @param  latitude  latitude of location in decimal degrees
   * @return           extraterrestrial radiation value
   */
  def Ra(todayDate:Long, latitude:Double):Double = {
    val latitudeRad = deg2rad(latitude)
    val J = Utils.solarDate2Julian(todayDate)         //Julian date
    val daysInYear = Utils.solarDate2Julian(Utils.solarDate2Long(Utils.solarDate2String(todayDate,"yyyy")+"1231","yyyyMMdd"))
    val delta = 0.409*math.sin(J*2*math.Pi/daysInYear-1.39)         //Solar declination
    val Ws = math.acos(-math.tan(latitudeRad)*math.tan(delta))  //Sunset hour angle
    val dr = 1+0.033*math.cos(J*2*math.Pi/daysInYear)

    return 24*60/math.Pi*0.082*dr*(Ws*math.sin(latitudeRad)*math.sin(delta)+math.cos(latitudeRad)*math.cos(delta)*math.sin(Ws))
  }
  
  /**
   * Computes Solar radiation
   * 
   *
   * @param  todayDate date of current day
   * @param  n         actual duration of sunshine [h]
   * @param  N         amaximum possible duration of sunshine or daylight hours [h]
   * @param  latitude  latitude of location in decimal degrees
   * @param  a         regression constant, expressing the fraction of extraterrestrial radiation reaching the earth on overcast days (n = 0)
   * @param  b         regression constant, (a+b) is the fraction of extraterrestrial radiation reaching the earth on clear days (n = N).
   * @return           solar radiation value
   */
  def Rs_fromSunshine_(todayDate:Long, n:Double, N:Double, latitude:Double, a:Double=0.25, b:Double=0.50):Double = {

     (a + b * n / N ) * Ra(todayDate,latitude)
  }
  def Rs_fromSunshine(Ra:Double, n:Double, N:Double, a:Double=0.25, b:Double=0.50):Double = {

     (a + b * n / N ) * Ra
  }
  
  /**
   * Computes Solar radiation
   * 
   *
   * @param  todayDate date of current day
   * @param  Tmax      maximal temperature
   * @param  Tmin      minimal temperature
   * @param  latitude  latitude of location in decimal degrees
   * @param  krs       krs correction value
   * @return           solar radiation value
   */
//  def Rs_fromT(todayDate:Long,Tmax:Double,Tmin:Double,latitude:Double,krs:Double):Double = {
//    return Ra(todayDate,latitude) * krs * sqrt(Tmax-Tmin)
//  }
  def Rs_fromT(todayDate:Long,Tmax:Double,Tmin:Double,latitude:Double,krs:Double):Double = {
    return Ra(todayDate,latitude) * krs * sqrt(Tmax-Tmin)
  }
  def Rs_fromT(Ra:Double,Tmax:Double,Tmin:Double,krs:Double):Double = {
    return Ra * krs * sqrt(Tmax-Tmin)
  }
  
  /**
   * Computes Clear-sky solar radiation
   * 
   *
   * @param  todayDate date of current day
   * @param  latitude  latitude of location
   * @param  altitude  altitude of location
   * @return           clear-sky solar radiation value
   */
  def Rso(todayDate:Long,latitude:Double,altitude:Double):Double = {
    return  Ra(todayDate,latitude)*(0.75 + 2 * pow(10,-5)*altitude)
  }
  def Rso(Ra:Double,altitude:Double):Double = {
    return  Ra*(0.75 + 2 * pow(10,-5)*altitude)
  }
  
  /**
   * Computes Net shortwave radiation
   * 
   *
   * @param  todayDate date of current day
   * @param  Tmax      maximal temperature
   * @param  Tmin      minimal temperature
   * @param  latitude  latitude of location
   * @param  krs       krs correction value
   * @param  albedo    albedo correction value
   * @return           net shortwave radiation value
   */
  def Rns(todayDate:Long,Tmax:Double,Tmin:Double,latitude:Double,krs:Double,albedo:Double):Double = {
    return  (1.0-albedo)*Rs_fromT(todayDate,Tmax,Tmin,latitude,krs)
  }
  def Rns(Rs:Double,albedo:Double):Double = {
    return  (1.0-albedo)*Rs
  }
  
  /**
   * Computes Net longwave radiation
   * 
   *
   * @param  todayDate date of current day
   * @param  H         relative humidity
   * @param  Tmax      maximal temperature
   * @param  Tmin      minimal temperature
   * @param  latitude  latitude of location
   * @param  altitude  altitude of location
   * @param  krs       krs correction value
   * @return           net longwave radiation value
   */
  def Rnl(todayDate:Long,H:Double,Tmax:Double,Tmin:Double,latitude:Double,altitude:Double,krs:Double):Double = {
//    val Es = 0.5*(0.6108*math.exp(17.27*Tmax/(Tmax+237.3))+0.6108*math.exp(17.27*Tmin/(Tmin+237.3))) //vapor saturation pressure    eq 19 Allen
    val Es = VPD(H, Tmax,Tmin) //vapor saturation pressure    eq 19 Allen
    val Ea = Es*H/100
    val rho = 4.903*pow(10,-9)
    return  rho*((pow(Tmax+273.16,4)+pow(Tmin+273.16,4))/2)*(0.34-0.14*sqrt(Ea))*(1.35*min(1,Rs_fromT(todayDate,Tmax,Tmin,latitude,krs)/Rso(todayDate,latitude,altitude))-0.35)
  }
  def Rnl(Rs:Double,Ra:Double,H:Double,Tmax:Double,Tmin:Double,altitude:Double):Double = {
//    val Es = 0.5*(0.6108*math.exp(17.27*Tmax/(Tmax+237.3))+0.6108*math.exp(17.27*Tmin/(Tmin+237.3))) //vapor saturation pressure    eq 19 Allen
    val Es = VPD(H, Tmax,Tmin) //vapor saturation pressure    eq 19 Allen
    val Ea = Es*H/100
    val rho = 4.903*pow(10,-9)
    return  rho*((pow(Tmax+273.16,4)+pow(Tmin+273.16,4))/2)*(0.34-0.14*sqrt(Ea))*(1.35*math.min(1,Rs/Rso(Ra,altitude))-0.35)
  }

  
  /**
   * Computes Net radiation
   * 
   *
   * @param  todayDate date of current day
   * @param  H         relative humidity
   * @param  Tmax      maximal temperature
   * @param  Tmin      minimal temperature
   * @param  latitude  latitude of location
   * @param  altitude  altitude of location
   * @param  krs       krs correction value
   * @param  albedo    albedo correction value
   * @return           net radiation value
   */
  def Rnet(todayDate:Long,H:Double,Tmax:Double,Tmin:Double,latitude:Double,altitude:Double,krs:Double,albedo:Double):Double = {
    return   Rns(todayDate,Tmax,Tmin,latitude,krs,albedo)-Rnl(todayDate,H,Tmax,Tmin,latitude,altitude,krs)
  }
  def Rnet(Rs:Double,Ra:Double,H:Double,Tmax:Double,Tmin:Double,altitude:Double,albedo:Double):Double = {
    return   Rns(Rs,albedo)-Rnl(Rs, Ra,H,Tmax,Tmin,altitude)
  }
  
  /**
   * Computes atmospheric pressure (in Allen et al.(1998) eq. 7)
   * 
   *
   * @param   altitude altitude of location
   * @return           atmospheric pressure value [kPa]
   */
  def patm(altitude:Double):Double = {
    101.3*math.pow((293-0.0065*altitude)/293,5.26)  //atmospheric pressure
  }
  
  /**
   * Computes Potential evapotranspiration (Penman formula), after Shuttleworth (1993)
   * 
   *
   * @param   netRad   net radiation value
   * @param   T        temperature
   * @param   U        wind speed
   * @param   vpd      vapor pressure deficit value
   * @param   altitude altitude of location
   * @return           potential evapotranspiration value (Penman formula)
   */
  def PETpen(netRad:Double, T:Double, U:Double, vpd:Double, altitude:Double):Double = {
    val PA = patm(altitude)                            //atmospheric pressure
    val lhv = 2.501-0.002368*T                         //latent heat of vaporization (Shuttleworth 1993)
    val gamma = 0.0016286*PA/lhv                       //psychrometric constant (Shuttleworth 1993)
    val delta = slope_Es_curve(T)                      //slope of the saturation vapor pressure curve (Allen et al. 1998)
    return delta/(delta+gamma)*netRad/lhv + gamma/(delta+gamma)*(6.43*(1+0.536*U)*vpd)/lhv
  }
  
  /**
   * Computes fine fuel moisture code (canadian index)
   * 
   * The calculation starts when no snowcover is present.
   *
   *
   * @param  start      start value for initialization
   * @param  prev       previous day FFMC value
   * @param  P          rainfall amount [mm]
   * @param  T12        temperature at noon (12:00) [C]
   * @param  H12        relative humidity at noon (12:00) [%]
   * @param  U12        wind speed at noon (12:00) [m/s]
   * @param  snowcover  snowcover (0/1)
   * @return            FFMC index value
   */
  def FFMC(prev:Double, P12:Double, T12:Double, H12:Double, U12:Double, snowcover:Int, start:Double=85.0):Double = {
    if (Null.is(P12) || Null.is(T12) || Null.is(H12) || Null.is(U12) || Null.is(snowcover)) return Double.NaN 
    if (snowcover == 1) return Double.NaN
    else {
      val prev_ = if (prev.isNaN) start
                    else prev

      var m0 = 147.2*(101.0- prev_)/(59.5+ prev_)       //fine fuel moisture content from the previous day
      val rf = math.max(P12-0.5,0.0)                  //effective rainfall
      var m  = Double.NaN                           //fine fuel moisture content
      if (rf>0){
        //fine fuel moisture content of the current day
        var mr = m0 + 42.5*rf*math.exp(-100.0/(251-m0))*(1-math.exp(-6.93/rf))
        if (m0>150) mr += 0.0015*math.pow((m0-150),2)*math.pow(rf,0.5)
          mr = math.min(mr,250)                       //fine fuel moisture content of the current day
        m0 = mr
      }
        val Ed = 0.942*math.pow(H12,0.679)+11*math.exp((H12-100)/10)+0.18*(21.1-T12)*(1-math.exp(-0.115*H12))  //fine fuel equilibrium moisture content for drying phases
      if (m0>Ed){
          val k0 = 0.424*(1-math.pow(H12/100,1.7))+0.0694*math.pow(U12*3.6,0.5)*(1-math.pow(H12/100,8))
          val kd = k0*0.581*math.exp(0.0365*T12)        //log drying rate
        m = Ed+(m0-Ed)*math.pow(10,-kd)               //fine fuel moisture content
      }else if (m0<Ed){
          val Ew = 0.618*math.pow(H12,0.753)+10*math.exp((H12-100)/10)+0.18*(21.1-T12)*(1-math.exp(-0.115*H12))  //fine fuel equilibrium moisture content for wetting phases
        if (Ew>m0){
            val k1 = 0.424*(1-math.pow((100-H12)/100,1.7))+0.0694*math.pow(U12*3.6,0.5)*(1-math.pow((100-H12)/100,8))
            val kw = k1*0.581*math.exp(0.0365*T12)      //log wetting rate
            m = Ew-(Ew-m0)*math.pow(10,-kw)             //fine fuel moisture content
        }else{
            m = m0                                      //fine fuel moisture content
        }
      } else {   //m0=Ed
        m = m0
      }
      return 59.5*(250-m)/(147.2+m) //*T12/T12
    }
  }
  
  /**
   * Computes duff moisture code (canadian index)
   * 
   *The calculation starts when no snowcover is present.
   *The influence of latitude is implemented as done in the R-package cffdrs
   *
   * @param  start      start value for initialization
   * @param  prev       previous day DMC value
   * @param  P          rainfall amount [mm]
   * @param  T12        temperature at noon (12:00) [C]
   * @param  H12        relative humidity at noon (12:00)[%]
   * @param  snowcover  snowcover (0/1)
   * @param  latitude   latitude of location (if NaN will use only the standard table to get daylength (no latitude influence)
   * @return            DMC index value
   */
  def DMC(prev:Double, date:Long, P12:Double, T12:Double, H12:Double,
          snowcover:Int, latitude:Double=Double.NaN, start:Double=6.0):Double = {
    
    if (Null.is(snowcover) || snowcover == 1) return Double.NaN
    else {

      val prevDMC = if (Null.is(prev)) start
                    else prev
      val DMCr=if(P12 > 1.5){
                  val Re = 0.92*P12-1.27          //effective rainfall
                  
//                  val m0 = 20.0 + math.exp(5.6348-prevDMC/43.43)  //duff moisture content from previous day  //original formulation
                  val m0 = 20.0 + 280/math.exp(0.023*prevDMC)  //duff moisture content from previous day   //formulation as in R-package cffdrs
                  
                  var b = if (prevDMC <= 33) 100.0/(0.5+0.3*prevDMC)
                          else if (prevDMC > 65) 6.2*math.log(prevDMC)-17.2
                          else 14-1.3*math.log(prevDMC)
                  val mR = m0 + 1000*Re/(48.77+b*Re)            //duff moisture content after P
                  //math.max(244.72 - 43.43*math.log(mR-20.0),0.0)
                  
//                  244.72 - 43.43*math.log(mR-20.0)  //original formulation
                  43.43 * (5.6348 - math.log(mR-20.0)) //formulation as in R-package cffdrs
                }else {//if (P12<=1.5) {
                  prevDMC
                }
      val MM = Utils.solarDate2String(date).substring(4,6).toLong //get month
      val dl = if (Null.is(latitude) || (latitude <= 90 & latitude > 30))  MM match{           //day-length - 3
                                         case 1 => 6.5
                                         case 2 => 7.5
                                         case 3 => 9
                                         case 4 => 12.8
                                         case 5 => 13.9
                                         case 6 => 13.9
                                         case 7 => 12.4
                                         case 8 => 10.9
                                         case 9 => 9.4
                                         case 10 => 8
                                         case 11 => 7
                                         case 12 => 6
                                           
                } else if (latitude <= 30 & latitude > 10)   MM match{  
                                         case 1 => 7.9
                                         case 2 => 8.4
                                         case 3 => 8.9
                                         case 4 => 9.5
                                         case 5 => 9.9
                                         case 6 => 10.2
                                         case 7 => 10.1
                                         case 8 => 9.7
                                         case 9 => 9.1
                                         case 10 => 8.6
                                         case 11 => 8.1
                                         case 12 => 7.8
                                           
                } else if (latitude <= -10 & latitude > -30)   MM match{  
                                         case 1 => 10.1
                                         case 2 => 9.6
                                         case 3 => 9.1
                                         case 4 => 8.5
                                         case 5 => 8.1
                                         case 6 => 7.8
                                         case 7 => 7.9
                                         case 8 => 8.3
                                         case 9 => 8.9
                                         case 10 => 9.4
                                         case 11 => 9.9
                                         case 12 => 10.2
                                           
                } else if (latitude <= -30 & latitude >= -90)   MM match{  
                                         case 1 => 11.5
                                         case 2 => 10.5
                                         case 3 => 9.2
                                         case 4 => 7.9
                                         case 5 => 6.8
                                         case 6 => 6.2
                                         case 7 => 6.5
                                         case 8 => 7.4
                                         case 9 => 8.7
                                         case 10 => 10.0
                                         case 11 => 11.2
                                         case 12 => 11.8
                  
                } else if (latitude <= 10 & latitude > -10) { 
                  9
                } else {   //should never be reached, but can be a universal solution
                  DaylightHoursFAO(date,latitude) -3   
                }
      val K = 1.894*(math.max(T12,-1.1)+1.1)*(100.0-H12)*dl*math.pow(10,-6)
      return math.max(0.0, DMCr + 100.0*K)
    }
  }

  
  /**
   * Computes Drought code (canadian index)
   * 
   *The calculation starts when no snowcover is present.
   *No overwintering of DC is calculated.
   *The influence of latitude is implemented as done in the R-package cffdrs
   *
   * @param  start      start value for initialization
   * @param  prev       previous day DC value
   * @param  date       current date
   * @param  P          rainfall amount [mm]
   * @param  T12        temperature at noon (12:00) [C]
   * @param  snowcover  snowcover [0/1]
   * @param  latitude   latitude of location (if NaN will use only the standard table to get daylength (no latitude influence))
   * @return            DC index value
   */
  def DC(prev:Double, date:Long, P12:Double, T12:Double,
         snowcover:Int, latitude:Double=Double.NaN, start:Double=15.0):Double = {
    
    if (Null.is(snowcover) || snowcover == 1) return Double.NaN
    else {

      val prevDC = if (Null.is(prev)) start
                    else prev

      val DC0 = if (P12>2.8) {
                  val rd =  0.83*P12-1.27                      //Effective rainfall
                  val Q0 = 800.0*math.exp(-prevDC/400.0)            //Moisture equivalent of the previous day's DC
                  val Qr = Q0 + 3.937*rd                        //moisture equivalent after P
                  val DCr = math.max(0.0, 400.0*math.log(800.0/Qr))   //DC after P
                  DCr
                }else {
                  prevDC
                }
      val MM = Utils.solarDate2String(date).substring(4,6).toLong //get month
      val Lf =  if (Null.is(latitude)|| latitude >10)  MM match{           //day-length factor 
                    case 1 => -1.6
                    case 2 => -1.6
                    case 3 => -1.6
                    case 4 => 0.9
                    case 5 => 3.8
                    case 6 => 5.8
                    case 7 => 6.4
                    case 8 => 5.0
                    case 9 => 2.4
                    case 10 => 0.4
                    case 11 => -1.6
                    case 12 => -1.6
                      
                } else if (latitude <= -10)  MM match{   
                    case 1 => 6.4
                    case 2 => 5.0
                    case 3 => 2.4
                    case 4 => 0.4
                    case 5 => -1.6
                    case 6 => -1.6
                    case 7 => -1.6
                    case 8 => -1.6
                    case 9 => -1.6
                    case 10 => 0.9
                    case 11 => 3.8
                    case 12 => 5.8
                                      
                } else if (latitude > -10 & latitude <= 10) {
                    1.4
        
                } else {  //should not be reached;  
                    Double.NaN   //todo   think of a solution with DaylightHoursFAO(date,latitude)
                }
      
      val V = math.max(0.0, 0.36*(2.8+math.max(-2.8, T12))+Lf)   //potential evapotranspiration

      return (DC0 + V/2.0)
    }
  }
  
  /**
   * Computes Initial spread index (canadian index)
   * 
   *
   * @param  FFMC FFMC index value
   * @param  U12  wind speed at noon (12:00) [m/s]
   * @return      ISI index value
   */
  def ISI(FFMC:Double, U12:Double):Double = {
    val m = 147.2*(101-FFMC)/(59.5+FFMC)
    val F = 91.9*math.exp(-0.1386*m)*(1+math.pow(m,5.31)/(4.93*math.pow(10,7)))
    val W = math.exp(0.05039*U12*3.6)
    return 0.208*F*W
  }
  
  /**
   * Computes Buildup index (canadian index)
   * 
   *
   * @param  FFMC FFMC index value
   * @param  DC   DC index value
   * @return      BUI index value
   */
  def BUI(DMC:Double, DC:Double):Double = {
    val bui = if (DMC==0.0 && DC==0.0) 0.0
              else{
                if (DMC<=0.4*DC) 0.8*DMC*DC/(DMC+0.4*DC)
//                else DMC-(1-0.8*DC/(DMC+0.4*DC))*math.pow((0.92+0.0114*DMC),1.7)  //wrong
                else DMC-(1-0.8*DC/(DMC+0.4*DC)) * (0.92 + math.pow(0.0114*DMC, 1.7))
              }
    return math.max(0.0,bui)
  }
  
  /**
   * Computes Fire weather index in S-scale(canadian index)
   * 
   *
   * @param  BUI  BUI index value
   * @param  ISI  ISI index value
   * @return      FWI index value
   */
  def FWI(BUI:Double,ISI:Double):Double = {
    val D = if (BUI<=80) 0.626*math.pow(BUI,0.809)+2.0
            else 1000.0/(25+108.64*math.exp(-0.023*BUI))
    val B = 0.1*ISI*D
    return if (B>1) math.exp(2.72*math.pow(0.434*math.log(B),0.647))
           else B
  }
  
  /**
   * Computes Baumgartner index
   * 
   *
   * @param  FivePrecDaysPETpen  cumulative sum of five previous days PET penmann value
   *                             (PET values should be calculated at 14:00)
   *                             (is calculated with the data until yesterday)
   * @param  FivePrecDaysRain    cumulative sum of five previous days rainfall
   * @return                     Baumgartner index value
   */
  def Baumgartner(FivePrecDaysPETpen:Double,FivePrecDaysRain:Double):Double = {
      return FivePrecDaysPETpen-FivePrecDaysRain
  }
  
  /**
   * Computes Baumgartner Danger classes (form March to September)
   * 
   *
   * @param  BaumgartnerIndex  Baumgartner Index
   * @param  month             month umber (3-9)
   * @return                   Baumgartner Danger classes
   */
  def BaumgartnerDanger(BaumgartnerIndex:Double,month:Int):Double = {
      val thresholds = Seq( -5D ::  3D ::  9D :: 15D ::Nil,
                        -3.0 ::  8D :: 16D :: 27D ::Nil,
                         3.0 :: 16D :: 25d :: 35D ::Nil,
                        12.0 :: 24D :: 32D :: 41D ::Nil,
                        12.0 :: 24D :: 31D :: 40D ::Nil,
                         8.0 :: 20D :: 28D :: 37D ::Nil,
                         6.0 :: 18D :: 26D :: 35D ::Nil  )  
      
      if (month <3 || month >9 || Null.is(BaumgartnerIndex))  Double.NaN
      else intervalIndex(thresholds(month-3), BaumgartnerIndex)
  }

  /**
   * Computes the Numerical Risk index (Sol 1990).
   *
   * r is set = 45 for the non vegetative season (form November to April).
   *
   *
   * @param  T          air temperature
   * @param  Tdew       dewpoint temperature
   * @param  U          wind [m/s]
   * @param  Cc         cloud cover fraction [ratio]
   * @param  r          soil water reserve [mm]
   * @param  solarDate  date [ms] (to check period of the year)
   * @return            numerical risk index value
   */
  def RN(T:Double,Tdew:Double,U:Double,Cc:Double,r:Double,solarDate:Long):Double = {
    val CcOkta = Cc*8   //cloud cover in Okta
    val month = (Utils.solarDate2String(solarDate, "MM")).toInt
    val rmod = if (month >= 5 && month <=11) r
               else  45   //for spring and winter season  (non vegetative period) set r = 45

    val Tlitter = if (CcOkta<=2) 0.874*T - 0.189*U/3.6 + 21.38
                  else 1.36*T - 1.422*CcOkta - 0.22*Tdew + 13.42
    val FH = 100.0 * Es(Tdew) / Es(Tlitter)

    val WRF = 3 + 2* math.tanh((rmod-50)/25)
    val WF = 3.0+ 3*math.tanh((45-U/3.6)/50)
    val ROS = 180* math.exp(T*1714)*math.tanh((100-rmod)/150)*(1+2*(0.8483+math.tanh(U/3.6/30 -1.25)))
    val RSF = if (ROS<=600) -3
              else if (ROS<1000)  0
              else  2

    25 - FH*WRF*WF/15 + RSF
  }

  /**
   * Computes the Ignition index (step for the Portuguese index Ifa).
   *
   *
   * @param  T12        air temperature at noon
   * @param  Tdew12     dewpoint temperature at noon
   * @return            numerical risk index value
   */
  def IG(T12:Double,Tdew12:Double):Double = {

    T12 * (T12 - Tdew12)
  }

  /**
   * Computes the Ifa index (Portuguese index).
   *
   *
   * @param  IG               ignition index
   * @param  P                rainfall [mm]
   * @param  U                wind speed [m/s]
   * @param  FireSeasonStart  start of fire season [msec] 
   * @param  FireSeasonEnd    end of fire season [msec]
   * @return                  Ifa index value
   */
  def Ifa(dates:List[Long],IG:List[Double],P:List[Double],U:List[Double],FireSeasonStart:Long,FireSeasonEnd:Long):List[Double] = {
    val IGcum = new ListBuffer[Double]
    val start = Utils.solarDate2String(FireSeasonStart).substring(4, 8).toLong
    val end = Utils.solarDate2String(FireSeasonEnd).substring(4, 8).toLong
    //     val IGcum = ListFunctions.cumSum(IG)
    var prevIG=0.0
    for (i <- 0 until dates.length){
      val MMdd = Utils.solarDate2String(dates(i),"MMdd").toLong
      IGcum += ( if (start==end) Double.NaN
                  else if (MMdd==start) IG(i)    //initialize cumulative sum
                  else if (start<end) {if (MMdd<start||MMdd>=end) Double.NaN else prevIG+IG(i)}
                  else {if (MMdd>=start&&MMdd<=1231 || MMdd>=101&&MMdd<end)  prevIG+IG(i) else  Double.NaN })
      prevIG = IGcum(i)
    }
    val p = P.map(x=> if (x<=1)      1.0
                     else if (x<=2)  0.8
                     else if (x<=3)  0.6
                     else if (x<=4)  0.4
                     else if (x<=10) 0.2
                     else            0.1 )
    val B = for (i <- 0 until IGcum.length) yield p(i)*IGcum(i)

    val IGclass = IG.map(x=> if (x<151)      1.0            //TODO verify if this is in the original publication
                             else if (x<301) 2
                             else if (x<451) 3
                             else if (x<601) 4
                             else if (x<751) 5
                             else if (x>=751) 6
                             else Double.NaN)
    val Bclass = B.map(x=> if (x<301)      1.0            //TODO verify if this is in the original publication
                             else if (x<1001) 2
                             else if (x<2001) 3
                             else if (x<4001) 4
                             else if (x<6001) 5
                             else if (x<8001) 6
                             else if (x<10001) 7
                             else if (x<12001) 8
                             else if (x<15001) 9
                             else if (x<18001) 10
                             else if (x<21001) 11
                             else if (x<24001) 12
                             else if (x<27001) 13
                             else if (x<31001) 14
                             else if (x<35001) 15
                             else if (x<39001) 16
                             else if (x<43001) 17
                             else if (x<47001) 18
                             else if (x<51001) 19
                             else if (x>=51001) 20
                             else Double.NaN)


    val Ifatemp = for (i<-1 until IG.length) yield (IGclass(i)+ Bclass(i-1)) //skip first value
    val Ifa = Double.NaN::(Ifatemp.toList)
    
    val Ukmh = U.map(_*3.6)
    (for (i <- 0 until Ifa.length) yield (Ifa(i) + (if (Ukmh(i)<=10)    0
                                                  else if (Ukmh(i)<=15) 1
                                                  else if (Ukmh(i)<=20) 2
                                                  else if (Ukmh(i)<=30) 3
                                                  else if (Ukmh(i)<=40) 4
                                                  else                  5))).toList   //correct for wind

  }

  ///**
   //* Computes the cumulative Index B, a subindex of the Portuguese index Ifa.
   //* Note that in this implementation the value B at a certain time refers to the preceeding day
   //*
   //* @param  IG               ignition index
   //* @param  P                rainfall [mm]
   //* @param  FireSeasonStart  start of fire season [msec] 
   //* @param  FireSeasonEnd    end of fire season [msec]
   //* @return                  B index value
   //*/
  //def B(dates:List[Long],IG:List[Double],P:List[Double],FireSeasonStart:Long,FireSeasonEnd:Long):List[Double] = {
    //var IGcum = new ListBuffer[Double]
    //val start = Utils.solarDate2String(FireSeasonStart).substring(4, 8).toLong
    //val end = Utils.solarDate2String(FireSeasonEnd).substring(4, 8).toLong
    ////     val IGcum = ListFunctions.cumSum(IG)
    //var prevIG=0.0
    //for (i <- 0 until dates.length){
      //val MMdd = Utils.solarDate2String(dates(i),"MMdd").toLong
      //IGcum += ( if (start==end) Double.NaN
                  //else if (MMdd==start) IG(i)    //initialize cumulative sum
                  //else if (start<end) {if (MMdd<start||MMdd>=end) Double.NaN else prevIG+IG(i)}
                  //else {if (MMdd>=start&&MMdd<=1231 || MMdd>=101&&MMdd<end)  prevIG+IG(i) else  Double.NaN })
      //prevIG = IGcum(i)
    //}
    //val p = P.map(x=> if (x<=1)      1.0
                     //else if (x<=2)  0.8
                     //else if (x<=3)  0.6
                     //else if (x<=4)  0.4
                     //else if (x<=10) 0.2
                     //else            0.1 )
    //val B = for (i <- 0 until IGcum.length) yield p(i)*IGcum(i)
    
    //Double.NaN::(B.slice(0,B.length-1).toList)     //move serie to next day  
  //}
  
  ///**
   //* Computes the cumulative Index B, a subindex of the Portuguese index Ifa.
   //* Note that in this implementation the value B at a certain time refers to the preceeding day
   //*
   //* @param  prevIG           ignition index of previous day
   //* @param  prevP            rainfall [mm] of previous day
   //* @param  FireSeasonStart  start of fire season [msec] 
   //* @param  FireSeasonEnd    end of fire season [msec]
   //* @return                  B index value
   //*/
  //def Bnew(prev:Double=0.0, date:Long,prevIG:Double,prevP:Double,FireSeasonStart:Long,FireSeasonEnd:Long):Double = {
    //val start = Utils.solarDate2String(FireSeasonStart).substring(4, 8).toLong
    //val end = Utils.solarDate2String(FireSeasonEnd).substring(4, 8).toLong
	//val MMdd = Utils.solarDate2String(date,"MMdd").toLong

    //val IGcum =  if (start==end) Double.NaN
				  //else if (MMdd==start) prevIG    //initialize cumulative sum
				  //else if (start<end) {if (MMdd<start||MMdd>=end) Double.NaN else prevIG}
				  //else {if (MMdd>=start&&MMdd<=1231 || MMdd>=101&&MMdd<end)  prevIG+IG(i) else  Double.NaN }
	//prevIG = IGcum(i)

    //val p = P.map(x=> if (x<=1)      1.0
                     //else if (x<=2)  0.8
                     //else if (x<=3)  0.6
                     //else if (x<=4)  0.4
                     //else if (x<=10) 0.2
                     //else            0.1 )
    //val B = for (i <- 0 until IGcum.length) yield p(i)*IGcum(i)
    
    //Double.NaN::(B.slice(0,B.length-1).toList)     //move serie to next day  
  //}

  ///**
   //* Computes the Ifa index (Portuguese index).
   //*
   //*
   //* @param  IG               ignition index
   //* @param  B                cumulative index 
   //* @param  U                wind speed [m/s]
   //* @return                  Ifa index value
   //*/
  //def IfaNew(dates:List[Long],IG:List[Double],B:List[Double],U:List[Double]):List[Double] = {

    //val IGclass = IG.map(x=> if (x<151)      1.0            //TODO verify if this is in the original publication
                             //else if (x<301) 2
                             //else if (x<451) 3
                             //else if (x<601) 4
                             //else if (x<751) 5
                             //else if (x>=751) 6
                             //else Double.NaN)
    //val Bclass = B.map(x=> if (x<301)      1.0            //TODO verify if this is in the original publication
                             //else if (x<1001) 2
                             //else if (x<2001) 3
                             //else if (x<4001) 4
                             //else if (x<6001) 5
                             //else if (x<8001) 6
                             //else if (x<10001) 7
                             //else if (x<12001) 8
                             //else if (x<15001) 9
                             //else if (x<18001) 10
                             //else if (x<21001) 11
                             //else if (x<24001) 12
                             //else if (x<27001) 13
                             //else if (x<31001) 14
                             //else if (x<35001) 15
                             //else if (x<39001) 16
                             //else if (x<43001) 17
                             //else if (x<47001) 18
                             //else if (x<51001) 19
                             //else if (x>=51001) 20
                             //else Double.NaN)


    //val Ifatemp = for (i<-0 until IG.length) yield (IGclass(i)+ Bclass(i)) //do not skip first value, since B refers already to previous day
    //val Ifa = Double.NaN::(Ifatemp.toList)
    
    //val Ukmh = U.map(_*3.6)
    //(for (i <- 0 until Ifa.length) yield (Ifa(i) + (if (Ukmh(i)<=10)    0
                                                  //else if (Ukmh(i)<=15) 1
                                                  //else if (Ukmh(i)<=20) 2
                                                  //else if (Ukmh(i)<=30) 3
                                                  //else if (Ukmh(i)<=40) 4
                                                  //else                  5))).toList   //correct for wind

  //}


//RISIKO
  
  object Risico {
    
    val A1 = 1.0
    val A2 = 0.555
    val A3 = 10.6
    val A4 = 0.5022
    val A5 = 0.0133
    val A6 = 0.000343
    val A7 = 0.00722

    val B1 = 3.0
    val B2 = 0.60
    val B3 = 0.1

    val GAMMA1 = 1.0
    val GAMMA2 = 0.01
    val GAMMA3 = 1.4

    val DELTA1 = 1.5
    val DELTA2 = 0.8483
    val DELTA3 = 16000.0
    val DELTA4 = 1.25
    val DELTA5 = 250000.0

    val LAMBDA = 2.0

    val QEPSIX2 = 8.0
    val Q = 2442.0
    val PHI = 6.667

    val R1 = 12.119
    val R2 = 20.77
    val R3 = 3.2
  }

    // contributo del vento alla velocità di propagazione
    def WindEffect(U:Double,  D:Double, slope:Double, aspect:Double) ={
            import Risico._
            //Contributo del vento
            val U_ = U * 60 * 60   //transform m/s in m/h
            val WS = (1.0  + 
                      DELTA1 * 
                      (DELTA2 + tanh((U_ / DELTA3) - DELTA4))) * 
                      (1.0 - (U_ / DELTA5))
            val eta = deg2rad(D -aspect)
            //Contributo della pendenza
            val N = max(1.0, 
                        1.0 + (deg2rad(slope) / (Pi / 2.0)) * (WS - 1.0)* exp(-pow((eta - Pi), 2.0) / QEPSIX2))

//            if (WS != Double.NaN) {
//                    W = WS / N
//            }
          WS / N  //W
    }


    def SlopeEffect(slope:Double) = {
            import Risico._
            
            1.0 + LAMBDA * (deg2rad(slope) / (Pi / 2.0)) 
    }

    //calcolo della velocità iniziale
    def V0(dffm:Double, snowCover:Double, v0:Double, d0:Double, d1:Double) = {

            if(snowCover <= 0 && !Null.is(d0))   
              v0 * exp(-1 * pow((dffm / 20.0f), 2.0f))
            else
              0.0
    }

    //velocità di propagazione
    def V(dffm:Double, W_Effect:Double, snowCover:Double, v0:Double, d0:Double, d1:Double, slope:Double)={ 
            V0(dffm, snowCover, v0, d0, d1) * W_Effect * SlopeEffect(slope) 
    }
//    //velocità di propagazione
//    def V(V0:Double, W_Effect:Double, S_Effect:Double)={ 
//            V0 * W_Effect * S_Effect 
//    }

    //calcolo LHV per la lettiera
    def LHVdff(hhv:Double, dffm:Double)={
            import Risico._
            hhv * (1.0 - (dffm / 100.0)) - Q * (dffm / 100.0)
    }
    
    //calcolo LHV per la vegetazione viva
    def LHVL1(humidity:Double, hhv:Double)={
            import Risico._
            if(Null.is(humidity))
              0.0
            else
              hhv * (1.0 - (humidity / 100.0)) - Q * (humidity / 100.0)
    }

    //Calcolo Intensità
    def FI(dffm:Double, V:Double, d0:Double, d1:Double, hhv:Double, humidity:Double)={ 
            val d1_ = if(Null.is(d1)) 0.0 else d1
            val d0_ = if(Null.is(d0)) 0.0 else d0

            V * (LHVdff(hhv, dffm) * d0_ + LHVL1(humidity, hhv) * d1_)/3600.0
    }
//    //Calcolo Intensità
//    def I(d0:Double, d1:Double, V:Double, LHVdff:Double, LHVl1:Double)={ 
//            val d1_ = if(d1 == Double.NaN) 0.0 else d1
//            val d0_ = if(d0 == Double.NaN) 0.0 else d0
//
//            V * (LHVdff * d0_ + LHVl1 * d1_)/3600.0
//    }


    /**
     * Get the new value for the dfmm when is raining (p>p*)
     * 
     * //modello per l'umidità della necromassa in caso di pioggia
     */

    def Dffm_Rain(prev:Double, P:Double, sat:Double)={
            import Risico._
            
            val delta_dffm = P * R1 * exp(-R2 / ((sat + 1.0) - prev))*(1.0 - exp(-R3 / P))
            
            min(prev + delta_dffm, sat)
    }

    /**
     * Get the new value for the dfmm when there is no rain (p<p*)
     * 
     * //modello senza pioggia. dT è lo step temporale in ore
     *
     */
    def Dffm_Dry(prev:Double, sat:Double, T:Double, U:Double, H:Double, T0:Double, dT_h:Double)={
            import Risico._
            
            val U_ = U * 60 * 60   //transform m/s in m/h
            
            val EMC  = A1 * pow(H, A2) + A3 * exp((H - 100.0)/10.0) + A4 * (30.0 - T)*(1.0 - exp(-A5 * H))
            val K1 = T0 / (1.0 + A6 * pow(T, B1) + A7 * pow(U_, B2))

            //dinamica di drying / wetting
            max(0.0, EMC + (prev - EMC) * exp(-dT_h/K1))
    }

    /**
     * Get the new value for the dfmm 
     * 
     * 
     *
     */
    def Dffm(prev:Double, 
                   T:Double, H:Double, U:Double, P:Double, 
                   sat:Double, T0:Double, dT_h:Double, RainThreshold:Double=0.0)={
            
      val prev_ = if (Null.is(prev)) sat/2 
                                     else prev

      if (P > RainThreshold)
        Dffm_Rain(prev_, P, sat)
      else
        Dffm_Dry(prev_, sat, T, U, H, T0, dT_h)
    }
    
  
    


    /*
            // parametri della vegetazione:
            //	v0: velocità iniziale
            //	d0: densità del combustibile nella lettiera
            //	d1: densità del combustibile vivo
            //	T0: tempo di risposta
            //	sat: umidità di saturazione
            //	hhv: higher heating value
            //	humidity: umidità del combustibile vivo

            // dffm è l'umidità del combustibile morto

            //modello per l'umidità della necromassa in caso di pioggia
            dffm = updateDffm_Rain(R, dffm, sat);

            //modello senza pioggia. dT è lo step temporale in ore
            dffm = updateDffm_Dry(dffm, sat, T, W, H, T0, dT);

            //calcolo della velocità iniziale
            V0 = getV0(v0, d0, d1, dffm, snowCover);

            // contributo del vento alla velocità di propagazione
            W_Effect = getWindEffect(WSpeed, WDir, slope, aspect); --------------------------------

            //velocità di propagazione
            V = getV(V0, W_Effect, slopeEffect);----------------------------------

            //calcolo LHV per la lettiera
            LHVdff = getLHVdff(hhv, dffm);
            //calcolo LHV per la vegetazione viva
            LHVl1 = getLHVL1(humidity, MSI, hhv);
            //Calcolo Intensità
            I = getI(d0, d1, V, RG, LHVdff, LHVl1); -----------------------------------------

            // Le variabili I, V, W_Effect e dffm vanno esportate
    */


  
  
    //IREPI
//    
//    object IREPIconst {
//      val soilWaterAtSaturation = 40.3
//      val soilWaterAtWilting = 34.2
//      val percolation = 0.15  
//      
//    }
//    
//  
//    def IREPI (prevP:Double, P:Double, PET:Double, formerEccessP:Double = 0, soilWater:Double = IREPIconst.soilWaterAtSaturation):(Double, Double, Double) = {
//      import IREPIconst._
//      
//      val HER = P + formerEccessP                                //acqua evapotraspirabile realmente presente nel terreno
//      val TP = PET - HER                                         //traspirazione potenziale
//      val TCR = 0.004 * TP * (soilWater - soilWaterAtWilting)    //traspirazione cuticolare reale
//      val TSP = TP - TCR                                         // traspirazione stomatica potanziale
//      val TSR = TSP * (1/ Pi * atan((soilWater - 32.43 - 14.068 * TSP) * 0.174 / TSP) + 0.5)   // traspirazione stomatica reale
//      val RET = if (HER >= PET) PET
//                else P + TCR + TSR 
//      
//      val irepi = (PET - RET)/ PET * 100
//         
//      //IREPI           soilWater                                     formerEccessP
//        
//      if (P >= PET || HER < PET)       //Bovio 1984 says >,  Marcozzi 1994 and COnedera 1996 say >=
//        (irepi, soilWater + formerEccessP - percolation * P,           P - RET)
//      else
//        (irepi, soilWater + formerEccessP - percolation * P + P - RET, 0     )
//       
//    }
//
//  
//    def IREPI (PET:Double, RET:Double):Double = {
//      (PET - RET)/ PET * 100
//    }
//    
//    def SoilWater_IREPI (prev:Double = IREPIconst.soilWaterAtSaturation, 
//                         prevprevEccessP:Double, prevRET:Double, prevP:Double = 0, prevPET:Double):Double = {
//      import IREPIconst._
//        
//      if (prevP >= prevPET || (prevP + prevprevEccessP) < prevPET)       //Bovio 1984 says >,  Marcozzi 1994 and COnedera 1996 say >=
//        prev + prevprevEccessP - percolation * prevP
//      else
//        prev + prevprevEccessP - percolation * prevP + prevP - prevRET
//       
//    }
//    
//    def EccessP_IREPI(prev:Double, P:Double, PET:Double, RET:Double):Double={
//      if (P > PET || (P + prev) < PET)
//                P - RET
//      else
//                0.0
//    }
//
//  //real ET according to IREPI
//    def RET_IREPI (prevEccessP:Double, P:Double, PET:Double, soilWater:Double = IREPIconst.soilWaterAtSaturation):Double = {
//      import IREPIconst._
//      
//      val HER = P + prevEccessP                                              //acqua evapotraspirabile realmente presente nel terreno
//                                                                             //(prevP - prev)  is P in eccess at former day
//      val RET = if (HER >= PET) 
//                  PET
//                else {
//                  val TP = PET - HER                                         //traspirazione potenziale
//                  val TCR = 0.004 * TP * (soilWater - soilWaterAtWilting)    //traspirazione cuticolare reale
//                  val TSP = TP - TCR                                         // traspirazione stomatica potanziale
//                  val TSR = TSP * (1/ Pi * atan((soilWater - 32.43 - 14.068 * TSP) * 0.174 / TSP) + 0.5)   // traspirazione stomatica reale
//                  P + TCR + TSR 
//                }
//      
//      RET    
//    }
//  
  
  
}
