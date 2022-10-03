package ch.wsl.fireindices.functions
import ch.wsl.fireindices.metadata.Null
import java.io._
import java.sql._
import scala.collection.mutable.ListBuffer
import math._

/**
* Contains functions to apply the base functions with data series (Lists).
*
**/
object ListFunctions {

  
  /**
   * Applies a given function (funct) on input list of Data
   *
   * @return     list with results of the function
   */
  def applyFunction[A,B,To] (funct: (A,B)=>(To),a:List[A],b:List[B]):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head)::applyFunction(funct,a.tail,b.tail)
  }
  
  /**
   * Applies a given function (funct) on input list of Data
   *
   * @return   list with results of the function
   */
  def applyFunction[A,B,C,To] (funct: (A,B,C)=>(To),a:List[A],b:List[B],c:List[C]):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c.head)::applyFunction(funct,a.tail,b.tail,c.tail)
  }

  /**
   * Applies a given function (funct) on input list of Data
   *
   * @return     list with results of the function
   */
  def applyFunction[A,B,C,D,To] (funct: (A,B,C,D)=>(To),a:List[A],b:List[B],c:List[C],d:List[D]):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c.head,d.head)::applyFunction(funct,a.tail,b.tail,c.tail,d.tail)
  }
  /**
   * Applies a given function (funct) on input list of Data
   *
   * @return     list with results of the function
   */
  def applyFunction[A,B,C,D,E,To] (funct: (A,B,C,D,E)=>(To),a:List[A],b:List[B],c:List[C],d:List[D],e:List[E]):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c.head,d.head,e.head)::applyFunction(funct,a.tail,b.tail,c.tail,d.tail,e.tail)
  }
  /**
   * Applies a given function (funct) on input list of Data
   *
   * @return     list with results of the function
   */
  def applyFunction[A,B,C,D,E,F,To] (funct: (A,B,C,D,E,F)=>(To),a:List[A],b:List[B],c:List[C],d:List[D],e:List[E],f:List[F]):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c.head,d.head,e.head,f.head)::applyFunction(funct,a.tail,b.tail,c.tail,d.tail,e.tail,f.tail)
  }
  /**
   * Applies a given function (funct) on input list of Data
   *
   * @return     list with results of the function
   */
  def applyFunction[A,B,C,D,E,F,G,To] (funct: (A,B,C,D,E,F,G)=>(To),a:List[A],b:List[B],c:List[C],d:List[D],e:List[E],f:List[F],g:List[G]):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c.head,d.head,e.head,f.head,g.head)::applyFunction(funct,a.tail,b.tail,c.tail,d.tail,e.tail,f.tail,g.tail)
  }



   /**
   * Applies a given function (funct) on input list of Data keeping the previous result and previous days input data serie
   *
   * @return    list with results of the function
   */
  def applyFunction_k[A,B,To] (funct: (A,B)=>(To), a:List[A], b:B):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head, b)::applyFunction_k(funct, a.tail, b)
  }

   /**
   * Applies a given function (funct) on input list of Data keeping the previous result and previous days input data serie
   *
   * @return    list with results of the function
   */
  def applyFunction_k[A,B,C,To] (funct: (A,B,C)=>(To),a:List[A],b:List[B],c:C):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c)::applyFunction_k(funct,a.tail,b.tail,c)
  }
   /**
   * Applies a given function (funct) on input list of Data keeping the previous result and previous days input data serie
   *
   * @return    list with results of the function
   */
  def applyFunction_k[A,B,C,D,To] (funct: (A,B,C,D)=>(To),a:List[A],b:List[B],c:List[C],d:D):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c.head,d)::applyFunction_k(funct,a.tail,b.tail,c.tail,d)
  }
  /**
   * Applies a given function (funct) on input list of Data
   *
   * @return     list with results of the function
   */
  def applyFunction_k[A,B,C,D,E,To] (funct: (A,B,C,D,E)=>(To),a:List[A],b:List[B],c:List[C],d:List[D],e:E):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c.head,d.head,e)::applyFunction_k(funct,a.tail,b.tail,c.tail,d.tail,e)
  }
  /**
   * Applies a given function (funct) on input list of Data keeping the previous result and previous days input data serie
   *
   * @return    list with results of the function
   */
  def applyFunction_kk[A,B,C,D,To] (funct: (A,B,C,D)=>(To),a:List[A],b:List[B],c:C,d:D):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c,d)::applyFunction_kk(funct,a.tail,b.tail,c,d)
  }
  def applyFunction_kk[A,B,C,D,E,To] (funct: (A,B,C,D,E)=>(To),a:List[A],b:List[B],c:List[C],d:D,e:E):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c.head,d,e)::applyFunction_kk(funct,a.tail,b.tail,c.tail,d,e)
  }
  def applyFunction_kk[A,B,C,D,E,G,H,To] (funct: (A,B,C,D,E,G,H)=>(To),a:List[A],b:List[B],c:List[C],d:List[D],e:List[E],g:G,h:H):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c.head,d.head,e.head,g,h)::applyFunction_kk(funct,a.tail,b.tail,c.tail,d.tail,e.tail,g,h)
  }
//  /**
//   * Applies a given function (funct) on input list of Data
//   *
//   * @return     list with results of the function
//   */
//  def applyFunctionRN[A,B,C,D,E,F,To] (funct: (A,B,C,D,E)=>(To),a:List[A],b:List[B],c:List[C],d:List[D],e:List[E]):List[To] = {
//    if (a.isEmpty) Nil
//    else funct(a.head,b.head,c.head,d.head,e.head)::applyFunctionRN(funct,a.tail,b.tail,c.tail,d.tail,e.tail)
//  }
  
  /**
   * Applies a given function (funct) on input list of Data
   *
   * @return     list with results of the function
   */
  def applyFunction_kkk[A,B,C,D,E,G,To] (funct: (A,B,C,D,E,G)=>(To),a:List[A],b:List[B],c:List[C],d:D,e:E,g:G):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c.head,d,e,g)::applyFunction_kkk(funct,a.tail,b.tail,c.tail,d,e,g)
  }  
  /**
   * Applies a given function (funct) on input list of Data
   *
   * @return     list with results of the function
   */
  def applyFunction_kkkk[A,B,C,D,E,G,To] (funct: (A,B,C,D,E,G)=>(To),a:List[A],b:List[B],c:C,d:D,e:E,g:G):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c,d,e,g)::applyFunction_kkkk(funct,a.tail,b.tail,c,d,e,g)
  }
  /**
   * Applies a given function (funct) on input list of Data
   *
   * @return     list with results of the function
   */
  def applyFunction_kkkk[A,B,C,D,E,G,H,To] (funct: (A,B,C,D,E,G,H)=>(To),a:List[A],b:List[B],c:List[C],d:D,e:E,g:G,h:H):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c.head,d,e,g,h)::applyFunction_kkkk(funct,a.tail,b.tail,c.tail,d,e,g,h)
  }
  /**
   * Applies a given function (funct) on input list of Data
   *
   * @return     list with results of the function
   */
  def applyFunction_kkkk[A,B,C,D,E,G,H,I,To] (funct: (A,B,C,D,E,G,H,I)=>(To),a:List[A],b:List[B],c:List[C],d:List[D],e:E,g:G,h:H,i:I):List[To] = {
    if (a.isEmpty) Nil
    else funct(a.head,b.head,c.head,d.head,e,g,h,i)::applyFunction_kkkk(funct,a.tail,b.tail,c.tail,d.tail,e,g,h,i)
  }






  /**
   * Applies a given function (funct) on input list of Data keeping the previous result
   *
   * @return    list with results of the function
   */
  def applyFunctionPrev[A,To] (funct: (To,A)=>(To),prev:To,a:List[A]):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,a.head)::applyFunctionPrev(funct,funct(prev,a.head),a.tail)
  }
  
  /**
   * Applies a given function (funct) on input list of Data keeping the previous result
   *
   * @return    list with results of the function
   */
  def applyFunctionPrev[A,B,To] (funct: (To,A,B)=>(To),prev:To,a:List[A],b:List[B]):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,a.head,b.head)::applyFunctionPrev(funct,funct(prev,a.head,b.head),a.tail,b.tail)
  }
  
  /**
   * Applies a given function (funct) on input list of Data keeping the previous result
   *
   * @return    list with results of the function
   */
  def applyFunctionPrev[A,B,C,To] (funct: (To,A,B,C)=>(To),prev:To,a:List[A],b:List[B],c:List[C]):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,a.head,b.head,c.head)::applyFunctionPrev(funct,funct(prev,a.head,b.head,c.head),a.tail,b.tail,c.tail)
  }
  
  /**
   * Applies a given function (funct) on input list of Data keeping the previous result
   *
   * @return    list with results of the function
   */
  def applyFunctionPrev[A,B,C,D,To] (funct: (To,A,B,C,D)=>(To),prev:To,a:List[A],b:List[B],c:List[C],d:List[D]):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,a.head,b.head,c.head,d.head)::applyFunctionPrev(funct,funct(prev,a.head,b.head,c.head,d.head),a.tail,b.tail,c.tail,d.tail)
  }




  /**
   * Applies a given function (funct) on input list of Data keeping the previous result
   *
   * @return    list with results of the function
   */
  def applyFunctionPrev_k[A,B,C,D,E,To] (funct: (To,A,B,C,D,E)=>(To),prev:To,a:List[A],b:List[B],c:List[C],d:List[D],e:E):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,a.head,b.head,c.head,d.head,e)::applyFunctionPrev_k(funct,funct(prev,a.head,b.head,c.head,d.head,e),a.tail,b.tail,c.tail,d.tail,e)
  }
  
  /**
   * Applies a given function (funct) on input list of Data keeping the previous result
   *
   * @return    list with results of the function
   */
  def applyFunctionPrev_kkkk[A,B,C,D,E,F,G,H,To] (funct: (To,A,B,C,D,E,F,G,H)=>(To),prev:To,a:List[A],b:List[B],c:List[C],d:List[D],e:E,f:F,g:G,h:H):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,a.head,b.head,c.head,d.head,e,f,g,h)::applyFunctionPrev_kkkk(funct,funct(prev,a.head,b.head,c.head,d.head,e,f,g,h),a.tail,b.tail,c.tail,d.tail,e,f,g,h)
  }
  /**
   * Applies a given function (funct) on input list of Data keeping the previous result
   *
   * @return    list with results of the function
   */
  def applyFunctionPrev_kkkkk[A,B,C,D,E,F,G,H,I,To] (funct: (To,A,B,C,D,E,F,G,H,I)=>(To),prev:To,a:List[A],b:List[B],c:List[C],d:List[D],e:E,f:F,g:G,h:H,i:I):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,a.head,b.head,c.head,d.head,e,f,g,h,i)::applyFunctionPrev_kkkkk(funct,funct(prev,a.head,b.head,c.head,d.head,e,f,g,h,i),a.tail,b.tail,c.tail,d.tail,e,f,g,h,i)
  }

//  /**
//   * Applies a given function (funct) on input list of Data keeping the previous result
//   *
//   * @return    list with results of the function
//   */
//  def applyFunctionPrev_k[A,B,C,D,E,F,To] (funct: (To,A,B,C,D,E,F)=>(To),prev:To,a:List[A],b:List[B],c:List[C],d:List[D],e:E,f:F):List[To] = {
//    if (a.isEmpty) Nil
//    else funct(prev,a.head,b.head,c.head,d.head,e,f)::applyFunctionPrev_k(funct,funct(prev,a.head,b.head,c.head,d.head,e,f),a.tail,b.tail,c.tail,d.tail,e,f)
//  }
  
  /**
   * Applies a given function (funct) on input list of Data keeping the previous result
   *
   * @return    list with results of the function
   */
  def applyFunctionPrev_k[A,B,C,D,E,F,To] (funct: (To,A,B,C,D,E,F)=>(To),prev:To,a:List[A],b:List[B],c:List[C],d:List[D],e:List[E],f:F):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,a.head,b.head,c.head,d.head,e.head,f)::applyFunctionPrev_k(funct,funct(prev,a.head,b.head,c.head,d.head,e.head,f),a.tail,b.tail,c.tail,d.tail,e.tail,f)
  }
  
//  /**
//   * Applies a given function (funct) on input list of Data keeping the previous result
//   *
//   * @return    list with results of the function
//   */
//  def applyFunctionPrev[A,B,C,D,E,F,G,To] (funct: (To,A,B,C,D,G,F)=>(To),prev:To,a:List[A],b:List[B],c:List[C],d:List[D],e:E,f:F):List[To] = {
//    if (e.isInstanceOf[Double]) applyFunctionPrev1(funct,prev,a,b,c,d,e.asInstanceOf[G],f)
//    else  applyFunctionPrev2(funct,prev,a,b,c,d,e.asInstanceOf[List[G]],f)
//  }
  
  /**
   * Applies a given function (funct) on input list of Data keeping the previous result
   *
   * @return    list with results of the function
   */
  def applyFunctionPrev_kk[A,B,C,D,E,F,To] (funct: (To,A,B,C,D,E,F)=>(To),prev:To,a:List[A],b:List[B],c:List[C],d:List[D],e:E,f:F):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,a.head,b.head,c.head,d.head,e,f)::applyFunctionPrev_kk(funct,funct(prev,a.head,b.head,c.head,d.head,e,f),a.tail,b.tail,c.tail,d.tail,e,f)
  }
  
//  /**
//   * Applies a given function (funct) on input list of Data keeping the previous result
//   *
//   * @return    list with results of the function
//   */
//  def applyFunctionPrev_k[A,B,C,D,E,F,To] (funct: (To,A,B,C,D,E,F)=>(To),prev:To,a:List[A],b:List[B],c:List[C],d:List[D],e:List[E],f:F):List[To] = {
//    if (a.isEmpty) Nil
//    else funct(prev,a.head,b.head,c.head,d.head,e.head,f)::applyFunctionPrev_k(funct,funct(prev,a.head,b.head,c.head,d.head,e.head,f),a.tail,b.tail,c.tail,d.tail,e.tail,f)
//  }
 
  
  /**
   * Applies the DMC index funtion on input list of Data keeping the previous result
   *
   * @return    list with results of the function
   */
  def applyFunctionPrev_kk[A,B,C,D,E,F,G,To] (funct: (To,A,B,C,D,E,F,G)=>(To),prev:To,a:List[A],b:List[B],c:List[C],d:List[D],e:List[E],f:F,g:G):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,a.head,b.head,c.head,d.head,e.head,f,g)::applyFunctionPrev_kk(funct,funct(prev,a.head,b.head,c.head,d.head,e.head,f,g),a.tail,b.tail,c.tail,d.tail,e.tail,f,g)
  }
  
//  /**
//   * Applies the DC index funtion on input list of Data keeping the previous result
//   *
//   * @return    list with results of the function
//   */
//  def applyFunctionPrev_k[A,B,C,D,E,To] (funct: (To,A,B,C,D,E)=>(To),prev:To,a:List[A],b:List[B],c:List[C],d:List[D],e:E):List[To] = {
//    if (a.isEmpty) Nil
//    else funct(prev,a.head,b.head,c.head,d.head,e)::applyFunctionPrev_k(funct,funct(prev,a.head,b.head,c.head,d.head,e),a.tail,b.tail,c.tail,d.tail,e)
//  }
  
  /**
   * Applies a given function (funct) on input list of Data keeping the previous result
   *
   * @return    list with results of the function
   */
  def applyFunctionPrev[A,B,C,D,E,F,G,H,To] (funct: (To,A,B,C,D,E,E,E,E,F,F,F,G,H)=>(To),prev:To,a:List[A],b:List[B],c:List[C],d:List[D],e:List[E],e1:E,e2:E,e3:E,f:List[F],f1:F,f2:F,g:G,h:H):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,a.head,b.head,c.head,d.head,e.head,e1,e2,e3,f.head,f1,f2,g,h)::applyFunctionPrev(funct,funct(prev,a.head,b.head,c.head,d.head,e.head,e1,e2,e3,f.head,f1,f2,g,h),a.tail,b.tail,c.tail,d.tail,e.tail,e.head,e1,e2,f.tail,f.head,f1,g,h)
  }
  
  /**
   * Applies a given function (funct) on input list of Data keeping the previous result
   *
   * @return    list with results of the function
   */
  def applyFunctionPrev[A,C,D,E,F,G,H,To] (funct: (To,A,C,D,E,E,E,E,F,F,F,G,H)=>(To),prev:To,a:List[A],c:List[C],d:List[D],e:List[E],e1:E,e2:E,e3:E,f:List[F],f1:F,f2:F,g:G,h:H):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,a.head,c.head,d.head,e.head,e1,e2,e3,f.head,f1,f2,g,h)::applyFunctionPrev(funct,funct(prev,a.head,c.head,d.head,e.head,e1,e2,e3,f.head,f1,f2,g,h),a.tail,c.tail,d.tail,e.tail,e.head,e1,e2,f.tail,f.head,f1,g,h)
  }
  
  /**
   * Applies a given function (funct) on input list of Data keeping the previous result
   *
   * @return    list with results of the function
   */
  def applyFunctionPrevThreshold[Thr,A,To] (funct: (To,Thr,A)=>(To),prev:To,threshold:Thr,a:List[A]):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,threshold,a.head)::applyFunctionPrevThreshold(funct,funct(prev,threshold,a.head),threshold,a.tail)
  }
  
  /**
   * Applies a given function (funct) on input list of Data keeping the previous result and a given threshold
   *
   * @return    list with results of the function
   */
  def applyFunctionPrevThreshold[Thr,Num,A,B,C,D,To] (funct: (To,Thr,Num,A,B,C,D)=>(To),prev:To,threshold:Thr,num:Num,a:List[A],b:List[B],c:List[C],d:List[D]):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,threshold,num,a.head,b.head,c.head,d.head)::applyFunctionPrevThreshold(funct,funct(prev,threshold,num,a.head,b.head,c.head,d.head),threshold,num,a.tail,b.tail,c.tail,d.tail)
  }
  /**
   * Applies a given function (funct) on input list of Data keeping the previous result and a given threshold
   *
   * @return    list with results of the function
   */
  def applyFunctionPrevThreshold[Thr,A,B,C,To] (funct: (To,Thr,A,B,C)=>(To),prev:To,threshold:Thr,a:List[A],b:List[B],c:List[C]):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,threshold,a.head,b.head,c.head)::applyFunctionPrevThreshold(funct,funct(prev,threshold,a.head,b.head,c.head),threshold,a.tail,b.tail,c.tail)
  }
   /**
   * Applies a given function (funct) on input list of Data keeping the previous result and previous days input data serie
   *
   * @return    list with results of the function
   */
  def applyFunctionPrevLastRainSum[A,To] (funct: (To,A,A)=>(To),prev:To,a:List[A],a_1:A):List[To] = {
    if (a.isEmpty) Nil
    else funct(prev,a.head,a_1)::applyFunctionPrevLastRainSum(funct,funct(prev,a.head,a_1),a.tail,a.head)
  }
//   /**
//   * Applies a given function (funct) on input list of Data keeping the previous result and previous days input data serie
//   *
//   * @return    list with results of the function
//   */
//  def applyFunctionPrevLastRainSumThreshold[A,To] (funct: (To,A,A,A)=>(To),prev:To,a:List[A],a_1:A,thr:A):List[To] = {
//    if (a.isEmpty) Nil
//    else funct(prev,a.head,a_1,thr)::applyFunctionPrevLastRainSumThreshold(funct,funct(prev,a.head,a_1,thr),a.tail,a.head,thr)
//  }
//  
  /**
   * Computes amount (sum) of the last significant rainfall event.
   * The rainfall event is defined as consecutive days with rainfall above a threshold
   * (2 mm inFinkele 2006).
   *
   * To be used for the calculation of the DF (Drought Factor) according to Griffith and Finkele.
   *
   *
   * @param  prev                   previous day sum of consecutive rainfall [°C]
   * @param  P                      List of Rainfall amount [mm]
   * @param  rainfallEventThreshold threshold daily rain to consider a rain evnet
   * @param  pastDaysToConsider     number of past days to consider
   * @return                        amount of last significant rainfall event [mm]
   */
  def lastRainSum_withThreshold_20days(P:List[Double], rainfallEventThreshold:Double=2.00,pastDaysToConsider:Int=20):(List[Double],List[Double]) = {
//    if (P.map(Null.is(_)).reduceRight(_||_)) return (List.fill(P.length)(Double.NaN), List.fill(P.length)(Double.NaN)) // wrong
    
    val out:ListBuffer[Double]= new ListBuffer//(P.length)
    val ageRainEvent:ListBuffer[Double]=new ListBuffer//(P.length)
    for (i<- 0 until pastDaysToConsider-1){  //create first null values
      out+=Double.NaN
      ageRainEvent+=Double.NaN
    }
    for (i <- 0 until P.length){
      val p=P.view.slice(math.max(0,i-pastDaysToConsider+1),i+1)  //include current day in past days
      
      var age=0.0
      var maxRain=0.0
      var rainSum=0.0
      var prevP=0.0
          
      if (p.map(Null.is(_)).reduceRight(_||_)){
        rainSum = Double.NaN
        age = Double.NaN
      } else {
        
      
          for (j<- 0 until p.size){
            val pp:Double = {p.slice(j, j).headOption match{
              case None =>  null
              case Some(x) => x
            }}.asInstanceOf[Double]
            if (j>0)
              prevP=p.slice(j-1, j).head

            if (prevP<rainfallEventThreshold && pp>=rainfallEventThreshold) {   //new rainfall event
              maxRain = pp
              age=j+1
              rainSum=pp
            } else if (pp>=rainfallEventThreshold) {  //same rainfall
              if (pp>=maxRain) maxRain=pp
              age = j+1
              rainSum += pp
            } 
          }
        
      }
      if (i>=pastDaysToConsider-1){    //add only for the not in the first period
        out+=rainSum
        ageRainEvent+=p.size-age
      }
      
    }   
    (out.toList, ageRainEvent.toList)
  }
 
  
    
    object IREPIconst {
      val soilWaterAtSaturation = 40.3  //cm
      val soilWaterAtWilting = 34.2     //cm
      val percolationCoef = 0.15            
      
    }
    
    def IREPI (P:List[Double], PET:List[Double], soilWaterStart:Double = IREPIconst.soilWaterAtSaturation, PweekThreshold:Double, WeeklyRain:List[Double]):List[Double] = {
      import Utils._
      import IREPIconst._

      var PUprev = 0.0
      var HER = 0.0
      var TP = 0.0
      var TCR = 0.0
      var TSP = 0.0
      var TSR = 0.0
      var RET = 0.0
      var soilWater = Double.NaN
      val irepi:ListBuffer[Double]= new ListBuffer//(P.length)
      
    
      for (i<- 0 until 1){  //create first null values
        irepi += Double.NaN
      }   
      for (i <- 1 until P.length){
        val p = P(i)/10.0      //to transform mm/d to cm/d
        val pet = PET(i)/10.0  //to transform mm/d to cm/d
        
        if (soilWater.isNaN & WeeklyRain(i)>=PweekThreshold) soilWater=soilWaterStart

        HER = p + PUprev                                    //acqua evapotraspirabile realmente presente nel terreno
        TP = pet - HER                                      //traspirazione potenziale
        TCR = 0.004 * TP * (soilWater - soilWaterAtWilting)    //traspirazione cuticolare reale
        TSP = TP - TCR                                         // traspirazione stomatica potanziale
        TSR = TSP * (1/ Pi * atan((soilWater - 32.43 - 14.068 * TSP) * 0.174 / TSP) + 0.5)   //traspirazione stomatica reale       
        
        RET = if (HER >= pet)                  //evapotraspirazione reale
                    pet
              else 
                    p + TCR + TSR 

//        println("PET: "+pet+"  PUprev: "+PUprev+"  soilWater: "+soilWater+"  RET: "+RET+"  HER: "+HER+"  TP: "+TP+"  TCR: "+TCR+"  TSR: "+TSR)
        irepi += (pet - RET)/ pet * 100
        
        if (p >= pet){      
          soilWater = min(soilWaterAtSaturation, soilWater + PUprev - percolationCoef * p)
          PUprev = max(0.0, p - RET)
        }else{
          soilWater = soilWater + PUprev - percolationCoef * p + p - RET
          PUprev = 0.0
        }
        soilWater =crop(soilWater,soilWaterAtWilting, soilWaterAtSaturation)
        
      }
//      for (i <- 0 until P.length) irepi+=1.0 //test 
      irepi.toList
   
    }
//    
//    def IREPIcont (P:List[Double], PET:List[Double], 
//                   soilWaterStart:Double = IREPIconst.soilWaterAtSaturation,
//                   PUprev:Double = 0.0):List[Double] = {
//      import IREPIconst._
//
//      var HER = 0.0
//      var TP = 0.0
//      var TCR = 0.0
//      var TSP = 0.0
//      var TSR = 0.0
//      var RET = 0.0
//      var soilWater = soilWaterStart
//      val irepi:ListBuffer[Double]= new ListBuffer//(P.length)
//      
//      for (i<- 0 until 2-1){  //create first null values
//        irepi += Double.NaN
//      }   
//      for (i <- 2 until P.length){
//        HER = P(i) + PUprev                                    //acqua evapotraspirabile realmente presente nel terreno
//        TP = PET(i) - HER                                      //traspirazione potenziale
//        TCR = 0.004 * TP * (soilWater - soilWaterAtWilting)    //traspirazione cuticolare reale
//        TSP = TP - TCR                                         // traspirazione stomatica potanziale
//        TSR = TSP * (1/ Pi * atan((soilWater - 32.43 - 14.068 * TSP) * 0.174 / TSP) + 0.5)   //traspirazione stomatica reale
//        RET = if (HER >= PET(i)) 
//                    PET(i)
//              else 
//                    P(i) + TCR + TSR 
//
//        irepi += (PET(i) - RET)/ PET(i) * 100
//        
//        if (P(i) >= PET(i) || HER < PET(i)){      
//          soilWater = soilWater + PUprev - percolation * P(i)
//          PUprev = P(i) - RET
//        }else{
//          soilWater = soilWater + PUprev - percolation * P(i) + P(i) - RET
//          PUprev = 0.0
//        }
//        
//      }
//      irepi.toList
//   
//    }
//    
  
  
  /**
   * for correction of wrong input with last correct data
   */
  private def addPrevElement(start:Double,NbNull:Double):List[Double]={
    var LinList:ListBuffer[Double] = new ListBuffer
    for(i <- 1 to NbNull.toInt)  LinList += start
    return LinList.toList
  }
  
  /**
   * for correction of wrong input with linearization of correct data
   */
  private def addLinElement(start:Double,NbNull:Double,stop:Double):List[Double]={
    var LinList:ListBuffer[Double] = new ListBuffer
    for(i <- 1 to NbNull.toInt)  LinList += (start + (stop-start)/(NbNull+1)*i)
    return LinList.toList
  }
  
  /**
   * for correction of wrong input with linearization of correct data
   */
  private def addLinElement2(start:Double,inc:Double,stop:Double):List[Double]={
    val next = start+inc
    if (math.abs(stop-next)<math.abs(inc/1000000))Nil
    else next::addLinElement2(next,inc,stop)
  }
  
  /**
   * for correction of void inputs with last correct data
   */
  def gapFillwithPrev (MaxNullGap: Double, list:List[Double]):List[Double] = {
    if (list.isEmpty) Nil
    else if (list.head.isNaN || list.tail.isEmpty || !list.tail.head.isNaN) list.head::gapFillwithPrev(MaxNullGap,list.tail)
    else{
       val t = list.tail.span(_.isNaN)
       if (t._1.length>MaxNullGap) list.head::t._1:::t._2
       else{
         val l=addPrevElement(list.head,t._1.length)
         list.head::l:::gapFillwithPrev(MaxNullGap,t._2)
       }
    }
  }
  
  /**
   * for correction of void inputs with linearization of correct data
   */
  def gapLinearization (MaxNullGap: Double, list:List[Double]):List[Double] = {
    if (list.isEmpty) Nil
    else if (list.head.isNaN || list.tail.isEmpty || !list.tail.head.isNaN) list.head::gapLinearization(MaxNullGap,list.tail)
    else{
       val t = list.tail.span(_.isNaN)
       if (t._1.length>MaxNullGap) list.head::t._1:::t._2
       else{
         val l=addLinElement2(list.head,(t._2.head-list.head)/(t._1.length+1),t._2.head)   //NonFunzionante
         list.head::l:::gapLinearization(MaxNullGap,t._2)
       }
    }
  }
  
  /**
   * returns a list with the cumulative sums of a list of data
   *
   * @param   list   List[Double] > the list with datas
   * @return         the cumulative sum list
   */
  def cumSum(list:List[Double],start:Double=0.0):List[Double] = {
      if (list.length==0) Nil
      else (start+list.head)::cumSum(list.tail,start+list.head)
  }

  /**
   * returns the consecutive sum of a list of data for a given period
   *
   * @param   period the period for consecutive sum
   * @param   list   List[Double] > the list with datas
   * @return         the sum
   */
  def runningSum(period:Int,list:List[Double]):List[Double] = {
    if (list.length<period) getNaNList(list.length)//Nil
    else {
      return list.take(period).sum::runningSum(period,list.tail)
    }
  }
  
  /**
   * returns the consecutive sum of a list of data for a given period and from a given position
   *
   * @param   period   the period for consecutive sum
   * @param   position the position of the result (0-based, eg. 0 means at first element = runningSum)
   * @param   list     List[Double] > the list with datas
   * @return           the sum
   */
  def runningSumPos(period:Int,position:Int,list:List[Double]):List[Double] = {
    val rs = runningSum(period,list)
    if (position>=0)
            //getNaNList(position):::(rs.take(rs.length - Math.max(0, position-period+1)))
            getNaNList(position):::(rs.take(rs.length - position))
    else 
            rs.slice(-position,rs.length):::getNaNList(-position)
  }
  
  /**
   * return the consecutive sum of a list of data for period of seven days
   *
   * @param   list     List[Double] > the list with datas
   * @return           the sum
   */
  def weeklySum(list:List[Double]):List[Double] = {
    runningSumPos(7,6,list)
  }
  
  /**
   * returns a list of Double.NaN of given length
   *
   * @param   number number of NaN in the list
   * @return         List[Double] > the NaN list
   */
  def getNaNList(number: Int):List[Double]={
    if (number>0) Double.NaN::getNaNList(number-1)
    else Nil
  }
}
