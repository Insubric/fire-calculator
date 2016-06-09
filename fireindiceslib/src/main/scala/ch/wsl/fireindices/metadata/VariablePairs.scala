

package ch.wsl.fireindices.metadata

import scala.collection.mutable.LinkedHashMap

class VariablePairs/*(key:Variable,value:Data)*/ extends LinkedHashMap[Variable,Variable]{
    
    ///**
     //* return a  Variable corresponding to the given key-variable from the VariablePairMap object
     //*
     //* @param   key    Variable > the given key
     //* @return         Variable
     //*/
    //def  get[A](key: Variable): Variable = {
      //val e = findEntry(key.asInstanceOf[Variable])
      //e.value.asInstanceOf[Variable]
    //}
   

//    override def  get[A<:T](key: Variable): Option[Data[A]] = {
//      val e = findEntry(key.asInstanceOf[Variable])
//      if (e == null) None
//      else Some(e.value.asInstanceOf[Data[A]])
//    }
//    def  getS[A<:T](key: Variable): Option[DataSerie[A]] = {
//      val e = findEntry(key.asInstanceOf[Variable])
//      if (e == null) None
//      else Some(e.value.asInstanceOf[DataSerie[A]])
//    }
//    def  getP[A<:T](key: Variable): Option[Parameter[A]] = {
//      val e = findEntry(key.asInstanceOf[Variable])
//      if (e == null) None
//      else Some(e.value.asInstanceOf[Parameter[A]])
//    }
   
}
