

package ch.wsl.fireindices.metadata

import scala.collection.mutable.LinkedHashMap



class DataMapper(val dc: DataCollection) extends VariablePairs {
   
    def this(dc: DataCollection, mappings: VariablePairs)={
	  this(dc)
	  this ++= mappings
    }
    def this(dc: DataCollection, mappings: LinkedHashMap[Variable,Variable])={
	  this(dc)
	  this ++= mappings
    }
   
    
    /**
     * return a specific Data from VariableDataMap
     *
     * @param   key    Variable > the serched variable
     * @return         Data
     */
    def data(key: Variable): Data = {
      val e = findEntry(key.asInstanceOf[Variable])
      dc(e.value).asInstanceOf[Data]
    }
    
    
    
    /**
     * return all data relative to the keys
     *
     * @return         DataCollection
     */
    def newDc: DataCollection ={
		new DataCollection(dc.filterKeys(super.valuesIterator.contains(_)).values)
    }
    
    
    /**
     * return a specific variable DataSerie from VariableDataMap
     *
     * @param   key    Variable > the serched DataSerie
     * @return         DataSerie
     */
    def  ds(key: Variable): DataSerie = {
      data(key).asInstanceOf[DataSerie]
    }
    
    /**
     * return a specific variable DataSerie from VariableDataMap, with only the last data
     *
     * @param   key    Variable > the serched DataSerie
     * @param   nr     Int > the number of last values to be included in DataSerie
     * @return         DataSerie
     */
    def  ds(key: Variable, nr:Int): DataSerie = {
      ds(key).lastDs(nr).asInstanceOf[DataSerie]
    }
  
    /**
     * return a specific varaible Parameter from VariableDataMap
     *
     * @param   key    Variable > the serched parameter
     * @return         Parameter
     */
    def  par(key: Variable): Parameter = {
      dc(key).asInstanceOf[Parameter]
    }
    
    /**
     * return a DataMapper containing all DataSeries only
     *
     * @return         VariableDataMap
     */
    def dssDM: DataMapper ={
      new DataMapper(newDc, super.clone.filter(_._1.isInstanceOf[Serie]))
    }
    
    /**
     * return a DataMapper containing all Parameters only
     *
     * @return         VariableDataMap
     */
    def parsDM: DataMapper ={
      new DataMapper(newDc, super.clone.filter(!_._1.isInstanceOf[Serie]))
    }  
    
    def print={
//      for (k <- this.keys){
//        println(k.abbr+"\t\t\t"+this(k).abbr)
//      }
      this.keys.map(k => k.abbr+"\t\t\t"+this(k).abbr).mkString("\n")
    }


    


//    override def  get[A<:T](key: Variable): Option[Data] = {
//      val e = findEntry(key.asInstanceOf[Variable])
//      if (e == null) None
//      else Some(e.value.asInstanceOf[Data])
//    }
//    def  getS[A<:T](key: Variable): Option[DataSerie] = {
//      val e = findEntry(key.asInstanceOf[Variable])
//      if (e == null) None
//      else Some(e.value.asInstanceOf[DataSerie])
//    }
//    def  getP[A<:T](key: Variable): Option[Parameter] = {
//      val e = findEntry(key.asInstanceOf[Variable])
//      if (e == null) None
//      else Some(e.value.asInstanceOf[Parameter])
//    }
   
}
