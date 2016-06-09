

package ch.wsl.fireindices.metadata

import scala.collection.mutable.LinkedHashMap


/**
 * HashMap which build the key value appying a method (buildKey) to the value object.
 *
 **/
 
abstract class KeySet[K,V]  extends LinkedHashMap[K,V] {
									//with Map[K,V] 
									//with MapLike[K,V, KeySet[K,V]]
									//with HashTable[K, LinkedEntry[K,V]]{

   /**
   * constructor that already adds all elements passed in
   *
   * @param          that        > collection of the newelements to add
   */ 
  def this(that: TraversableOnce[V])= {
	  this()
	  this ++= that
  }
  
  /**
   * return a key for a new variable in the KeySet (abstract)
   *
   * @param   v  V > the variable
   * @return     K > the key
   */
  protected def buildKey(v :V):K
  
  /**
   * add a new element in the KeySet (abstract)
   *
   * @param   v  V > the variable
   * @return     KeySet
   */
  def += (v: V): this.type = { put(buildKey(v), v); this }
  
  /**
   * add a new key & variable in the KeySet (abstract)
   *
   * @param   kv Tuple[K,V] > the key and the variable
   * @return     KeySet
   */
  override def += (kv: (K,V)): this.type = {
    if (kv._1 != buildKey(kv._2)) throw new IllegalArgumentException("Key must correspond to buildKey result!")
    this += kv._2
  }

  /**
   * add new elements in the KeySet
   *
   * @param   v  V > the variable
   * @return     KeySet
   */
  def ++= (that: TraversableOnce[V]): this.type = {  that.foreach(this += _) ; this }



}
