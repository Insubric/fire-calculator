package ch.wsl.fireindices.model

import ch.wsl.fireindices.metadata.{Serie, Variable}


case class FCHeader(
                      date:String,
                      defined:Seq[String],
                      undefined:Seq[String] //holds the header of the columns not recognized
                    ) {
  val serie = Variable.getByAbbrCaseInsensitive(date).asInstanceOf[Serie]
}

object FCHeader{
  def empty = FCHeader("",Seq(),Seq())
}