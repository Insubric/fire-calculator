

package ch.wsl.fireindices.app.ui

import javax.swing.JPanel
import scala.swing.Component
import scala.swing.LayoutContainer
import scala.swing.Panel

class MigPanel(layoutConstraints:String="",columnConstraints:String="",rowConstraints:String="") extends Panel with LayoutContainer {
//  import BorderPanel._
  import net.miginfocom.swing._

  type Constraints = String
  override lazy val peer = new JPanel(new MigLayout(layoutConstraints,columnConstraints,rowConstraints))

  def layoutManager = peer.getLayout.asInstanceOf[MigLayout]

  protected def constraintsFor(comp: Component) = layoutManager.getComponentConstraints(comp.peer).asInstanceOf[String]
  protected def areValid(c: Constraints): (Boolean, String) = (true, "")
  def add(c: Component, l: Constraints ="") {
    peer.add(c.peer, l.toString)
  }
  def addJ(c: javax.swing.JComponent, l: Constraints ="") {
    peer.add(c, l.toString)
  }

}
