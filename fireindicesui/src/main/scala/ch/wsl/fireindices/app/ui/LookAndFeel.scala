/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package ch.wsl.fireindices.app.ui

import javax.swing.UIManager

object LookAndFeel {
  def setNativeLookAndFeel() {
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
//      if ("GTK look and feel".equals(UIManager.getLookAndFeel().getName())){
//            UIManager.put("FileChooserUI", "eu.kostia.gtkjfilechooser.ui.GtkFileChooserUI")
//      }
      
//      if ("Windows look and feel".equals(UIManager.getLookAndFeel().getName())){
//            MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
//
//      }
    } catch {
      case e:Exception => System.out.println("Error setting native LAF: " + e)
    }
  }

  def setJavaLookAndFeel() {
    try {
      UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName())
    } catch {
      case e:Exception => System.out.println("Error setting native LAF: " + e)
    }
  }

   def setMotifLookAndFeel() {
    try {
      UIManager.setLookAndFeel("com.sun.java.swing.plaf.motif.MotifLookAndFeel")
    } catch {
      case e:Exception => System.out.println("Error setting native LAF: " + e)
    }
  }
   def setWindowsLookAndFeel() {
    try {
      UIManager.setLookAndFeel("com.sun.java.swing.plaf.windows.WindowsClassicLookAndFeel")
    } catch {
      case e:Exception => System.out.println("Error setting native LAF: " + e)
    }
  }
   def setGtkLookAndFeel() {
    try {
      UIManager.setLookAndFeel("com.sun.java.swing.plaf.gtk.GTKLookAndFeel")
    } catch {
      case e:Exception => System.out.println("Error setting native LAF: " + e)
    }
  }
}

