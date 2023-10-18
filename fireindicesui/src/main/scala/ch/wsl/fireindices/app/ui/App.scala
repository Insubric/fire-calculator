
package ch.wsl.fireindices.app.ui

import java.awt.Color
import java.awt.Cursor
import java.awt.Dimension
import java.awt.Point
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.text.DateFormat
import java.text.DecimalFormat
import java.text.Format
import java.text.NumberFormat
import java.text.SimpleDateFormat
import com.toedter.calendar.JDateChooser
import javax.swing.ImageIcon
import javax.swing.JPanel
import scala.collection.mutable.ArraySeq
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.swing.Alignment
import scala.swing.Button
import scala.swing.CheckBox
import scala.swing.ComboBox
import scala.swing.Component
import scala.swing.Dialog
import scala.swing.EditorPane
import scala.swing.FileChooser
import scala.swing.FormattedTextField
import scala.swing.Label
import scala.swing.LayoutContainer
import scala.swing.MainFrame
import scala.swing.Panel
import scala.swing.ScrollPane
import java.awt.Font
import scala.swing.Separator
import scala.swing.SimpleSwingApplication
//import scala.swing.SwingWorker
import scala.swing.TabbedPane
import scala.swing.TextArea
import scala.swing.TextField
import scala.swing.event.ActionEvent
import scala.swing.event.ButtonClicked
import scala.swing.event.Event
import scala.swing.Alignment
import javax.swing.filechooser.FileNameExtensionFilter
import ch.wsl.fireindices.app.AppUtils
import ch.wsl.fireindices.app.SimpleApp4CSV
import ch.wsl.fireindices.functions.Utils
import ch.wsl.fireindices.metadata.Parameters._
import ch.wsl.fireindices.metadata._
import ch.wsl.fireindices.log.ReportLog
import java.util.prefs.Preferences
import ch.wsl.fireindices.app._

object App extends SimpleSwingApplication {

  val mf:MainFrame = new MainFrame {
    LookAndFeel.setNativeLookAndFeel

    val version = BuildInfo.version
    var appPref:Preferences = Preferences.userNodeForPackage(this.getClass());

    var elab:SimpleApp4CSV = null
    var file:File = null
    var files:scala.collection.Seq[File] = null
    var report:ReportLog = null
    var reports:ListBuffer[ReportLog] = new scala.collection.mutable.ListBuffer[ReportLog]

    title = "Fire Weather Indices Calculator"

    val mp = new MigPanel("","[grow 0][grow, align center][grow 0,align right]","[][]") //main
    val mpC = new MigPanel("","[][grow][100!]","[][][][][grow 0][align top]")           //calculate
    val mpM = new MigPanel("","[][grow][100!]","[][][grow 0][align top]")               //Multi files calculator
    val mpS = new MigPanel("","","20[][]")                                              //settings
    val mpO = new MigPanel("","","20[][]")                                              //missing input variables
    val mpP = new MigPanel("","","20[][]")                                              //preferences
    val mpH = new MigPanel("","[grow]","")                                              //help
    val mpA = new MigPanel("","[grow]","")                                              //about
    val titleFont = new Font("Arial",1,28)
    mp.add(new Label{
        text=""
        icon=createImageIcon("res/wsl_logo_38x38.gif","wsl")
        horizontalAlignment=Alignment.Left
        tooltip="www.wsl.ch"})
    mp.add(new Label {
      text = "Fire Weather Indices Calculator"
      font = titleFont
      horizontalAlignment = Alignment.Center
    },"growx")
//    },"growx,wrap")
    mp.add(new Label{
        text= ""
        icon=createImageIcon("res/alpffirs_40.gif","alpffirs")
        horizontalAlignment =Alignment.Right
        tooltip="www.alpffirs.eu"}
           ,"wrap")
    val t:TabbedPane = new TabbedPane{
      pages+= new TabbedPane.Page("Calculate",mpC)
      pages+= new TabbedPane.Page("Multi File",mpM)
      pages+= new TabbedPane.Page("Settings",mpS)
      pages+= new TabbedPane.Page("Missing variables",mpO)
      pages+= new TabbedPane.Page("Preferences",mpP)
      pages+= new TabbedPane.Page("Help",mpH)
      pages+= new TabbedPane.Page("About",mpA)

    }
    mp.add(t,"span 3, grow, width max(100%),height max(100%)")


    //calculate
    //single file
    val fldFile = new TextField
    val datStart = new JDateChooser
    val datEnd = new JDateChooser
    val btnBrowseFile = new Button("Browse")
    val fc:FileChooser = new FileChooser
    val fc2:FileChooser = new FileChooser
    val btnReadAndCheck = new Button("Read and Check")
    btnReadAndCheck.tooltip = "Check input values"
    val btnCalculate = new Button("Calculate")
    btnCalculate.tooltip = "Calculate indices"
    val txtReport = new TextArea
    txtReport.editable=false

    mpC.add(new Label("Choose input file"),"newline 20")
    mpC.add(fldFile," growx")
    mpC.add(btnBrowseFile,"wrap")
//    mpC.add(new Separator,"growx")
    mpC.add(new Label("Period"),"newline 20")
    mpC.add(new Label("Begin"),"split 4")
    mpC.addJ(datStart, "width 120:140:160")
    mpC.add(new Label("End"))
    mpC.addJ(datEnd, "width 120:140:160, wrap")
    mpC.add(btnReadAndCheck,"growx,growy 0,wrap")
    mpC.add(btnCalculate,"growx")
    mpC.add(new ScrollPane(txtReport),"cell 1 4 2 2, grow, height 100:1000:10000, width 200:1000:10000")

    //Multi file
    val txtMulti = new TextField {
      tooltip = "Select multiple files"
    }
    val btnBrowseFiles = new Button("Browse")
    val chkCheckMulti = new CheckBox
    val chkCalculateMulti = new CheckBox
    val btnGoMulti = new Button("Perform on all files")
    val btnSaveCommonLog = new Button("Save common log")
    val txtReportMulti = new TextArea
    chkCheckMulti.selected = false
    chkCalculateMulti.selected = true
    txtReportMulti.editable = false

    mpM.add(new Label("Choose input files"),"newline 20")
    mpM.add(txtMulti," growx")
    mpM.add(btnBrowseFiles,"wrap 40")
    mpM.add(chkCheckMulti,"split 2")
    mpM.add(new Label("check"),"wrap")
    mpM.add(chkCalculateMulti,"split 2")
    mpM.add(new Label("calculate"),"wrap")
    mpM.add(btnGoMulti,"growx,growy 0,wrap")
    mpM.add(btnSaveCommonLog,"growx,growy 0,bottom, wrap")
    mpM.add(new ScrollPane(txtReportMulti),"cell 1 2 2 4, grow, height 100:1000:10000, width 200:1000:10000")

    //settings
    val numFormat = new DecimalFormat("###.##")
    val intFormat = new DecimalFormat("###")

    addSeparator(mpS,"General")
    val ffldAltitude = addStandardDoubleField(mpS, Altitude)
    val ffldLatitude = addStandardDoubleField(mpS, Latitude)
    val ffldAspect = addStandardDoubleField(mpS, Aspect)
    val ffldSlope = addStandardDoubleField(mpS, Slope)
    val ffldRainyDayThreshold = addStandardDoubleField(mpS, RainyDayThreshold, true, true)
    val ffldMeanAnnualRainfall = addNumericField(mpS,"Mean annual rainfall",MeanAnnualRain.format(appPref.getDouble(MeanAnnualRain.abbr,Double.NaN)),numFormat,"","growx,wrap 20")
    ffldMeanAnnualRainfall.tooltip_=("Only if you wish to specify a value not calculated over the period with P(rain)")
    addSeparator(mpS,"Net Radiation")
    val ffldKrs = addNumericField(mpS,"Krs",Krs.format(appPref.getDouble(Krs.abbr,0.16)),numFormat,"","growx")
    mpS.add(new Label("between 0.16 (interior locations) and 0.19 (coastal locations)"),"wrap")
    val ffldAlbedo = addNumericField(mpS,"Albedo",Albedo.format(appPref.getDouble(Albedo.abbr,0.25)),numFormat,"","growx")
    mpS.add(new Label("between 0.20 and 0.25 for green vegetation covers"),"wrap 20")
    addSeparator(mpS,"KBDI and Orieux")
    val ffldRainyWeekThreshold= addStandardDoubleField(mpS, RainyWeekThreshold, true, true)
    mpS.add(new Label("(to initialize index)"),"wrap 20")
    
    addSeparator(mpS,"PET Thorntwaite")
    val ffldI = addNumericField(mpS,"Heat index",I.format(appPref.getDouble(I.abbr,Double.NaN)),numFormat,"","growx,wrap 20")
    ffldI.tooltip_=("if omitted will be calculated with given data")
    
    addSeparator(mpS,"FWI")
    val ffldFFMCstart = addStandardDoubleField(mpS, FFMCstart, false, true)
    val ffldDMCstart = addStandardDoubleField(mpS, DMCstart, false, true)
    val ffldDCstart = addStandardDoubleField(mpS, DCstart, false, true)
    val ffldSDMCstart = addStandardDoubleField(mpS, SDMCstart, false, true)
    mpS.add(new Label("(to initialize index)"),"wrap 20")

    addSeparator(mpS,"Risico")
    val ffldRisico_v0 = addStandardDoubleField(mpS, Risico_v0)
    val ffldRisico_d0 = addStandardDoubleField(mpS, Risico_d0)
    val ffldRisico_d1 = addStandardDoubleField(mpS, Risico_d1)
    val ffldRisico_T0 = addStandardDoubleField(mpS, Risico_T0)
    val ffldRisico_sat = addStandardDoubleField(mpS, Risico_sat)
    val ffldRisico_hhv = addStandardDoubleField(mpS, Risico_hhv)
    val ffldRisico_humidity = addStandardDoubleField(mpS, Risico_humidity, true, false,true)

    
    addSeparator(mpS,"M68")
    val datFireSeasonStart = addDateField(mpS,"Begin fire season",appPref.get("FireSeasonStart","15.02"),"dd.MM","","growx,wrap")
    val datFireSeasonEnd = addDateField(mpS,"End fire season",appPref.get("FireSeasonEnd","30.09"),"dd.MM","","growx,wrap")
    val datM68vegCorrStep3start = addDateField(mpS,"Step 3 start",appPref.get("M68VegCorrStep3Start","15.08"),"dd.MM","","growx,wrap")
    val datM68vegCorrStep3end = addDateField(mpS,"Step 3 end",appPref.get("M68VegCorrStep3End","01.09"),"dd.MM","","growx,wrap")

    // missing inputs
    addSeparator(mpO,"General")
    val datSnowcoverStart = addDateField(mpO,"Start of snowcover",appPref.get("XsnowcoverStart","01.01"),"dd.MM","","growx,wrap")
    val datSnowcoverEnd = addDateField(mpO,"End of snowcover",appPref.get("XsnowcoverEnd","01.01"),"dd.MM","","growx,wrap")
    val datBirchLeaves = addDateField(mpO,"First birch leaves",appPref.get("XbirchLeaves","01.04"),"dd.MM","","growx,wrap")
    val datRobiniaBlossom = addDateField(mpO,"Robina blossom",appPref.get("XrobiniaBlossom","01.06"),"dd.MM","","growx,wrap 20")

    addSeparator(mpO,"Duration of rainfall in a day")
    val ffldClimate = addNumericField(mpO,"Climate class\n(Deeming et al.1977)",Climate.format(appPref.getInt(Climate.abbr,3)),intFormat,"","growx,wrap")
    ffldClimate.tooltip_=("1 arid - semiarid \n 2 subhumid with drought in summer\n 3 subhumid without drought in summer \n 4 humid - wet")

    //preferences
    val cboParam:ComboBox[String] = new ComboBox("file"::"ui"::"both"::Nil)
    cboParam.selection.index = 2
    cboParam.tooltip="The file that will be search will be a file \n"+
                     "with the same name as the input file, but ending with *_PAR.txt"

    addSeparator(mpP,"Parameter source")
    mpP.add(new Label("Get parameters from"),"gapx 40")
    mpP.add(cboParam,"wrap")
    
    val chkRisico:CheckBox = new CheckBox("")
    chkRisico.tooltip = "for sub-daily timesteps"
      
    addSeparator(mpP,"Risico")
    mpP.add(new Label("Calculate only Risico"),"gapx 40")
    mpP.add(chkRisico,"wrap")
    

    //help
    val btnManual = new Button("Go to manual")
    mpH.add(btnManual,"wrap")

    var help ="<br>This tool reads meteorological data from a *.csv file and calculates the fire weather indices according to the inputs, storing the results in another *.csv file and the details of the calculation in a text file and a json file (logs).<br> For each index multiple possible input parameters have been specified, according to a ranking from the optimal to the less suitable choice. For example for the canadian FWI if the parameters at noon are not available, the program will try to compute it with other meaningful daily values. Variable choice is indicated in the logs.<br>"
        
    help+="<br><h2>Input file </h2>The program can read *.csv files (comma separated values), with headers with a specific name (columns with other names will not be considered). The case (upper/lower) is not relevant.<br>For a list of the abbreviations used see the Variable section.<br>The values are checked for missing values and range validity. The values of the input variables wich are out of range are replaced with a missing value (not in the original file, only to perform the calculations).<br>"

    help+="<br><h2>Settings </h2>You can change the settings according to your local situation.<br>In the missing variables tab you can specify some parameters which can be used for the calculation as a replacement.<br>The parameter can also be imported form a file (see Preferences).<br>"

    help+="<br><h2>Preferences </h2>You can specify if the parameters and missing variables should be read from the ui (user interface), a parameter file (*_PAR.txt) or both (file parameters have the precedence over ui parameters).<br> The file option allows you to use different parameters for different stations in the multi file calculation.<br>"+
          "The parameters in the *_PAR.txt file should be specified as the following example:<br><br>Altitude 500<br>Latitude 46.0<br>FireSeasonStart 15.01<br>FireSeasonEnd 15.10<br>Climate 3<br>"

    help += "<br><h2>Output files </h2>The program generates two files, one containing the results (xxx_RESULTS.csv) and one reporting the log (xxx_LOG.txt), which are stored in the same directory as the input file (xxx.csv).<br>For each calculated variable, a log is given with the calclulation details:<br>&nbsp;&nbsp;<b>start</b>: initial value<br>&nbsp;&nbsp;<b>(</b>required variables<b>)->(</b>used variables<b>)</b><br>&nbsp;&nbsp;<b>[</b>parameters used<b>]</b><br>"

    var strInputs = ""
    Variable.parameters.foreach(x => strInputs += x.abbr + "</td><td style=\"vertical-align: top;\">" + x.name + "</td><td style=\"vertical-align: top;\">" + x.unit +  "<br></td></tr><tr><td style=\"vertical-align: top;\">")
    help+="<br><h2>Parameters </h2>Here is a list of the parameters and missing variables that can be specified in a *_PAR.txt file (see paragraph on preferences).<br><br>"
    help+="<table width=\"520\" style=\"text-align: left; width: 100%;\" border=\"0\" cellpadding=\"2\" cellspacing=\"2\"><COLGROUP><COL width=\"1*\"><COL width=\"3*\"><COL width=\"1*\"><COL width=\"3*\"><tbody><tr>"+strInputs+"<br></td></tr></tbody></table><br>\n"

    strInputs = ""
    Variable.inputs.foreach(x => strInputs += x.abbr + "</td><td style=\"vertical-align: top;\">" + x.name + "</td><td style=\"vertical-align: top;\">" + x.unit +  "<br></td></tr><tr><td style=\"vertical-align: top;\">")
    help+="<br><h2>Variables </h2>Here is a list of the variables used with the abbreviations, units and variables required for the computation.<br><br>"
    help+="<table width=\"800\" style=\"text-align: left; width: 100%;\" border=\"0\" cellpadding=\"2\" cellspacing=\"2\"><COLGROUP><COL width=\"1*\"><COL width=\"3*\"><COL width=\"1*\"><COL width=\"3*\"><tbody><tr><td colspan=\"4\"><b>Inputs:</b> </td></tr><tr><td style=\"vertical-align: top;\">DateYYYYMMDD&nbsp; <br></td><td style=\"vertical-align: top;\">e.g.&nbsp; 20081231</td></tr><tr><td style=\"vertical-align: top;\">DateYYYYMMDDHHMI&nbsp; <br></td><td style=\"vertical-align: top;\">e.g.&nbsp; 200812311200 </td></tr><tr>"+strInputs+"<br></td></tr>\n";

    strInputs = ""
    Variable.optionalInputs.foreach(x => strInputs += x.abbr + "</td><td style=\"vertical-align: top;\">" + x.name + "</td><td style=\"vertical-align: top;\">" + x.unit +  "</td><td style=\"vertical-align: top;\">" + x.asInstanceOf[Calculable].printRequiredVariables + "<br></td></tr><tr><td style=\"vertical-align: top;\">")
    help+="<tr><td colspan=\"4\"><b>Optional inputs:</b> </td></tr> "+strInputs+"<br></td></tr>\n"

    strInputs = ""
    Variable.calculables.foreach(x => strInputs += x.abbr + "</td><td style=\"vertical-align: top;\">" + x.name + "</td><td style=\"vertical-align: top;\">" + x.unit + "</td><td style=\"vertical-align: top;\">" + x.asInstanceOf[Calculable].printRequiredVariables +"<br></td></tr><tr><td style=\"vertical-align: top;\">")
    help+="<tr><td colspan=\"4\"><b>Calculables: (from inputs or settings)</b></td></tr>"+strInputs+"<br></td></tr></tbody></table><br>\n"


    val scrollHelp = new ScrollPane(new EditorPane("text/html",help){
      editable = false
      background = Color.LIGHT_GRAY
    })
    mpH.add(scrollHelp,"grow, height 100:1000:10000, width 200:1000:10000")

    //about
    val about="<h2>Fire Weather Indices Calculator</h2><br>This application has been developed by the Swiss Federal Institute for Forest, Snow and Landscape Research WSL (www.wsl.ch)<br>in the frame of the Alpine Forest Fire Warning System ALP FFIRS project (www.alpffirs.eu).<br><br>Developers: Boris Pezzatti & Eliseo Zarro.<br><br>Version "+version
    val scrollAbout = new ScrollPane(new EditorPane("text/html",about){
      editable = false
      background = Color.LIGHT_GRAY
    })
    mpA.add(scrollAbout,"grow, height 100:1000:10000, width 200:1000:10000")

    contents = mp
    size =  new Dimension(appPref.getInt("FrameSizeX",400),appPref.getInt("FrameSizeY",640))
    location = new Point(appPref.getInt("FramePosX",50),appPref.getInt("FramePosY",50))

    //Initialize buttons
    btnReadAndCheck.enabled = false
    btnCalculate.enabled = false
    datStart.setEnabled(false)
    datEnd.setEnabled(false)
    btnGoMulti.enabled = false


    listenTo(btnBrowseFile,btnReadAndCheck,btnCalculate,btnBrowseFiles,btnGoMulti,btnSaveCommonLog,btnManual)
    reactions+={
      case ButtonClicked(`btnBrowseFile`) =>
        fc.showOpenDialog(btnBrowseFile) match {
          case FileChooser.Result.Approve =>
            btnBrowseFile.enabled = false
            btnBrowseFiles.enabled = false
            try {
              if (fc.selectedFile.exists) {
                file = fc.selectedFile

                elab = new SimpleApp4CSV
//                new Actor {
//                  override def act {
                    try {                      
                        btnCalculate.enabled = false
                        mf.cursor=Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
                        report = new ReportLog(file.getPath)
                        txtReport.text=""
                        println(fc.selectedFile)
                        fldFile.text=file.getPath
//                        reportTEMP.append("File: " + currentFile.getName() + "\n\n")
                        if (elab.readHeadersFile(file, report)){    //if date column has been recognized
                          printMessage(report.headers.format)
                          printMessage("\nFinding first and last Date...")
//                          reportTEMP.append("Finding first and last Date...")
                          val firstDate = AppUtils.getFirstDate(file, elab.dt)
                          val lastDate = AppUtils.getLastDate(file, elab.dt)
                          if (firstDate != null) {
                              datStart.setMinSelectableDate(firstDate)
                              datEnd.setMinSelectableDate(firstDate)
                              datStart.setDate(firstDate)
                          }
                          if (firstDate != null) {
                              datStart.setMaxSelectableDate(lastDate)
                              datEnd.setMaxSelectableDate(lastDate)
                              datEnd.setDate(lastDate)
                          }
                          btnReadAndCheck.enabled = true
                          datStart.setEnabled(true)
                          datEnd.setEnabled(true)

                          val sd = new SimpleDateFormat(elab.dt.unit).format(firstDate)
                          val ed = new SimpleDateFormat(elab.dt.unit).format(lastDate)
                          printMessage(" OK (" + sd + ", " + ed + ")\n\n")

                          if (cboParam.selection.index!=1){
                            printMessage(reportFileParameters(file))
//                            parFile = new File(currentFile.getPath.take(currentFile.getPath.size-4)+"_PAR.txt")
//                            if (parFile.exists) {
//                              val parameters = new Parameters
//                              parameters.read(parFile)
//                              printMessage("Parameter file exists: "+ parameters.keySet.map(_.abbr).mkString(" ")+"\n\n")
//                            }
                          }
                        }
                        else {
                          printMessage("\nPlease check the file format\n\n")
                        }
                    } catch {
                        case e:Exception => e.printStackTrace();
                    } finally {
                        mf.cursor=Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)
                    }
                    //return null
//                  }
//                }.start
                
              } else {
//                editScroll.editArea.text = ""
//                FileHandler = fc.selectedFile
              }
            } catch {
              case e: IOException =>
//                Dialog.showMessage(this, "ファイルを開けませんでした。\n" + e.getMessage)
            }
            btnBrowseFile.enabled = true
            btnBrowseFiles.enabled = true
          case FileChooser.Result.Cancel => ;
          case FileChooser.Result.Error => ;
        }     
     
       case ButtonClicked(`btnBrowseFiles`) =>
          fc2.multiSelectionEnabled = true
          fc2.showOpenDialog(btnBrowseFiles) match {
            case FileChooser.Result.Approve =>{
              btnBrowseFiles.enabled = false
              btnBrowseFile.enabled = false
              try {
                if (fc2.selectedFiles.length!=0) {
                  files = fc2.selectedFiles
                  btnGoMulti.enabled = true
                  txtMulti.text = files.map(_.getName).mkString(", ")
                }
              } catch {
                case e: IOException =>
          //                Dialog.showMessage(this, "ファイルを開けませんでした。\n" + e.getMessage)
              }
              btnBrowseFile.enabled = true
              btnBrowseFiles.enabled = true}
            case FileChooser.Result.Cancel => ;
            case FileChooser.Result.Error => ;
          }
          txtReportMulti.text=""
        
      case ButtonClicked(`btnReadAndCheck`) =>
//          new SwingWorker{
//            override def act {
                try {
                    btnReadAndCheck.enabled=false
                    mf.cursor=Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
                    val parameters = readParameters(file)
                    printMessage("Reading inputs...")
                    val sd = Utils.solarJDate2String(datStart.getDate(), elab.dt.unit)
                    val ed = Utils.solarJDate2String(datEnd.getDate(), elab.dt.unit)
                    printMessage( " OK (" + sd + ", " + ed + ")\n" + elab.readDataFile(file, parameters, Some(sd), Some(ed), elab.dt.unit))
                    printMessage("Checking inputs...")
                    printMessage(if (elab.check(report)==true) " OK\n\n"  else "(some errors)\n" + report.check.format + "\n\n")
                } catch {
                    case e:Exception => e.printStackTrace()
                } finally{
                    mf.cursor=Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)
                    btnCalculate.enabled=true
                    btnReadAndCheck.enabled=true
                }
//            }
//        }.start

      case ButtonClicked(`btnCalculate`) =>
//          new SwingWorker{
//            override def act {
                try {            
                    btnCalculate.enabled=false
                    btnReadAndCheck.enabled=false
                    fldFile.text=""
                    datStart.setEnabled(false)
                    datEnd.setEnabled(false)
                    mf.cursor=Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
                    val parameters = readParameters(file)
                    printMessage("Calculating output...")
                    if (chkRisico.selected) 
                        elab.calculateFile(parameters,report, Risico_dffm::Risico_WindEffect::Risico_V::Risico_FI::Nil)
                    else 
                        elab.calculateFile(parameters,report)
                    printMessage(" OK\n" + report.formatCalculateLog)
//                    printMessage("Parameters used for the calculation \n" + parameters.print("\t") + "\n")
//                    elab.printLog(txtReport.text)
                    elab.writeLog(report, true, true)
                    file = null
                } catch {
                    case e:Exception => e.printStackTrace()
                } finally{
                    mf.cursor=Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)
                }
//            }
//        }.start


        case ButtonClicked(`btnGoMulti`) =>
//          new SwingWorker{
//            override def act {
                try {
                    btnCalculate.enabled=false
                    btnGoMulti.enabled=false
                    btnReadAndCheck.enabled=false
                    btnBrowseFiles.enabled=false
//                    btnBrowseFile.enabled=false
//                    datStart.setEnabled(false)
//                    datEnd.setEnabled(false)
                    chkCheckMulti.enabled = false
                    chkCalculateMulti.enabled = false
                    
                    mf.cursor=Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
                    txtReportMulti.text = ""
                    for (i <- 0 until files.length){
                      var txtRep = ""
                      val elabTmp = new SimpleApp4CSV
                      try{
                        printMessageMulti("Reading input file "+files(i).getName+"...")
                        reports += new ReportLog(files(i).getPath)
                        val ans = elabTmp.readHeadersFile(files(i), reports(i))
                        if (!ans) printMessageMulti(reports(i).headers.format)
                        txtRep+=reports(i).headers.format

                        if (cboParam.selection.index!=1){
                          val repPar = reportFileParameters(files(i))
                          printMessageMulti(repPar)
                          txtRep+=repPar
                        }                                   
                        val parameters = readParameters(files(i))

                        elabTmp.readDataFile(files(i),parameters)
                        printMessageMulti(" OK\n")

                        if (chkCheckMulti.selected){
                          printMessageMulti("Checking inputs...")
                          printMessageMulti(if (elabTmp.check(reports(i))==true) " OK\n\n"  else "(some errors)\n" + reports(i).check + "\n\n")
                          txtRep+=reports(i).check
                        }
                        if (chkCalculateMulti.selected){
                          printMessageMulti("Calculating output ("+files(i).getName+")...")
                          if (chkRisico.selected) 
                              elabTmp.calculateFile(parameters, reports(i), Risico_dffm::Risico_WindEffect::Risico_V::Risico_FI::Nil)
                          else 
                              elabTmp.calculateFile(parameters, reports(i))
                          txtRep+=reports(i).formatLog
//                          txtRep+="Parameters used for the calculation \n" + parameters.print("\t") + "\n"
                          printMessageMulti(" OK\n")
                          printMessageMulti(" \n\n\n")
                        }
                      } catch {
                        case e: Throwable => e.printStackTrace()
                          printMessageMulti(" An error occurred\n")
                          txtRep+=" An error occurred\n"
                      } finally {
                        elabTmp.writeLog(reports(i), true, true)
                      }
                    }
                    txtMulti.text = ""
                    printMessageMulti("End of elaboration. \nCheck single logs for more details.")

                } catch {
                    case e:Exception => e.printStackTrace()
                } finally{
                    mf.cursor=Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)
                    btnBrowseFiles.enabled=true
                    btnBrowseFile.enabled=true
                    chkCheckMulti.enabled = true
                    chkCalculateMulti.enabled = true
                    btnGoMulti.enabled=true
                }
//            }
//        }.start

        case ButtonClicked(`btnSaveCommonLog`) =>
          val fc3 = new FileChooser{
            fileFilter = new FileNameExtensionFilter("Text file", "txt")
          }        
          fc3.showSaveDialog(btnSaveCommonLog) match {
          case FileChooser.Result.Approve =>{
            val sf = fc3.selectedFile
//            val confResult = if (sf.exists) {
//              Dialog.showConfirmation(btnSaveCommonLog,
//                "This file already exists.\n Do you want to replce it？",
//                "Save", Dialog.Options.YesNo)
//            } else {
//              Dialog.Result.Yes
//            }
//            if (confResult == Dialog.Result.Yes) {
              val fw = new FileWriter(sf)
              try {
                fw.write(txtReportMulti.text)
                fw.close
//                FileUtils.writeStringToFile(sf, txtMulti.text)
              } catch {
                case e: IOException =>
                  Dialog.showMessage(btnSaveCommonLog, "Could not save the log." + e.getMessage)
                  if (fw!=null) fw.close
              }
//            }
          }
          case FileChooser.Result.Cancel => ;
          case FileChooser.Result.Error => ;
        }
        
        
        case ButtonClicked(`btnManual`) =>
            val url = "http://wiki.fire.wsl.ch/tiki-index.php?page=Software&structure=Fire"
            java.awt.Desktop.getDesktop().browse(java.net.URI.create(url));
    }


    def printMessage(text:String)={
          txtReport.append(text)
          txtReport.peer.setCaretPosition(txtReport.peer.getLineStartOffset(txtReport.lineCount -1))
      }
    def printMessage(text:StringBuffer):Unit= printMessage(text.toString)
      
    def printMessageMulti(text:String)={
          txtReportMulti.append(text)
          txtReportMulti.peer.setCaretPosition(txtReportMulti.peer.getLineStartOffset(txtReportMulti.lineCount -1))
      }
    def printMessageMulti(text:StringBuffer):Unit= printMessageMulti(text.toString)

    def addSeparator(panel:MigPanel, text:String)={
      val l = new Label(text)
      l.foreground= Color.BLUE
      panel.add(l, "gapbottom 1, span, split 2, aligny center")
      panel.add(new Separator, "gapleft rel, growx")
     }

     def addNumericField[T](panel:MigPanel, label:String, value:T,format:Format,labelLayout:String="",componentLayout:String):TextField={
      val l= new Label(label)
      val ffld = new TextField
      ffld.text = value.toString
      ffld.horizontalAlignment = Alignment.Right
      panel.add(l, labelLayout)
      panel.add(ffld, componentLayout)
      ffld
     }
     def addStandardDoubleField[T](panel:MigPanel, variable:Variable, 
                                   addUnit:Boolean=true, defaultToolTip:Boolean=false,
                                   endOfParagraph:Boolean=false):TextField = {
       val componentLayout = if (addUnit) "growx" else if (endOfParagraph) "growx, wrap 20" else "growx, wrap" 
       val ffld = addNumericField(panel, variable.name, variable.format(appPref.getDouble(variable.abbr, defaults(variable))),numFormat,"",componentLayout)
       if (addUnit) panel.add(new Label(variable.unit), if (endOfParagraph) "wrap 20" else "wrap")
       if (defaultToolTip) ffld.tooltip_=("Default = "+ defaults(variable))
       ffld
     }
     
     def addDateField(panel:MigPanel, label:String, value:String,format:String,labelLayout:String="",componentLayout:String):JDateChooser={    
      val l= new Label(label)
      val dat = new JDateChooser
      dat.setDateFormatString(format)
      dat.setDate(new SimpleDateFormat(format).parse(value))   
      panel.add(l, labelLayout)
      panel.addJ(dat, componentLayout)
      dat
     }
     def readUIParameters:Parameters={
        val parameters = new Parameters

        val parMap = new HashMap[Variable,TextField]
        parMap += (Altitude -> ffldAltitude)
        parMap += (Latitude -> ffldLatitude)
        parMap += (Aspect -> ffldAspect)
        parMap += (Slope -> ffldSlope)
        parMap += (RainyDayThreshold -> ffldRainyDayThreshold)
        parMap += (MeanAnnualRain -> ffldMeanAnnualRainfall)
        parMap += (Krs -> ffldKrs)
        parMap += (Albedo -> ffldAlbedo)
        parMap += (RainyWeekThreshold -> ffldRainyWeekThreshold)
        parMap += (I -> ffldI)
        parMap += (FFMCstart -> ffldFFMCstart)
        parMap += (DMCstart -> ffldDMCstart)
        parMap += (DCstart -> ffldDCstart)
        parMap += (SDMCstart -> ffldSDMCstart)
        parMap += (Risico_v0 -> ffldRisico_v0)
        parMap += (Risico_d0 -> ffldRisico_d0)
        parMap += (Risico_d1 -> ffldRisico_d1)
        parMap += (Risico_T0 -> ffldRisico_T0)
        parMap += (Risico_sat -> ffldRisico_sat)
        parMap += (Risico_hhv -> ffldRisico_hhv)
        parMap += (Risico_humidity -> ffldRisico_humidity)
//        parMap += (Climate -> ffldClimate)

        parMap.foreach( (x) => {
          //Get local regional settings to parse double
          if (x._2.text!=""&&x._2.text!="NaN"){
//            val nbrFrmt:NumberFormat = NumberFormat.getInstance
//            val parsedNmber:Number = nbrFrmt.parse(x._2.text)
//            val doubleVal:Double = parsedNmber.doubleValue
            val doubleVal:Double = NumberFormat.getInstance.parse(x._2.text).doubleValue
            val doubleTxt:String = doubleVal.toString

            parameters.addIfNotNull_Txt(x._1, doubleTxt)
            x._1.c.toString match{
               case "double" => appPref.putDouble(x._1.abbr,x._1.toValue(doubleTxt).asInstanceOf[Double])
               case "int" => appPref.putInt(x._1.abbr,x._1.toValue(doubleTxt).asInstanceOf[Int])
            }
//            x._1.dataManifest.toString match{
//               case "Double" => appPref.putDouble(x._1.abbr,doubleVal)//asInstanceOf[Double])
//               case "Int" => appPref.putInt(x._1.abbr,x._1.toValue(doubleTxt).asInstanceOf[Int])
//            }
          }
        })

        parameters.addIfNotNull(Climate, ffldClimate.text.toInt)
        appPref.putInt("Climate", ffldClimate.text.toInt)

        parameters.addIfNotNull(FireSeasonStart, Utils.solarDate2Long(datFireSeasonStart.getDate))
        appPref.put("FireSeasonStart",Utils.solarJDate2String(datFireSeasonStart.getDate,"dd.MM"))
        parameters.addIfNotNull(FireSeasonEnd, Utils.solarDate2Long(datFireSeasonEnd.getDate))
        appPref.put("FireSeasonEnd",Utils.solarJDate2String(datFireSeasonEnd.getDate,"dd.MM"))
        parameters.addIfNotNull(M68VegCorrStep3Start, Utils.solarDate2Long(datM68vegCorrStep3start.getDate))
        appPref.put("M68VegCorrStep3Start",Utils.solarJDate2String(datM68vegCorrStep3start.getDate,"dd.MM"))
        parameters.addIfNotNull(M68VegCorrStep3End, Utils.solarDate2Long(datM68vegCorrStep3end.getDate))
        appPref.put("M68VegCorrStep3End",Utils.solarJDate2String(datM68vegCorrStep3end.getDate,"dd.MM"))
        parameters.addIfNotNull(XbirchLeaves, Utils.solarDate2Long(datBirchLeaves.getDate))
        appPref.put("XbirchLeaves",Utils.solarJDate2String(datBirchLeaves.getDate,"dd.MM"))
        parameters.addIfNotNull(XrobiniaBlossom, Utils.solarDate2Long(datRobiniaBlossom.getDate))
        appPref.put("XrobiniaBlossom",Utils.solarJDate2String(datRobiniaBlossom.getDate,"dd.MM"))
        parameters.addIfNotNull(XsnowcoverStart, Utils.solarDate2Long(datSnowcoverStart.getDate))
        appPref.put("XsnowcoverStart",Utils.solarJDate2String(datSnowcoverStart.getDate,"dd.MM"))
        parameters.addIfNotNull(XsnowcoverEnd, Utils.solarDate2Long(datSnowcoverEnd.getDate))
        appPref.put("XsnowcoverEnd",Utils.solarJDate2String(datSnowcoverEnd.getDate,"dd.MM"))


        appPref.flush

        parameters
     }

    def readParameters(dataFile:File) ={
        val parameters = if (cboParam.selection.index!=0) readUIParameters else new Parameters
        if (cboParam.selection.index!=1){
            val parFile = new File(dataFile.getPath.take(dataFile.getPath.size-4)+"_PAR.txt")
            if (parFile.exists) parameters.read(parFile)
        }
        parameters
    }
    def reportFileParameters(dataFile:File):String ={
      val parFile = new File(dataFile.getPath.take(dataFile.getPath.size-4)+"_PAR.txt")
      if (parFile.exists) {
        val parameters = new Parameters
        parameters.read(parFile)
        "Parameter file exists: "+ parameters.keySet.map(_.abbr).mkString(" ")+"\n\n"
      }else{
        ""
      }
    }

     /** Returns an ImageIcon, or null if the path was invalid. */
     def  createImageIcon(path:String, description:String=""):ImageIcon ={
          val imgURL:java.net.URL  = getClass().getResource(path)
          if (imgURL != null) {
              new ImageIcon(imgURL, description)
          } else {
              System.err.println("Couldn't find file: " + path)
              null
          }
      }

    
     override def closeOperation () ={
       appPref.putInt("FrameSizeX",this.peer.getWidth)
       appPref.putInt("FrameSizeY",this.peer.getHeight)
       appPref.putInt("FramePosX",this.peer.getX)
       appPref.putInt("FramePosY",this.peer.getY)

       appPref.flush

       super.closeOperation()
     }
//
  }


  def top = mf


}


