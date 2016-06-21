
# fire calculator

The fire calculator is a library to calculate some fire weather indices from meteorological parameters. It can be used as a jar library (from the Java Virtual Machine), a command line application or a gui application (graphical user interface).
It is coded in the [Scala](http://www.scala-lang.org) language, which is targetting the Java virtual machine (jvm).

This tool reads meteorological data from a \*.csv file and calculates the fire weather indices according to the inputs, storing the results in another \*.csv file and the details of the calculation in a log file (text or json format). For each index multiple possible input parameters have been specified, according to a ranking from the optimal to the less suitable choice. For example for the canadian FWI if the parameters at noon are not available, the program will try to compute it with other meaningful daily values. Variable choice is indicated in the logs.


##Hardware and software requirements
Has been written in scala 2.11.8.
Requires a java virtual machine (JRE) version >= 1.7.0.



##Installation instructions, getting started tips, and documentation
Copy and unzip the content to a desired folder location.

For gui version double click on the launch/win/firecalculator-ui.exe (Windows) or launch/linux/firecalculator-ui.sh (Linux) file.

For the command line version you can use direclty the jar library: open a terminal window 
and move to the folder containing the jar files (launch/firecalculator.jar). 
In both Windows or Linux you can type e.g.:
```bash
   java -Xmx512m -Xss10m firecalculator.jar  --Altitude 300 --fileparam  --addparam  -s 20000201 -e 20040101  ./DATA_sample.csv
```
Note that if you do not specify arguments, the gui version will open.
   
Alternatively in linux you can call the launch/linux/firecalculator.sh bash script:
```bash
   bash firecalculator.sh --Altitude 300 --fileparam --addparam  -s 20000201 -e 20040101  ./DATA_sample.csv
```
or windows you can use the launch/win/firecalculator.exe:
```bash
   firecalculator.exe --Altitude 300 --fileparam --addparam  -s 20000201 -e 20040101  .\DATA_sample.csv
```   



##Important known problems
Tell me!


##Version history

See changelog.txt 


##Pricing information
This is provided as a freeware under the Gnu Public License GPL v.2. 
It has been developed by WSL in the frame of the ALPFFIRS project by WSL.
It is currently maintained and improved by WSL.
WSL does not take any responsibilty from the ussage of the software and of the correctness of the results.

##Contact information
Gianni Boris Pezzatti, 
Swiss federal research institute of forest, snow and lanscape research WSL

boris.pezzatti at wsl.ch


##Date or copyright date, and other legal information
Copyright © 2016 Swiss federal research institute of forest, snow and lanscape research WSL.

Following open source libraries have been used:
- csvjdbc.jar    		GNU Lesser General Public License (v. 2.0)
- argot.jar      		© 2010 Brian M. Clapper. Released under a BSD License.

The library jar can be compacted using ProGuard, Copyright © 2002-2016 Eric Lafortune, but json logging is then not working. 


##Citation
If you use the fire-calculator software and publish the results based on this software, please cite this software with:
- Swiss Federal Institute for Forest, Snow and Landscape Research WSL, 2016, Fire-calculator v1.22, https://github.com/Insubric/fire-calculator.


