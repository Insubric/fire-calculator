
# fire calculator

The fire calculator is a library to calculate some fire weather indices from meteorological parameters. It consists of a library and a command line application.
It is coded in the [Scala](http://www.scala-lang.org) language, which is targetting the Java virtual machine (jvm).

This tool reads meteorological data from a \*.csv file and calculates the fire weather indices according to the inputs, storing the results in another \*.csv file and the details of the calculation in a text file (log). For each index multiple possible input parameters have been specified, according to a ranking from the optimal to the less suitable choice. For example for the canadian FWI if the parameters at noon are not available, the program will try to compute it with other meaningful daily values. Variable choice is indicated in the logs.


##Hardware and software requirements
Has been written in scala 2.11.7.
Requires a java virtual machine (JRE) version >= 1.7.0.



##Installation instructions, getting started tips, and documentation
Copy and unzip the content to a desired folder location.

For UI version double click on the fireindicesui.exe (Windows) or ui.sh (Linux) file.

For the command line version you need to open a terminal window 
and move to the folder containing the jar files. 
Fore Linux you can type e.g.:

   java -Xmx512m -Xss10m -cp fireindicesui.jar ch.wsl.fireindices.app.ConsoleApp --Altitude 300 --fileparam  --addparam  -s 20000201 -e 20040101  ./DATA_sample.csv
   
or you can shorten this by running the shell script 
  
   bash console.sh --Altitude 300 --fileparam --addparam  -s 20000201 -e 20040101  ./DATA_sample.csv
   
For windows you can use in a terminal (cmd):

   fireindicesconsole.exe --Altitude 300 --fileparam --addparam  -s 20000201 -e 20040101  ./DATA_sample.csv
   



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

The library jar has been compacted using ProGuard, Copyright © 2002-2015 Eric Lafortune. 


