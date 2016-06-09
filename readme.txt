Readme.txt, Boris Pezzatti 13.01.2016


------------------------------------------------------------------------

Fire Weather Indices Calculator

------------------------------------------------------------------------

Version: 1.22
Company: Swiss federal research institute of forest, snow and lanscape research WSL
Developers:  Gianni Boris Pezzatti, Eliseo Zarro

This tool reads meteorological data from a *.csv file and calculates the 
fire weather indices according to the inputs, storing the results in 
another *.csv file and the details of the calculation in a text file and
a json file (logs). 
For each index multiple possible input parameters have been specified, 
according to a ranking from the optimal to the less suitable choice. 
For example for the canadian FWI if the parameters at noon are not available, 
the program will try to compute it with other meaningful daily values. 
Variable choice is indicated in the logs.

The tool is provided in 2 versions: a graphical user interface (ui) application
and a command line application.


Hardware and software requirements
----------------------------------
Has been written in scala 2.11.8.
Requires a java virtual machine (JRE) version >= 1.7.0.



Installation instructions, getting started tips, and documentation
------------------------------------------------------------------
Copy and unzip the content to a desired folder location.

For UI version double click on the fireindicesui.exe (Windows) or ui.sh (Linux) file.

For the command line version you need to open a terminal window 
and move to the folder containing the jar files. 
For Linux you can type e.g.:

   java -Xmx512m -Xss10m -cp fireindicesui.jar ch.wsl.fireindices.app.ConsoleApp --Altitude 300 --fileparam  --addparam  -s 20000201 -e 20040101  ./DATA_sample.csv
   
or you can shorten this by running the shell script 
  
   bash console.sh --Altitude 300 --fileparam --addparam  -s 20000201 -e 20040101  ./DATA_sample.csv
   
For windows you can use in a terminal (cmd):

   fireindicesconsole.exe --Altitude 300 --fileparam --addparam  -s 20000201 -e 20040101  ./DATA_sample.csv
   



Important known problems
------------------------
Tell me!


Version history
---------------
See changelog.txt


Pricing information
-------------------
This is provided as a freeware. It has been developed by WSL in the frame of the ALPFFIRS project by WSL.
It is currently maintained and improved by WSL.

Contact information
-------------------
Gianni Boris Pezzatti, 
Swiss federal research institute of forest, snow and lanscape research WSL

boris.pezzatti@wsl.ch


Date or copyright date, and other legal information
-------------------------------------------------
Copyright © 2016 Swiss federal research institute of forest, snow and lanscape research WSL.

Following open source libraries have been used:
- csvjdbc.jar    		GNU Lesser General Public License (v. 2.0)
- GtkJFileChooser.jar	2010, GNU Lesser General Public License
- JCalendar		        © Kai Toedter 1999 - 2009, GNU Lesser General Public License
- argot.jar      		© 2010 Brian M. Clapper. Released under a BSD License.
- lift-json				Apache 2.0 license

The files jar have been compacted using ProGuard, Copyright © 2002-2015 Eric Lafortune. 
The exe's from jar have been created with launch4j, which has a BDS 3-clause license and includes into the exe package some code with MIT license.






























