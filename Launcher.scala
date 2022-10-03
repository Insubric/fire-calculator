package ch.wsl.fireindices.app

import ch.wsl.fireindices.app.ui.App

object LauncherApp {
	
	
	def main(args: Array[String])={
		
		if (args.length == 0) 
			App.main(args)
		else
			ConsoleApp.main(args)

	}
	
}
