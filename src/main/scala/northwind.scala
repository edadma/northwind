package xyz.hyperreal

import scala.util.parsing.input.Position


package object northwind {

	def problem( pos: Position, error: String ) =
		if (pos eq null)
			sys.error( error )
		else if (pos.line == 1)
			sys.error( error + "\n" + pos.longString )
		else
			sys.error( pos.line + ": " + error + "\n" + pos.longString )

}