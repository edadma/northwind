package xyz.hyperreal.northwind


object Main extends App {

	println( InsertParser.parseStatement( """INSERT INTO orders VALUES (10941, 'SAVEA', 7, '1998-03-11', '1998-04-08', '1998-03-20', 2, 400.809998, 'Save-a-lot Markets', '187 Suffolk Ln.', 'Boise', 'ID', '83720', 'USA');""" ) )

}