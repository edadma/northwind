package xyz.hyperreal.northwind


object Main extends App {

	println( InsertParser.parseStatement( """INSERT INTO customers VALUES ("ALFKI", "Alfreds Futterkiste", "Maria Anders", "Sales Representative", "Obere Str. 57", "Berlin", NULL, "12209", "Germany", "030-0074321", "030-0076545");""" ) )

}