package xyz.hyperreal.northwind

import java.io.PrintWriter

import collection.mutable.{ArrayBuffer, HashMap}
import xyz.hyperreal.table.TextTable


object Main extends App {

	val inserts = new ArrayBuffer[Insert]
	val ids = new HashMap[String, Int]
	var nextid = 1

	for (ins <- InsertParser.parseStatement( io.Source.fromFile("customers") mkString ))
		inserts += ins

	for (ins@Insert( table, row ) <- inserts if table.name == "customers")
		ids get row(0) match {
			case None =>
				ids(row(0)) = nextid
				ins.row = row.updated( 0, nextid.toString )
				nextid += 1
			case Some( _ ) => problem( table.pos, "duplicate id" )
		}

	for (ins@Insert( table, row ) <- inserts if table.name == "orders")
		ids get row(1) match {
			case None => problem( table.pos, "unknown id" )
			case Some( id ) =>
				ins.row = row.updated( 1, id.toString )
		}

	val customersHeader = Vector( "CustomerID", "CompanyName", "ContactName", "ContactTitle", "Address", "City", "Region", "PostalCode", "Country", "Phone", "Fax" )
	val customers =
		new TextTable( headerBold = false, headerLine = true, headerUnderlined = false, columnDividers = true ) {
			headerSeq( customersHeader )
			rightAlignment( 1 )

			for (Insert( table, row ) <- inserts if table.name == "customers")
				rowSeq( row )
		}

	val w = new PrintWriter( "northwind.txt" )

		w.print( customers )
		w.println

	val ordersHeader = Vector( "OrderID", "CustomerID", "EmployeeID", "OrderDate", "RequiredDate", "ShippedDate", "ShipVia", "Freight", "ShipName", "ShipAddress", "ShipCity", "ShipRegion", "ShipPostalCode", "ShipCountry" )
	val orders =
		new TextTable( headerBold = false, headerLine = true, headerUnderlined = false, columnDividers = true ) {
			headerSeq( ordersHeader )
			rightAlignment( 1 )
			rightAlignment( 2 )
			rightAlignment( 3 )
			rightAlignment( 7 )
			rightAlignment( 8 )

			for (Insert( table, row ) <- inserts if table.name == "orders")
				rowSeq( row )
		}

	w.print( orders )
	w.close
}