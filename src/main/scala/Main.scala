package xyz.hyperreal.northwind

import java.io.PrintWriter

import collection.mutable.{ArrayBuffer, HashMap}
import xyz.hyperreal.table.{ASCII, TextTable}


object Main extends App {

	val inserts = new ArrayBuffer[Insert]
	val w = new PrintWriter( "northwind.md" )

	val ids = new HashMap[String, Int]
	var nextid = 1

	for (ins <- InsertParser.parseStatement( io.Source.fromFile("northwind.in") mkString ))
		inserts += ins

	//////////////////////// categories
	val categoriesHeader = Vector( "CategoryID", "CategoryName", "Description", "Picture" )
	val categories =
		new TextTable( headerBold = false, headerLine = true, headerUnderlined = false, columnDividers = true, borderStyle = ASCII ) {
			headerSeq( categoriesHeader )
			rightAlignment( 1 )

			for (Insert( table, row ) <- inserts if table.name == "categories")
				rowSeq( row.updated(3, s"pic${row(0)}.jpg") )
		}

	w.println( "Categories" )
	w.print( categories )
	w.println

	//////////////////////// customers
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

	w.println( "Customers" )
	w.print( customers )
	w.println

	//////////////////////// employees
	val employeesHeader = Vector( "EmployeeID", "LastName", "FirstName", "Title", "TitleOfCourtesy", "BirthDate", "HireDate",
		"Address", "City", "Region", "PostalCode", "Country", "HomePhone", "Extension", "Notes", "ReportsTo", "Photopath" )
	val employees =
		new TextTable( headerBold = false, headerLine = true, headerUnderlined = false, columnDividers = true ) {
			headerSeq( employeesHeader )
			rightAlignment( 1 )
			rightAlignment( 16 )

			for (Insert( table, row ) <- inserts if table.name == "employees")
				rowSeq( row.slice(0, 14) ++ row.slice(15, row.length) )
		}

	w.println( "Employees" )
	w.print( employees )
	w.println

	//////////////////////// orders
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

	w.println( "Orders" )
	w.print( orders )
	w.println


	w.close
}