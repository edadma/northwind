package xyz.hyperreal.northwind

import java.io.PrintWriter

import collection.mutable.{ArrayBuffer, HashMap}
import xyz.hyperreal.table.TextTable


object Main extends App {

	val inserts = new ArrayBuffer[Insert]
	val mdout = new PrintWriter( "northwind.md" )
	val tabout = new PrintWriter( "northwind.tab" )

	for (ins <- InsertParser.parseStatement( io.Source.fromFile("northwind.in") mkString ))
		inserts += ins

	//////////////////////// categories
	for (ins@Insert( table, row ) <- inserts if table.name == "categories")
		ins.row = row.updated(3, s"pic${row(0)}.jpg")

	mdout.println( "## Categories" )
	mdout.println
	mdout.print(
		new TextTable( markdown = true ) {
			headerSeq( Vector( "CategoryID", "CategoryName", "Description", "Picture" ) )
			rightAlignment( 1 )

			for (Insert( table, row ) <- inserts if table.name == "categories")
				rowSeq( row )
		} )
	mdout.println
	mdout.println

	//////////////////////// customers
	val customerids = new HashMap[String, Int]
	var nextcustomerid = 1

	for (ins@Insert( table, row ) <- inserts if table.name == "customers")
		customerids get row(0) match {
			case None =>
				customerids(row(0)) = nextcustomerid
				ins.row = row.updated( 0, nextcustomerid.toString )
				nextcustomerid += 1
			case Some( _ ) => problem( table.pos, "duplicate id" )
		}

	for (ins@Insert( table, row ) <- inserts if table.name == "orders")
		customerids get row(1) match {
			case None => problem( table.pos, "unknown id" )
			case Some( id ) =>
				ins.row = row.updated( 1, id.toString )
		}

	mdout.println( "## Customers" )
	mdout.println
	mdout.print(
		new TextTable( markdown = true ) {
			headerSeq( Vector( "CustomerID", "CompanyName", "ContactName", "ContactTitle", "Address", "City", "Region", "PostalCode", "Country", "Phone", "Fax" ) )
			rightAlignment( 1 )

			for (Insert( table, row ) <- inserts if table.name == "customers")
				rowSeq( row )
		} )
	mdout.println
	mdout.println

	//////////////////////// employees
	for (ins@Insert( table, row ) <- inserts if table.name == "employees")
		ins.row = row.slice(0, 14) ++ row.slice(15, row.length)

	mdout.println( "## Employees" )
	mdout.println
	mdout.print(
		new TextTable( markdown = true ) {
			headerSeq( Vector( "EmployeeID", "LastName", "FirstName", "Title", "TitleOfCourtesy", "BirthDate", "HireDate",
				"Address", "City", "Region", "PostalCode", "Country", "HomePhone", "Extension", "Notes", "ReportsTo", "Photopath" ) )
			rightAlignment( 1 )
			rightAlignment( 16 )

			for (Insert( table, row ) <- inserts if table.name == "employees")
				rowSeq( row )
		} )
	mdout.println
	mdout.println

	//////////////////////// territories
	val territoryids = new HashMap[String, Int]
	var nextterritoryid = 1

	for (ins@Insert( table, row ) <- inserts if table.name == "territories")
		territoryids get row(0) match {
			case None =>
				territoryids(row(0)) = nextterritoryid
				ins.row = nextterritoryid.toString +: row
				nextterritoryid += 1
			case Some( _ ) => problem( table.pos, "duplicate id" )
		}

	var nextemployeeterritoryid = 1

	for (ins@Insert( table, row ) <- inserts if table.name == "employeeterritories")
		territoryids get row(1) match {
			case None => problem( table.pos, "unknown id" )
			case Some( id ) =>
				ins.row = nextemployeeterritoryid.toString +: row.updated( 1, id.toString )
				nextemployeeterritoryid += 1
		}

	mdout.println( "## Territories" )
	mdout.println
	mdout.print(
		new TextTable( markdown = true ) {
			headerSeq( Vector( "TerritoryID", "Territory", "TerritoryDescription", "RegionID" ) )
			rightAlignment( 1 )
			rightAlignment( 4 )

			for (Insert( table, row ) <- inserts if table.name == "territories")
				rowSeq( row )
		}	)
	mdout.println
	mdout.println

	//////////////////////// employeeterritories
	mdout.println( "## EmployeeTerritories" )
	mdout.println
	mdout.print(
		new TextTable( markdown = true ) {
			headerSeq( Vector( "EmployeeTerritoryID", "EmployeeID", "TerritoryID" ) )
			rightAlignment( 1 )
			rightAlignment( 2 )
			rightAlignment( 3 )

			for (Insert( table, row ) <- inserts if table.name == "employeeterritories")
				rowSeq( row )
		}	)
	mdout.println
	mdout.println

	//////////////////////// order_details
	var order_detailid = 1

	for (ins@Insert( table, row ) <- inserts if table.name == "order_details") {
		ins.row = order_detailid.toString +: row
		order_detailid += 1
	}

	mdout.println( "## OrderDetails" )
	mdout.println
	mdout.print(
		new TextTable( markdown = true ) {
			headerSeq( Vector( "OrderDetailID", "OrderID", "ProductID", "UnitPrice", "Quantity", "Discount" ) )
			rightAlignment( 1 )
			rightAlignment( 2 )
			rightAlignment( 3 )
			rightAlignment( 4 )
			rightAlignment( 5 )
			rightAlignment( 6 )

			for (Insert( table, row ) <- inserts if table.name == "order_details")
				rowSeq( row )
		}	)
	mdout.println
	mdout.println

	//////////////////////// orders
	mdout.println( "## Orders" )
	mdout.println
	mdout.print(
		new TextTable( markdown = true ) {
			headerSeq( Vector( "OrderID", "CustomerID", "EmployeeID", "OrderDate", "RequiredDate", "ShippedDate", "ShipVia", "Freight", "ShipName", "ShipAddress", "ShipCity", "ShipRegion", "ShipPostalCode", "ShipCountry" ) )
			rightAlignment( 1 )
			rightAlignment( 2 )
			rightAlignment( 3 )
			rightAlignment( 7 )
			rightAlignment( 8 )

			for (Insert( table, row ) <- inserts if table.name == "orders")
				rowSeq( row )
		}	)
	mdout.println
	mdout.println

	//////////////////////// products
	mdout.println( "## Products" )
	mdout.println
	mdout.print(
		new TextTable( markdown = true ) {
			headerSeq( Vector( "ProductID", "ProductName", "SupplierID", "CategoryID", "QuantityPerUnit", "UnitPrice", "UnitsInStock", "UnitsOnOrder", "ReorderLevel", "Discontinued" ) )
			rightAlignment( 1 )
			rightAlignment( 3 )
			rightAlignment( 4 )
			rightAlignment( 5 )
			rightAlignment( 6 )
			rightAlignment( 7 )
			rightAlignment( 8 )
			rightAlignment( 9 )
			rightAlignment( 10 )

			for (Insert( table, row ) <- inserts if table.name == "products")
				rowSeq( row )
		}	)
	mdout.println
	mdout.println

	//////////////////////// region
	mdout.println( "## Regions" )
	mdout.println
	mdout.print(
		new TextTable( markdown = true ) {
			headerSeq( Vector( "RegionID", "RegionDescription" ) )
			rightAlignment( 1 )

			for (Insert( table, row ) <- inserts if table.name == "region")
				rowSeq( row )
		}	)
	mdout.println
	mdout.println

	//////////////////////// shippers
	mdout.println( "## Shippers" )
	mdout.println
	mdout.print(
		new TextTable( markdown = true ) {
			headerSeq( Vector( "ShipperID", "CompanyName", "Phone" ) )
			rightAlignment( 1 )

			for (Insert( table, row ) <- inserts if table.name == "shippers")
				rowSeq( row )
		}	)
	mdout.println
	mdout.println

	//////////////////////// suppliers
	mdout.println( "## Suppliers" )
	mdout.println
	mdout.print(
		new TextTable( markdown = true ) {
			headerSeq( Vector( "SupplierID", "CompanyName", "ContactName", "ContactTitle", "Address", "City", "Region", "PostalCode", "Country", "Phone", "Fax", "Homepage" ) )
			rightAlignment( 1 )

			for (Insert( table, row ) <- inserts if table.name == "suppliers")
				rowSeq( row )
		}	)
	mdout.println
	mdout.println


	mdout.close
}