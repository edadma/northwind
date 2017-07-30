package xyz.hyperreal.northwind

import java.io.PrintWriter

import collection.mutable.{ArrayBuffer, HashMap}
import xyz.hyperreal.table.TextTable


object Main extends App {

	val inserts = new ArrayBuffer[Insert]
	val mdout = new PrintWriter( "northwind.md" )

	def mdprint( heading: String, columns: Vector[String], right: List[Int], name: String ) = {
		mdout.println( s"## $heading" )
		mdout.println
		mdout.print(
			new TextTable( markdown = true ) {
				headerSeq( columns )

				for (c <- right)
					rightAlignment( c )

				for (Insert( table, row ) <- inserts if table.name == name)
					rowSeq( row )
			} )
		mdout.println
		mdout.println
	}

	val tabout = new PrintWriter( "northwind.tab" )

	def tabprint( heading: String, columns: Vector[String], right: List[Int], name: String ) = {
		tabout.println( heading )
		tabout.print(
			new TextTable( tabbed = true ) {
				headerSeq( columns )

				for (c <- right)
					rightAlignment( c )

				for (Insert( table, row ) <- inserts if table.name == name) {
					rowSeq( row )
				}
			} )
		tabout.println
	}

	for (ins <- InsertParser.parseStatement( io.Source.fromFile("northwind.in") mkString ))
		inserts += ins

	//////////////////////// categories
	for (ins@Insert( table, row ) <- inserts if table.name == "categories")
		ins.row = row.updated(3, s"pic${row(0)}.jpg")

	mdprint( "Categories", Vector("CategoryID", "CategoryName", "Description", "Picture"), List(1), "categories" )
	tabprint( "Categories", Vector("CategoryID:integer", "CategoryName", "Description", "Picture"), List(1), "categories" )

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

	mdprint( "Customers", Vector("CustomerID", "CompanyName", "ContactName", "ContactTitle", "Address", "City", "Region", "PostalCode", "Country", "Phone", "Fax"), List(1), "customers" )
	tabprint( "Customers", Vector("CustomerID:integer", "CompanyName", "ContactName", "ContactTitle", "Address", "City", "Region", "PostalCode", "Country", "Phone", "Fax"), List(1), "customers" )

	//////////////////////// employees
	for (ins@Insert( table, row ) <- inserts if table.name == "employees")
		ins.row = row.slice(0, 14) ++ row.slice(15, row.length)

	mdprint( "Employees", Vector("EmployeeID", "LastName", "FirstName", "Title", "TitleOfCourtesy", "BirthDate", "HireDate",
		"Address", "City", "Region", "PostalCode", "Country", "HomePhone", "Extension", "Notes", "ReportsTo", "Photopath"), List(1, 16), "employees" )
	tabprint( "Employees", Vector("EmployeeID:integer", "LastName", "FirstName", "Title", "TitleOfCourtesy", "BirthDate:date", "HireDate:date",
		"Address", "City", "Region", "PostalCode", "Country", "HomePhone", "Extension", "Notes", "ReportsTo", "Photopath"), List(1, 16), "employees" )

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

	mdprint( "Territories", Vector("TerritoryID", "Territory", "TerritoryDescription", "RegionID"), List(1, 4), "territories" )
	tabprint( "Territories", Vector("TerritoryID:integer", "Territory", "TerritoryDescription", "RegionID:integer"), List(1, 4), "territories" )

	//////////////////////// employeeterritories
	mdprint( "EmployeeTerritories", Vector("EmployeeTerritoryID", "EmployeeID", "TerritoryID"), List(1, 2, 3), "employeeterritories" )
	tabprint( "EmployeeTerritories", Vector("EmployeeTerritoryID:integer", "EmployeeID:integer", "TerritoryID:integer"), List(1, 2, 3), "employeeterritories" )

	//////////////////////// order_details
	var order_detailid = 1

	for (ins@Insert( table, row ) <- inserts if table.name == "order_details") {
		ins.row = order_detailid.toString +: row
		order_detailid += 1
	}

	mdprint( "OrderDetails", Vector("OrderDetailID", "OrderID", "ProductID", "UnitPrice", "Quantity", "Discount"), List(1, 2, 3, 4, 5, 6), "order_details" )
	tabprint( "OrderDetails", Vector("OrderDetailID:integer", "OrderID:integer", "ProductID:integer", "UnitPrice:decimal", "Quantity:integer", "Discount:decimal"), List(1, 2, 3, 4, 5, 6), "order_details" )

	//////////////////////// orders
	mdprint( "Orders", Vector("OrderID", "CustomerID", "EmployeeID", "OrderDate", "RequiredDate", "ShippedDate", "ShipVia", "Freight", "ShipName", "ShipAddress", "ShipCity", "ShipRegion", "ShipPostalCode", "ShipCountry"), List(1, 2, 3, 7, 8), "orders" )
	tabprint( "Orders", Vector("OrderID:integer", "CustomerID:integer", "EmployeeID:integer", "OrderDate:date", "RequiredDate:date", "ShippedDate:date", "ShipVia", "Freight:decimal", "ShipName", "ShipAddress", "ShipCity", "ShipRegion", "ShipPostalCode", "ShipCountry"), List(1, 2, 3, 7, 8), "orders" )

	//////////////////////// products
	mdprint( "Products", Vector("ProductID", "ProductName", "SupplierID", "CategoryID", "QuantityPerUnit", "UnitPrice", "UnitsInStock", "UnitsOnOrder", "ReorderLevel", "Discontinued"), List(1, 3, 4, 5, 6, 7, 8, 9), "products" )
	tabprint( "Products", Vector("ProductID:integer", "ProductName", "SupplierID:integer", "CategoryID:integer", "QuantityPerUnit", "UnitPrice:decimal", "UnitsInStock:integer", "UnitsOnOrder:integer", "ReorderLevel:integer", "Discontinued:integer"), List(1, 3, 4, 5, 6, 7, 8, 9), "products" )

	//////////////////////// region
	mdprint( "Regions", Vector("RegionID", "RegionDescription"), List(1), "region" )
	tabprint( "Regions", Vector("RegionID:integer", "RegionDescription"), List(1), "region" )

	//////////////////////// shippers
	mdprint( "Shippers", Vector("ShipperID", "CompanyName", "Phone"), List(1), "shippers" )
	tabprint( "Shippers", Vector("ShipperID:integer", "CompanyName", "Phone"), List(1), "shippers" )

	//////////////////////// suppliers
	mdprint( "Suppliers", Vector("SupplierID", "CompanyName", "ContactName", "ContactTitle", "Address", "City", "Region", "PostalCode", "Country", "Phone", "Fax", "Homepage"), List(1), "suppliers" )
	tabprint( "Suppliers", Vector("SupplierID:integer", "CompanyName", "ContactName", "ContactTitle", "Address", "City", "Region", "PostalCode", "Country", "Phone", "Fax", "Homepage"), List(1), "suppliers" )

	mdout.close
	tabout.close
}