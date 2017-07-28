package xyz.hyperreal.northwind

import java.io.PrintWriter

import collection.mutable.{ArrayBuffer, HashMap}
import xyz.hyperreal.table.TextTable


object Main extends App {

	val inserts = new ArrayBuffer[Insert]
	val w = new PrintWriter( "northwind.md" )

	for (ins <- InsertParser.parseStatement( io.Source.fromFile("northwind.in") mkString ))
		inserts += ins

	//////////////////////// categories
	val categoriesHeader = Vector( "CategoryID", "CategoryName", "Description", "Picture" )
	val categories =
		new TextTable( markdown = true ) {
			headerSeq( categoriesHeader )
			rightAlignment( 1 )

			for (Insert( table, row ) <- inserts if table.name == "categories")
				rowSeq( row.updated(3, s"pic${row(0)}.jpg") )
		}

	w.println( "## Categories" )
	w.println
	w.print( categories )
	w.println
	w.println

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

	val customersHeader = Vector( "CustomerID", "CompanyName", "ContactName", "ContactTitle", "Address", "City", "Region", "PostalCode", "Country", "Phone", "Fax" )
	val customers =
		new TextTable( markdown = true ) {
			headerSeq( customersHeader )
			rightAlignment( 1 )

			for (Insert( table, row ) <- inserts if table.name == "customers")
				rowSeq( row )
		}

	w.println( "## Customers" )
	w.println
	w.print( customers )
	w.println
	w.println

	//////////////////////// employees
	val employeesHeader = Vector( "EmployeeID", "LastName", "FirstName", "Title", "TitleOfCourtesy", "BirthDate", "HireDate",
		"Address", "City", "Region", "PostalCode", "Country", "HomePhone", "Extension", "Notes", "ReportsTo", "Photopath" )
	val employees =
		new TextTable( markdown = true ) {
			headerSeq( employeesHeader )
			rightAlignment( 1 )
			rightAlignment( 16 )

			for (Insert( table, row ) <- inserts if table.name == "employees")
				rowSeq( row.slice(0, 14) ++ row.slice(15, row.length) )
		}

	w.println( "## Employees" )
	w.println
	w.print( employees )
	w.println
	w.println

//	CREATE TABLE territories (
//		territoryid character varying(20) NOT NULL,
//		territorydescription bpchar NOT NULL,
//		regionid smallint NOT NULL

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

	val territoriesHeader = Vector( "TerritoryID", "Territory", "TerritoryDescription", "RegionID" )
	val territories =
		new TextTable( markdown = true ) {
			headerSeq( territoriesHeader )
			rightAlignment( 1 )
			rightAlignment( 4 )

			for (Insert( table, row ) <- inserts if table.name == "territories")
				rowSeq( row )
		}

	w.println( "## Territories" )
	w.println
	w.print( territories )
	w.println
	w.println

//	CREATE TABLE employeeterritories (
//		employeeid smallint NOT NULL,
//		territoryid character varying(20) NOT NULL

	//////////////////////// employeeterritories
	val employeeterritoriesHeader = Vector( "EmployeeTerritoryID", "EmployeeID", "TerritoryID" )
	val employeeterritories =
		new TextTable( markdown = true ) {
			headerSeq( employeeterritoriesHeader )
			rightAlignment( 1 )
			rightAlignment( 2 )
			rightAlignment( 3 )

			for (Insert( table, row ) <- inserts if table.name == "employeeterritories")
				rowSeq( row.slice(0, 14) ++ row.slice(15, row.length) )
		}

	w.println( "## EmployeeTerritories" )
	w.println
	w.print( employeeterritories )
	w.println
	w.println

	//////////////////////// order_details
	var order_detailid = 1

	val order_detailsHeader = Vector( "OrderDetailID", "OrderID", "ProductID", "UnitPrice", "Quantity", "Discount" )
	val order_details =
		new TextTable( markdown = true ) {
			headerSeq( order_detailsHeader )
			rightAlignment( 1 )
			rightAlignment( 2 )
			rightAlignment( 3 )
			rightAlignment( 4 )
			rightAlignment( 5 )
			rightAlignment( 6 )

			for (Insert( table, row ) <- inserts if table.name == "order_details") {
				rowSeq( order_detailid.toString +: row )
				order_detailid += 1
			}
		}

	w.println( "## OrderDetails" )
	w.println
	w.print( order_details )
	w.println
	w.println

	//////////////////////// orders
	val ordersHeader = Vector( "OrderID", "CustomerID", "EmployeeID", "OrderDate", "RequiredDate", "ShippedDate", "ShipVia", "Freight", "ShipName", "ShipAddress", "ShipCity", "ShipRegion", "ShipPostalCode", "ShipCountry" )
	val orders =
		new TextTable( markdown = true ) {
			headerSeq( ordersHeader )
			rightAlignment( 1 )
			rightAlignment( 2 )
			rightAlignment( 3 )
			rightAlignment( 7 )
			rightAlignment( 8 )

			for (Insert( table, row ) <- inserts if table.name == "orders")
				rowSeq( row )
		}

	w.println( "## Orders" )
	w.println
	w.print( orders )
	w.println
	w.println

	//////////////////////// products
	val productsHeader = Vector( "ProductID", "ProductName", "SupplierID", "CategoryID", "QuantityPerUnit", "UnitPrice", "UnitsInStock", "UnitsOnOrder", "ReorderLevel", "Discontinued" )
	val products =
		new TextTable( markdown = true ) {
			headerSeq( productsHeader )
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
		}

	w.println( "## Products" )
	w.println
	w.print( products )
	w.println
	w.println

	//////////////////////// region
	val regionHeader = Vector( "RegionID", "RegionDescription" )
	val region =
		new TextTable( markdown = true ) {
			headerSeq( regionHeader )
			rightAlignment( 1 )

			for (Insert( table, row ) <- inserts if table.name == "region")
				rowSeq( row )
		}

	w.println( "## Regions" )
	w.println
	w.print( region )
	w.println
	w.println

	//////////////////////// shippers
	val shippersHeader = Vector( "ShipperID", "CompanyName", "Phone" )
	val shippers =
		new TextTable( markdown = true ) {
			headerSeq( shippersHeader )
			rightAlignment( 1 )

			for (Insert( table, row ) <- inserts if table.name == "shippers")
				rowSeq( row )
		}

	w.println( "## Shippers" )
	w.println
	w.print( shippers )
	w.println
	w.println

//	CREATE TABLE suppliers (
//		supplierid smallint NOT NULL,
//		companyname character varying(40) NOT NULL,
//		contactname character varying(30),
//		contacttitle character varying(30),
//		address character varying(60),
//		city character varying(15),
//		region character varying(15),
//		postalcode character varying(10),
//		country character varying(15),
//		phone character varying(24),
//		fax character varying(24),
//		homepage text

	//////////////////////// suppliers
	val suppliersHeader = Vector( "SupplierID", "CompanyName", "ContactName", "ContactTitle", "Address", "City", "Region", "PostalCode", "Country", "Phone", "Fax", "Homepage" )
	val suppliers =
		new TextTable( markdown = true ) {
			headerSeq( suppliersHeader )
			rightAlignment( 1 )

			for (Insert( table, row ) <- inserts if table.name == "suppliers")
				rowSeq( row )
		}

	w.println( "## Suppliers" )
	w.println
	w.print( suppliers )
	w.println
	w.println


	w.close
}