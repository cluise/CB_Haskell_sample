import Database.HDBC
import Database.HDBC.ODBC
import Data.List

-- ODBC Connection string to Connect Bridge server
-- PUT YOUR CONNECTION STRING HERE !!!
connectionString = "Driver={Media Gateway ODBC Driver};IMPL=CORBA;HOST=pg.connecting-software.com;PORT=8087;UID='someuser';PWD='some_pwd';ACC='CRM2011_PLAYGROUND_some'";

-- executes query and displays data into console
executeQuery :: IO()
executeQuery = do
{
	putStr "Enter query: ";	
	query <- getLine;
	putStrLn "Connecting to Connect Bridge Server ..." ;
	conn <- connectODBC connectionString;	
	putStrLn ("Executing query '" ++ query ++ "'");
	vals <- quickQuery conn query [];
	putStrLn ("Returned row count " ++ show (length vals));
	putStrLn (convertResultSetToString vals)
}

-- executes query and write result into CSV file
csvExportQuery :: IO()
csvExportQuery = do
{
	putStr "Enter query: ";	
	query <- getLine;
	putStrLn "Connecting to Connect Bridge Server ..." ;
	conn <- connectODBC connectionString;	
	putStrLn ("Executing query '" ++ query ++ "'");
	vals <- quickQuery conn query [];
	putStrLn ("Returned row count " ++ show (length vals));

	putStr "Enter CSV filename: ";	
	filename <- getLine;

	writeFile filename (convertResultSetToString vals)
}

-- convert resultset rerutned by 'quickQuery' into CSV string
convertResultSetToString :: [[SqlValue]] -> String
convertResultSetToString resultSet =
	intercalate "\n" (map (\row -> intercalate "," (map convertFieldToString row)) resultSet)

-- convert SqlValue field to its string representation
convertFieldToString :: SqlValue -> String
convertFieldToString val = 
	case val of  
    	SqlNull -> "NULL"
    	_       -> (fromSql val)::String


