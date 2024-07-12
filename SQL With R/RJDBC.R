install.packages(c("DBI","rJava","RJDBC","curl"))
library(DBI)
library(rJava)
library(RJDBC)
library(curl)

curl_download("https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-DB0201EN-SkillsNetwork/labs/Labs_Coursera_V5/datasets/Instructors.db","Instructors.db")
curl_download("https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0103EN-SkillsNetwork/jars/sqlite-jdbc-3.27.2.1.jar","sqlite-jdbc-3.27.2.1.jar")

#1. Enter the SQLITE jdbc driver class details which 
#will be used for connecting to SQLite database
dsn_driver = "org.sqlite.JDBC"



#2. Create a JDBC connection string
jcc = JDBC(dsn_driver, "sqlite-jdbc-3.27.2.1.jar")
jdbc_path = paste("jdbc:sqlite:Instructors.db")

#3. Use the driver and connection string to 
#actually connect to the database using the RJDBC function dbConnect().
conn = dbConnect(jcc, jdbc_path)


#4. Execute a query against the SQLITE user defined table 
#Instructor and fetch the first 3 rows into a R dataframe.
query = "SELECT name FROM sqlite_master"
rs = dbSendQuery(conn, query);
df = fetch(rs, -1);
head(df)
#5. Dis-connect the database
dbDisconnect(conn)
