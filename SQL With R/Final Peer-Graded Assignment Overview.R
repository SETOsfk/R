#Final Peer-Graded Assignment Overview
library(RSQLite)
conn<- dbConnect(RSQLite::SQLite(),"FinalDB_lab4.sqlite")

#Problem 1: Create tables
df1<-dbExecute(conn,"CREATE TABLE CROP_DATA(
CD_ID INTEGER NOT NULL,
YEAR DATE NOT NULL,
CROP_TYPE VARCHAR(20) NOT NULL,
GEO VARCHAR(20) NOT NULL,
SEEDED_AREA INTEGER NOT NULL,
HARVESTED_AREA INTEGER NOT NULL,
PRODUCTION INTEGER NOT NULL,
AVG_YIELD INTEGER NOT NULL,
PRIMARY KEY (CD_ID)
               
)")

df2<- dbExecute(conn, "CREATE TABLE DAILY_FX (
                DFX_ID INTEGER NOT NULL,
                DATE DATE NOT NULL,
                FXUSDCAD INTEGER NOT NULL,
                PRIMARY KEY(DFX_ID)
)")

df3<- dbExecute(conn, "CREATE TABLE MONTHLY_FX (
                DFX_ID INTEGER NOT NULL,
                DATE DATE NOT NULL,
                FXUSDCAD INTEGER NOT NULL,
                PRIMARY KEY(DFX_ID)
)")

df4<-dbExecute(conn,"CREATE TABLE FARM_PRICES(
CD_ID INTEGER NOT NULL,
DATE DATE NOT NULL,
CROP_TYPE VARCHAR(20) NOT NULL,
GEO VARCHAR(20) NOT NULL,
PRICE_PRERMT INTEGER NOT NULL,
PRIMARY KEY (CD_ID)
               
)")

dbWriteTable(conn, "FARM_PRICES", Monthly_Farm_Prices, overwrite=TRUE, header=TRUE)
dbWriteTable(conn, "MONTHLY_FX", Monthly_FX, overwrite=TRUE, header=TRUE)

dbListTables(conn)


#how many records are in the farm prices dataset
dbGetQuery(conn, "SELECT COUNT(CD_ID) FROM FARM_PRICES")

#WHİCH PROVINCES ARE INCLUDEED
dbGetQuery(conn,"SELECT DISTINCT(GEO) FROM FARM_PRICES")

#How many hectares of Rye were harvested in Canada in 1968?
dbGetQuery(conn, "SELECT strftime('%Y',YEAR) AS YEAR, GEO, CROP_TYPE,SUM(HARVESTED_AREA) AS TOTAL_HARVESTED_AREA 
            FROM CROP_DATA WHERE GEO = 'Canada' AND CROP_TYPE = 'Rye' AND YEAR <= 1969 AND YEAR >=1968 ")

#Query and display the first 6 rows of the farm prices table for Rye.

dbGetQuery(conn,"SELECT PRICE_PRERMT FROM FARM_PRICES LIMIT 6")

#Problem 7: Which provinces grew Barley?
dbGetQuery(conn, "SELECT DISTINCT(GEO) FROM CROP_DATA 
           WHERE CROP_TYPE = 'Barley'")

#Problem 8: Find the first and last dates for the farm prices data.
dbGetQuery(conn, "SELECT MIN(DATE) AS FIRST_DATE, MAX(DATE) AS LAST_DATE FROM FARM_PRICES")

#Problem 9: Which crops have ever reached a farm price greater than or equal to $350 per metric tonne?
dbGetQuery(conn, "SELECT DISTINCT(CROP_TYPE) FROM FARM_PRICES WHERE PRICE_PRERMT >=350")

#Problem 10: Rank the crop types harvested in Saskatchewan in the year 2000 by 
#their average yield. Which crop performed best?

dbGetQuery(conn, "SELECT CROP_TYPE, strftime('%Y', YEAR) AS YEAR, GEO, AVG_YIELD FROM CROP_DATA
           WHERE YEAR <=2001 AND YEAR >=2000 AND GEO='Saskatchewan'
           ORDER BY AVG_YIELD DESC")


#Problem 11: Rank the crops and geographies by their 
#average yield (KG per hectare) since the year 2000. 
#Which crop and province had the highest average yield since the year 2000?

dbGetQuery(conn, "SELECT CROP_TYPE, strftime('%Y', YEAR) AS YEAR, GEO, max(AVG_YIELD) FROM CROP_DATA
           WHERE YEAR >=2000
           ORDER BY AVG_YIELD DESC")



#Problem 12: Use a subquery to determine how much wheat was harvested in 
#Canada in the most recent year of the data.
dbGetQuery(conn,"SELECT GEO,CROP_TYPE, HARVESTED_AREA, strftime('%Y', YEAR) AS YEAR FROM CROP_DATA
           WHERE YEAR = (SELECT MAX(YEAR) FROM CROP_DATA ) AND CROP_TYPE = 'Wheat' AND
           GEO= 'Canada'")




#Problem 13: Use an implicit inner join to calculate the 
#monthly price per metric tonne of Canola(crop_type) grown in Saskatchewan(geo) 
#in both Canadian and US dollars. Display the most recent 6 months of the data.


dbGetQuery(conn,"SELECT CD_ID CROP_TYPE, GEO, PRICE_PRERMT, FXUSDCAD,  FROM FARM_PRICESS, MONTHLY_FX WHERE GEO='Saskatchewan' AND
           CROP_TYPE = 'Canola' LIMIT 6")




















