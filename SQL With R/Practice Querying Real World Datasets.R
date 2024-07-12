
library(RSQLite)



#Load the csv files into dataframes and inspect them
crop_df <- read.csv('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Practice%20Assignment/Annual_Crop_Data.csv', colClasses=c(YEAR="character"))
daily_df <- read.csv('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Practice%20Assignment/Daily_FX.csv', colClasses=c(date="character"))


#Connect to the database
conn<- dbConnect(RSQLite::SQLite(),"FinalDB_lab4.sqlite")

#Check whether these tables already exist, and drop them if so.
tables <- c("CROP_DATA", "DAILY_FX") 






df1<- dbExecute(conn, "CREATE TABLE CROP_DATA (
                CD_ID INTEGER NOT NULL,
                YEAR DATE NOT NULL,
                CROP_TYPE VARCHAR(20) NOT NULL,
                GEO VARCHAR(20) NOT NULL,
                SEEDED_AREA INTEGER NOT NULL,
                HARVESTED_AREA INTEGER NOT NULL,
                PRODUCTION INTEGER NOT NULL,
                AVG_YIELD INTEGER NOT NULL,
                PRIMARY KEY(CD_ID)
)", errors=FALSE)

df2<- dbExecute(conn, "CREATE TABLE DAILY_FX(
                DFX_ID INTEGER NOT NULL,
                DATE DATE NOT NULL,
                FXUSDCAD FLOAT(6),
                PRIMARY KEY(DFX_ID)
)",errors=FALSE)

dbWriteTable(conn, "DAILY_FX", daily_df, overwrite=TRUE, header=TRUE)
dbWriteTable(conn,"CROP_DATA", crop_df, overwrite=TRUE, header=TRUE)

#find the number of rows in each table

dbGetQuery(conn,"SELECT COUNT(CD_ID) FROM CROP_DATA")
dbGetQuery(conn,"SELECT COUNT(DFX_ID) FROM DAILY_FX")

#Display the first 6 rows crop_Data

dbGetQuery(conn, "SELECT * FROM CROP_DATA LIMIT 6")

#List the types of crops
dbGetQuery(conn,"SELECT DISTINCT(CROP_TYPE) FROM CROP_DATA")


#Query and display the first 6 rows of the crop data for Rye
dbGetQuery(conn,"SELECT * FROM CROP_DATA WHERE CROP_TYPE= 'Rye' LIMIT 6")

#Which crops have had an average yield greater than or equal to 3000 KG per Hectare?

dbGetQuery(conn, "SELECT DISTINCT(CROP_TYPE) FROM CROP_DATA WHERE AVG_YIELD >= 3000")

#first and last dates

dbGetQuery(conn,"SELECT MIN(YEAR) FIRST_DATE, MAX(YEAR) LAST_DATE FROM CROP_DATA")

dbGetQuery(conn,"SELECT MIN(DATE) FIRST_DATE, MAX(DATE) LAST_DATE FROM DAILY_FX")


#LİST THE TOP 10 years of wheat production in saskatchewena

dbGetQuery(conn,"SELECT YEAR AS TOP_10_YEARS, GEO, HARVESTED_AREA
           FROM CROP_DATA
           WHERE CROP_TYPE ='Wheat' AND
           GEO='Saskatchewan' 
           ORDER BY HARVESTED_AREA LIMIT 10")

#how many years barley yield at least 2k kg per hectare

dbGetQuery(conn, "SELECT COUNT(DISTINCT(YEAR)) AS BLY_YEARS
           FROM CROP_DATA
           WHERE AVG_YIELD >= 2000 AND
           CROP_TYPE = 'Barley' AND
           GEO= 'Canada'")

#How much farm land was seeeded with Barley in Alberta but 
#not harvested each year since the year 2000?
query <- 
  "SELECT YEAR(YEAR) AS YEAR, GEO, CROP_TYPE,
            SEEDED_AREA, HARVESTED_AREA, 
            100*(SEEDED_AREA-HARVESTED_AREA)/SEEDED_AREA AS PCT_UNHARVESTED_AREA
    FROM CROP_DATA
    WHERE YEAR(YEAR) >= 2000 AND
          GEO = 'Alberta' AND
          CROP_TYPE = 'Barley';"

view <- sqlQuery(conn,query)
view


#Over the last 3 calendar years of data, what was 
#the average value of the Canadian dollar relative to the USD?


query <-
  "SELECT MIN(DATE) AS AS_OF_DATE, 
            AVG(FXUSDCAD) AS FX_DAILY_AVG_CAD 
    FROM  DAILY_FX
    WHERE DATE >= (SELECT MAX(DATE) - 3 YEARS FROM DAILY_FX);
    "
view <- sqlQuery(conn,query)
view

#Use an implicit inner join to create a 
#view of the crop data with an FX column included.



query <- "SELECT CD_ID, YEAR, CROP_TYPE, GEO, SEEDED_AREA, HARVESTED_AREA, PRODUCTION, AVG_YIELD, FXUSDCAD  
    FROM CROP_DATA, MONTHLY_FX 
    WHERE YEAR(CROP_DATA.YEAR)=YEAR(MONTHLY_FX.DATE) AND MONTH(CROP_DATA.YEAR)=MONTH(MONTHLY_FX.DATE)
    LIMIT 5;"
view <- dbGetQuery(conn,query)
view









