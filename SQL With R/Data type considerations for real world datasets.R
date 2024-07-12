install.packages(c('RSQLite'), repos = 'http://cran.rstudio.com',dependecies=TRUE)
library(RSQLite)

#Connect to the database
conn<- dbConnect(RSQLite::SQLite(),"FinalDB_lab4.sqlite")

#Create "CROP_DATA" table in RSQLite.

df1<- dbExecute(conn,
                "CREATE TABLE CROP_DATA(
                CD_ID INTEGER NOT NULL,
                YEAR DATE NOT NULL,
                CROP_TYPE VARCHAR(20) NOT NULL,
                GEO VARCHAR(20) NOT NULL,
                SEEDED_AREA INTEGER NOT NULL,
                HARVESTED_AREA INTEGER NOT NULL,
                PRODUCTION INTEGER NOT NULL,
                AVG_YIELD INTEGER NOT NULL,
                PRIMARY KEY (CD_ID))",
                errors=FALSE)
if(df1==-1){
  cat("An error has occured.\n")
  msg<- odbcGetErrMsg(conn)
  print(msg)
} else {
  cat("Table var created succesfully.\n")
}

#Create "DAILY_FX" table in RSQLite

df3 <- dbExecute(conn, "CREATE TABLE DAILY_FX (
                                DFX_ID INTEGER NOT NULL,
                                DATE DATE NOT NULL, 
                                FXUSDCAD FLOAT(6),
                                PRIMARY KEY (DFX_ID)
                                )",
                 errors=FALSE
)

if (df3 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
} 

crop_df <- read.csv('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Practice%20Assignment/Annual_Crop_Data.csv', colClasses=c(YEAR="character"))
daily_df <- read.csv('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Practice%20Assignment/Daily_FX.csv', colClasses=c(date="character"))

head(crop_df)
head(daily_df)

dbWriteTable(conn, "DAILY_FX", daily_df, overwrite=TRUE, header=TRUE)
dbWriteTable(conn, "CROP_DATA", crop_df, overwrite=TRUE, header=TRUE)
#Check list of tables in the present db
dbListTables(conn)

#Find the number of rows in each table
dbGetQuery(conn, 'SELECT COUNT(CD_ID) FROM CROP_DATA')


dbGetQuery(conn, "SELECT COUNT(DFX_ID) FROM DAILY_FX")


#Query and display the first 6 rows of the crop data.

dbGetQuery(conn, "SELECT * FROM CROP_DATA LIMIT 6")

#List the types of crops in the crop dataset.

dbGetQuery(conn, "SELECT DISTINCT(CROP_TYPE) FROM CROP_DATA")

#Query and display the first 6 rows of the crop data for Rye.

dbGetQuery(conn, "select * from crop_data where crop_type = 'Rye' limit 6")

#Which crops have had an average yield greater than or equal to 3000 KG per Hectare?
dbGetQuery(conn, "SELECT DISTINCT(CROP_TYPE)
           FROM CROP_DATA
           WHERE AVG_YIELD > 3000")


#Find the first and last dates of each table.
dbGetQuery(conn, 'SELECT min(YEAR) FIRST_DATE, max(YEAR) LAST_DATE FROM CROP_DATA')


dbGetQuery(conn, 'SELECT min(DATE) FIRST_DATE, max(DATE) LAST_DATE FROM DAILY_FX')


#List the top 10 years of Wheat production in Saskatchewan in terms of harvested area
dbGetQuery(conn, "SELECT strftime('%Y',YEAR) AS TOP_10_YRS, GEO, HARVESTED_AREA 
    FROM CROP_DATA 
    WHERE CROP_TYPE='Wheat' AND 
          GEO='Saskatchewan'
    ORDER BY HARVESTED_AREA DESC
    LIMIT 10")


#How many years did Barley yield at least 2000 KG per Hectare in Canada?
dbGetQuery(conn, "SELECT COUNT(DISTINCT(YEAR)) AS BLY_YRS_ABOVE_2MTPH
    FROM CROP_DATA 
    WHERE AVG_YIELD > 2000 AND 
          CROP_TYPE='Barley' AND 
          GEO='Canada' ")

#How much farm land was seeeded with Barley in Alberta but not harvested each year since the year 2000?

dbGetQuery(conn, "SELECT strftime('%Y',YEAR) AS YEAR, GEO, CROP_TYPE,
            SEEDED_AREA, HARVESTED_AREA, 
            100*(SEEDED_AREA-HARVESTED_AREA)/SEEDED_AREA AS PCT_UNHARVESTED_AREA
            FROM CROP_DATA WHERE YEAR >= 2000 AND
            GEO = 'Alberta' AND CROP_TYPE = 'Barley'")

#Over the last 3 calendar years of data, what was the average value of the Canadian dollar relative to the USD

dbGetQuery(conn, "SELECT MIN(DATE) AS AS_OF_DATE, 
            AVG(FXUSDCAD) AS FX_DAILY_AVG_CAD 
    FROM  DAILY_FX
    WHERE DATE >= (SELECT MAX(DATE) - 3 YEARS FROM DAILY_FX)")

#Use an implicit inner join to create a view of the crop data with an FX column included.
dbGetQuery(conn, "SELECT CD_ID,YEAR ,CROP_TYPE, GEO, SEEDED_AREA, HARVESTED_AREA, PRODUCTION, AVG_YIELD, FXUSDCAD  
    FROM CROP_DATA, DAILY_FX 
    WHERE strftime('%Y',CROP_DATA.YEAR) = strftime('%Y',DAILY_FX.DATE) and strftime('%m', CROP_DATA.YEAR) = strftime('%m', DAILY_FX.DATE) LIMIT 5")


dbDisconnect(conn)
