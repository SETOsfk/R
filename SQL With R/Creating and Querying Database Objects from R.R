install.packages(c('RSQLite'), repos = 'https://cran.rstudio.com',dependecies=TRUE)
library("RSQLite")


#Create a databese connection
conn <- dbConnect(RSQLite::SQLite(),"Querying_DatabaseDB.sqlite")


#Let’s create the BOARD table in the database.

df1 <- dbExecute(conn, "CREATE TABLE BOARD (
                            B_ID CHAR(6) NOT NULL, 
                            B_NAME VARCHAR(75) NOT NULL, 
                            TYPE VARCHAR(50) NOT NULL, 
                            LANGUAGE VARCHAR(50), 
                            PRIMARY KEY (B_ID))", 
                 errors=FALSE)
df2<-dbExecute(conn, "CREATE TABLE SCHOOL (
               B_ID CHAR(6) NOT NULL,
               S_ID CHAR(6) NOT NULL,
               S_NAME VARCHAR(100),
               LEVEL VARCHAR(70),
               ENROLLMENT INTEGER WITH DEFAULT 10,
               PRIMARY KEY (B_ID, S_ID))", errors=FALSE)

# Load the data into the database
schooldf <- read.csv('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0103EN-SkillsNetwork/data/school.csv')
boarddf <- read.csv('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0103EN-SkillsNetwork/data/board.csv')

head(schooldf)
head(boarddf)


dbWriteTable(conn, "SCHOOL", schooldf, overwrite=TRUE, header = TRUE)
dbWriteTable(conn, "BOARD", boarddf, overwrite=TRUE, header = TRUE)


dbListTables(conn)

#Print details for the tables BOARD and SCHOOL
dbListTables(conn)
for (table in tables1){  
  cat ("\nColumn info for table", table, ":\n")
  col.detail <- dbColumnInfo(dbSendQuery(conn,paste( "select * from",table)))
  print(col.detail)
}

#Display some records from the beginning of the SCHOOL dataframe.
dbGetQuery(conn, 'SELECT * FROM BOARD limit 5')
dbGetQuery(conn, 'SELECT * FROM  SCHOOL limit 5')




#Fetch data from the database

boarddb <- dbGetQuery(conn, 'SELECT * FROM BOARD limit 5')
tail(boarddb)
schooldb<- dbGetQuery(conn, "SELECT * FROM SCHOOL limit 5")
head(schooldb)

library(ggplot2)

#Get the elementary school data from the database 
#from both tables in descending sequence.

elequery <- paste("select ENROLLMENT 
from SCHOOL s, BOARD b 
where b.B_NAME = 'Toronto DSB' and b.B_ID=s.B_ID 
and s.LEVEL = 'Elementary' 
order by ENROLLMENT desc")



eledf <- dbGetQuery(conn, elequery)
dim(eledf)


qplot(ENROLLMENT, data=eledf, geom="density",  main="TDSB School Size - Elementary")

#Create the secondary school enrollments query in descending sequence.
secquery <- paste("select s.ENROLLMENT 
from SCHOOL s, BOARD b 
where b.B_NAME = 'Toronto DSB' and b.B_ID=s.B_ID
and s.LEVEL = 'Secondary' 
order by ENROLLMENT desc")

#dataframe and plot
secdf<- dbGetQuery(conn, secquery)
qplot(ENROLLMENT, data=secdf, geom="density", main="TDSB School Size - Secondary")

#Query the BOARD database for enrollments.

denquery<- paste(" select b.B_NAME, s.S_NAME, LEVEL, ENROLLMENT from BOARD b,
                 SCHOOL s where b.B_ID = s.B_ID and b.B_NAME = 'Toronto DSB'")

dendf<- dbGetQuery(conn, denquery)

#Create a box plot of enrollements in elementary and secondary schools in Toronto.
dendf$LEVEL <- as.factor(dendf$LEVEL)
boxplot(ENROLLMENT ~ LEVEL, dendf, names =c("Secondary","Elementary"), main="Toronto DSB")
