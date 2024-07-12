#Enter the values for your database connection
dsn_driver = "com.ibm.db2.jcc.DB2Driver"
dsn_database = "bludb"            
dsn_hostname = "<yourhostname>"   #replace <yourhostname> with your hostname from the Service credentials
dsn_port = ""                     #replace with the port number from Service Credentials 
dsn_protocol = "TCPIP"            
dsn_uid = "<username>"            #replace <username> with your username from Service Credentials
dsn_pwd = "<password>"            #replace <password> with your password from Service Credentials
dsn_security = "ssl"




conn_path = paste("DRIVER=",dsn_driver,
                  ";DATABASE=",dsn_database,
                  ";HOSTNAME=",dsn_hostname,
                  ";PORT=",dsn_port,
                  ";PROTOCOL=",dsn_protocol,
                  ";UID=",dsn_uid,
                  ";PWD=",dsn_pwd,
                  ";SECURITY=",dsn_security,        
                  sep="")
conn = odbcDriverConnect(conn_path)
conn






conn.info = odbcGetInfo(conn)
conn.info["DBMS_Name"]
conn.info["DBMS_Ver"] 
conn.info["Driver_ODBC_Ver"]



sql.info = sqlTypeInfo(conn)
print(sql.info)

print(sql.info[c(1,3)], row.names=FALSE)





tab.frame = sqlTables(conn, schema="<Enter Schema>") # e.g. "SYSIBM"
nrow(tab.frame)
tab.frame$TABLE_NAME










tab.name <- "<Enter Table>" # e.g. "SYSSCHEMATA"
col.detail <- sqlColumns(conn, tab.name)
print(col.detail[c(2,3,4,6,7,9,18)], row.names=FALSE)






odbcCloseAll()





