
##==============================================================================
## LOAD PACKAGES
##==============================================================================
library(geneorama)
# install.packages("RPostgreSQL")
library("RPostgreSQL")

##==============================================================================
## LOAD DRIVER AND CREATE DB CONNECTION
##==============================================================================
drv <- dbDriver("PostgreSQL")

params <- yaml::yaml.load_file("untracked/pg_credentials.yaml")

con <- dbConnect(drv, 
                 dbname = params$database,
                 host = params$server, 
                 port = params$port,
                 user = params$username, 
                 password = params$password)

##==============================================================================
## VIEW TABLES
##==============================================================================

schemas <- as.data.table(dbGetQuery(con, "SELECT * FROM information_schema.tables"))
schemas
schemas[grep("traps", table_name), table_name]

##==============================================================================
## Execute query
##==============================================================================
traps <- as.data.table(dbGetQuery(con, "SELECT * FROM zdt.zdt_traps"))
traps

