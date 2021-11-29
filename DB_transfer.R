library(dplyr)
library(haven)
library(purrr)
library(RPostgres)
library(DBI)

con <- dbConnect(
  RPostgres::Postgres(), 
  dbname = "bss_old", 
  host="localhost", 
  port="5432", 
  user="rstudio", 
  password="rstudio"
)  

#reading original file
GSS <- read_dta("gss7221_r1a.dta") %>%
  mutate(uid = seq(1:n())) %>%
  select(year, uid, everything())

splitGSS <- GSS %>% 
  haven::zap_labels() %>%
  group_split(year)

# strip vector information out
#splitGSS <- as.list(splitGSS)

#cleaning empty variables
not_all_na <- function(x) any(!is.na(x))

splitGSS <- splitGSS %>%
  map(~select_if(., not_all_na))

save(splitGSS,file="splitgss.RData")

gss_years <- unique(GSS$year)

for (i in 1:length(splitGSS)) {
  l <- as.character(gss_years[i])
  copy_to(con, as.data.frame(splitGSS[[i]]), l,
          temporary=FALSE)
}
