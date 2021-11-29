#This file will take some data from DB and save it as separate files for 
#   *years prior 2021 and 2021 (because of different design weights)
#   *numerical and categorical inference separately to go with the project, to not lose the year-specific data

library(dplyr)
library(purrr)
library(RPostgres)
library(DBI)

con <- dbConnect(
  RPostgres::Postgres(), 
  dbname = "gss", 
  host="localhost", 
  port="5432", 
  user="rstudio", 
  password="rstudio"
)  

#selecting all years which have weights (wtssall), cluster id (vpsu) and health columns. 
#It would be nice to write in one SQL query, but I don't know yet how.

all_tables_wtssall <- con %>% dbGetQuery("
                                  select t.table_schema,
                                  t.table_name
                                  from information_schema.tables t
                                  inner join information_schema.columns c on c.table_name = t.table_name 
                                    and c.table_schema = t.table_schema
                                  where c.column_name = 'wtssall'
                                    and t.table_schema not in ('information_schema', 'pg_catalog')
                                    and t.table_type = 'BASE TABLE'
                                  order by t.table_schema;
                             ")

all_tables_vpsu <- con %>% dbGetQuery("
                                  select t.table_schema,
                                  t.table_name
                                  from information_schema.tables t
                                  inner join information_schema.columns c on c.table_name = t.table_name 
                                    and c.table_schema = t.table_schema
                                  where c.column_name = 'vpsu'
                                    and t.table_schema not in ('information_schema', 'pg_catalog')
                                    and t.table_type = 'BASE TABLE'
                                  order by t.table_schema;
                             ")

all_tables_health <- con %>% dbGetQuery("
                                  select t.table_schema,
                                  t.table_name
                                  from information_schema.tables t
                                  inner join information_schema.columns c on c.table_name = t.table_name 
                                    and c.table_schema = t.table_schema
                                  where c.column_name = 'health'
                                    and t.table_schema not in ('information_schema', 'pg_catalog')
                                    and t.table_type = 'BASE TABLE'
                                  order by t.table_schema;
                             ")

all_tables <- intersect(all_tables_health,all_tables_vpsu)
all_tables <- intersect(all_tables,all_tables_wtssall)

#I'm using map function of the purrr package to run multiple SQL and union them
tmp <- all_tables$table_name %>% 
  map(~{
    tbl(con, .x) %>%
      select(uid,class,health,age,vpsu,vstrat,wtssall,year,natfare) 
  }) %>% 
  reduce(function(x, y) union(x, y))

gss <- collect(tmp)

#sprucing up the data - adding categories and labels
gss <- gss %>% 
  mutate(
    age_group = case_when(
      age <= 24            ~ "18-24",
      age > 24 & age <= 44 ~ "25-44",
      age > 44 & age <= 64 ~ "45-64",
      age > 64             ~ "> 64"
    ),
    age_group = factor(
      age_group,
      level = c("18-24", "25-44","45-64", "> 64")
    ),
    class = factor(
      as.character(class),
      labels = c("lower class", "working class","middle class", "upper class","no class")
    ),
    health = factor(
      health,
      labels = c("excellent", "good","fair", "poor")
    ),
    #natfare = ifelse( natfare == 9 | natfare == 0, NA, natfare),
    natfare = factor(
      natfare,
      labels = c("Too little","About right","Too much")
    )
  )

save(gss,file="gss.RData")

#############################################

#numerical inference doesn't need health, so I can use all years with weights and clusters
# it will give me three extra years

all_tables_num <- intersect(all_tables_wtssall,all_tables_vpsu)

#I'm using map function of the purrr package to run multiple SQL and union them
tmp <- all_tables_num$table_name %>% 
  map(~{
    tbl(con, .x) %>%
      select(uid,class,age,vpsu,vstrat,wtssall,year,conrinc,degree,sex,race) 
  }) %>% 
  reduce(function(x, y) union(x, y))

gss_num <- collect(tmp)

#sprucing up the data - adding categories and labels
gss_num <- gss_num %>% 
  mutate(
    age_group = case_when(
      age <= 24            ~ "18-24",
      age > 24 & age <= 44 ~ "25-44",
      age > 44 & age <= 64 ~ "45-64",
      age > 64             ~ "> 64"
    ),
    age_group = factor(
      age_group,
      level = c("18-24", "25-44","45-64", "> 64")
    ),
    class = factor(
      as.character(class),
      labels = c("lower class", "working class","middle class", "upper class","no class")
    ),
    degree = ifelse( degree == 8 | degree == 9, NA, degree),
    degree = factor(
      degree,
      labels = c("< high school","High school","Junior college","Bachelor","Graduate")
    ),
    sex = factor(
      sex,
      labels = c("Male","Female")
    ),
    race = factor(
      race,
      labels = c("White","Black","Other")
    )
  )

save(gss_num,file="gss_num.RData")

#####################################################
#loading 2021 separately.

tmp <- tbl(con, "2021") %>%
  select(uid,class,health,age,vpsu,vstrat,wtssnrps,coninc,degree,sex,natfare) 

gss2021 <- collect(tmp)

gss2021 <- gss2021 %>% 
  mutate(
    age_group = case_when(
      age <= 24            ~ "18-24",
      age > 24 & age <= 44 ~ "25-44",
      age > 44 & age <= 64 ~ "45-64",
      age > 64             ~ "> 64"
    ),
    age_group = factor(
      age_group,
      level = c("18-24", "25-44","45-64", "> 64")
    ),
    class = factor(
      as.character(class),
      labels = c("lower class", "working class","middle class", "upper class")
    ),
    health = factor(
      health,
      labels = c("excellent", "good","fair", "poor")
    ),
    degree = ifelse( degree == 8 | degree == 9, NA, degree),
    degree = factor(
      degree,
      labels = c("< high school","High school","Junior college","Bachelor","Graduate")
    ),
    sex = factor(
      sex,
      labels = c("Male","Female")
    ),
    natfare = factor(
      natfare,
      labels = c("Too little","About right","Too much")
    )
  )

save(gss2021,file="gss2021.RData")
