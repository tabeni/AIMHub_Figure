
#Title: Sub module for IIASA submission
#Name: Osamu Nishiura
#Date: 2024/6/7

#Package -----------------------------------------------------------------------

library(tidyverse)
library(readxl)

#Setting------------------------------------------------------------------------

dir_tab1<-"IIASA"

#Directory preparation----------------------------------------------------------
if (dir.exists(paste("../output/table/",dir_tab1,sep="")) == "FALSE") {dir.create(paste("../output/table/",dir_tab1,sep=""), recursive = T)}

#data import--------------------------------------------------------------------

df_iiasa_temp <- read_xlsx("../../template/ELEVATE_TEMPLATE.xlsx", sheet= "variable")
df_iiasa <- read_csv("../../output/iiasa_database/txt/global_17_IAMC.csv")

#Data processing----------------------------------------------------------------

df_iiasa_sub <- df_iiasa%>%
  inner_join(select(df_iiasa_temp,variable,unit), by = c("VARIABLE"="variable","UNIT"="unit"))%>%
  inner_join(df_sce, by = c("SCENARIO"="SCENARIO"))%>%
  select("MODEL","SCENARIO2","REGION","VARIABLE","UNIT","2010","2015","2020","2025","2030","2035","2040","2045","2050","2055","2060","2065","2070","2075","2080","2085","2090","2095","2100")%>%
  rename(SCENARIO=SCENARIO2)


df_iiasa_sub_check <- bind_rows(df_iiasa%>%
  inner_join(select(df_iiasa_temp,variable,unit), by = c("VARIABLE"="variable","UNIT"="unit"))%>%
  select(VARIABLE,UNIT)%>%
  unique()%>%
  mutate(check="reported"),
  df_iiasa%>%
  anti_join(select(df_iiasa_temp,variable,unit), by = c("VARIABLE"="variable","UNIT"="unit"))%>%
  select(VARIABLE,UNIT)%>%
  unique()%>%
  inner_join(select(df_iiasa_temp,variable,unit), by = c("VARIABLE"="variable"))%>%
  rename("VARIABLE"=VARIABLE,"UNIT"=UNIT,"check"=unit))%>%
  bind_rows(df_iiasa%>%
  filter(!VARIABLE %in% df_iiasa_temp$variable)%>%
  select(VARIABLE,UNIT)%>%
  unique()%>%
  mutate(check="not exist in the template"))


df_iiasa_sub2 <- df_iiasa%>%
  inner_join(select(df_iiasa_temp,variable,unit), by = c("VARIABLE"="variable"))%>%
  filter(VARIABLE!="Price|Agriculture|Corn|Index",VARIABLE!="Price|Agriculture|Soybean|Index",VARIABLE!="Price|Agriculture|Wheat|Index")%>%
  select("MODEL","SCENARIO","REGION","VARIABLE","unit","2010","2015","2020","2025","2030","2035","2040","2045","2050","2055","2060","2065","2070","2075","2080","2085","2090","2095","2100")%>%
  rename(UNIT=unit)%>%
  inner_join(df_sce, by = c("SCENARIO"="SCENARIO"))%>%
  select("MODEL","SCENARIO2","REGION","VARIABLE","UNIT","2010","2015","2020","2025","2030","2035","2040","2045","2050","2055","2060","2065","2070","2075","2080","2085","2090","2095","2100")%>%
  rename(SCENARIO=SCENARIO2)

#df_iiasa_sub_check <- df_iiasa%>%
#  select(VARIABLE,UNIT)%>%
#  unique()

#export data--------------------------------------------------------------------

write_csv(df_iiasa_sub,paste(df_path[df_path$name=="table",]$path,paste(dir_tab1,"IIASA_temp.csv",sep="/") ,sep="/"))
write_csv(df_iiasa_sub2,paste(df_path[df_path$name=="table",]$path,paste(dir_tab1,"IIASA_temp2.csv",sep="/") ,sep="/"))


