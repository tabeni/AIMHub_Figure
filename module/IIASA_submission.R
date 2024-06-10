
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
  inner_join(select(df_iiasa_temp,variable,unit), by = c("VARIABLE"="variable","UNIT"="unit"))

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

#df_iiasa_sub_check <- df_iiasa%>%
#  select(VARIABLE,UNIT)%>%
#  unique()

#export data--------------------------------------------------------------------




