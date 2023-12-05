#packages load----------------------------------------------------------------------------------

library(tidyverse)
library(pipeR)
library(rlist)
igdx("C:/GAMS/38")
#file and parameter search with regular expressions---------------------------------------------
#-----------------------------------------------------------------------------------------------
#import_data_path     serch folder
#file_search          search file names by regular expressions
#param_search         search parameter names by regular expressions
#-----------------------------------------------------------------------------------------------

import_data_path<-"../../output/iiasa_database/gdx" 
file_search<-".gdx"
param_search<-"IAMC_Template"

#import file list-------------------------------------------------------------------------------

li_fi<-list.files(import_data_path,recursive=T, include.dirs=T, pattern=file_search)
li_fi<-unique(li_fi)%>%
  sort()
View(li_fi)

#data import------------------------------------------------------------------------------------

li_df<-list()
for(na_fi in li_fi){
  df_param<-gdxInfo(paste(import_data_path,na_fi,sep="/"), dump=F, returnList=F, returnDF=T)$parameters
  df_param<-filter(df_param, str_detect(name, param_search))
  for(na_param in df_param$name){
    param<-rgdx.param(paste(import_data_path,na_fi,sep="/"), na_param)%>%
      mutate(path=na_fi)
    li_df_k<-list(param)
    names(li_df_k)<-paste(na_fi,na_param,sep="///")
    li_df<-c(li_df,li_df_k)
  }}


#clear data when row number is 0----------------------------------------------------------------

li_df_k<-list.match(li_df,param_search)%>%
  list.clean(function(.) nrow(.)==0)

#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
#search parameters------------------------------------------------------------------------------

li_param<-names(li_df_k)%>%
  str_replace(".*///", "")%>%
  unique()%>%
  sort()

li_df_s<-list()
for(na_param in li_param){
  li_df_s_k<-list(list.match(li_df,paste("///",na_param,"$",sep="")))
  names(li_df_s_k)<-na_param
  li_df_s<-c(li_df_s,li_df_s_k)
}

#rm(li_df,li_df_k,li_df_s_k,param,li_param,na_fi,na_param)
rm(li_df_k,li_df_s_k,param,li_param,na_fi,na_param)
View(li_df_s)

