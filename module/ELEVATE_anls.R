
#Title: Sub module for ELEVATE
#Name: Osamu Nishiura
#Date: 2024/6/1

#Package -----------------------------------------------------------------------

#Setting------------------------------------------------------------------------
dir_fig1<-"ELEVATE"

#Directory preparation----------------------------------------------------------
if (dir.exists(paste("../output/figure/",dir_fig1,sep="")) == "FALSE") {dir.create(paste("../output/figure/",dir_fig1,sep=""), recursive = T)}

#data import--------------------------------------------------------------------
df_iamc_TT<-bind_rows(df_iamc,
  rgdx.param(paste(df_path[df_path$name=="IAMC",]$path,"global_17_IAMC_TT.gdx",sep=""), "IAMC_Template")%>%
  filter(str_detect(VEMF, paste(df_filter$VEMF,collapse="|")),
         str_detect(YEMF, paste(df_filter$YEMF,collapse="|")),
         str_detect(REMF, paste(df_filter$REMF,collapse="|")),
         SCENARIO %in% df_sce[[1]])%>%
  left_join(df_sce)%>%
  mutate(year = as.numeric(as.character(YEMF)),
         SCENARIO = factor(SCENARIO,levels = df_sce[[1]]),
         SCENARIO2 = factor(SCENARIO2,levels = df_sce[[2]])))


#Figures------------------------------------------------------------------------

#Carbon price and CO2 emissions ------------------------------------------------

p <- df_iamc_TT%>%
  filter(YEMF!="2005",YEMF!="2010",YEMF!="2015",VEMF=="Emi_CO2_Ene_and_Ind_Pro",
         SCENARIO=="SSP2_BaU_NoCC_No"|SCENARIO=="SSP2_2020NDC_CONT5_NoCC_No"|SCENARIO=="SSP2_2020NDC_NZE_NoCC_No"|SCENARIO=="SSP2_NZE_NoCC_No"|SCENARIO=="SSP2_CurPol_CONT4_NoCC_No"|SCENARIO=="SSP2_2020NDC_NZE_NoCC")%>%
  ggplot(aes(x=YEMF , y=IAMC_Template/1000, color = SCENARIO,group = SCENARIO))
p <- p  + facet_wrap( ~ REMF,scales="free")
p <- p + geom_line(linewidth=0.8)
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + labs(x="Year", y="Carboin price (US$)",color="Scenario") 
p <- p + my_theme  
p <- p + labs(x="Year", y="CO2 emission (Gt-CO2/year)", fill = "")

png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"Emission_FFINDCO2.png",sep="/"), width =6000, height = 4000,res = 300)
print(p)
dev.off()


p <- df_iamc_TT%>%
  filter(YEMF!="2005",YEMF!="2010",YEMF!="2015",VEMF=="Emi_CO2",
         SCENARIO=="SSP2_BaU_NoCC_No"|SCENARIO=="SSP2_2020NDC_CONT5_NoCC_No"|SCENARIO=="SSP2_2020NDC_NZE_NoCC_No"|SCENARIO=="SSP2_NZE_NoCC_No"|SCENARIO=="SSP2_CurPol_CONT4_NoCC_No"|SCENARIO=="SSP2_2020NDC_NZE_NoCC")%>%
  ggplot(aes(x=YEMF , y=IAMC_Template/1000, color = SCENARIO,group = SCENARIO))
p <- p  + facet_wrap( ~ REMF,scales="free")
p <- p + geom_line(linewidth=0.8)
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + labs(x="Year", y="Carboin price (US$)",color="Scenario") 
p <- p + my_theme  
p <- p + labs(x="Year", y="CO2 emission (Gt-CO2/year)", fill = "")

png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"Emission_CO2.png",sep="/"), width =6000, height = 4000,res = 300)
print(p)
dev.off()

p <- df_iamc_TT%>%
  filter(YEMF!="2005",YEMF!="2010",YEMF!="2015",VEMF=="Emi_Kyo_Gas",
         SCENARIO=="SSP2_BaU_NoCC_No"|SCENARIO=="SSP2_2020NDC_CONT5_NoCC_No"|SCENARIO=="SSP2_2020NDC_NZE_NoCC_No"|SCENARIO=="SSP2_NZE_NoCC_No"|SCENARIO=="SSP2_CurPol_CONT4_NoCC_No"|SCENARIO=="SSP2_2020NDC_NZE_NoCC")%>%
  ggplot(aes(x=YEMF , y=IAMC_Template/1000, color = SCENARIO,group = SCENARIO))
p <- p  + facet_wrap( ~ REMF,scales="free")
p <- p + geom_line(linewidth=0.8)
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + labs(x="Year", y="Carboin price (US$)",color="Scenario") 
p <- p + my_theme  
p <- p + labs(x="Year", y="GHG emission (Gt-CO2/year)", fill = "")

png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"Emission_GHG.png",sep="/"), width = 6000, height = 4000,res = 300)
print(p)
dev.off()




p <- df_iamc_TT%>%
  filter(YEMF!="2005",YEMF!="2010",YEMF!="2015",VEMF=="Prc_Car",REMF!="World",
         SCENARIO=="SSP2_BaU_NoCC_No"|SCENARIO=="SSP2_2020NDC_CONT5_NoCC_No"|SCENARIO=="SSP2_2020NDC_NZE_NoCC_No"|SCENARIO=="SSP2_NZE_NoCC_No"|SCENARIO=="SSP2_CurPol_CONT4_NoCC_No"|SCENARIO=="SSP2_2020NDC_NZE_NoCC")%>%
  ggplot(aes(x=YEMF , y=IAMC_Template, color = SCENARIO,group = SCENARIO))
p <- p  + facet_wrap( ~ REMF,scales="free")
p <- p + geom_line(linewidth=0.8)
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + labs(x="Year", y="Carboin price (US$)",color="Scenario") 
p <- p + my_theme  
p <- p + labs(x="Year", y="Carbon Price", fill = "")

png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"Carbon_Price.png",sep="/"), width = 6000, height = 4000,res = 300)
print(p)
dev.off()






