
#Ttile: visualize AIMHub output
#Date: 2023/6/13
#Name: Osamu Nishiura

# ------------------------------------------------------------------------------
#Setting------------------------------------------------------------------------
#SCENARIO
df_sce <- data.frame(c("SSP2_BaU_NoCC","SSP2_500C_CACN_NoCC","SSP2_500C_CACN_DAC_NoCC","SSP2_500C_CACN_Synf_NoCC","SSP2_500C_CACN_Synf_BioLim_NoCC","SSP2_500C_CACN_Synf_H2LowCost_NoCC"),
                     c("Baseline","1.5C_NoDAC","1.5C_DAC","1.5C_SYN","1.5C_SYN_BioLim","1.5C_SYN_H2LowCost"))

colnames(df_sce) <- c("SCENARIO","SCENARIO2")
v_sce <- df_sce[[2]]
names(v_sce) <- df_sce[[1]]

#Package load-------------------------------------------------------------------

library(tidyverse)
library(gridExtra)
library(gdxrrw)
library(ggpattern)

#Directory Preparation----------------------------------------------------------

if (dir.exists("../output/figure/main") == "FALSE") {dir.create("../output/figure/main", recursive = T)}
if (dir.exists("../output/table/main") == "FALSE") {dir.create("../output/table/main", recursive = T)}
if (dir.exists("../output/figure/other") == "FALSE") {dir.create("../output/figure/other", recursive = T)}
if (dir.exists("../module") == "FALSE") {dir.create("../module")}

#define import-----------------------------------------------------------------

df_path    <- read_csv("../define/path.csv", locale = locale(encoding = "shift-jis"))
df_filter  <- read_csv("../define/filter.csv", locale = locale(encoding = "shift-jis"))
df_pri     <- read_csv("../define/primary_energy.csv", locale = locale(encoding = "shift-jis"))
df_sec     <- read_csv("../define/secondary_energy.csv", locale = locale(encoding = "shift-jis"))
df_fin     <- read_csv("../define/final_energy.csv", locale = locale(encoding = "shift-jis"))
df_emi     <- read_csv("../define/emission.csv", locale = locale(encoding = "shift-jis"))
df_val     <- read_csv("../define/value_added.csv", locale = locale(encoding = "shift-jis"))
df_lan     <- read_csv("../define/landuse.csv", locale = locale(encoding = "shift-jis"))

my_theme<-theme(
  panel.background = element_rect(fill = "transparent", colour = "black"),
  panel.grid = element_blank(),
  strip.background = element_blank(),
  legend.key = element_blank(),
  strip.text.y = element_text(size = 15), 
  strip.text.x = element_text(size = 15),
  axis.title = element_text(size = 25),
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 20),
  axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 15),
  axis.text.y = element_text(size = 20)
)
#legend.key.size = unit(1, 'cm'),legend.position ="right",

#Data import--------------------------------------------------------------------

df_iamc<-rgdx.param(paste(df_path[df_path$name=="IAMC",]$path,"global_17_IAMC.gdx",sep="/"), "IAMC_Template")%>%
  filter(str_detect(VEMF, paste(df_filter$VEMF,collapse="|")),
         str_detect(YEMF, paste(df_filter$YEMF,collapse="|")),
         str_detect(REMF, paste(df_filter$REMF,collapse="|")),
         SCENARIO %in% df_sce[[1]])%>%
  left_join(df_sce)%>%
  mutate(year = as.numeric(as.character(YEMF)),
         SCENARIO = factor(SCENARIO,levels = df_sce[[1]]),
         SCENARIO2 = factor(SCENARIO2,levels = df_sce[[2]]))

#Figures------------------------------------------------------------------------
#GDP and Consumption loss-------------------------------------------------------

p <- df_iamc%>%
  filter(VEMF=="Pol_Cos_GDP_Los_rat"|VEMF=="Pol_Cos_Cns_Los_rat"|VEMF=="Pol_Cos_Equ_Var_rat",REMF=="World",YEMF!="2010",YEMF!="2015")%>%
  ggplot(aes(x=year , y = IAMC_Template, color = SCENARIO,group = SCENARIO)) 
p <- p + facet_wrap(. ~ VEMF, scales="free", nrow=1,labeller = as_labeller(c(Pol_Cos_GDP_Los_rat="GDP",Pol_Cos_Cns_Los_rat="Consumption",Pol_Cos_Equ_Var_rat="Equivalent variation")))
p <- p + geom_line()
p <- p + scale_color_hue(labels = v_sce)
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + labs(x="Year", y="Economic loss (%)",color="Scenarios") 
p <- p + my_theme
png(paste(df_path[df_path$name=="figure",]$path,"main/GDP_HH_loss.png",sep="/"), width = 5000, height = 2000,res = 300)
print(p)
dev.off()

df_iamc%>%
  filter(YEMF=="2050",VEMF=="Pol_Cos_GDP_Los_rat"|VEMF=="Pol_Cos_Cns_Los_rat"|VEMF=="Pol_Cos_Equ_Var_rat",REMF=="World")%>%
  select(c("VEMF","SCENARIO2","IAMC_Template"))%>%
  pivot_wider(names_from = VEMF, values_from = IAMC_Template)%>%
  write_csv(paste(df_path[df_path$name=="table",]$path,"main/Economic_loss.csv",sep="/"))

p <- df_iamc%>%
  filter(YEMF=="2050",YEMF!="2015",VEMF=="Pol_Cos_GDP_Los_rat"|VEMF=="Pol_Cos_Cns_Los_rat"|VEMF=="Pol_Cos_Equ_Var_rat")%>%
  ggplot(aes(x=REMF , y=IAMC_Template ))
p <- p + facet_wrap(. ~ VEMF, scales="free", nrow=1,labeller = as_labeller(c(Pol_Cos_GDP_Los_rat="GDP",Pol_Cos_Cns_Los_rat="Consumption",Pol_Cos_Equ_Var_rat="Equivalent variation")))
p <- p  + geom_bar(stat="identity",position = "dodge",  aes(fill=SCENARIO)) 
#p <- p + scale_fill_hue(labels = df_sce[[2]])
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p　+ my_theme 
p <- p + labs(x="Year", y="Synthetic fuel production (EJ/year)", fill = "Sourse")
png(paste(df_path[df_path$name=="figure",]$path,"main/Economic_loss_region.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*1800+1000, height = 3000,res = 300)
print(p)
dev.off()

#Carbon price-------------------------------------------------------------------
p <- df_iamc%>%
  filter(VEMF=="Prc_Car",REMF=="World",YEMF!="2010",YEMF!="2015")%>%
  ggplot(aes(x=year , y = IAMC_Template, color = SCENARIO,group = SCENARIO)) 
#p <- p + facet_wrap(. ~ VEMF, scales="free", nrow=1,labeller = as_labeller(c(Pol_Cos_GDP_Los_rat="GDP",Pol_Cos_Cns_Los_rat="Consumption",Pol_Cos_Equ_Var_rat="Equivalent variation")))
p <- p + geom_line()
p <- p + scale_color_hue(labels = v_sce)
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + labs(x="Year", y="Carboin price (US$)",color="Scenarios") 
p <- p + my_theme
png(paste(df_path[df_path$name=="figure",]$path,"main/Carbon_price.png",sep="/"), width = 3000, height = 2000,res = 300)
print(p)
dev.off()

#commodity price----------------------------------------------------------------

p <- df_iamc%>%
  filter(VEMF=="Prc_Agr_NonEneCro_and_Liv_Ind"|VEMF=="Prc_Fin_Ene_Res_Ele"|VEMF=="Prc_Fin_Ene_Res_Gas_Nat_Gas"|VEMF=="Prc_Fin_Ene_Res_Liq_Bio"|VEMF=="Prc_Fin_Ene_Res_Liq_Hyd_syn"|VEMF=="Prc_Fin_Ene_Res_Liq_Oil"|VEMF=="Prc_Fin_Ene_Res_SolidsCoa",REMF=="World",YEMF!="2010",YEMF!="2015")%>%
  ggplot(aes(x=year , y = IAMC_Template, color = SCENARIO,group = SCENARIO)) 
#p <- p + facet_wrap(. ~ VEMF, scales="free", nrow=1,labeller = as_labeller(c(Pol_Cos_GDP_Los_rat="GDP",Pol_Cos_Cns_Los_rat="Consumption",Pol_Cos_Equ_Var_rat="Equivalent variation")))
p <- p + facet_wrap(. ~ VEMF, scales="free", nrow=2)
p <- p + geom_line()
p <- p + scale_color_hue(labels = v_sce)
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + labs(x="Year", y="Commodity price",color="Scenarios") 
p <- p + my_theme
png(paste(df_path[df_path$name=="figure",]$path,"main/price.png",sep="/"), width = 6000, height = 3000,res = 300)
print(p)
dev.off()

df_iamc%>%
  filter(VEMF=="Prc_Agr_NonEneCro_and_Liv_Ind"|VEMF=="Prc_Fin_Ene_Res_Ele"|VEMF=="Prc_Fin_Ene_Res_Gas_Nat_Gas"|VEMF=="Prc_Fin_Ene_Res_Liq_Bio"|VEMF=="Prc_Fin_Ene_Res_Liq_Hyd_syn"|VEMF=="Prc_Fin_Ene_Res_Liq_Oil"|VEMF=="Prc_Fin_Ene_Res_SolidsCoa",REMF=="World",YEMF=="2050")%>%  
  select(c("VEMF","SCENARIO2","IAMC_Template"))%>%
  pivot_wider(names_from = VEMF, values_from = IAMC_Template)%>%
  write_csv(paste(df_path[df_path$name=="table",]$path,"main/Price.csv",sep="/"))






#Primary Energy-----------------------------------------------------------------

p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(VEMF %in% df_pri[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_pri[1,]))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce))
p <- p  + geom_bar(stat="identity",aes(fill=VEMF)) 
p <- p  + scale_fill_manual(values = df_pri[4,], labels = df_pri[2,])
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p + labs(x="Year", y="Primary energy (EJ/year)", fill = "Source")
p <- p　+ my_theme 
png(paste(df_path[df_path$name=="figure",]$path,"main/primary_energy.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*1500+1000, height = 3000,res = 300)
print(p)
dev.off()

df_iamc%>%
  filter(YEMF=="2050",REMF=="World")%>%
  filter(VEMF %in% df_pri[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_pri[1,]))%>%
  select(c("VEMF","SCENARIO2","IAMC_Template"))%>%
  pivot_wider(names_from = VEMF, values_from = IAMC_Template)%>%
  write_csv(paste(df_path[df_path$name=="table",]$path,"main/primary_energy.csv",sep="/"))


#Secoundary Energy--------------------------------------------------------------
#Electricity
p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(str_detect(VEMF, "^Sec_Ene_Ele_"))%>%
  filter(VEMF %in% df_sec[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_sec[1,]))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce))
p <- p  + geom_bar(stat="identity",aes(fill=VEMF))
p <- p  + scale_fill_manual(values = df_sec[4,], labels = df_sec[2,])
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p　+ my_theme 
p <- p + labs(x="Year", y="Power generation (EJ/year)", fill = "Source")
png(paste(df_path[df_path$name=="figure",]$path,"main/Electricity.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*1500+1000, height = 3000,res = 300)
print(p)
dev.off()

df_iamc%>%
  filter(YEMF=="2050",REMF=="World")%>%
  filter(str_detect(VEMF, "^Sec_Ene_Ele_"))%>%
  filter(VEMF %in% df_sec[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_sec[1,]))%>%
  select(c("VEMF","SCENARIO2","IAMC_Template"))%>%
  pivot_wider(names_from = VEMF, values_from = IAMC_Template)%>%
  write_csv(paste(df_path[df_path$name=="table",]$path,"main/electricity.csv",sep="/"))


#liquid
p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(str_detect(VEMF, "^Sec_Ene_Liq_"))%>%
  filter(VEMF %in% df_sec[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_sec[1,]))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce))
p <- p  + geom_bar(stat="identity",aes(fill=VEMF))
p <- p  + scale_fill_manual(values = df_sec[4,], labels = df_sec[2,])
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p　+ my_theme 
p <- p + labs(x="Year", y="Liquid fuel production (EJ/year)", fill = "Source")
png(paste(df_path[df_path$name=="figure",]$path,"main/Liquid_fuel.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*1500+1000, height = 3000,res = 300)
print(p)
dev.off()

df_iamc%>%
  filter(YEMF=="2050",REMF=="World")%>%
  filter(str_detect(VEMF, "^Sec_Ene_Liq_"))%>%
  filter(VEMF %in% df_sec[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_sec[1,]))%>%
  select(c("VEMF","SCENARIO2","IAMC_Template"))%>%
  pivot_wider(names_from = VEMF, values_from = IAMC_Template)%>%
  write_csv(paste(df_path[df_path$name=="table",]$path,"main/Liquid_fuel.csv",sep="/"))




#Hydrogen
p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(str_detect(VEMF, "^Sec_Ene_Hyd_"))%>%
  filter(VEMF %in% df_sec[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_sec[1,]))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fixed")
p <- p  + geom_bar(stat="identity",aes(fill=VEMF)) 
p <- p  + scale_fill_manual(values = df_sec[4,], labels = df_sec[2,])
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p　+ my_theme 
p <- p + labs(x="Year", y="Power generation (EJ/year)", fill = "Source")
png(paste(df_path[df_path$name=="figure",]$path,"main/Hydrogen.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*1500+1000, height = 3000,res = 300)
print(p)
dev.off()

df_iamc%>%
  filter(YEMF=="2050",REMF=="World")%>%
  filter(str_detect(VEMF, "^Sec_Ene_Hyd_"))%>%
  filter(VEMF %in% df_sec[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_sec[1,]))%>%
  select(c("VEMF","SCENARIO2","IAMC_Template"))%>%
  pivot_wider(names_from = VEMF, values_from = IAMC_Template)%>%
  write_csv(paste(df_path[df_path$name=="table",]$path,"main/hydrogen.csv",sep="/"))




#Synfuel
p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015",VEMF=="Sec_Ene_Liq_Hyd_syn")%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
#p <- p  + facet_grid( ~ SCENARIO ,scales="fixed")
p <- p  + geom_bar(stat="identity") 
#p <- p  + scale_fill_manual(values = df_sec[4,], labels = df_sec[2,])
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p　+ my_theme 
p <- p + labs(x="Year", y="Synthetic fuel production (EJ/year)", fill = "Source")
png(paste(df_path[df_path$name=="figure",]$path,"main/Synfuel.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*1000+1000, height = 3000,res = 300)
print(p)
dev.off()

p <- df_iamc%>%
  filter(YEMF=="2050",YEMF!="2015",VEMF=="Fin_Ene_Tra_Liq_Hyd_syn"|VEMF=="Fin_Ene_Tra")%>%
  pivot_wider(names_from = VEMF, values_from = IAMC_Template)%>%
  mutate(IAMC_Template=Fin_Ene_Tra_Liq_Hyd_syn*100/Fin_Ene_Tra)%>%
  ggplot(aes(x=REMF , y=IAMC_Template ))
#p <- p  + facet_grid( ~ SCENARIO ,scales="fixed")
p <- p  + geom_bar(stat="identity",position = "dodge",  aes(fill=SCENARIO)) 
#p <- p  + scale_fill_manual(values = df_sec[4,], labels = df_sec[2,])
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p　+ my_theme 
p <- p + labs(x="Year", y="Synthetic fuel production (EJ/year)", fill = "Source")
png(paste(df_path[df_path$name=="figure",]$path,"main/Synfuel_region.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*1000+1000, height = 3000,res = 300)
print(p)
dev.off()

#Final Energy-------------------------------------------------------------------
#Total

p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(!(str_detect(VEMF, "^Fin_Ene_Ind_|^Fin_Ene_Tra_|^Fin_Ene_Bui_")))%>%
  filter(VEMF %in% df_fin[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_fin[1,]))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce))
p <- p  + geom_bar(stat="identity",aes(fill=VEMF)) 
p <- p  + scale_fill_manual(values = df_fin[4,], labels = df_fin[2,])
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p　+ my_theme 
p <- p  + labs(x="Year", y="Final energy (EJ/year)", fill = "fuel")
png(paste(df_path[df_path$name=="figure",]$path,"main/Final_energy.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*1500+1000, height = 3000,res = 300)
print(p)
dev.off()

df_iamc%>%
  filter(YEMF=="2050",REMF=="World")%>%
  filter(!(str_detect(VEMF, "^Fin_Ene_Ind_|^Fin_Ene_Tra_|^Fin_Ene_Bui_")))%>%
  filter(VEMF %in% df_fin[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_fin[1,]))%>%
  select(c("VEMF","SCENARIO2","IAMC_Template"))%>%
  pivot_wider(names_from = VEMF, values_from = IAMC_Template)%>%
  write_csv(paste(df_path[df_path$name=="table",]$path,"main/Final_energy.csv",sep="/"))

p <- df_iamc%>%
  filter(VEMF=="Fin_Ene_Ele"|VEMF=="Fin_Ene_Liq_Oil"|VEMF=="Fin_Ene_Liq_Hyd_syn"|VEMF=="Fin_Ene_Liq_Bio"|VEMF=="Fin_Ene_Gas"|VEMF=="Fin_Ene_Gas_Hyd_syn"|VEMF=="Fin_Ene_Hyd",REMF=="World",YEMF!="2010",YEMF!="2015")%>%
  ggplot(aes(x=year , y = IAMC_Template, color = SCENARIO,group = SCENARIO)) 
#p <- p + facet_wrap(. ~ VEMF, scales="free", nrow=1,labeller = as_labeller(c(Pol_Cos_GDP_Los_rat="GDP",Pol_Cos_Cns_Los_rat="Consumption",Pol_Cos_Equ_Var_rat="Equivalent variation")))
p <- p + facet_wrap(. ~ VEMF, scales="free", nrow=2)
p <- p + geom_line()
p <- p + scale_color_hue(labels = v_sce)
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + labs(x="Year", y="Energy consumption (EJ/yr)",color="Scenarios") 
p <- p + my_theme
png(paste(df_path[df_path$name=="figure",]$path,"main/final_energy_com.png",sep="/"), width = 5000, height = 3000,res = 300)
print(p)
dev.off()



#Transport

p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(str_detect(VEMF, "^Fin_Ene_Tra_"))%>%
  filter(VEMF %in% df_fin[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_fin[1,]))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce))
p <- p  + geom_bar(stat="identity",aes(fill=VEMF))
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p  + scale_fill_manual(values = df_fin[4,], labels = df_fin[2,])
p <- p　+ my_theme 
p <- p  + labs(x="Year", y="Final energy (EJ/year)", fill = "fuel")
png(paste(df_path[df_path$name=="figure",]$path,"main/Final_energy_transport.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*1500+1000, height = 3000,res = 300)
print(p)
dev.off()

df_iamc%>%
  filter(YEMF=="2050",REMF=="World")%>%
  filter(str_detect(VEMF, "^Fin_Ene_Tra_"))%>%
  filter(VEMF %in% df_fin[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_fin[1,]))%>%
  select(c("VEMF","SCENARIO2","IAMC_Template"))%>%
  pivot_wider(names_from = VEMF, values_from = IAMC_Template)%>%
  write_csv(paste(df_path[df_path$name=="table",]$path,"main/Final_energy_transport.csv",sep="/"))


#Industry

p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(str_detect(VEMF, "^Fin_Ene_Ind_"))%>%
  filter(VEMF %in% df_fin[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_fin[1,]))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce))
p <- p  + geom_bar(stat="identity",aes(fill=VEMF)) 
p <- p  + scale_fill_manual(values = df_fin[4,], labels = df_fin[2,])
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p　+ my_theme 
p <- p  + labs(x="Year", y="Final energy (EJ/year)", fill = "fuel")
png(paste(df_path[df_path$name=="figure",]$path,"main/Final_energy_industry.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*1500+1000, height = 3000,res = 300)
print(p)
dev.off()

df_iamc%>%
  filter(YEMF=="2050",REMF=="World")%>%
  filter(str_detect(VEMF, "^Fin_Ene_Ind_"))%>%
  filter(VEMF %in% df_fin[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_fin[1,]))%>%
  select(c("VEMF","SCENARIO2","IAMC_Template"))%>%
  pivot_wider(names_from = VEMF, values_from = IAMC_Template)%>%
  write_csv(paste(df_path[df_path$name=="table",]$path,"main/Final_energy_industry.csv",sep="/"))



#Residential and commercial

p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(str_detect(VEMF, "^Fin_Ene_Bui_Res_and_Com"))%>%
  filter(VEMF %in% df_fin[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_fin[1,]))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce))
p <- p  + geom_bar(stat="identity",aes(fill=VEMF)) 
p <- p  + scale_fill_manual(values = df_fin[4,], labels = df_fin[2,])
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p　+ my_theme 
p <- p  + labs(x="Year", y="Final energy (EJ/year)", fill = "fuel")
png(paste(df_path[df_path$name=="figure",]$path,"main/Final_energy_building.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*1500+1000, height = 3000,res = 300)
print(p)
dev.off()

df_iamc%>%
  filter(YEMF=="2050",REMF=="World")%>%
  filter(str_detect(VEMF, "^Fin_Ene_Bui_Res_and_Com"))%>%
  filter(VEMF %in% df_fin[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_fin[1,]))%>%
  select(c("VEMF","SCENARIO2","IAMC_Template"))%>%
  pivot_wider(names_from = VEMF, values_from = IAMC_Template)%>%
  write_csv(paste(df_path[df_path$name=="table",]$path,"main/residential_commercial.csv",sep="/"))


#Value added--------------------------------------------------------------------

p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(VEMF %in% df_val[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_val[1,]))%>%
  pivot_wider(names_from = SCENARIO, values_from = IAMC_Template)%>%
  mutate(SSP2_500C_CACN_NoCC = (SSP2_500C_CACN_NoCC - SSP2_BaU_NoCC)*100/SSP2_BaU_NoCC,
         SSP2_500C_CACN_DAC_NoCC = (SSP2_500C_CACN_DAC_NoCC - SSP2_BaU_NoCC)*100/SSP2_BaU_NoCC,
         SSP2_500C_CACN_Synf_NoCC = (SSP2_500C_CACN_Synf_NoCC - SSP2_BaU_NoCC)*100/SSP2_BaU_NoCC,
         SSP2_500C_CACN_Synf_Subs_NoCC = (SSP2_500C_CACN_Synf_Subs_NoCC - SSP2_BaU_NoCC)*100/SSP2_BaU_NoCC)%>%
  select(-c("SSP2_BaU_NoCC"))%>%
  pivot_longer(cols = -c(VEMF, REMF, YEMF, year), names_to = "SCENARIO", values_to = "IAMC_Template")%>%
  ggplot(aes(x=year , y = IAMC_Template, color = SCENARIO,group = SCENARIO)) 
p <- p + facet_wrap(. ~ VEMF, scales="free", nrow=2)
p <- p + geom_line()
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + labs(x="Year", y="Economic loss (%)",color="Scenarios") 
p <- p + my_theme
png(paste(df_path[df_path$name=="figure",]$path,"main/value_added_loss.png",sep="/"), width = 6000, height = 3000,res = 300)
print(p)
dev.off()

#CO2 emission-------------------------------------------------------------------

p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(VEMF %in% df_emi[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_emi[1,]))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce))
p <- p  + geom_bar(stat="identity",aes(fill=VEMF)) 
p <- p  + scale_fill_manual(values = c("green4","blue","red","yellow","purple","chocolate","navy","grey","pink","skyblue","gold"))
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p　+ my_theme 
p <- p + labs(x="Year", y="CO2 emission", fill = "source")
png(paste(df_path[df_path$name=="figure",]$path,"main/emission.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*1500+1000, height = 3000,res = 300)
print(p)
dev.off()


df_iamc%>%
  filter(YEMF=="2050",REMF=="World")%>%
  filter(VEMF %in% df_emi[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_emi[1,]))%>%
  select(c("VEMF","SCENARIO2","IAMC_Template"))%>%
  pivot_wider(names_from = VEMF, values_from = IAMC_Template)%>%
  write_csv(paste(df_path[df_path$name=="table",]$path,"main/emission_.csv",sep="/"))




























#TRB----------------------------------------------------------------------------










