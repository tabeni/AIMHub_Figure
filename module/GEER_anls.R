#packages load----------------------------------------------------------------------------------

library(tidyverse)
library(pipeR)
library(rlist)
library(gridExtra)
library(ggpattern)
library(lemon)
library(gdxrrw)
igdx("C:/GAMS/38")

output_path_fig<-"../output/fig"
output_path_tab<-"../output/tab"

#data import-------------------------------------------------------------------
df_iamc<-rgdx.param(paste("../../output/iiasa_database/gdx","global_17_IAMC.gdx",sep="/"), "IAMC_Template")%>%
  filter(str_detect(VEMF, c("^Prm_Ene|^Fin_Ene|^Pol_Cos|^Prc|^Emi_CO2|^Car_Seq|^GDP|^CNS")),str_detect(YEMF, c("2005|2010|2015|2020|2025|2030|2035|2040|2045|2050")),str_detect(REMF, c("USA|XE25|XER|TUR|XOC|CHN|IND|JPN|XSE|XSA|CAN|BRA|XLM|CIS|XME|XNF|XAF|World")))

df_iamc<-mutate(df_iamc,year=as.numeric(as.character(df_iamc$YEMF)))

var_names<-data.frame(unique(as.character(df_iamc$VEMF)))

df_ccsg<-data.frame(CCSgrade=c("grade1",  "grade2",  "grade3",  "grade4",  "grade5",  "grade6",  "grade7",  "grade8",  "grade9",  "grade10", "grade11"),
                    CCSres=factor(c("FF_land", "FF_land","FF_land","FF_land","FF_land","FF_sea","FF_sea","Aquifer_land", "FF_sea","FF_sea", "Aquifer_sea"),levels=c("Aquifer_sea","FF_sea","Aquifer_land","FF_land" )))

#CCS storage estimate
df_ccs  <- rgdx.param(paste("../data/CCS/","CCS_capacity_sr17.gdx",sep=""), "CO2_capacity_sr17_practical_lo")%>%
  left_join(rgdx.param(paste("../data/CCS/","CCS_capacity_sr17.gdx",sep=""), "CO2_capacity_sr17_practical_hi"))%>%
  left_join(df_ccsg)%>%
  group_by(CCSres,sr17)%>%
  summarise(ccs_hi=sum(CO2_capacity_sr17_practical_hi)/1000,
            ccs_lo=sum(CO2_capacity_sr17_practical_lo)/1000)%>%
  ungroup()
df_ccs<-df_ccs%>%
  bind_rows(df_ccs%>%
  group_by(CCSres)%>%
  summarise(ccs_hi=sum(ccs_hi),
            ccs_lo=sum(ccs_lo))%>%
  ungroup()%>%
  mutate(sr17="World"))%>%
  group_by(sr17)%>%
  summarise(ccs_hi=sum(ccs_hi),
            ccs_lo=sum(ccs_lo))
  
  

#Figures tables----------------------------------------------------------
#theme label scale_manual----------------------------------------------------------------

my_theme<-theme(
  panel.background = element_rect(fill = "transparent", colour = "black"),
  panel.grid = element_blank(),
  strip.background = element_blank(),
  legend.key = element_blank()
)


lb_sce <- c("SSP2_BaU_NoCC"="ベースライン",
            "SSP2_500C_CACN_NoCC"="１.５度_ＤＡＣなし",
            "SSP2_500C_CACN_DAC_NoCC "="１.５度_ＤＡＣあり")
lt_sce <- c("SSP2_BaU_NoCC"="dotted",
            "SSP2_500C_CACN_NoCC"="solid",
            "SSP2_500C_CACN_DAC_NoCC "="longdash")
v_re17<-c("USA","XE25","XER","TUR","XOC","CHN","IND","JPN","XSE","XSA","XLM","XME","CAN","CIS","BRA","XNF","XAF","World")

lb_r <- as_labeller(c( BRA = "ブラジル",CAN = "カナダ",CHN = "中国",IND = "インド",JPN = "日本",TUR = "トルコ",USA = "アメリカ",XER = "その他ヨーロッパ",XME = "中東",XOC = "オセアニア",XSA = "その他アジア",XSE = "東南アジア",XE25 = "ヨーロッパ25カ国",XLM = "その他南米",CIS = "旧ソビエト連邦",XNF = "北アフリカ",XAF = "その他アフリカ",World = "世界"))
lb_x_r<- c(BRA = "ブラジル",CAN = "カナダ",CHN = "中国",IND = "インド",JPN = "日本",TUR = "トルコ",USA = "アメリカ",XER = "その他ヨーロッパ",XME = "中東",XOC = "オセアニア",XSA = "その他アジア",XSE = "東南アジア",XE25 = "ヨーロッパ25カ国",XLM = "その他南米",CIS = "旧ソビエト連邦",XNF = "北アフリカ",XAF = "その他アフリカ",World = "世界",IDN = "インドネシア", KOR = "韓国", THA = "タイ", MYS = "マレーシア", VNM = "ベトナム")

lb_x_r2<- c(BRA = "Brazil",CAN = "Canada",CHN = "China",IND = "India",JPN = "Japan",TUR = "Turkey",USA = "United States",XER = "Rest of Europe",XME = "Middle East",XOC = "Oceania",XSA = "Rest of Asia",XSE = "Rest of Southeast Asia",XE25 = "EU25", XLM = "Rest of South America", CIS = "Former Soviet Union",XNF = "North Africa",XAF = "Rest of Africa",World = "World")

lb_x_r3<- c(BRA = "Brazil",CAN = "Canada",CHN = "China",IND = "India",JPN = "Japan",TUR = "Turkey",USA = "USA",XER = "Rest of EU",XME = "Middle East",XOC = "Oceania",XSA = "Rest of Asia",XSE = "Southeast Asia",XE25 = "EU25", XLM = "Rest of South America", CIS = "Former Soviet Union",XNF = "North Africa",XAF = "Rest of Africa",World = "World")

lb_y<-c("2020","2030","2040","2050")

lb_sc <- as_labeller(c("SSP2_BaU_NoCC"="ベースライン",
           "SSP2_500C_CACN_NoCC"="１.５度_ＤＡＣなし",
           "SSP2_500C_CACN_DAC_NoCC"="１.５度_ＤＡＣあり"))
lb_sc2 <- as_labeller(c("SSP2_BaU_NoCC"="Baseline",
                       "SSP2_500C_CACN_NoCC"="1.5c_w/oDAC",
                       "SSP2_500C_CACN_DAC_NoCC"="1.5c_w/DAC"))

fl_pri<-c(Prm_Ene_Bio_w_CCS="#bbffbb",
          Prm_Ene_Bio_wo_CCS="#66dd66",
          Prm_Ene_Geo  = "#886644", 
          Prm_Ene_Win  = "#8833ff",
          Prm_Ene_Solar  = "#ffee22",           
          Prm_Ene_Hyd  = "#0022ff",
          Prm_Ene_Nuc  = "#ff3300",
          Prm_Ene_Gas_w_CCS="#ccffff",
          Prm_Ene_Gas_wo_CCS="#66ccff", 
          Prm_Ene_Oil_w_CCS  = "#bbbbbb", 
          Prm_Ene_Oil_wo_CCS  = "#888888",
          Prm_Ene_Coa_w_CCS="#ffcc77",
          Prm_Ene_Coa_wo_CCS="#ee8833")

lb_pri<-c(Prm_Ene_Bio_w_CCS="バイオマス(CCSあり)",
          Prm_Ene_Bio_wo_CCS="バイオマス(CCSなし)",
          Prm_Ene_Geo  = "地熱",
          Prm_Ene_Win  = "風力",
          Prm_Ene_Solar  = "太陽光", 
          Prm_Ene_Hyd  = "水力",           
          Prm_Ene_Nuc  = "原子力",
          Prm_Ene_Gas_w_CCS="天然ガス(CCSあり)",
          Prm_Ene_Gas_wo_CCS="天然ガス(CCSなし)", 
          Prm_Ene_Oil_w_CCS  = "石油(CCSあり)", 
          Prm_Ene_Oil_wo_CCS  = "石油(CCSなし)", 
          Prm_Ene_Coa_w_CCS="石炭(CCSあり)",
          Prm_Ene_Coa_wo_CCS="石炭(CCSなし)")

lb_pri2<-c(Prm_Ene_Bio_w_CCS="Biomass w/CCS",
          Prm_Ene_Bio_wo_CCS="Biomass w/oCCS",
          Prm_Ene_Geo  = "Geothermal",
          Prm_Ene_Win  = "Windr",
          Prm_Ene_Solar  = "Solar", 
          Prm_Ene_Hyd  = "Hydro",           
          Prm_Ene_Nuc  = "Nuclear",
          Prm_Ene_Gas_w_CCS="Natural gas w/CCS",
          Prm_Ene_Gas_wo_CCS="Natural gas w/oCCS", 
          Prm_Ene_Oil_w_CCS  = "Oil w/CCS", 
          Prm_Ene_Oil_wo_CCS  = "Oil w/oCCS", 
          Prm_Ene_Coa_w_CCS="Coal w/CCS",
          Prm_Ene_Coa_wo_CCS="Coal w/oCCS")


lb_fin<-c(
Fin_Ene_Ele="電力",Fin_Ene_Gas="気体燃料",Fin_Ene_Heat="熱",Fin_Ene_Hyd="水素",Fin_Ene_Liq="液体燃料",Fin_Ene_Solids="固体燃料"
)
lb_fin2<-c(
  Fin_Ene_Ele="Electricity",Fin_Ene_Gas="Gas",Fin_Ene_Heat="Heat",Fin_Ene_Hyd="Hydrogen",Fin_Ene_Liq="Liquid",Fin_Ene_Solids="Solid"
)
fl_fin<-c(
  Fin_Ene_Ele="gold",Fin_Ene_Gas="skyblue",Fin_Ene_Heat="red",Fin_Ene_Hyd="navy",Fin_Ene_Liq="grey",Fin_Ene_Solids="chocolate"
)


#CO2 emission scenario----------------------------------------------------------------

p <- df_iamc%>%
  filter(VEMF=="Emi_CO2",REMF=="World",SCENARIO!="SSP2_500C_CACN_DAC_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC")%>%
  ggplot(aes(x=year , y = IAMC_Template/1000, group = SCENARIO,  linetype = SCENARIO)) 
p <- p + geom_line(size=1) 
p <- p + labs(x="年", y="二酸化炭素排出量 (Gt-CO2/年)",linetype="シナリオ") 
p <- p + my_theme 
p <- p + scale_linetype_manual(labels = c("SSP2_BaU_NoCC"="ベースライン","SSP2_500C_CACN_NoCC"="１．５度シナリオ"),values = c("SSP2_BaU_NoCC"="dotted","SSP2_500C_CACN_NoCC"="solid"))
p <- p + theme(legend.position ="right",strip.text.y = element_text(size = 28), strip.text.x = element_text(size = 28),axis.title = element_text(size = 25),legend.title= element_text(size = 28),legend.text= element_text(size = 28),axis.text.x = element_text(angle = 0, hjust = 0.5,size = 25),axis.text.y = element_text(size = 25), legend.key.size=unit(1.6, 'cm'),axis.title.x = element_blank())
png(paste(output_path_fig,"CO2_emission_path.png",sep="/"), width = 750, height = 400)
print(p)
dev.off()



p <- df_iamc%>%
  filter(VEMF=="Emi_CO2",REMF=="World",SCENARIO!="SSP2_500C_CACN_DAC_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC")%>%
  ggplot(aes(x=year , y = IAMC_Template/1000, group = SCENARIO,  linetype = SCENARIO)) 
p <- p + geom_line(size=1) 
p <- p + labs(x="年", y="二酸化炭素排出量 (Gt-CO2/年)",linetype="シナリオ") 
p <- p + my_theme 
p <- p + scale_linetype_manual(labels = c("SSP2_BaU_NoCC"="ベースライン","SSP2_500C_CACN_NoCC"="１．５度シナリオ"),values = c("SSP2_BaU_NoCC"="dotted","SSP2_500C_CACN_NoCC"="solid"))
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 28), strip.text.x = element_text(size = 28),axis.title = element_text(size = 25),legend.title= element_text(size = 28),legend.text= element_text(size = 28),axis.text.x = element_text(angle = 0, hjust = 0.5,size = 25),axis.text.y = element_text(size = 25), legend.key.size=unit(1.6, 'cm'),axis.title.x = element_blank())
png(paste(output_path_fig,"CO2_emission_path2.png",sep="/"), width = 600, height = 500)
print(p)
dev.off()

p <- df_iamc%>%
  filter(VEMF=="Emi_CO2",REMF=="World",SCENARIO!="SSP2_500C_CACN_DAC_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC")%>%
  ggplot(aes(x=year , y = IAMC_Template/1000, group = SCENARIO,  color = SCENARIO)) 
p <- p + geom_line(size=1.5) 
p <- p + labs(x="", y="CO2 emissio  (Gt-CO2/yr)",color="Scneario") 
p <- p + my_theme 
p <- p + scale_color_manual(labels = c("SSP2_BaU_NoCC"="baseline","SSP2_500C_CACN_NoCC"="1.5C"),values = c("SSP2_BaU_NoCC"="olivedrab","SSP2_500C_CACN_NoCC"="#F8766D"))
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 40), strip.text.x = element_text(size = 40),axis.title = element_text(size = 35),legend.title= element_text(size = 35),legend.text= element_text(size = 35),axis.text.x = element_text(angle = 0, hjust = 0.5,size = 35),axis.text.y = element_text(size = 35), legend.key.size=unit(1.6, 'cm'),axis.title.x = element_blank())
png(paste(output_path_fig,"CO2_emission_path3.png",sep="/"), width = 1000, height = 800)
print(p)
dev.off()


p <- df_iamc%>%
  filter(VEMF=="Emi_CO2",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF=="2050",REMF!="World")%>%
  ggplot(aes(x=REMF , y = IAMC_Template/1000, pattern = SCENARIO))
p <- p + geom_bar_pattern(stat="identity",
                          aes(pattern = SCENARIO),
                          position="dodge", 
                          pattern_fill = "black",
                          fill = "white",
                          color = "black",
                          pattern_color = NA,
                          pattern_spacing = 0.01,
                          pattern_density = 0.25)
p <- p + labs(x="地域", y="二酸化炭素排出量(Gt-CO2/年)",pattern="削減シナリオ") 
p <- p + scale_pattern_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5度_DACなし","SSP2_500C_CACN_DAC_NoCC"="1.5度_DACあり"),values=c( "none","crosshatch"))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + my_theme
#p <- p + scale_x_discrete(label=lb_x_r)
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 20), strip.text.x = element_text(size = 20),axis.title = element_text(size = 20),legend.title= element_text(size = 20),legend.text= element_text(size = 20),axis.text.x = element_text(angle = 90, hjust = 1,size = 18),axis.text.y = element_text(size = 18))
png(paste(output_path_fig,"emission_region.png",sep="/"), width = 700, height = 600)
print(p)
dev.off()
  
#Carbon price fig and tab---------------------------------------------------------------------
df_iamc%>%
  filter(VEMF=="Prc_Car",YEMF=="2050"|YEMF=="2040"|YEMF=="2030",SCENARIO=="SSP2_500C_CACN_NoCC"|SCENARIO=="SSP2_500C_CACN_DAC_NoCC",REMF=="World")%>%
  select(YEMF,SCENARIO,IAMC_Template)%>%
  spread(key = SCENARIO, value = IAMC_Template)%>%
  write.csv(paste(output_path_tab,"Carbon_price.csv",sep="/"))

p <- df_iamc%>%
  filter(VEMF=="Prc_Car",REMF=="World",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC")%>%
  ggplot(aes(x=year , y = IAMC_Template, group = SCENARIO, linetype = SCENARIO)) 
p <- p +geom_line(size=1.05)
p <- p + labs(x="", y="炭素価格 (ドル/t-CO2)",linetype="シナリオ") 
p <- p + scale_linetype_manual(labels = c("SSP2_500C_CACN_NoCC"="１.５度_ＤＡＣなし","SSP2_500C_CACN_DAC_NoCC"="１.５度_ＤＡＣあり"),values = c("SSP2_500C_CACN_NoCC"="solid","SSP2_500C_CACN_DAC_NoCC"="longdash"))
p <- p + my_theme
p <- p + theme(legend.position ="right",strip.text.y = element_text(size = 30), strip.text.x = element_text(size = 30),axis.title = element_text(size = 30),legend.title= element_text(size = 28),legend.text= element_text(size = 28),axis.text.x = element_text(angle = 0, hjust = 0.5,size = 28),axis.text.y = element_text(size = 28), legend.key.size=unit(1.6, 'cm'),axis.title.x = element_blank())
p <- p + ylim(0,225)
png(paste(output_path_fig,"Car_Prc.png",sep="/"), width = 900, height = 500)
print(p)
dev.off()




p <- df_iamc%>%
  filter(VEMF=="Prc_Car",REMF=="World",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC")%>%
  View()
  ggplot(aes(x=year , y = IAMC_Template, group = SCENARIO, color = SCENARIO)) 
p <- p +geom_line(size=1.5)
p <- p + labs(x="", y="Carbon price (USD/t-CO2)",color="Scenario") 
p <- p + scale_color_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5c_w/oDAC","SSP2_500C_CACN_DAC_NoCC"="1.5c_w/DAC"),values = c("SSP2_500C_CACN_DAC_NoCC"="#00BFC4","SSP2_500C_CACN_NoCC"="#F8766D"))
p <- p + my_theme
p <- p + theme(legend.position ="right",strip.text.y = element_text(size = 30), strip.text.x = element_text(size = 30),axis.title = element_text(size = 30),legend.title= element_text(size = 28),legend.text= element_text(size = 28),axis.text.x = element_text(angle = 0, hjust = 0.5,size = 28),axis.text.y = element_text(size = 28), legend.key.size=unit(1.6, 'cm'),axis.title.x = element_blank())
p <- p + ylim(0,225)
png(paste(output_path_fig,"Car_Prc2.png",sep="/"), width = 1000, height = 500)
print(p)
dev.off()

#nets -----------------------------------------------------------------------------------

p<-df_iamc%>%
  filter(VEMF=="Car_Seq_Dir_Air_Cap"|VEMF=="Car_Seq_Lan_Use_Aff"|VEMF=="Car_Seq_CCS_Bio",REMF=="World",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  mutate(VEMF2 = factor(VEMF, levels=c("Car_Seq_Dir_Air_Cap","Car_Seq_CCS_Bio","Car_Seq_Lan_Use_Aff")))%>%
  ggplot(aes(x=YEMF , y = abs(IAMC_Template)/1000 )) 
p<- p + facet_wrap( ~ SCENARIO, scales="fix",labeller = lb_sc) 
p <- p + geom_bar_pattern(stat="identity",
                          aes(pattern = VEMF2),
                          pattern_fill = "black",
                          fill = "white",
                          color = "black",
                          pattern_color = NA,
                          pattern_spacing = 0.015,
                          pattern_density = 0.18)
p <- p + labs(x="年", y="二酸化炭素回収量 (Gt-CO2/年)",pattern="二酸化炭素回収方法") 
p <- p + scale_pattern_manual(labels = c("Car_Seq_CCS_Bio"="BECCS","Car_Seq_Dir_Air_Cap"="DACCS","Car_Seq_Lan_Use_Aff"="植林"),values=c("stripe","none","crosshatch"))
p <- p + my_theme
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 20), strip.text.x = element_text(size = 20),axis.title = element_text(size = 20),legend.title= element_text(size = 20),legend.text= element_text(size = 20),axis.text.x = element_text(angle = 90, hjust = 1,size = 18),axis.text.y = element_text(size = 18))
png(paste(output_path_fig,"CO2_seq.png",sep="/"), width = 700, height = 700)
print(p)
dev.off()



p<-df_iamc%>%
  filter(VEMF=="Car_Seq_Dir_Air_Cap"|VEMF=="Car_Seq_Lan_Use_Aff"|VEMF=="Car_Seq_CCS_Bio",REMF=="World",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  mutate(VEMF2 = factor(VEMF, levels=c("Car_Seq_Dir_Air_Cap","Car_Seq_CCS_Bio","Car_Seq_Lan_Use_Aff")))%>%
  ggplot(aes(x=YEMF , y = abs(IAMC_Template)/1000 )) 
p<- p + facet_wrap( ~ SCENARIO, scales="fix",labeller = lb_sc) 
p <- p + geom_bar_pattern(stat="identity",
                          aes(pattern = VEMF2),
                          pattern_fill = "black",
                          fill = "white",
                          color = "black",
                          size = 0.9,
                          pattern_color = NA,
                          pattern_spacing = 0.020,
                          pattern_density = 0.15)
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + labs(x="年", y="二酸化炭素回収量 (Gt-CO2/年)",pattern="二酸化炭素\n回収方法") 
p <- p + scale_x_discrete(breaks=lb_y)
p <- p + scale_pattern_manual(labels = c("Car_Seq_CCS_Bio"="BECCS","Car_Seq_Dir_Air_Cap"="DAC","Car_Seq_Lan_Use_Aff"="植林"),values=c("stripe","none","crosshatch"))
p <- p + my_theme
p <- p + theme(legend.position ="right",strip.text.y = element_text(size = 38), strip.text.x = element_text(size = 45),axis.title = element_text(size = 38),legend.title= element_text(size = 38),legend.text= element_text(size = 38),axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 0.5,size = 34),axis.text.y = element_text(size = 34), legend.key.size=unit(1.5, 'cm'),axis.title.x = element_blank())

png(paste(output_path_fig,"CO2_seq2.png",sep="/"), width = 1600, height = 800)
print(p)
dev.off()


p<-df_iamc%>%
  filter(VEMF=="Car_Seq_Dir_Air_Cap"|VEMF=="Car_Seq_CCS_Bio",REMF=="World",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  mutate(VEMF2 = factor(VEMF, levels=c("Car_Seq_CCS_Bio","Car_Seq_Dir_Air_Cap")))%>%
  ggplot(aes(x=YEMF , y = abs(IAMC_Template)/1000 )) 
p<- p + facet_wrap( ~ SCENARIO, scales="fix",labeller = lb_sc) 
p <- p + geom_bar_pattern(stat="identity",
                          aes(pattern = VEMF2),
                          pattern_fill = "black",
                          fill = "white",
                          color = "black",
                          size = 0.9,
                          width=0.6,
                          pattern_color = NA,
                          pattern_spacing = 0.015,
                          pattern_density = 0.15)
p <- p + labs(x="年", y="二酸化炭素回収量 (Gt-CO2/年)",pattern="二酸化炭素回収方法") 
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + scale_x_discrete(breaks=lb_y)
p <- p + scale_pattern_manual(labels = c("Car_Seq_CCS_Bio"="BECCS","Car_Seq_Dir_Air_Cap"="DACCS"),values=c("stripe","none"))
p <- p + my_theme
p <- p + theme(legend.position ="right",strip.text.y = element_text(size = 45), strip.text.x = element_text(size = 45),axis.title = element_text(size = 38),legend.title= element_text(size = 45),legend.text= element_text(size = 45),axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 0.5,size = 40),axis.text.y = element_text(size = 40), legend.key.size=unit(1.5, 'cm'),axis.title.x = element_blank())
png(paste(output_path_fig,"CO2_seq4.png",sep="/"), width = 1600, height = 800)
print(p)
dev.off()


p<-df_iamc%>%
  filter(VEMF=="Car_Seq_Dir_Air_Cap"|VEMF=="Car_Seq_Lan_Use_Aff"|VEMF=="Car_Seq_CCS_Bio",REMF=="World",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  mutate(VEMF2 = factor(VEMF, levels=c("Car_Seq_Dir_Air_Cap","Car_Seq_CCS_Bio","Car_Seq_Lan_Use_Aff")))%>%
  ggplot(aes(x=YEMF , y = abs(IAMC_Template)/1000 )) 
p<- p + facet_wrap( ~ SCENARIO, scales="fix",labeller = lb_sc) 
p <- p + geom_bar(stat="identity",aes(fill = VEMF2),color="black",size=1.2)
p <- p + labs(x="年", y="二酸化炭素回収量 (Gt-CO2/年)",fill="二酸化炭素\n回収方法") 
p <- p + scale_fill_manual(labels = c("Car_Seq_CCS_Bio"="BECCS","Car_Seq_Dir_Air_Cap"="DAC","Car_Seq_Lan_Use_Aff"="植林"),values=c("cyan2","forestgreen","tomato"))
p <- p + scale_x_discrete(breaks=lb_y)
p <- p + my_theme
p <- p + theme(legend.position ="right",strip.text.y = element_text(size = 38), strip.text.x = element_text(size = 45),axis.title = element_text(size = 38),legend.title= element_text(size = 38),legend.text= element_text(size = 38),axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 0.5,size = 34),axis.text.y = element_text(size = 34), legend.key.size=unit(1.5, 'cm'),axis.title.x = element_blank())
png(paste(output_path_fig,"CO2_seq3.png",sep="/"), width = 1000, height = 1200)
print(p)
dev.off()



df_iamc%>%
  filter(VEMF=="Car_Seq_Dir_Air_Cap"|VEMF=="Car_Seq_Lan_Use_Aff"|VEMF=="Car_Seq_CCS_Bio",REMF=="World",SCENARIO!="SSP2_BaU_NoCC",YEMF=="2050",REMF=="World")%>%
  select(SCENARIO,IAMC_Template,VEMF)%>%
  spread(key = SCENARIO, value = IAMC_Template)%>%
  mutate(ratio = (SSP2_500C_CACN_DAC_NoCC - SSP2_500C_CACN_NoCC)*100/SSP2_500C_CACN_NoCC)%>%
  write.csv(paste(output_path_tab,"net.csv",sep="/"))










p<-df_iamc%>%
  filter(VEMF=="Car_Seq_Dir_Air_Cap"|VEMF=="Car_Seq_Lan_Use_Aff"|VEMF=="Car_Seq_CCS_Bio",REMF=="World",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  mutate(VEMF2 = factor(VEMF, levels=c("Car_Seq_Dir_Air_Cap","Car_Seq_CCS_Bio","Car_Seq_Lan_Use_Aff")))%>%
  ggplot(aes(x=YEMF , y = abs(IAMC_Template)/1000 )) 
p<- p + facet_wrap( ~ SCENARIO, scales="fix",labeller = lb_sc2) 

p <- p + geom_bar(stat="identity",aes(fill = VEMF2))

p <- p + labs(x="Year", y="Recovered carbon dioxide(Gt-CO2/year)",fill="Negative emission\n technology") 
p <- p + scale_fill_manual(labels = c("Car_Seq_CCS_Bio"="BECCS","Car_Seq_Dir_Air_Cap"="DAC","Car_Seq_Lan_Use_Aff"="Afforestation"),values=c("cyan2","forestgreen","tomato"))
p <- p + my_theme
p <- p + theme(legend.position ="right",strip.text.y = element_text(size = 34), strip.text.x = element_text(size = 34),axis.title = element_text(size = 34),legend.title= element_text(size = 34),legend.text= element_text(size = 34),axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 32),axis.text.y = element_text(size = 32))

png(paste(output_path_fig,"CarPrc_iamc.png",sep="/"), width = 1200, height = 800)
print(p)
dev.off()






#CCS amount--------------------------------------------------------------
#Car_Seq_CCS Car_Seq_Dir_Air_Cap

p<-df_iamc%>%
  filter(VEMF=="Car_Seq_CCS"|VEMF=="Car_Seq_Dir_Air_Cap",SCENARIO=="SSP2_500C_CACN_DAC_NoCC"|SCENARIO=="SSP2_500C_CACN_NoCC")%>%
  group_by(SCENARIO,REMF,YEMF,year)%>%
  summarise(IAMC_Template=sum(IAMC_Template)/1000)%>%
  ungroup()%>%
  group_by(SCENARIO,REMF)%>%
  mutate(cumccs=cumsum(IAMC_Template)*5)%>%
  ungroup()%>%
  filter(REMF=="World")%>%
  ggplot(aes(x=YEMF,y=cumccs,group = SCENARIO,color = SCENARIO))
p<-p+geom_line()
p <- p +geom_line(size=2.5)
p <- p + labs(x="", y="Cumurative CCS amount (Gt-CO2)",color="Scenario") 
p <- p + scale_color_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5c_w/oDAC","SSP2_500C_CACN_DAC_NoCC"="1.5c_w/DAC"),values=c( "#00BFC4","#F8766D"))
p <- p + my_theme
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 50), strip.text.x = element_text(size =50),axis.title = element_text(size = 50),legend.title= element_text(size = 40),legend.text= element_text(size = 40),axis.text.x = element_text(angle = 0, hjust = 0.5,size = 40),axis.text.y = element_text(size = 40), legend.key.size=unit(1.6, 'cm'),axis.title.x = element_blank())
png(paste(output_path_fig,"CumCCS.png",sep="/"), width = 1000, height = 1000)
print(p)
dev.off()


 




p<-df_iamc%>%
  filter(VEMF=="Car_Seq_CCS"|VEMF=="Car_Seq_Dir_Air_Cap",SCENARIO=="SSP2_500C_CACN_DAC_NoCC"|SCENARIO=="SSP2_500C_CACN_NoCC")%>%
  group_by(SCENARIO,REMF,YEMF,year)%>%
  summarise(IAMC_Template=sum(IAMC_Template)/1000)%>%
  ungroup()%>%
  group_by(SCENARIO,REMF)%>%
  mutate(cumccs=cumsum(IAMC_Template)*5)%>%
  ungroup()%>%
  filter(YEMF=="2050")%>%
  left_join(df_ccs,by=c("REMF"="sr17"))%>%
  mutate(rate=cumccs*100/ccs_lo)%>%
  mutate(REMF=factor(REMF, levels=v_re17))%>%
  ggplot(aes(x=REMF,y=rate,group = SCENARIO,fill = SCENARIO))
p<-p+geom_bar(stat="identity", position="dodge")
p <- p + labs(x="Region", y="storage use (%)",fill="Scenario") 
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + my_theme
p <- p + scale_x_discrete(label=lb_x_r3)
p <- p + scale_fill_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5c_w/oDAC","SSP2_500C_CACN_DAC_NoCC"="1.5c_w/DAC"),values=c( "#00BFC4","#F8766D"))
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 40), strip.text.x = element_text(size = 40),axis.title = element_text(size = 40),legend.title= element_text(size = 40),legend.text= element_text(size = 40),axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,size = 38),axis.text.y = element_text(size = 36))
png(paste(output_path_fig,"CCS_storage_use.png",sep="/"), width = 1200, height = 1000)
print(p)
dev.off()


  
#GDP CNS loss and EV-----------------------------------------------------------------------

p <- df_iamc%>%
  filter(VEMF=="Pol_Cos_GDP_Los_rat",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF=="2050")%>%
  ggplot(aes(x=REMF , y = IAMC_Template, pattern = SCENARIO))
p <- p + geom_bar_pattern(stat="identity",
                          aes(pattern = SCENARIO),
                          position="dodge", 
                          pattern_fill = "black",
                          fill = "white",
                          color = "black",
                          pattern_color = NA,
                          pattern_spacing = 0.01,
                          pattern_density = 0.25)
p <- p + labs(x="地域", y="GDP損失率 (%)",pattern="削減シナリオ") 
p <- p + scale_pattern_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5度_DACなし","SSP2_500C_CACN_DAC_NoCC"="1.5度_DACあり"),values=c( "none","crosshatch"))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + my_theme
p <- p + scale_x_discrete(label=lb_x_r)
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 20), strip.text.x = element_text(size = 20),axis.title = element_text(size = 20),legend.title= element_text(size = 20),legend.text= element_text(size = 20),axis.text.x = element_text(angle = 90, hjust = 1,size = 18),axis.text.y = element_text(size = 18))
png(paste(output_path_fig,"GDP_loss.png",sep="/"), width = 700, height = 500)
print(p)
dev.off()



p <- df_iamc%>%
  filter(VEMF=="Pol_Cos_GDP_Los_rat"|VEMF=="Pol_Cos_Cns_Los_rat",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",REMF=="World",YEMF!="2010",YEMF!="2015")%>%
  ggplot(aes(x=year , y = IAMC_Template, linetype = SCENARIO,group = SCENARIO))
p <- p + facet_wrap(. ~ VEMF, scales = "free",nrow=1,labeller=as_labeller(c("Pol_Cos_GDP_Los_rat"="GDP損失",
                                                                            "Pol_Cos_Cns_Los_rat"="家計消費損失")))
p <- p + geom_line(size=1.5)
p <- p + labs(x="", y="排出削減による損失率 (%)",linetype="削減シナリオ") 
p <- p + scale_linetype_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5度_DACなし","SSP2_500C_CACN_DAC_NoCC"="1.5度_DACあり"),values=c( "solid","dashed"))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + my_theme

#p <- p + scale_x_discrete(label=lb_x_r)
p <- p + theme(legend.key.size = unit(1.5, 'cm'),legend.position ="right",strip.text.y = element_text(size = 40), strip.text.x = element_text(size = 40),axis.title = element_text(size = 40),legend.title= element_text(size = 40),legend.text= element_text(size = 40),axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size = 35),axis.text.y = element_text(size = 36))
png(paste(output_path_fig,"GDP_HH_loss.png",sep="/"), width = 5200, height = 2600,res = 300)
print(p)
dev.off()


p <- df_iamc%>%
  filter(VEMF=="Fin_Ene_Liq_Bio"|VEMF=="Fin_Ene_Hyd",SCENARIO=="SSP2_500C_CACN_NoCC",REMF=="World",YEMF!="2010",YEMF!="2015")%>%
  ggplot(aes(x=year , y = IAMC_Template, group = SCENARIO))
p <- p + facet_wrap(. ~ VEMF, scales = "free",nrow=1,labeller=as_labeller(c("Fin_Ene_Liq_Bio"="バイオマス転換燃料",
                                                                            "Fin_Ene_Hyd"="水素")))
p <- p + geom_line(size=1.5)
p <- p + labs(x="", y="エネルギー消費量(EJ/年)") 
#p <- p + scale_linetype_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5度_DACなし","SSP2_500C_CACN_DAC_NoCC"="1.5度_DACあり"),values=c( "solid","dashed"))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + my_theme

#p <- p + scale_x_discrete(label=lb_x_r)
p <- p + theme(legend.key.size = unit(1.5, 'cm'),legend.position ="right",strip.text.y = element_text(size = 40), strip.text.x = element_text(size = 40),axis.title = element_text(size = 40),legend.title= element_text(size = 40),legend.text= element_text(size = 40),axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size = 30),axis.text.y = element_text(size = 36))
png(paste(output_path_fig,"Bio_Hyd_loss.png",sep="/"), width = 5200, height = 2600,res = 300)
print(p)
dev.off()



p <- df_iamc%>%
  filter(VEMF=="Pol_Cos_GDP_Los_rat",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF=="2050")%>%
  ggplot(aes(x=REMF , y = IAMC_Template, pattern = SCENARIO))
p <- p + geom_bar_pattern(stat="identity",
                          aes(pattern = SCENARIO),
                          position="dodge", 
                          pattern_fill = "black",
                          fill = "white",
                          color = "black",
                          size = 1.2,
                          pattern_color = NA,
                          pattern_spacing = 0.01,
                          pattern_density = 0.23)
p <- p + labs(x="地域", y="GDP損失率 (%)",pattern="削減シナリオ") 
p <- p + scale_pattern_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5度_DACなし","SSP2_500C_CACN_DAC_NoCC"="1.5度_DACあり"),values=c( "none","crosshatch"))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + my_theme
p <- p + scale_x_discrete(label=lb_x_r)
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 40), strip.text.x = element_text(size = 40),axis.title = element_text(size = 40),legend.title= element_text(size = 40),legend.text= element_text(size = 40),axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 40),axis.text.y = element_text(size = 36))

png(paste(output_path_fig,"GDP_loss2.png",sep="/"), width = 1100, height =900)
print(p)
dev.off()



p <- df_iamc%>%
  filter(VEMF=="Pol_Cos_GDP_Los_rat",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF=="2050")%>%
  ggplot(aes(x=REMF , y = IAMC_Template, pattern = SCENARIO))
p <- p + geom_bar(stat="identity",
                          aes(fill = SCENARIO),
                          position="dodge")
p <- p + labs(x="地域", y="GDP損失率 (%)",fill="削減シナリオ") 
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + scale_x_discrete(label=lb_x_r)
p <- p + scale_fill_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5度_DACなし","SSP2_500C_CACN_DAC_NoCC"="1.5度_DACあり"),values=c( "#00BFC4","#F8766D"))
p <- p + my_theme
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 32), strip.text.x = element_text(size = 32),axis.title = element_text(size = 32),legend.title= element_text(size = 32),legend.text= element_text(size = 32),axis.text.x = element_text(angle = 90, hjust = 1,size = 32),axis.text.y = element_text(size = 27))
png(paste(output_path_fig,"GDP_loss_color.png",sep="/"), width = 1200, height = 1000)
print(p)
dev.off()


p <- df_iamc%>%
  filter(VEMF=="Pol_Cos_GDP_Los_rat",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF=="2050")%>%
  ggplot(aes(x=REMF , y = IAMC_Template, pattern = SCENARIO))
p <- p + geom_bar(stat="identity",
                  aes(fill = SCENARIO),
                  position="dodge")
p <- p + labs(x="Region", y="GDP loss (%)",fill="Scenario") 
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + scale_x_discrete(label=lb_x_r3)
p <- p + scale_fill_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5c_w/oDAC","SSP2_500C_CACN_DAC_NoCC"="1.5c_w/DAC"),values=c( "#00BFC4","#F8766D"))
p <- p + my_theme
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 32), strip.text.x = element_text(size = 32),axis.title = element_text(size = 32),legend.title= element_text(size = 32),legend.text= element_text(size = 32),axis.text.x = element_text(angle = 45, hjust = 1,size = 32),axis.text.y = element_text(size = 27))
png(paste(output_path_fig,"GDP_loss_color_iamc.png",sep="/"), width = 1600, height = 1000)
print(p)
dev.off()



df_iamc%>%
  filter(VEMF=="Pol_Cos_GDP_Los_rat",YEMF=="2050")%>%
  select(SCENARIO,IAMC_Template,REMF)%>%
  spread(key = SCENARIO, value = IAMC_Template)%>%
  mutate(ratio = (SSP2_500C_CACN_DAC_NoCC - SSP2_500C_CACN_NoCC)*100/SSP2_500C_CACN_NoCC)%>%
  write.csv(paste(output_path_tab,"gdp.csv",sep="/"))



p <- df_iamc%>%
  filter(VEMF=="Pol_Cos_Cns_Los_rat",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF=="2050")%>%
  ggplot(aes(x=REMF , y = IAMC_Template, pattern = SCENARIO))
p <- p + geom_bar_pattern(stat="identity",
                          aes(pattern = SCENARIO),
                          position="dodge", 
                          pattern_fill = "black",
                          fill = "white",
                          color = "black",
                          pattern_color = NA,
                          size = 1.2,
                          pattern_spacing = 0.01,
                          pattern_density = 0.25)
p <- p + labs(x="地域", y="消費損失率 (%)",pattern="削減シナリオ") 
p <- p + scale_pattern_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5度_DACなし","SSP2_500C_CACN_DAC_NoCC"="1.5度_DACあり"),values=c( "none","crosshatch"))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + my_theme
p <- p + scale_x_discrete(label=lb_x_r)
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 40), strip.text.x = element_text(size = 40),axis.title = element_text(size = 40),legend.title= element_text(size = 40),legend.text= element_text(size = 40),axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 40),axis.text.y = element_text(size = 36))
png(paste(output_path_fig,"CNS_loss.png",sep="/"), width = 1100, height = 900)
print(p)
dev.off()


p <- df_iamc%>%
  filter(VEMF=="Pol_Cos_Cns_Los_rat",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF=="2050")%>%
  ggplot(aes(x=REMF , y = IAMC_Template, pattern = SCENARIO))
p <- p + geom_bar(stat="identity",
                  aes(fill = SCENARIO),
                  position="dodge")
p <- p + labs(x="地域", y="消費損失率 (%)",fill="削減シナリオ") 
p <- p + scale_pattern_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5度_DACなし","SSP2_500C_CACN_DAC_NoCC"="1.5度_DACあり"),values=c( "none","crosshatch"))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + my_theme
p <- p + scale_x_discrete(label=lb_x_r)
p <- p + scale_fill_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5度_DACなし","SSP2_500C_CACN_DAC_NoCC"="1.5度_DACあり"),values=c( "#00BFC4","#F8766D"))
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 40), strip.text.x = element_text(size = 40),axis.title = element_text(size = 40),legend.title= element_text(size = 40),legend.text= element_text(size = 40),axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 40),axis.text.y = element_text(size = 36))
png(paste(output_path_fig,"CNS_loss_color.png",sep="/"), width = 1600, height = 900)
print(p)
dev.off()



p <- df_iamc%>%
  filter(VEMF=="Pol_Cos_Cns_Los_rat",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF=="2050")%>%
  ggplot(aes(x=REMF , y = IAMC_Template, pattern = SCENARIO))
p <- p + geom_bar(stat="identity",
                  aes(fill = SCENARIO),
                  position="dodge")
p <- p + labs(x="Region", y="Consumption loss (%)",fill="Scenario") 
#p <- p + scale_pattern_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5度_DACなし","SSP2_500C_CACN_DAC_NoCC"="1.5度_DACあり"),values=c( "none","crosshatch"))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + my_theme
p <- p + scale_x_discrete(label=lb_x_r3)
p <- p + scale_fill_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5c_w/oDAC","SSP2_500C_CACN_DAC_NoCC"="1.5c_w/DAC"),values=c( "#00BFC4","#F8766D"))
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 40), strip.text.x = element_text(size = 40),axis.title = element_text(size = 40),legend.title= element_text(size = 40),legend.text= element_text(size = 40),axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,size = 38),axis.text.y = element_text(size = 36))
png(paste(output_path_fig,"CNS_loss_color_iamc.png",sep="/"), width = 1600, height = 1000)
print(p)
dev.off()


df_iamc%>%
  filter(VEMF=="Pol_Cos_Cns_Los_rat",YEMF=="2050")%>%
  select(SCENARIO,IAMC_Template,REMF)%>%
  spread(key = SCENARIO, value = IAMC_Template)%>%
  mutate(ratio = (SSP2_500C_CACN_DAC_NoCC - SSP2_500C_CACN_NoCC)*100/SSP2_500C_CACN_NoCC)%>%
  write.csv(paste(output_path_tab,"cns.csv",sep="/"))



p <- df_iamc%>%
  filter(VEMF=="Pol_Cos_Equ_Var_rat",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF=="2050")%>%
  ggplot(aes(x=REMF , y = IAMC_Template, pattern = SCENARIO))
p <- p + geom_bar_pattern(stat="identity",
                          aes(pattern = SCENARIO),
                          position="dodge", 
                          pattern_fill = "black",
                          fill = "white",
                          color = "black",
                          size = 1.2,
                          pattern_color = NA,
                          pattern_spacing = 0.01,
                          pattern_density = 0.25)
p <- p + labs(x="地域", y="等価変分の減少率 (%)",pattern="削減シナリオ") 
p <- p + scale_pattern_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5度_DACなし","SSP2_500C_CACN_DAC_NoCC"="1.5度_DACあり"),values=c( "none","crosshatch"))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + my_theme
p <- p + scale_x_discrete(label=lb_x_r)
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 40), strip.text.x = element_text(size = 40),axis.title = element_text(size = 40),legend.title= element_text(size = 40),legend.text= element_text(size = 40),axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 40),axis.text.y = element_text(size = 36))
png(paste(output_path_fig,"equivalent_valuation.png",sep="/"), width = 1100, height = 900)
print(p)
dev.off()

df_iamc%>%
  filter(VEMF=="Pol_Cos_Equ_Var_rat",YEMF=="2050")%>%
  select(SCENARIO,IAMC_Template,REMF)%>%
  spread(key = SCENARIO, value = IAMC_Template)%>%
  mutate(ratio = (SSP2_500C_CACN_DAC_NoCC - SSP2_500C_CACN_NoCC)*100/SSP2_500C_CACN_NoCC)%>%
  write.csv(paste(output_path_tab,"ev.csv",sep="/"))




p <- df_iamc%>%
  filter(VEMF=="Pol_Cos_Equ_Var_rat"|VEMF=="Pol_Cos_Cns_Los_rat",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF=="2050")%>%
  select(SCENARIO,IAMC_Template,REMF,VEMF)%>%
  spread(key = SCENARIO, value = IAMC_Template)%>%
  mutate(dif=-SSP2_500C_CACN_DAC_NoCC+SSP2_500C_CACN_NoCC)%>%
  mutate(rat=dif*100/SSP2_500C_CACN_NoCC)%>%
ggplot(aes(x=REMF, y=dif, pattern=VEMF))
#p <- p + geom_bar(stat="identity", position="dodge")
p <- p + geom_bar_pattern(stat="identity",
                          aes(pattern = VEMF),
                          position="dodge", 
                          pattern_fill = "black",
                          fill = "white",
                          color = "black",
                          pattern_color = NA,
                          pattern_spacing = 0.01,
                          pattern_density = 0.25)
p <- p + labs(x="地域", y="消費および等価変分の変化 (pt)",pattern=" ") 
p <- p + scale_pattern_manual(labels = c("Pol_Cos_Cns_Los_rat"="消費","Pol_Cos_Equ_Var_rat"="等価変分"),values=c( "none","crosshatch"))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + my_theme
p <- p + scale_x_discrete(label=lb_x_r)
p <- p + theme(legend.position ="bottom",strip.text.y = element_text(size = 20), strip.text.x = element_text(size = 20),axis.title = element_text(size = 20),legend.title= element_text(size = 20),legend.text= element_text(size = 20),axis.text.x = element_text(angle = 90, hjust = 1,size = 18),axis.text.y = element_text(size = 18))
png(paste(output_path_fig,"cns_ev_dif.png",sep="/"), width = 700, height = 600)
print(p)
dev.off()


df_iamc%>%
filter(VEMF=="Pol_Cos_Equ_Var_rat",YEMF=="2050")%>%
  select(SCENARIO,IAMC_Template,REMF)%>%
  spread(key = SCENARIO, value = IAMC_Template)%>%
  mutate(ratio = (SSP2_500C_CACN_DAC_NoCC - SSP2_500C_CACN_NoCC)*100/SSP2_500C_CACN_NoCC)%>%
  write.csv(paste(output_path_tab,"ev.csv",sep="/"))















#GDP and EV

p1 <- df_iamc%>%
  filter(VEMF=="Pol_Cos_GDP_Los_rat",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF=="2050")%>%
  ggplot(aes(x=REMF , y = IAMC_Template, pattern = SCENARIO))
p1 <- p1 + geom_bar(stat="identity",
                  aes(fill = SCENARIO),
                  position="dodge")
p1 <- p1 + labs(x="", y="GDP loss (%)",fill="Scenario") 
p1 <- p1 + geom_hline(yintercept=0,color = "grey")
p1 <- p1 + scale_x_discrete(label=lb_x_r2)
p1 <- p1 + scale_fill_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5c_woDAC","SSP2_500C_CACN_DAC_NoCC"="1.5c_wDAC"),values=c( "#00BFC4","#F8766D"))
p1 <- p1 + my_theme
p1 <- p1 + theme(legend.position ="none",strip.text.y = element_text(size = 40), strip.text.x = element_text(size = 40),axis.title = element_text(size = 40),legend.title= element_text(size = 40),legend.text= element_text(size = 40),axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5, size = 40),axis.text.y = element_text(size = 36))

p2 <- df_iamc%>%
  filter(VEMF=="Pol_Cos_Equ_Var_rat",SCENARIO!="SSP2_BaU_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",YEMF=="2050")%>%
  ggplot(aes(x=REMF , y = IAMC_Template, pattern = SCENARIO))
p2 <- p2 + geom_bar(stat="identity",
                  aes(fill = SCENARIO),
                  position="dodge")
p2 <- p2 + labs(x="", y="EV loss (%)",pattern="Scenario") 
p2 <- p2 + scale_fill_manual(labels = c("SSP2_500C_CACN_NoCC"="1.5c_w/oDAC","SSP2_500C_CACN_DAC_NoCC"="1.5c_w/DAC"),values=c( "#00BFC4","#F8766D"))
p2 <- p2 + geom_hline(yintercept=0,color = "grey")
p2 <- p2 + my_theme
p2 <- p2 + scale_x_discrete(label=lb_x_r2)
p2 <- p2 + theme(strip.text.y = element_text(size = 40), strip.text.x = element_text(size = 40),axis.title = element_text(size = 40),legend.title= element_text(size = 40),legend.text= element_text(size = 40),axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 40),axis.text.y = element_text(size = 36))

p3 <- g_legend(p2)

p2 <- p2 + theme(legend.position ="none",strip.text.y = element_text(size = 40), strip.text.x = element_text(size = 40),axis.title = element_text(size = 40),legend.title= element_text(size = 40),legend.text= element_text(size = 40),axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5, size = 40),axis.text.y = element_text(size = 36))


png(paste(output_path_fig,"GDP_EV_loss.png",sep="/"), width = 2200, height = 1000)
layout1<-rbind(c(1,1,1,2,2,2,3))
p<-grid.arrange(p1, p2,p3,layout_matrix =layout1)
print(p)
dev.off()



#Primary energy-----------------------------------------------------------------------------------

p <- df_iamc%>%
  filter(SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",REMF=="World",VEMF=="Prm_Ene_Geo"|VEMF=="Prm_Ene_Bio_w_CCS"|VEMF=="Prm_Ene_Bio_wo_CCS"|VEMF=="Prm_Ene_Coa_wo_CCS"|VEMF=="Prm_Ene_Coa_w_CCS"|VEMF=="Prm_Ene_Gas_w_CCS"|VEMF=="Prm_Ene_Gas_wo_CCS"|VEMF=="Prm_Ene_Oil_w_CCS"|VEMF=="Prm_Ene_Oil_wo_CCS"|VEMF=="Prm_Ene_Hyd"|VEMF=="Prm_Ene_Nuc"|VEMF=="Prm_Ene_Solar"|VEMF=="Prm_Ene_Win",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  mutate(VEMF2 = factor(VEMF, levels=c("Prm_Ene_Bio_w_CCS","Prm_Ene_Bio_wo_CCS","Prm_Ene_Geo","Prm_Ene_Win","Prm_Ene_Solar","Prm_Ene_Hyd","Prm_Ene_Nuc","Prm_Ene_Gas_w_CCS","Prm_Ene_Gas_wo_CCS","Prm_Ene_Oil_w_CCS","Prm_Ene_Oil_wo_CCS","Prm_Ene_Coa_w_CCS","Prm_Ene_Coa_wo_CCS")))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fix",labeller = lb_sc)
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p  + geom_bar(stat="identity",aes(fill=VEMF2)) 
p <- p  + scale_fill_manual(labels = lb_pri,values = fl_pri)
#p <- p  + scale_fill_manual(values = fl_pri)
p <- p　+ my_theme 
p <- p　+ theme(strip.text.y = element_text(size = 28), 
               strip.text.x = element_text(size = 28),
               axis.title = element_text(size = 25),
               legend.title= element_text(size = 27),
               legend.text= element_text(size = 27),
               axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 22),
               axis.text.y = element_text(size = 22),
               axis.title.x = element_blank())
p <- p + labs(x="年", y="一次エネルギー供給量(EJ/年)", fill = "エネルギー種")
png(paste(output_path_fig,"primary_energy.png",sep="/"), width = 1500, height = 500)
print(p)
dev.off()


p <- df_iamc%>%
  filter(SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",REMF=="World",VEMF=="Prm_Ene_Geo"|VEMF=="Prm_Ene_Bio_w_CCS"|VEMF=="Prm_Ene_Bio_wo_CCS"|VEMF=="Prm_Ene_Coa_wo_CCS"|VEMF=="Prm_Ene_Coa_w_CCS"|VEMF=="Prm_Ene_Gas_w_CCS"|VEMF=="Prm_Ene_Gas_wo_CCS"|VEMF=="Prm_Ene_Oil_w_CCS"|VEMF=="Prm_Ene_Oil_wo_CCS"|VEMF=="Prm_Ene_Hyd"|VEMF=="Prm_Ene_Nuc"|VEMF=="Prm_Ene_Solar"|VEMF=="Prm_Ene_Win",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  mutate(VEMF2 = factor(VEMF, levels=c("Prm_Ene_Bio_w_CCS","Prm_Ene_Bio_wo_CCS","Prm_Ene_Geo","Prm_Ene_Win","Prm_Ene_Solar","Prm_Ene_Hyd","Prm_Ene_Nuc","Prm_Ene_Gas_w_CCS","Prm_Ene_Gas_wo_CCS","Prm_Ene_Oil_w_CCS","Prm_Ene_Oil_wo_CCS","Prm_Ene_Coa_w_CCS","Prm_Ene_Coa_wo_CCS")))%>%
  mutate(SCENARIO = factor(SCENARIO, levels=c("SSP2_BaU_NoCC","SSP2_500C_CACN_NoCC","SSP2_500C_CACN_DAC_NoCC")))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fix",labeller = lb_sc)
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p  + geom_bar(stat="identity",aes(fill=VEMF2),size=0.8,color="black") 
p <- p  + scale_fill_manual(labels = lb_pri,values = fl_pri)
#p <- p  + scale_fill_manual(values = fl_pri)
p <- p　+ my_theme 
p <- p　+ theme(strip.text.y = element_text(size = 28), 
               strip.text.x = element_text(size = 28),
               axis.title = element_text(size = 25),
               legend.title= element_text(size = 27),
               legend.text= element_text(size = 27),
               axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 22),
               axis.text.y = element_text(size = 22),
               axis.title.x = element_blank(),
               legend.position ="none")
p <- p + labs(x="年", y="一次エネルギー供給量(EJ/年)", fill = "エネルギー種")


p2<- df_iamc%>%
  filter(SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",REMF=="World",VEMF=="Prm_Ene_Geo"|VEMF=="Prm_Ene_Bio_w_CCS"|VEMF=="Prm_Ene_Bio_wo_CCS"|VEMF=="Prm_Ene_Coa_wo_CCS"|VEMF=="Prm_Ene_Coa_w_CCS"|VEMF=="Prm_Ene_Gas_w_CCS"|VEMF=="Prm_Ene_Gas_wo_CCS"|VEMF=="Prm_Ene_Oil_w_CCS"|VEMF=="Prm_Ene_Oil_wo_CCS"|VEMF=="Prm_Ene_Hyd"|VEMF=="Prm_Ene_Nuc"|VEMF=="Prm_Ene_Solar"|VEMF=="Prm_Ene_Win",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  mutate(VEMF2 = factor(VEMF, levels=c("Prm_Ene_Bio_w_CCS","Prm_Ene_Bio_wo_CCS","Prm_Ene_Geo","Prm_Ene_Win","Prm_Ene_Solar","Prm_Ene_Hyd","Prm_Ene_Nuc","Prm_Ene_Gas_w_CCS","Prm_Ene_Gas_wo_CCS","Prm_Ene_Oil_w_CCS","Prm_Ene_Oil_wo_CCS","Prm_Ene_Coa_w_CCS","Prm_Ene_Coa_wo_CCS")))%>%
  filter(SCENARIO=="SSP2_500C_CACN_DAC_NoCC"|SCENARIO=="SSP2_500C_CACN_NoCC",YEMF!="2020",YEMF!="2025")%>%
  pivot_wider(names_from = SCENARIO, values_from = IAMC_Template)%>%
  mutate(IAMC_Template=SSP2_500C_CACN_DAC_NoCC-SSP2_500C_CACN_NoCC)%>%
  mutate(SCENARIO="１．５度DACあり\n-１．５度DACなし")%>%
  #  View()
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p2 <- p2  + facet_grid( ~ SCENARIO ,scales="fix")
p2 <- p2　+ geom_hline(yintercept=0,color = "grey")
p2 <- p2  + geom_bar(stat="identity",aes(fill=VEMF2),size=0.8,color="black") 
p2 <- p2  + scale_fill_manual(labels = lb_pri,values = fl_pri)
#p <- p  + scale_fill_manual(values = fl_pri)
p2 <- p2　+ my_theme 
p2 <- p2　+ theme(strip.text.y = element_text(size = 28), 
                 strip.text.x = element_text(size = 28),
                 axis.title = element_text(size = 25),
                 legend.title= element_text(size = 27),
                 legend.text= element_text(size = 27),
                 axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 22),
                 axis.text.y = element_text(size = 22),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank())
p2 <- p2 + labs(x="", y="Primary energy(EJ/year)", fill = "")

png(paste(output_path_fig,"primary_energy2.png",sep="/"), width = 1600, height =1000)
layout1<-rbind(c(1,1,1,1,1,2,2,2))
p1<-grid.arrange(p, p2,layout_matrix =layout1)
print(p1)
dev.off()


p <- df_iamc%>%
  filter(SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",REMF=="World",VEMF=="Prm_Ene_Geo"|VEMF=="Prm_Ene_Bio_w_CCS"|VEMF=="Prm_Ene_Bio_wo_CCS"|VEMF=="Prm_Ene_Coa_wo_CCS"|VEMF=="Prm_Ene_Coa_w_CCS"|VEMF=="Prm_Ene_Gas_w_CCS"|VEMF=="Prm_Ene_Gas_wo_CCS"|VEMF=="Prm_Ene_Oil_w_CCS"|VEMF=="Prm_Ene_Oil_wo_CCS"|VEMF=="Prm_Ene_Hyd"|VEMF=="Prm_Ene_Nuc"|VEMF=="Prm_Ene_Solar"|VEMF=="Prm_Ene_Win",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  mutate(VEMF2 = factor(VEMF, levels=c("Prm_Ene_Bio_w_CCS","Prm_Ene_Bio_wo_CCS","Prm_Ene_Geo","Prm_Ene_Win","Prm_Ene_Solar","Prm_Ene_Hyd","Prm_Ene_Nuc","Prm_Ene_Gas_w_CCS","Prm_Ene_Gas_wo_CCS","Prm_Ene_Oil_w_CCS","Prm_Ene_Oil_wo_CCS","Prm_Ene_Coa_w_CCS","Prm_Ene_Coa_wo_CCS")))%>%
  mutate(SCENARIO = factor(SCENARIO, levels=c("SSP2_BaU_NoCC","SSP2_500C_CACN_NoCC","SSP2_500C_CACN_DAC_NoCC")))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fix",labeller = lb_sc)
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p  + geom_bar(stat="identity",aes(fill=VEMF2),size=0.8) 
p <- p  + scale_fill_manual(labels = lb_pri,values = fl_pri)
#p <- p  + scale_fill_manual(values = fl_pri)
p <- p　+ my_theme 
p <- p　+ theme(strip.text.y = element_text(size = 28), 
               strip.text.x = element_text(size = 28),
               axis.title = element_text(size = 25),
               legend.title= element_text(size = 27),
               legend.text= element_text(size = 27),
               axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 22),
               axis.text.y = element_text(size = 22),
               axis.title.x = element_blank(),
               legend.position ="right")
p <- p + labs(x="年", y="一次エネルギー供給量(EJ/年)", fill = "エネルギー種")

png(paste(output_path_fig,"primary_energy4.png",sep="/"), width = 1400, height =800)
print(p)
dev.off()





df_iamc%>%
  filter(VEMF=="Prm_Ene_Geo"|VEMF=="Prm_Ene_Bio_w_CCS"|VEMF=="Prm_Ene_Bio_wo_CCS"|VEMF=="Prm_Ene_Coa_wo_CCS"|VEMF=="Prm_Ene_Coa_w_CCS"|VEMF=="Prm_Ene_Gas_w_CCS"|VEMF=="Prm_Ene_Gas_wo_CCS"|VEMF=="Prm_Ene_Oil_w_CCS"|VEMF=="Prm_Ene_Oil_wo_CCS"|VEMF=="Prm_Ene_Hyd"|VEMF=="Prm_Ene_Nuc"|VEMF=="Prm_Ene_Solar"|VEMF=="Prm_Ene_Win",REMF=="World",YEMF=="2050",REMF=="World")%>%
  select(SCENARIO,IAMC_Template,VEMF)%>%
  spread(key = SCENARIO, value = IAMC_Template)%>%
  mutate(ratio = (SSP2_500C_CACN_DAC_NoCC - SSP2_500C_CACN_NoCC)*100/SSP2_500C_CACN_NoCC)%>%
  write.csv(paste(output_path_tab,"prm.csv",sep="/"))





p <- df_iamc%>%
  filter(SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",REMF=="World",VEMF=="Prm_Ene_Geo"|VEMF=="Prm_Ene_Bio_w_CCS"|VEMF=="Prm_Ene_Bio_wo_CCS"|VEMF=="Prm_Ene_Coa_wo_CCS"|VEMF=="Prm_Ene_Coa_w_CCS"|VEMF=="Prm_Ene_Gas_w_CCS"|VEMF=="Prm_Ene_Gas_wo_CCS"|VEMF=="Prm_Ene_Oil_w_CCS"|VEMF=="Prm_Ene_Oil_wo_CCS"|VEMF=="Prm_Ene_Hyd"|VEMF=="Prm_Ene_Nuc"|VEMF=="Prm_Ene_Solar"|VEMF=="Prm_Ene_Win",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  mutate(VEMF2 = factor(VEMF, levels=c("Prm_Ene_Bio_w_CCS","Prm_Ene_Bio_wo_CCS","Prm_Ene_Geo","Prm_Ene_Win","Prm_Ene_Solar","Prm_Ene_Hyd","Prm_Ene_Nuc","Prm_Ene_Gas_w_CCS","Prm_Ene_Gas_wo_CCS","Prm_Ene_Oil_w_CCS","Prm_Ene_Oil_wo_CCS","Prm_Ene_Coa_w_CCS","Prm_Ene_Coa_wo_CCS")))%>%
  mutate(SCENARIO = factor(SCENARIO, levels=c("SSP2_BaU_NoCC","SSP2_500C_CACN_NoCC","SSP2_500C_CACN_DAC_NoCC")))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fix",labeller = lb_sc2)
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p  + geom_bar(stat="identity",aes(fill=VEMF2)) 
p <- p  + scale_fill_manual(labels = lb_pri2,values = fl_pri)
#p <- p  + scale_fill_manual(values = fl_pri)
p <- p　+ my_theme 
p <- p　+ theme(strip.text.y = element_text(size = 28), 
               strip.text.x = element_text(size = 28),
               axis.title = element_text(size = 25),
               legend.title= element_text(size = 27),
               legend.text= element_text(size = 27),
               axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 22),
               axis.text.y = element_text(size = 22),
               axis.title.x = element_blank(),
               legend.position ="none")
p <- p + labs(x="", y="Primary energy(EJ/year)", fill = "")

p2<- df_iamc%>%
  filter(SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",REMF=="World",VEMF=="Prm_Ene_Geo"|VEMF=="Prm_Ene_Bio_w_CCS"|VEMF=="Prm_Ene_Bio_wo_CCS"|VEMF=="Prm_Ene_Coa_wo_CCS"|VEMF=="Prm_Ene_Coa_w_CCS"|VEMF=="Prm_Ene_Gas_w_CCS"|VEMF=="Prm_Ene_Gas_wo_CCS"|VEMF=="Prm_Ene_Oil_w_CCS"|VEMF=="Prm_Ene_Oil_wo_CCS"|VEMF=="Prm_Ene_Hyd"|VEMF=="Prm_Ene_Nuc"|VEMF=="Prm_Ene_Solar"|VEMF=="Prm_Ene_Win",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  mutate(VEMF2 = factor(VEMF, levels=c("Prm_Ene_Bio_w_CCS","Prm_Ene_Bio_wo_CCS","Prm_Ene_Geo","Prm_Ene_Win","Prm_Ene_Solar","Prm_Ene_Hyd","Prm_Ene_Nuc","Prm_Ene_Gas_w_CCS","Prm_Ene_Gas_wo_CCS","Prm_Ene_Oil_w_CCS","Prm_Ene_Oil_wo_CCS","Prm_Ene_Coa_w_CCS","Prm_Ene_Coa_wo_CCS")))%>%
  filter(SCENARIO=="SSP2_500C_CACN_DAC_NoCC"|SCENARIO=="SSP2_500C_CACN_NoCC",YEMF!="2020",YEMF!="2025")%>%
  pivot_wider(names_from = SCENARIO, values_from = IAMC_Template)%>%
  mutate(IAMC_Template=SSP2_500C_CACN_DAC_NoCC-SSP2_500C_CACN_NoCC)%>%
  mutate(SCENARIO="1.5c_w/DAC\n-1.5c_w/oDAC")%>%
#  View()
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p2 <- p2  + facet_grid( ~ SCENARIO ,scales="fix")
p2 <- p2　+ geom_hline(yintercept=0,color = "grey")
p2 <- p2  + geom_bar(stat="identity",aes(fill=VEMF2)) 
p2 <- p2  + scale_fill_manual(labels = lb_pri2,values = fl_pri)
#p <- p  + scale_fill_manual(values = fl_pri)
p2 <- p2　+ my_theme 
p2 <- p2　+ theme(strip.text.y = element_text(size = 28), 
               strip.text.x = element_text(size = 28),
               axis.title = element_text(size = 25),
               legend.title= element_text(size = 27),
               legend.text= element_text(size = 27),
               axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 22),
               axis.text.y = element_text(size = 22),
               axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               legend.position ="right")
p2 <- p2 + labs(x="", y="Primary energy(EJ/year)", fill = "")

png(paste(output_path_fig,"primary_energy_iamc.png",sep="/"), width = 1200, height = 700)
layout1<-rbind(c(1,1,1,1,1,2,2,2,2))
p1<-grid.arrange(p, p2,layout_matrix =layout1)
print(p1)
dev.off()







#fin ene---------------------------------------------------------------

p <- df_iamc%>%
  filter(REMF=="World",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",VEMF=="Fin_Ene_AFO"|VEMF=="Fin_Ene_Bui_Res_and_Com"|VEMF=="Fin_Ene_Ind"|VEMF=="Fin_Ene_NonEneUse"|VEMF=="Fin_Ene_Oth_Sec"|VEMF=="Fin_Ene_Tra")%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fix",labeller = lb_sc)
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p  + geom_bar(stat="identity",aes(fill=VEMF)) 
#p <- p  + scale_fill_manual(labels = lb_fin,values = fl_fin)
p <- p　+ my_theme 
p <- p　+ theme(strip.text.y = element_text(size = 28), 
               strip.text.x = element_text(size = 28),
               axis.title = element_text(size = 25),
               legend.title= element_text(size = 27),
               legend.text= element_text(size = 27),
               axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 22),
               axis.text.y = element_text(size = 22),
               axis.title.x = element_blank())
p <- p + labs(x="年", y="最終エネルギー消費量 (EJ/年)", fill = "部門")
png(paste(output_path_fig,"final_energy1.png",sep="/"), width = 1500, height = 500)
print(p)
dev.off()



p <- df_iamc%>%
  filter(REMF=="World",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",VEMF=="Fin_Ene_Ele"|VEMF=="Fin_Ene_Gas"|VEMF=="Fin_Ene_Heat"|VEMF=="Fin_Ene_Hyd"|VEMF=="Fin_Ene_Liq"|VEMF=="Fin_Ene_Solids",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fix",labeller = lb_sc)
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p  + geom_bar(stat="identity",aes(fill=VEMF)) 
p <- p  + scale_fill_manual(labels = lb_fin,values = fl_fin)
p <- p　+ my_theme
p <- p　+ theme(strip.text.y = element_text(size = 28), 
               strip.text.x = element_text(size = 28),
               axis.title = element_text(size = 25),
               legend.title= element_text(size = 27),
               legend.text= element_text(size = 27),
               axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 22),
               axis.text.y = element_text(size = 22),
               axis.title.x = element_blank())
p <- p + labs(x="年", y="最終エネルギー消費量 (EJ/年)", fill = "エネルギー種")
png(paste(output_path_fig,"final_energy2.png",sep="/"), width = 1500, height = 500)
print(p)
dev.off()



p <- df_iamc%>%
  filter(REMF=="World",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",VEMF=="Fin_Ene_Ele"|VEMF=="Fin_Ene_Gas"|VEMF=="Fin_Ene_Heat"|VEMF=="Fin_Ene_Hyd"|VEMF=="Fin_Ene_Liq"|VEMF=="Fin_Ene_Solids",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  mutate(SCENARIO = factor(SCENARIO, levels=c("SSP2_BaU_NoCC","SSP2_500C_CACN_NoCC","SSP2_500C_CACN_DAC_NoCC")))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fix",labeller = lb_sc)
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p  + geom_bar(stat="identity",aes(fill=VEMF)) 
p <- p  + scale_fill_manual(labels = lb_fin,values = fl_fin)
p <- p　+ my_theme
p <- p　+ theme(strip.text.y = element_text(size = 28), 
               strip.text.x = element_text(size = 28),
               axis.title = element_text(size = 25),
               legend.title= element_text(size = 27),
               legend.text= element_text(size = 27),
               axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 22),
               axis.text.y = element_text(size = 22),
               axis.title.x = element_blank(),
               legend.position ="none")
p <- p + labs(x="年", y="最終エネルギー消費量 (EJ/年)", fill = "エネルギー種")

p2<- df_iamc%>%
  filter(REMF=="World",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",VEMF=="Fin_Ene_Ele"|VEMF=="Fin_Ene_Gas"|VEMF=="Fin_Ene_Heat"|VEMF=="Fin_Ene_Hyd"|VEMF=="Fin_Ene_Liq"|VEMF=="Fin_Ene_Solids",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(SCENARIO=="SSP2_500C_CACN_DAC_NoCC"|SCENARIO=="SSP2_500C_CACN_NoCC",YEMF!="2020",YEMF!="2025")%>%
  pivot_wider(names_from = SCENARIO, values_from = IAMC_Template)%>%
  mutate(IAMC_Template=SSP2_500C_CACN_DAC_NoCC-SSP2_500C_CACN_NoCC)%>%
  mutate(SCENARIO="１．５度DACあり\n-１．５度DACなし")%>%
  #  View()
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p2 <- p2  + facet_grid( ~ SCENARIO ,scales="fix")
p2 <- p2　+ geom_hline(yintercept=0,color = "grey")
p2 <- p2  + geom_bar(stat="identity",aes(fill=VEMF)) 
p2 <- p2  + scale_fill_manual(labels = lb_fin,values = fl_fin)
p2 <- p2　+ my_theme 
p2 <- p2　+ theme(strip.text.y = element_text(size = 28), 
                 strip.text.x = element_text(size = 28),
                 axis.title = element_text(size = 25),
                 legend.title= element_text(size = 27),
                 legend.text= element_text(size = 27),
                 axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 22),
                 axis.text.y = element_text(size = 22),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank())
p2 <- p2 + labs(x="", y="最終エネルギー消費量 (EJ/年)", fill = "")

png(paste(output_path_fig,"final_energy3.png",sep="/"), width = 1400, height = 600)
layout1<-rbind(c(1,1,1,1,1,2,2))
p1<-grid.arrange(p, p2,layout_matrix =layout1)
print(p1)
dev.off()



p <- df_iamc%>%
  filter(REMF=="World",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",VEMF=="Fin_Ene_Ele"|VEMF=="Fin_Ene_Gas"|VEMF=="Fin_Ene_Heat"|VEMF=="Fin_Ene_Hyd"|VEMF=="Fin_Ene_Liq"|VEMF=="Fin_Ene_Solids",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  mutate(SCENARIO = factor(SCENARIO, levels=c("SSP2_BaU_NoCC","SSP2_500C_CACN_NoCC","SSP2_500C_CACN_DAC_NoCC")))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fix",labeller = lb_sc2)
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p  + geom_bar(stat="identity",aes(fill=VEMF)) 
p <- p  + scale_fill_manual(labels = lb_fin2,values = fl_fin)
p <- p　+ my_theme
p <- p　+ theme(strip.text.y = element_text(size = 28), 
               strip.text.x = element_text(size = 28),
               axis.title = element_text(size = 25),
               legend.title= element_text(size = 27),
               legend.text= element_text(size = 27),
               axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 22),
               axis.text.y = element_text(size = 22),
               axis.title.x = element_blank(),
               legend.position ="none")
p <- p + labs(x="year", y="Final energy consumption (EJ/year)", fill = "Energy type")


p2<- df_iamc%>%
  filter(REMF=="World",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",VEMF=="Fin_Ene_Ele"|VEMF=="Fin_Ene_Gas"|VEMF=="Fin_Ene_Heat"|VEMF=="Fin_Ene_Hyd"|VEMF=="Fin_Ene_Liq"|VEMF=="Fin_Ene_Solids",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(SCENARIO=="SSP2_500C_CACN_DAC_NoCC"|SCENARIO=="SSP2_500C_CACN_NoCC",YEMF!="2020",YEMF!="2025")%>%
  pivot_wider(names_from = SCENARIO, values_from = IAMC_Template)%>%
  mutate(IAMC_Template=SSP2_500C_CACN_DAC_NoCC-SSP2_500C_CACN_NoCC)%>%
  mutate(SCENARIO="1.5c_w/DAC\n-1.5c_w/oDAC")%>%
  #  View()
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p2 <- p2  + facet_grid( ~ SCENARIO ,scales="fix")
p2 <- p2　+ geom_hline(yintercept=0,color = "grey")
p2 <- p2  + geom_bar(stat="identity",aes(fill=VEMF)) 
p2 <- p2  + scale_fill_manual(labels = lb_fin2,values = fl_fin)
p2 <- p2　+ my_theme 
p2 <- p2　+ theme(strip.text.y = element_text(size = 28), 
                 strip.text.x = element_text(size = 28),
                 axis.title = element_text(size = 25),
                 legend.title= element_text(size = 27),
                 legend.text= element_text(size = 27),
                 axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 22),
                 axis.text.y = element_text(size = 22),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank())
p2 <- p2 + labs(x="", y="Final energy consumption (EJ/year)", fill = "")

png(paste(output_path_fig,"final_energy_iamc.png",sep="/"), width = 1200, height = 700)
layout1<-rbind(c(1,1,1,1,1,1,2,2,2))
p1<-grid.arrange(p, p2,layout_matrix =layout1)
print(p1)
dev.off()




df_iamc%>%
  filter(VEMF=="Fin_Ene_Ele"|VEMF=="Fin_Ene_Gas"|VEMF=="Fin_Ene_Heat"|VEMF=="Fin_Ene_Hyd"|VEMF=="Fin_Ene_Liq"|VEMF=="Fin_Ene_Solids",REMF=="World",YEMF=="2050",REMF=="World")%>%
  select(SCENARIO,IAMC_Template,VEMF)%>%
  spread(key = SCENARIO, value = IAMC_Template)%>%
  mutate(ratio = (SSP2_500C_CACN_DAC_NoCC - SSP2_500C_CACN_NoCC)*100/SSP2_500C_CACN_NoCC)%>%
  write.csv(paste(output_path_tab,"fin.csv",sep="/"))





p1<- df_iamc%>%
  filter(REMF=="World",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",VEMF=="Fin_Ene_Res_and_Com_Ele"|VEMF=="Fin_Ene_Res_and_Com_Gas"|VEMF=="Fin_Ene_Res_and_Com_Heat"|VEMF=="Fin_Ene_Res_and_Com_Hyd"|VEMF=="Fin_Ene_Res_and_Com_Liq"|VEMF=="Fin_Ene_Res_and_Com_Solids",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(SCENARIO=="SSP2_500C_CACN_DAC_NoCC"|SCENARIO=="SSP2_500C_CACN_NoCC",YEMF!="2020",YEMF!="2025")%>%
  pivot_wider(names_from = SCENARIO, values_from = IAMC_Template)%>%
  mutate(IAMC_Template=SSP2_500C_CACN_DAC_NoCC-SSP2_500C_CACN_NoCC)%>%
  mutate(SCENARIO="1.5c_w/DAC-1.5c_w/oDAC")%>%
  mutate(VEMF=str_replace_all(VEMF, pattern="_Res_and_Com_", replacement="_"))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p1 <- p1  + facet_grid( ~ SCENARIO ,scales="fix")
p1 <- p1　+ geom_hline(yintercept=0,color = "grey")
p1 <- p1  + geom_bar(stat="identity",aes(fill=VEMF)) 
p1 <- p1  + scale_fill_manual(labels = lb_fin2,values = fl_fin)
p1 <- p1  + ggtitle("Residential Commercial")
p1 <- p1　+ my_theme 
p1 <- p1　+ theme(strip.text.y = element_text(size = 28), 
                 strip.text.x = element_text(size = 28),
                 axis.title = element_text(size = 25),
                 legend.title= element_text(size = 27),
                 legend.text= element_text(size = 27),
                 axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 0.5,size = 22),
                 axis.text.y = element_text(size = 22),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 legend.position ="none",
                 plot.title = element_text(size = 30))
p1 <- p1 + labs(x="", y="Final energy consumption (EJ/year)", fill = "")
p2<- df_iamc%>%
  filter(REMF=="World",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",VEMF=="Fin_Ene_Ind_Ele"|VEMF=="Fin_Ene_Ind_Gas"|VEMF=="Fin_Ene_Ind_Heat"|VEMF=="Fin_Ene_Ind_Hyd"|VEMF=="Fin_Ene_Ind_Liq"|VEMF=="Fin_Ene_Ind_Solids",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(SCENARIO=="SSP2_500C_CACN_DAC_NoCC"|SCENARIO=="SSP2_500C_CACN_NoCC",YEMF!="2020",YEMF!="2025")%>%
  pivot_wider(names_from = SCENARIO, values_from = IAMC_Template)%>%
  mutate(IAMC_Template=SSP2_500C_CACN_DAC_NoCC-SSP2_500C_CACN_NoCC)%>%
  mutate(SCENARIO="1.5c_w/DAC-1.5c_w/oDAC")%>%
  mutate(VEMF=str_replace_all(VEMF, pattern="_Ind_", replacement="_"))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p2 <- p2  + facet_grid( ~ SCENARIO ,scales="fix")
p2 <- p2　+ geom_hline(yintercept=0,color = "grey")
p2 <- p2  + geom_bar(stat="identity",aes(fill=VEMF)) 
p2 <- p2  + scale_fill_manual(labels = lb_fin2,values = fl_fin)
p2 <- p2  + ggtitle("Industry")
p2 <- p2　+ my_theme 
p2 <- p2　+ theme(strip.text.y = element_text(size = 28), 
                 strip.text.x = element_text(size = 28),
                 axis.title = element_text(size = 25),
                 legend.title= element_text(size = 27),
                 legend.text= element_text(size = 27),
                 axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 0.5,size = 22),
                 axis.text.y = element_text(size = 22),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 legend.position ="none",
                 plot.title = element_text(size = 30))
p2 <- p2 + labs(x="", y="Final energy consumption (EJ/year)", fill = "")
p3<- df_iamc%>%
  filter(REMF=="World",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",VEMF=="Fin_Ene_Tra_Ele"|VEMF=="Fin_Ene_Tra_Gas"|VEMF=="Fin_Ene_Tra_Heat"|VEMF=="Fin_Ene_Tra_Hyd"|VEMF=="Fin_Ene_Tra_Liq"|VEMF=="Fin_Ene_Tra_Solids",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(SCENARIO=="SSP2_500C_CACN_DAC_NoCC"|SCENARIO=="SSP2_500C_CACN_NoCC",YEMF!="2020",YEMF!="2025")%>%
  pivot_wider(names_from = SCENARIO, values_from = IAMC_Template)%>%
  mutate(IAMC_Template=SSP2_500C_CACN_DAC_NoCC-SSP2_500C_CACN_NoCC)%>%
  mutate(SCENARIO="1.5c_w/DAC-1.5c_w/oDAC")%>%
  mutate(VEMF=str_replace_all(VEMF, pattern="_Tra_", replacement="_"))%>%
  View()
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p3 <- p3  + facet_grid( ~ SCENARIO ,scales="fix")
p3 <- p3　+ geom_hline(yintercept=0,color = "grey")
p3 <- p3  + geom_bar(stat="identity",aes(fill=VEMF)) 
p3 <- p3  + scale_fill_manual(labels = lb_fin2,values = fl_fin)
p3 <- p3  + ggtitle("Transport")
p3 <- p3　+ my_theme 
p3 <- p3　+ theme(strip.text.y = element_text(size = 28), 
                 strip.text.x = element_text(size = 28),
                 axis.title = element_text(size = 25),
                 legend.title= element_text(size = 27),
                 legend.text= element_text(size = 27),
                 axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 0.5,size = 22),
                 axis.text.y = element_text(size = 22),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 legend.position ="none",
                 plot.title = element_text(size = 30))
p3 <- p3 + labs(x="", y="Final energy consumption (EJ/year)", fill = "")

png(paste(output_path_fig,"final_energy_sec.png",sep="/"), width = 1200, height = 800)
layout1<-rbind(c(1,2,3))
p<-grid.arrange(p1, p2,p3,layout_matrix =layout1)
print(p)
dev.off()



#prc table----------------------------------------------------------------

p<-df_iamc%>%
  filter(VEMF=="Prc_Agr_NonEneCro_and_Liv_Ind"|VEMF=="Prc_Agr_NonEneCro_Ind"|VEMF=="Prc_Fin_Ene_Res_Ele"|VEMF=="Prc_Fin_Ene_Res_Gas_Nat_Gas"|VEMF=="Prc_Fin_Ene_Res_Liq_Oil"|VEMF=="Prc_Fin_Ene_Res_Liq_Bio"|VEMF=="Prc_Fin_Ene_Res_SolidsCoa",YEMF=="2050",SCENARIO=="SSP2_500C_CACN_NoCC"|SCENARIO=="SSP2_500C_CACN_DAC_NoCC",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",REMF=="World")%>%
  select(SCENARIO,IAMC_Template,VEMF)%>%
  spread(key = SCENARIO, value = IAMC_Template)%>%
  mutate(ratio = (SSP2_500C_CACN_DAC_NoCC - SSP2_500C_CACN_NoCC)*100/SSP2_500C_CACN_NoCC)%>%
  write.csv(paste(output_path_tab,"price.csv",sep="/"))



#DAC payment----------------------------------------------------------------

df_dacp <- read.csv(paste(output_path_tab,"DAC_payment_from_gdx.csv",sep="/"))

p <- df_iamc%>%
  filter(VEMF=="GDP_MER",SCENARIO!="SSP2_500C_CACN_DAC_lim_NoCC",SCENARIO=="SSP2_500C_CACN_DAC_NoCC",YEMF=="2050")%>%
  full_join(df_dacp,by = c("REMF" = "R") )%>%
  mutate(pay_rat = dac_pay/IAMC_Template/10)%>%
ggplot(aes(x=REMF , y = pay_rat))
p <- p + geom_bar(stat="identity")
p <- p + labs(x="地域", y="(%)",fill="削減シナリオ") 
p <- p + my_theme
png(paste(output_path_fig,"pay_rat.png",sep="/"), width = 900, height = 800)
print(p)
dev.off()


p <- df_iamc%>%
  filter(VEMF=="CNS",SCENARIO=="SSP2_500C_CACN_DAC_NoCC",YEMF=="2050")%>%
  full_join(df_dacp,by = c("REMF" = "R") )%>%
  mutate(pay_rat = dac_pay/IAMC_Template/10)

#dac energy-------------------------------------------------------------------------

df_dace <- read.csv(paste(output_path_tab,"DAC_ene_from_gdx.csv",sep="/"))


p <- df_dace%>%
  filter(YEMF=="2035"|YEMF=="2040"|YEMF=="2045"|YEMF=="2050")%>%
  ggplot(aes(x=YEMF , y = ej))
p <- p + geom_bar(stat="identity",width = 2)
p <- p+ ggtitle("　DACによるエネルギー消費量")
p <- p + labs(x="年", y="エネルギー消費量(EJ)") 
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p + my_theme
p <- p　+ theme(strip.text.y = element_text(size = 28), 
               strip.text.x = element_text(size = 28),
               axis.title = element_text(size = 25),
               legend.title= element_text(size = 27),
               legend.text= element_text(size = 27),
               axis.text.x = element_text(angle = 0, hjust = 0.5,size = 22),
               axis.text.y = element_text(size = 22),
               title=element_text(size=20),
               axis.title.x = element_blank())
png(paste(output_path_fig,"DAC_ene.png",sep="/"), width = 400, height = 500)
print(p)
dev.off()



p <- df_dace%>%
  filter(YEMF=="2035"|YEMF=="2040"|YEMF=="2045"|YEMF=="2050")%>%
  ggplot(aes(x=YEMF , y = ej))
p <- p + geom_bar(stat="identity",width = 3)
#p <- p+ ggtitle("　DACによるエネルギー消費量")
p <- p + labs(x="年", y="Energy consumption(EJ)") 
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p + my_theme
p <- p　+ theme(strip.text.y = element_text(size = 28), 
               strip.text.x = element_text(size = 28),
               axis.title = element_text(size = 25),
               legend.title= element_text(size = 27),
               legend.text= element_text(size = 27),
               axis.text.x = element_text(angle = 0, hjust = 0.5,size = 22),
               axis.text.y = element_text(size = 22),
               title=element_text(size=20),
               axis.title.x = element_blank())
png(paste(output_path_fig,"DAC_ene_iamc.png",sep="/"), width = 450, height = 500)
print(p)
dev.off()






