
#Title: Sub module for English paper figures
#Name: Osamu Nishiura
#Date: 2024/4/19

#Package -----------------------------------------------------------------------

library(lemon)
library(ggalluvial)

#Setting------------------------------------------------------------------------

dir_fig1<-"EgPaper"
df_sce2 <- data.frame(c("SSP2_BaU_NoCC","SSP2_500C_CACN_NoCC","SSP2_500C_CACN_Synf_NoCC","SSP2_500C_CACN_Synf_H2LowCost_NoCC","SSP2_500C_CACN_Synf_CO2LowCost_NoCC","SSP2_500C_CACN_Synf_LowCost_NoCC","SSP2_500C_CACN_DAC_NoCC"),
                     c("Baseline","1.5C_NoDAC","1.5C","1.5C_H2Low","1.5C_DACLow","1.5C_SynfLow","1.5C_DAC"),
                     c("#999999","#E41A1C","#377EB8","#FF7F00","#4DAF4A","#984EA3","#F781BF"))

v_sce2 <- df_sce2[[2]]
names(v_sce2) <- df_sce2[[1]]
colnames(df_sce2) <- c("SCENARIO","SCENARIO2","color")
v_sce2_c <- df_sce2[[3]]
names(v_sce2_c) <- df_sce2[[1]]

df_com_sum<-data.frame(com=c("COM_COA",
                             "COM_P_P",
                             "COM_COP",
                             "COM_ELY",
                             "COM_GDT",
                             "COM_wht",
                             "COM_gro",
                             "COM_osd",
                             "COM_oth_a",
                             "COM_ctl",
                             "COM_rmk",
                             "COM_oth_l",
                             "COM_FRS",
                             "COM_OMN",
                             "COM_FPR",
                             "COM_OMT",
                             "COM_LIN",
                             "COM_PPP",
                             "COM_CRP",
                             "COM_NMM",
                             "COM_I_S",
                             "COM_NFM",
                             "COM_OMF",
                             "COM_TRS",
                             "COM_CSS",
                             "COM_CNS",
                             "COM_BIO",
                             "COM_HYG",
                             "COM_SYN",
                             "COM_SYG",
                             "COM_pdr",
                             "COM_c_b"),
                       sum=c("Ene_Sol",
                             "Ene_Liq",
                             "Ene_Sol",
                             "Ene_ElH",
                             "Ene_Gas",
                             "Food",
                             "Food",
                             "Food",
                             "Food",
                             "Food",
                             "Food",
                             "Food",
                             "Other",
                             "Other",
                             "Other",
                             "Other",
                             "Other",
                             "Other",
                             "Other",
                             "Other",
                             "Other",
                             "Other",
                             "Other",
                             "CSTR",
                             "CSTR",
                             "Other",
                             "Ene_Liq",
                             "Ene_ElH",
                             "Ene_Liq",
                             "Ene_Gas",
                             "Food",
                             "Food"))

#Directory preparation----------------------------------------------------------

if (dir.exists(paste("../output/figure/",dir_fig1,sep="")) == "FALSE") {dir.create(paste("../output/figure/",dir_fig1,sep=""), recursive = T)}

#Figures------------------------------------------------------------------------
#CO2 emission constraint--------------------------------------------------------
p <- df_iamc%>%
  filter(REMF=="World",VEMF=="Emi_CO2",
         SCENARIO=="SSP2_BaU_NoCC"|SCENARIO=="SSP2_500C_CACN_Synf_NoCC")%>%
  ggplot(aes(x=year, y=IAMC_Template/1000, color = SCENARIO,group = SCENARIO))
p <- p + geom_line(linewidth = 1.3)
p <- p + scale_color_manual(labels = v_sce2, values=v_sce2_c)
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p　+ my_theme + theme(legend.position ="bottom",
                          axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 0,size = 20))
p <- p + labs(x="Year", y="CO2 emission (Gt-CO2/year)")
png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"emi_constraint.png",sep="/"), width = 2000, height = 1700,res = 300)
print(p)
dev.off()
#Carbon price and CO2 emissions ------------------------------------------------
p0<- ggplot() + theme(panel.background = element_rect(fill = "transparent", colour = "transparent"))

p <- df_iamc%>%
  filter(VEMF=="Prc_Car",REMF=="World",YEMF!="2010",YEMF!="2015",
         SCENARIO=="SSP2_BaU_NoCC"|SCENARIO=="SSP2_500C_CACN_NoCC"|SCENARIO=="SSP2_500C_CACN_Synf_NoCC")%>%
  ggplot(aes(x=year , y = IAMC_Template, color = SCENARIO,group = SCENARIO)) 
#p <- p + facet_wrap(. ~ VEMF, scales="free", nrow=1,labeller = as_labeller(c(Pol_Cos_GDP_Los_rat="GDP",Pol_Cos_Cns_Los_rat="Consumption",Pol_Cos_Equ_Var_rat="Equivalent variation")))
p <- p + geom_line(linewidth=0.9)
p <- p + scale_color_manual(labels = v_sce2,values=v_sce2_c)
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + labs(x="Year", y="Carboin price (US$)",color="Scenario") 
p <- p + my_theme + theme(legend.position = "bottom",
                          legend.direction = "vertical")
p1<- p + ggtitle("a")+theme(plot.title = element_text(size = 25)) 
png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"Carbon_price.png",sep="/"), width = 3000, height = 1500,res = 300)
print(p)
dev.off()

p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015",
         SCENARIO=="SSP2_BaU_NoCC"|SCENARIO=="SSP2_500C_CACN_NoCC"|SCENARIO=="SSP2_500C_CACN_Synf_NoCC")%>%
  filter(VEMF %in% df_emi[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_emi[1,]))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template/1000))
p <- p  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce2))
p <- p  + geom_bar(stat="identity",aes(fill=VEMF)) 
#p <- p  + scale_fill_manual(values = c("green4","blue","red","yellow","purple","chocolate","navy","grey","pink","skyblue","gold"))
p <- p  + scale_fill_manual(values = df_emi[4,], labels = df_emi[2,])
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p  + scale_x_discrete(breaks = c("2020","2030","2040","2050","2060","2070","2080","2090","2100"))
p <- p　+ my_theme + theme(legend.position ="bottom") + guides(fill=guide_legend(nrow=4,byrow=FALSE))
p <- p + labs(x="Year", y="CO2 emission (Gt-CO2/year)", fill = "")
p2<- p + ggtitle("b")+theme(plot.title = element_text(size = 25))  
png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"emission.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*500, height = 2500,res = 300)
print(p)
dev.off()

png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"emi_cpri.png",sep="/"), width = 4300, height = 2150,res = 300)
#layout1<-rbind(c(1,1,1,3),c(1,1,1,3),c(2,2,2,2),c(2,2,2,2),c(2,2,2,2))
layout1<-rbind(c(1,2,2,2))
p<-grid.arrange(p1, p2, layout_matrix =layout1)
print(p)
dev.off()




#Primary and Final energy ------------------------------------------------------
p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015",
         SCENARIO=="SSP2_BaU_NoCC"|SCENARIO=="SSP2_500C_CACN_NoCC"|SCENARIO=="SSP2_500C_CACN_Synf_NoCC")%>%
  filter(VEMF %in% df_pri[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_pri[1,]))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce2))
p <- p  + geom_bar(stat="identity",aes(fill=VEMF)) 
p <- p  + scale_x_discrete(breaks = c("2020","2030","2040","2050","2060","2070","2080","2090","2100"))
p <- p  + scale_fill_manual(values = df_pri[4,], labels = df_pri[2,])
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p + labs(x="Year", y="Primary energy (EJ/year)", fill = "Source")
p <- p　+ my_theme 
p1<- p + ggtitle("a")+theme(plot.title = element_text(size = 25)) 
png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"primary_energy.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*600+500, height = 2000,res = 300)
print(p)
dev.off()

p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015",
         SCENARIO=="SSP2_BaU_NoCC"|SCENARIO=="SSP2_500C_CACN_NoCC"|SCENARIO=="SSP2_500C_CACN_Synf_NoCC")%>%
  filter(!(str_detect(VEMF, "^Fin_Ene_Ind_|^Fin_Ene_Tra_|^Fin_Ene_Bui_")))%>%
  filter(VEMF %in% df_fin[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_fin[1,]))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce2))
p <- p  + geom_bar(stat="identity",aes(fill=VEMF)) 
p <- p  + scale_x_discrete(breaks = c("2020","2030","2040","2050","2060","2070","2080","2090","2100"))
p <- p  + scale_fill_manual(values = df_fin[4,], labels = df_fin[2,])
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p　+ my_theme 
p <- p  + labs(x="Year", y="Final energy (EJ/year)", fill = "fuel")
p2<- p + ggtitle("b")+theme(plot.title = element_text(size = 25))  
png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"Final_energy.png",sep="/"), width = length(unique(df_iamc$SCENARIO))*600+500, height = 2000,res = 300)
print(p)
dev.off()

png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"pri_fin.png",sep="/"), width = 4000, height = 3000,res = 300)
layout1<-rbind(c(1),c(2))
p<-grid.arrange(p1, p2, layout_matrix =layout1)
print(p)
dev.off()

#Hydrogen CO2 flow --------------------------------------------------------------
p1<- df_iamc%>%
  filter(REMF=="World",YEMF=="2100",
         SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
           SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
           SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
           SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
         VEMF=="Fin_Ene_Tra_Hyd"|
           VEMF=="Fin_Ene_Ind_Hyd"|
           VEMF=="Fin_Ene_Res_and_Com_Hyd"|
           VEMF=="Fin_Ene_Tra_Liq_Hyd_syn"|
           VEMF=="Fin_Ene_Ind_Liq_Hyd_syn"|
           VEMF=="Fin_Ene_Res_and_Com_Liq_Hyd_syn"|
           VEMF=="Fin_Ene_Tra_Gas_Hyd_syn"|
           VEMF=="Fin_Ene_Ind_Gas_Hyd_syn"|
           VEMF=="Fin_Ene_Res_and_Com_Gas_Hyd_syn"
  )%>%
  select("SCENARIO","SCENARIO2","VEMF","IAMC_Template")%>%
  bind_rows(df_iamc%>%
              filter(REMF=="World",YEMF=="2100",
                     SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                       SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                       SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                       SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                     VEMF=="Sec_Ene_Inp_Hyd_Liq_Hyd_syn")%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Sec_Ene_Liq_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
      mutate(VEMF="Sec_Ene_Liq_Hyd_syn_loss",
             IAMC_Template=IAMC_Template-Sec_Ene_Liq_Hyd_syn)%>%
      select("SCENARIO","SCENARIO2","VEMF","IAMC_Template"))%>%
  bind_rows(df_iamc%>%
      filter(REMF=="World",YEMF=="2100",
             SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
               SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
               SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
               SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
             VEMF=="Sec_Ene_Inp_Hyd_Gas_Hyd_syn")%>%
      left_join(df_iamc%>%
                  filter(REMF=="World",YEMF=="2100",
                         SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                           SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                           SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                           SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                         VEMF=="Sec_Ene_Gas_Hyd_syn")%>%
                  pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
      mutate(VEMF="Sec_Ene_Gas_Hyd_syn_loss",
             IAMC_Template=IAMC_Template-Sec_Ene_Gas_Hyd_syn)%>%
      select("SCENARIO","SCENARIO2","VEMF","IAMC_Template"))%>%
  bind_rows(df_iamc%>%
      filter(REMF=="World",YEMF=="2100",
               SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
               SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
               SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
             VEMF=="Sec_Ene_Hyd")%>%
      mutate(VEMF="Sec_Ene_Hyd_loss",
             IAMC_Template=IAMC_Template*0.4/0.6)%>%
      select("SCENARIO","SCENARIO2","VEMF","IAMC_Template"))%>%
  bind_rows(df_iamc%>%
      filter(REMF=="World",YEMF=="2100",
             SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC",
             VEMF=="Sec_Ene_Hyd")%>%
      mutate(VEMF="Sec_Ene_Hyd_loss",
             IAMC_Template=IAMC_Template*0.3/0.7)%>%
      select("SCENARIO","SCENARIO2","VEMF","IAMC_Template"))%>%
  bind_rows(df_iamc%>%
      filter(REMF=="World",YEMF=="2100",
             SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
               SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
               SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
               SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
             VEMF=="Sec_Ene_Inp_Tot_DAC")%>%
      left_join(df_sam%>%
                  filter(i=="COM_ACO2",year=="2100",
                         scenario=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                           scenario=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                           scenario=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                           scenario=="SSP2_500C_CACN_Synf_NoCC")%>%
                  group_by(j,scenario)%>%
                  summarise(volume=sum(volume))%>%
                  ungroup()%>%
                  pivot_wider(names_from = j, values_from = volume),
                by = c("SCENARIO"="scenario"))%>%
      mutate(
        Sec_Ene_Inp_DACCS = IAMC_Template*STO/(STO+SYN+SYG),
        Sec_Ene_Inp_DAC_Liq_syn = IAMC_Template*SYN/(STO+SYN+SYG),
        Sec_Ene_Inp_DAC_Gas_syn = IAMC_Template*SYG/(STO+SYN+SYG),
      )%>%
      select(c("SCENARIO","SCENARIO2","Sec_Ene_Inp_DACCS","Sec_Ene_Inp_DAC_Liq_syn","Sec_Ene_Inp_DAC_Gas_syn"))%>%
      pivot_longer(cols = -c(SCENARIO, SCENARIO2), names_to = "VEMF", values_to = "IAMC_Template"))%>%
  mutate(axis4 = case_when( 
    VEMF == "Fin_Ene_Ind_Hyd" ~ "Industry", 
    VEMF == "Fin_Ene_Tra_Hyd" ~ "Transport",
    VEMF == "Fin_Ene_Res_and_Com_Hyd" ~ "Building",
    VEMF == "Fin_Ene_Ind_Liq_Hyd_syn" ~ "Industry", 
    VEMF == "Fin_Ene_Tra_Liq_Hyd_syn" ~ "Transport",
    VEMF == "Fin_Ene_Res_and_Com_Liq_Hyd_syn" ~ "Building",
    VEMF == "Fin_Ene_Ind_Gas_Hyd_syn" ~ "Industry", 
    VEMF == "Fin_Ene_Tra_Gas_Hyd_syn" ~ "Transport",
    VEMF == "Fin_Ene_Res_and_Com_Gas_Hyd_syn" ~ "Building",
    VEMF == "Sec_Ene_Hyd_loss" ~ "Loss",
    VEMF == "Sec_Ene_Liq_Hyd_syn_loss" ~ "Loss",
    VEMF == "Sec_Ene_Gas_Hyd_syn_loss" ~ "Loss",
    VEMF == "Sec_Ene_Inp_DAC_Liq_syn" ~ "Loss",
    VEMF == "Sec_Ene_Inp_DAC_Gas_syn" ~ "Loss",
    VEMF == "Sec_Ene_Inp_DACCS" ~ "CDR",
    TRUE ~ "Loss"),
    axis3 = case_when( 
      VEMF == "Fin_Ene_Ind_Hyd" ~ NA, 
      VEMF == "Fin_Ene_Tra_Hyd" ~ NA,
      VEMF == "Fin_Ene_Res_and_Com_Hyd" ~ NA,
      VEMF == "Fin_Ene_Ind_Liq_Hyd_syn" ~ "Liquid\n synfuel", 
      VEMF == "Fin_Ene_Tra_Liq_Hyd_syn" ~ "Liquid\n synfuel",
      VEMF == "Fin_Ene_Res_and_Com_Liq_Hyd_syn" ~ "Liquid\n synfuel",
      VEMF == "Fin_Ene_Ind_Gas_Hyd_syn" ~ "Gas synfuel", 
      VEMF == "Fin_Ene_Tra_Gas_Hyd_syn" ~ "Gas synfuel",
      VEMF == "Fin_Ene_Res_and_Com_Gas_Hyd_syn" ~ "Gas synfuel",
#      VEMF == "Sec_Ene_Hyd_loss" ~ "H2 Loss",
      VEMF == "Sec_Ene_Hyd_loss" ~ NA,
      VEMF == "Sec_Ene_Liq_Hyd_syn_loss" ~ NA,
      VEMF == "Sec_Ene_Gas_Hyd_syn_loss" ~ NA,
      VEMF == "Sec_Ene_Inp_DAC_Liq_syn" ~ "Liquid\n synfuel",
      VEMF == "Sec_Ene_Inp_DAC_Gas_syn" ~ "Gas synfuel",
      VEMF == "Sec_Ene_Inp_DACCS" ~ NA,
      TRUE ~ "Loss"),
    axis2 = case_when( 
      VEMF == "Fin_Ene_Ind_Hyd" ~ "Hydrogen", 
      VEMF == "Fin_Ene_Tra_Hyd" ~ "Hydrogen",
      VEMF == "Fin_Ene_Res_and_Com_Hyd" ~ "Hydrogen",
      VEMF == "Fin_Ene_Ind_Liq_Hyd_syn" ~ "Hydrogen", 
      VEMF == "Fin_Ene_Tra_Liq_Hyd_syn" ~ "Hydrogen",
      VEMF == "Fin_Ene_Res_and_Com_Liq_Hyd_syn" ~ "Hydrogen",
      VEMF == "Fin_Ene_Ind_Gas_Hyd_syn" ~ "Hydrogen", 
      VEMF == "Fin_Ene_Tra_Gas_Hyd_syn" ~ "Hydrogen",
      VEMF == "Fin_Ene_Res_and_Com_Gas_Hyd_syn" ~ "Hydrogen",
      VEMF == "Sec_Ene_Hyd_loss" ~ NA,
      VEMF == "Sec_Ene_Liq_Hyd_syn_loss" ~ "Hydrogen",
      VEMF == "Sec_Ene_Gas_Hyd_syn_loss" ~ "Hydrogen",
      VEMF == "Sec_Ene_Inp_DAC_Liq_syn" ~ "CO2",
      VEMF == "Sec_Ene_Inp_DAC_Gas_syn" ~ "CO2",
      VEMF == "Sec_Ene_Inp_DACCS" ~ "CO2",
      TRUE ~ "Loss"),
    axis1 = case_when( 
      VEMF == "Fin_Ene_Ind_Hyd" ~ "Electricity", 
      VEMF == "Fin_Ene_Tra_Hyd" ~ "Electricity",
      VEMF == "Fin_Ene_Res_and_Com_Hyd" ~ "Electricity",
      VEMF == "Fin_Ene_Ind_Liq_Hyd_syn" ~ "Electricity", 
      VEMF == "Fin_Ene_Tra_Liq_Hyd_syn" ~ "Electricity",
      VEMF == "Fin_Ene_Res_and_Com_Liq_Hyd_syn" ~ "Electricity",
      VEMF == "Fin_Ene_Ind_Gas_Hyd_syn" ~ "Electricity", 
      VEMF == "Fin_Ene_Tra_Gas_Hyd_syn" ~ "Electricity",
      VEMF == "Fin_Ene_Res_and_Com_Gas_Hyd_syn" ~ "Electricity",
      VEMF == "Sec_Ene_Hyd_loss" ~ "Electricity",
      VEMF == "Sec_Ene_Liq_Hyd_syn_loss" ~ "Electricity",
      VEMF == "Sec_Ene_Gas_Hyd_syn_loss" ~ "Electricity",
      VEMF == "Sec_Ene_Inp_DAC_Liq_syn" ~ "Nat. gas",
      VEMF == "Sec_Ene_Inp_DAC_Gas_syn" ~ "Nat. gas",
      VEMF == "Sec_Ene_Inp_DACCS" ~ "Nat. gas",
      TRUE ~ "Loss"))%>%
  mutate(
    axis1 = factor(axis1,levels = c("Nat. gas","Electricity")),
    axis2 = factor(axis2,levels = c("CO2","Hydrogen")),
    axis3 = factor(axis3,levels = c("Direct use","Gas synfuel","Liquid\n synfuel","DACCS")),
    axis4 = factor(axis4,levels = c("Loss","Transport","Industry","Building","CDR"))
  )%>%
#write_csv(paste(df_path[df_path$name=="table",]$path,"main/synf_ene_flow.csv",sep="/"))
  filter(axis4!="CDR")%>%
  ggplot(aes(y=IAMC_Template,fill =interaction(axis1,axis4,sep="_"), axis1 = axis1, axis2 = axis2, axis3 = axis3, axis4 = axis4)) +
  facet_grid(. ~ fct_rev(SCENARIO), labeller = as_labeller(v_sce2)) +
  scale_x_discrete(limits = c("Input\n ", "DAC\n Electrolysis", "Synfuel\nprod.", "Enduse\n "), expand = c(.1, .1)) +
  geom_alluvium(alpha=0.9,color="white",width = 0.6) + 
  geom_stratum(fill="white",alpha=0.3,color="black",width = 0.6) +
#  geom_lode() +
  guides(fill = "none") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  scale_fill_manual(values = c("Nat. gas_Loss"="pink","Electricity_Loss"="grey","Electricity_Transport"="skyblue","Electricity_Industry"="skyblue","Electricity_Building"="skyblue")) +
#  scale_y_continuous(sec.axis = sec_axis(~ . / 1, name = "CO2 flow (Gt-CO2/yr)")) +
  labs(x = "", y = "Energy flow (EJ/yr)") + 
  my_theme +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 0,size = 15),
        axis.title = element_text(size = 15)) + 
  ggtitle("a") +
  theme(plot.title = element_text(size = 25)) 

p2<- df_iamc%>%
  filter(REMF=="World",YEMF=="2100",
         SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
           SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
           SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
           SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
         VEMF=="Car_Seq_Dir_Air_Cap"|
           VEMF=="Car_Seq_Lan_Use_Aff"|
           VEMF=="Car_Seq_CCS_Bio")%>%
  select("SCENARIO","SCENARIO2","VEMF","IAMC_Template")%>%
  mutate(IAMC_Template=IAMC_Template/1000)%>%
  bind_rows(df_iamc%>%
              filter(REMF=="World",YEMF=="2100",
                     SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                       SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                       SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                       SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                     VEMF=="Car_Seq_Dir_Air_Cap")%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Liq_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Ind_Liq_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Tra_Liq_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Res_and_Com_Liq_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Gas_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Ind_Gas_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Tra_Gas_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Res_and_Com_Gas_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_sam%>%
                          filter(i=="COM_ACO2",year=="2100",
                                 scenario=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   scenario=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   scenario=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   scenario=="SSP2_500C_CACN_Synf_NoCC")%>%
                          group_by(j,scenario)%>%
                          summarise(volume=sum(volume))%>%
                          ungroup()%>%
                          pivot_wider(names_from = j, values_from = volume),
                        by = c("SCENARIO"="scenario"))%>%
              mutate(
                Car_Use_Ind_Liq_syn = IAMC_Template*SYN/STO*Fin_Ene_Ind_Liq_Hyd_syn/Fin_Ene_Liq_Hyd_syn/1000,
                Car_Use_Tra_Liq_syn = IAMC_Template*SYN/STO*Fin_Ene_Tra_Liq_Hyd_syn/Fin_Ene_Liq_Hyd_syn/1000,
                Car_Use_Bui_Liq_syn = IAMC_Template*SYN/STO*Fin_Ene_Res_and_Com_Liq_Hyd_syn/Fin_Ene_Liq_Hyd_syn/1000,
                Car_Use_Ind_Gas_syn = IAMC_Template*SYG/STO*Fin_Ene_Ind_Gas_Hyd_syn/Fin_Ene_Gas_Hyd_syn/1000,
                Car_Use_Bui_Gas_syn = IAMC_Template*SYG/STO*Fin_Ene_Res_and_Com_Gas_Hyd_syn/Fin_Ene_Gas_Hyd_syn/1000,
                Car_Use_syn = IAMC_Template*(SYN+SYG)/STO/1000
              )%>%
              select(c("SCENARIO","SCENARIO2","Car_Use_Ind_Liq_syn","Car_Use_Tra_Liq_syn","Car_Use_Bui_Liq_syn","Car_Use_Ind_Gas_syn","Car_Use_Bui_Gas_syn","Car_Use_syn"))%>%
              pivot_longer(cols = -c(SCENARIO, SCENARIO2), names_to = "VEMF", values_to = "IAMC_Template"))%>%
  mutate(axis1 = case_when( 
    VEMF == "Car_Seq_CCS_Bio" ~ "BECCS", 
    VEMF == "Car_Seq_Dir_Air_Cap" ~ "DAC",
    VEMF == "Car_Seq_Lan_Use_Aff" ~ "Afforestation",
    VEMF == "Car_Use_Ind_Liq_syn" ~ "DAC", 
    VEMF == "Car_Use_Tra_Liq_syn" ~ "DAC",
    VEMF == "Car_Use_Bui_Liq_syn" ~ "DAC",
    VEMF == "Car_Use_Ind_Gas_syn" ~ "DAC", 
    VEMF == "Car_Use_Bui_Gas_syn" ~ "DAC",
    VEMF == "Car_Use_syn" ~ "DAC",
    TRUE ~ "Loss"),
    axis3 = case_when( 
      VEMF == "Car_Seq_CCS_Bio" ~ "Underground", 
      VEMF == "Car_Seq_Dir_Air_Cap" ~ "Underground",
      VEMF == "Car_Seq_Lan_Use_Aff" ~ "Land",
      VEMF == "Car_Use_Ind_Liq_syn" ~ "Atmosphere", 
      VEMF == "Car_Use_Tra_Liq_syn" ~ "Atmosphere",
      VEMF == "Car_Use_Bui_Liq_syn" ~ "Atmosphere",
      VEMF == "Car_Use_Ind_Gas_syn" ~ "Atmosphere", 
      VEMF == "Car_Use_Bui_Gas_syn" ~ "Atmosphere",
      VEMF == "Car_Use_syn" ~ "Atmosphere",
      TRUE ~ "Loss"),
    axis2 = case_when( 
      VEMF == "Car_Seq_CCS_Bio" ~ "CDR", 
      VEMF == "Car_Seq_Dir_Air_Cap" ~ "CDR",
      VEMF == "Car_Seq_Lan_Use_Aff" ~ "CDR",
      VEMF == "Car_Use_Ind_Liq_syn" ~ "CCU", 
      VEMF == "Car_Use_Tra_Liq_syn" ~ "CCU",
      VEMF == "Car_Use_Bui_Liq_syn" ~ "CCU",
      VEMF == "Car_Use_Ind_Gas_syn" ~ "CCU", 
      VEMF == "Car_Use_Bui_Gas_syn" ~ "CCU",
      VEMF == "Car_Use_syn" ~ "CCU",
      TRUE ~ "Loss"))%>%
 mutate(
   axis1 = factor(axis1,levels = c("DAC","BECCS","Afforestation")),
   axis2 = factor(axis2,levels = c("CCU","CDR")),
   axis3 = factor(axis3,levels = c("Atmosphere","Land","Underground"))
 )%>%
  filter(VEMF!="Car_Use_Ind_Liq_syn",VEMF!="Car_Use_Tra_Liq_syn",VEMF!="Car_Use_Bui_Liq_syn",VEMF!="Car_Use_Ind_Gas_syn",VEMF!="Car_Use_Bui_Gas_syn")%>%
  ggplot(aes(y=IAMC_Template,fill =interaction(axis1,axis3,sep="_"), axis1 = axis1, axis2 = axis2, axis3 = axis3)) +
  facet_grid(. ~ fct_rev(SCENARIO), labeller = as_labeller(v_sce2)) +
  scale_x_discrete(limits = c("Input", "Process", "Destination"), expand = c(.15, .15)) +
  geom_alluvium(alpha=0.9,color="white",width = 0.4) + 
  geom_stratum(fill="white",alpha=0.3,color="black",width = 0.4) +
  guides(fill = "none") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  scale_fill_manual(values = c("DAC_Underground"="pink","DAC_Atmosphere"="grey","BECCS_Underground"="olivedrab2","Afforestation_Land"="skyblue")) +
  #  scale_y_continuous(sec.axis = sec_axis(~ . / 1, name = "CO2 flow (Gt-CO2/yr)")) +
  labs(x = "", y = "Carbon flow (Gt-CO2/yr)") + 
  my_theme +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust = 0,size = 15),
        axis.title = element_text(size = 15)) +
  ggtitle("b") +
  theme(plot.title = element_text(size = 25)) 

png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"H2_c_flow.png",sep="/"), width = 4500, height = 3000,res = 300)
layout1<-rbind(c(1,1,1,1),c(1,1,1,1),c(1,1,1,1),c(1,1,1,1),c(1,1,1,1),c(1,1,1,1),c(2,2,2,2),c(2,2,2,2),c(2,2,2,2),c(2,2,2,2),c(2,2,2,2))
p<-grid.arrange(p1, p2, layout_matrix =layout1)
print(p)
dev.off()







#Economic impact ---------------------------------------------------------------
p <- df_iamc%>%
  filter(VEMF=="Pol_Cos_GDP_Los_rat"|VEMF=="Pol_Cos_Cns_Los_rat",REMF=="World",YEMF!="2010",YEMF!="2015",
         SCENARIO=="SSP2_BaU_NoCC"|SCENARIO=="SSP2_500C_CACN_NoCC"|SCENARIO=="SSP2_500C_CACN_Synf_NoCC")%>%
  ggplot(aes(x=year , y = IAMC_Template, color = SCENARIO,group = SCENARIO)) 
p <- p + facet_wrap(. ~ VEMF, scales="free", nrow=2,labeller = as_labeller(c(Pol_Cos_GDP_Los_rat="GDP",Pol_Cos_Cns_Los_rat="Consumption",Pol_Cos_Equ_Var_rat="Equivalent variation")))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + geom_line(linewidth=1.1)
p <- p + scale_color_manual(labels = v_sce2, values = v_sce2_c)

p <- p + labs(x="Year", y="Economic loss (%)",color="Scenarios") 
p <- p + my_theme + theme(legend.position = "none")
p1<- p + ggtitle("a")+theme(plot.title = element_text(size = 25)) 
png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"GDP_HH_loss.png",sep="/"), width = 2000, height = 1500,res = 300)
print(p)
dev.off()

p<- df_sam%>%
  filter(j=="HURB",scenario=="SSP2_500C_CACN_NoCC"|scenario=="SSP2_500C_CACN_Synf_NoCC")%>%
  left_join(df_com_sum,by= c("i"="com"))%>%
  mutate(value=price*volume)%>%
  group_by(year,scenario,sum)%>%
  summarise(value=sum(value), volume=sum(volume))%>%
  ungroup()%>%
  mutate(price=value/volume)%>%
left_join(
  filter(df_sam,j=="HURB",scenario=="SSP2_BaU_NoCC")%>%
    left_join(df_com_sum,by= c("i"="com"))%>%
    mutate(value=price*volume)%>%
    group_by(year,scenario,sum)%>%
    summarise(value=sum(value), volume=sum(volume))%>%
    ungroup()%>%
    mutate(price=value/volume),
  by = c("year","sum"))%>%
  mutate(price_change = 100*(price.x-price.y)/price.y,
         sum = factor(sum, levels=c("Ene_ElH","Ene_Gas","Ene_Liq","Ene_Sol","Food","CSTR","Other")))%>%
  ggplot(aes(x=year , y = price_change, color = scenario.x,group = scenario.x)) 
p <- p + facet_wrap(. ~ sum, scales="free", nrow=2,labeller = as_labeller(c(Ene_ElH="Electricity",Ene_Gas="Energy_Gas",Ene_Liq="Energy_Liquid",Ene_Sol="Energy_Solid",Food="Food",CSTR="Service and Transport",Other="Other")))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + geom_line(linewidth=1.1)
p <- p + scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))
p <- p + scale_color_manual(labels = v_sce2, values = v_sce2_c)

p <- p + labs(x="Year", y="Price change (%)",color="Scenarios") 
p <- p + my_theme + theme(legend.position = c(0.85,0.2))
p2<- p + ggtitle("b")+theme(plot.title = element_text(size = 25)) 
png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"Price_change.png",sep="/"), width = 5000, height = 3000,res = 300)
print(p)
dev.off()

png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"gdp_hh_prc.png",sep="/"), width = 4600, height = 2300,res = 300)
layout1<-rbind(c(1,2,2,2,2))
p<-grid.arrange(p1, p2, layout_matrix =layout1)
print(p)
dev.off()






p <- df_iamc%>%
#  filter(VEMF=="GDP_MER"|VEMF=="CNS",
  filter(VEMF=="Pol_Cos_GDP_Los"|VEMF=="Pol_Cos_Cns_Los",
         REMF=="World",YEMF!="2010",YEMF!="2015",
         SCENARIO=="SSP2_500C_CACN_Synf_NoCC"|SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC")%>%
  left_join(filter(df_iamc,VEMF=="Pol_Cos_GDP_Los"|VEMF=="Pol_Cos_Cns_Los",REMF=="World",YEMF!="2010",YEMF!="2015",
                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC"),
            by = c("VEMF","YEMF","year"))%>%
  mutate(IAMC_Template=100*(IAMC_Template.x-IAMC_Template.y)/IAMC_Template.y)%>%
  ggplot(aes(x=year, y = IAMC_Template, color = SCENARIO.x,group = SCENARIO.x)) 
p <- p + facet_wrap(. ~ VEMF, scales="free", nrow=2,labeller = as_labeller(c(Pol_Cos_GDP_Los="GDP loss",Pol_Cos_Cns_Los="Consumption loss")))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + geom_line(linewidth=1.1)
p <- p + scale_color_manual(labels = v_sce2, values = v_sce2_c)

p <- p + labs(x="Year", y="Economic indicator difference(%)",color="Scenarios") 
p <- p + my_theme + theme(legend.position = "none",legend.direction="vertical",axis.title = element_text(size = 20))
p1<- p + ggtitle("a")+theme(plot.title = element_text(size = 25)) 

p<- df_sam%>%
  filter(j=="HURB",scenario=="SSP2_500C_CACN_Synf_NoCC"|scenario=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|scenario=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|scenario=="SSP2_500C_CACN_Synf_LowCost_NoCC")%>%
  left_join(df_com_sum,by= c("i"="com"))%>%
  mutate(value=price*volume)%>%
  group_by(year,scenario,sum)%>%
  summarise(value=sum(value), volume=sum(volume))%>%
  ungroup()%>%
  mutate(price=value/volume)%>%
  left_join(
    filter(df_sam,j=="HURB",scenario=="SSP2_500C_CACN_Synf_NoCC")%>%
      left_join(df_com_sum,by= c("i"="com"))%>%
      mutate(value=price*volume)%>%
      group_by(year,scenario,sum)%>%
      summarise(value=sum(value), volume=sum(volume))%>%
      ungroup()%>%
      mutate(price=value/volume),
    by = c("year","sum"))%>%
  mutate(price_change = 100*(price.x-price.y)/price.y,
         sum = factor(sum, levels=c("Ene_ElH","Ene_Gas","Ene_Liq","Ene_Sol","Food","CSTR","Other")))%>%
  ggplot(aes(x=year , y = price_change, color = scenario.x,group = scenario.x)) 
p <- p + facet_wrap(. ~ sum, scales="free", nrow=2,labeller = as_labeller(c(Ene_ElH="Electricity",Ene_Gas="Energy_Gas",Ene_Liq="Energy_Liquid",Ene_Sol="Energy_Solid",Food="Food",CSTR="Service and Transport",Other="Other")))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + geom_line(linewidth=1.1)
p <- p + scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))
p <- p + scale_color_manual(labels = v_sce2, values = v_sce2_c)

p <- p + labs(x="Year", y="Price change (%)",color="Scenarios") 
p <- p + my_theme + theme(legend.position = c(0.85,0.2))
p2<- p + ggtitle("b")+theme(plot.title = element_text(size = 25)) 

png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"gdp_hh_prc2.png",sep="/"), width = 4600, height = 2300,res = 300)
layout1<-rbind(c(1,2,2,2,2))
p<-grid.arrange(p1, p2, layout_matrix =layout1)
print(p)
dev.off()














p <- df_iamc%>%
  filter(VEMF=="Prc_Car",REMF=="World",YEMF!="2010",YEMF!="2015",
         SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|SCENARIO=="SSP2_500C_CACN_Synf_NoCC")%>%
  ggplot(aes(x=year , y = IAMC_Template, color = SCENARIO,group = SCENARIO)) 
#p <- p + facet_wrap(. ~ VEMF, scales="free", nrow=1,labeller = as_labeller(c(Pol_Cos_GDP_Los_rat="GDP",Pol_Cos_Cns_Los_rat="Consumption",Pol_Cos_Equ_Var_rat="Equivalent variation")))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + geom_line(linewidth=0.9)
p <- p + scale_color_manual(labels = v_sce2, values = v_sce2_c)

p <- p + labs(x="Year", y="Carbon price (US$)",color="Scenario") 
p <- p + my_theme + theme(legend.position = "right",
                          legend.direction = "vertical")
p1 <- g_legend(p)
p <- p + my_theme + theme(legend.position = "none",
                          legend.direction = "vertical")
p2<- p + ggtitle("a")+theme(plot.title = element_text(size = 25)) 

p <- df_iamc%>%
  filter(VEMF=="GDP_MER"|VEMF=="CNS",REMF=="World",YEMF!="2010",YEMF!="2015",
         SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC")%>%
  left_join(filter(df_iamc,VEMF=="GDP_MER"|VEMF=="CNS",REMF=="World",YEMF!="2010",YEMF!="2015",
                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC"),
            by = c("VEMF","YEMF","year"))%>%
  mutate(IAMC_Template=100*(IAMC_Template.x-IAMC_Template.y)/IAMC_Template.y)%>%
  ggplot(aes(x=year, y = IAMC_Template, color = SCENARIO.x,group = SCENARIO.x)) 
p <- p + facet_wrap(. ~ VEMF, scales="free", nrow=1,labeller = as_labeller(c(GDP_MER="GDP",CNS="Consumption")))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + geom_line(linewidth=1.1)
p <- p + scale_color_manual(labels = v_sce2, values = v_sce2_c)

p <- p + labs(x="Year", y="Economic indicator difference(%)",color="Scenarios") 
p <- p + my_theme + theme(legend.position = "bottom",legend.direction="vertical",axis.title = element_text(size = 20))
png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"gdp_hh.png",sep="/"), width = 2500, height = 2200,res = 300)
print(p)
dev.off()
p <- p + my_theme + theme(legend.position = "none")
p3<- p + ggtitle("b")+theme(plot.title = element_text(size = 25)) 

#i=="COM_COA"|i=="COM_P_P"|i=="COM_COP"|i=="COM_ELY"|i=="COM_GDT"|i=="COM_BIO"|i=="COM_HYG"|i=="COM_SYN"|i=="COM_SYG"|i=="COM_ACO2"
p<-df_sam%>%
  filter(j=="HURB"|j=="STO",i=="COM_HYG"|i=="COM_SYN"|i=="COM_SYG"|i=="COM_ACO2",scenario=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|scenario=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|scenario=="SSP2_500C_CACN_Synf_LowCost_NoCC"|scenario=="SSP2_500C_CACN_Synf_NoCC")%>%
  mutate(value=price*volume)%>%
  group_by(year,scenario,i)%>%
  summarise(value=sum(value), volume=sum(volume))%>%
  ungroup()%>%
  mutate(price=value/volume)%>%
  left_join(df_sam%>%
             filter(j=="HURB"|j=="STO",i=="COM_HYG"|i=="COM_SYN"|i=="COM_SYG"|i=="COM_ACO2",scenario=="SSP2_500C_CACN_Synf_NoCC")%>%
             mutate(value=price*volume)%>%
             group_by(year,scenario,i)%>%
             summarise(value=sum(value), volume=sum(volume))%>%
             ungroup()%>%
             mutate(price=value/volume),
    by = c("year","i"))%>%
  mutate(price_change = 100*(price.x-price.y)/price.y)%>%
  ggplot(aes(x=year , y = price_change, color = scenario.x,group = scenario.x)) 
p <- p + facet_wrap(. ~ i, scales="free", nrow=2,labeller = as_labeller(c(COM_HYG="Hydrogen",COM_SYN="Synfuel liquid",COM_SYG="Synfuel Gas",COM_ACO2="CO2")))
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + geom_line(linewidth=1.1)
p <- p + scale_x_discrete(breaks = c("2020","2040","2060","2080","2100"))
p <- p + scale_color_manual(labels = v_sce2, values = v_sce2_c)
p <- p + labs(x="Year", y="Price difference (%)",color="Scenarios") 
p <- p + my_theme + theme(legend.position = "none")
p4<- p + ggtitle("b")+theme(plot.title = element_text(size = 25)) 


png(paste(df_path[df_path$name=="figure",]$path,dir_fig1,"gdp_hh_eneprc.png",sep="/"), width = 2000, height = 2800,res = 300)
layout1<-rbind(c(2,1),c(3,3),c(3,3))
p<-grid.arrange(p1,p2,p4, layout_matrix =layout1)
print(p)
dev.off()


#TRB --------------------------------------------------------------
df_iamc%>%
  filter(REMF=="World",YEMF=="2050",
         SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
           SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
           SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
           SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
         VEMF=="Car_Seq_Dir_Air_Cap"|
           VEMF=="Car_Seq_Lan_Use_Aff"|
           VEMF=="Car_Seq_CCS_Bio")%>%
  select("SCENARIO","SCENARIO2","VEMF","IAMC_Template")%>%
  mutate(IAMC_Template=IAMC_Template/1000)%>%
  bind_rows(df_iamc%>%
              filter(REMF=="World",YEMF=="2100",
                     SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                       SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                       SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                       SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                     VEMF=="Car_Seq_Dir_Air_Cap")%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Liq_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Ind_Liq_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Tra_Liq_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Res_and_Com_Liq_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Gas_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Ind_Gas_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Tra_Gas_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_iamc%>%
                          filter(REMF=="World",YEMF=="2100",
                                 SCENARIO=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   SCENARIO=="SSP2_500C_CACN_Synf_NoCC",
                                 VEMF=="Fin_Ene_Res_and_Com_Gas_Hyd_syn")%>%
                          pivot_wider(names_from = VEMF, values_from = IAMC_Template)
                        ,by= c("SCENARIO","SCENARIO2","REMF","YEMF","year"))%>%
              left_join(df_sam%>%
                          filter(i=="COM_ACO2",year=="2100",
                                 scenario=="SSP2_500C_CACN_Synf_H2LowCost_NoCC"|
                                   scenario=="SSP2_500C_CACN_Synf_CO2LowCost_NoCC"|
                                   scenario=="SSP2_500C_CACN_Synf_LowCost_NoCC"|
                                   scenario=="SSP2_500C_CACN_Synf_NoCC")%>%
                          group_by(j,scenario)%>%
                          summarise(volume=sum(volume))%>%
                          ungroup()%>%
                          pivot_wider(names_from = j, values_from = volume),
                        by = c("SCENARIO"="scenario"))%>%
              mutate(
                Car_Use_Ind_Liq_syn = IAMC_Template*SYN/STO*Fin_Ene_Ind_Liq_Hyd_syn/Fin_Ene_Liq_Hyd_syn/1000,
                Car_Use_Tra_Liq_syn = IAMC_Template*SYN/STO*Fin_Ene_Tra_Liq_Hyd_syn/Fin_Ene_Liq_Hyd_syn/1000,
                Car_Use_Bui_Liq_syn = IAMC_Template*SYN/STO*Fin_Ene_Res_and_Com_Liq_Hyd_syn/Fin_Ene_Liq_Hyd_syn/1000,
                Car_Use_Ind_Gas_syn = IAMC_Template*SYG/STO*Fin_Ene_Ind_Gas_Hyd_syn/Fin_Ene_Gas_Hyd_syn/1000,
                Car_Use_Bui_Gas_syn = IAMC_Template*SYG/STO*Fin_Ene_Res_and_Com_Gas_Hyd_syn/Fin_Ene_Gas_Hyd_syn/1000
              )%>%
              select(c("SCENARIO","SCENARIO2","Car_Use_Ind_Liq_syn","Car_Use_Tra_Liq_syn","Car_Use_Bui_Liq_syn","Car_Use_Ind_Gas_syn","Car_Use_Bui_Gas_syn"))%>%
              pivot_longer(cols = -c(SCENARIO, SCENARIO2), names_to = "VEMF", values_to = "IAMC_Template"))%>%
  mutate(axis1 = case_when( 
    VEMF == "Car_Seq_CCS_Bio" ~ "Bio with\ncarbon capture", 
    VEMF == "Car_Seq_Dir_Air_Cap" ~ "DAC",
    VEMF == "Car_Seq_Lan_Use_Aff" ~ "Afforestation",
    VEMF == "Car_Use_Ind_Liq_syn" ~ "DAC", 
    VEMF == "Car_Use_Tra_Liq_syn" ~ "DAC",
    VEMF == "Car_Use_Bui_Liq_syn" ~ "DAC",
    VEMF == "Car_Use_Ind_Gas_syn" ~ "DAC", 
    VEMF == "Car_Use_Bui_Gas_syn" ~ "DAC",
    TRUE ~ "Loss"),
    axis3 = case_when( 
      VEMF == "Car_Seq_CCS_Bio" ~ "Underground", 
      VEMF == "Car_Seq_Dir_Air_Cap" ~ "Underground",
      VEMF == "Car_Seq_Lan_Use_Aff" ~ "Land",
      VEMF == "Car_Use_Ind_Liq_syn" ~ "Atmosphere", 
      VEMF == "Car_Use_Tra_Liq_syn" ~ "Atmosphere",
      VEMF == "Car_Use_Bui_Liq_syn" ~ "Atmosphere",
      VEMF == "Car_Use_Ind_Gas_syn" ~ "Atmosphere", 
      VEMF == "Car_Use_Bui_Gas_syn" ~ "Atmosphere",
      TRUE ~ "Loss"),
    axis2 = case_when( 
      VEMF == "Car_Seq_CCS_Bio" ~ "CDR", 
      VEMF == "Car_Seq_Dir_Air_Cap" ~ "CDR",
      VEMF == "Car_Seq_Lan_Use_Aff" ~ "CDR",
      VEMF == "Car_Use_Ind_Liq_syn" ~ "CCU", 
      VEMF == "Car_Use_Tra_Liq_syn" ~ "CCU",
      VEMF == "Car_Use_Bui_Liq_syn" ~ "CCU",
      VEMF == "Car_Use_Ind_Gas_syn" ~ "CCU", 
      VEMF == "Car_Use_Bui_Gas_syn" ~ "CCU",
      TRUE ~ "Loss"))%>%
  mutate(
    axis1 = factor(axis1,levels = c("DAC","Bio with\ncarbon capture","Afforestation")),
    axis2 = factor(axis2,levels = c("CCU","CDR")),
    axis3 = factor(axis3,levels = c("Atmosphere","Land","Underground"))
  )%>%
  View()
















