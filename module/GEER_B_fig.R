
#Title: Sub module for IAMC figures
#Name: Osamu Nishiura
#Date: 2023/7/17

#Directory preparation----------------------------------------------------------

if (dir.exists("../output/figure/GEER_B") == "FALSE") {dir.create("../output/figure/GEER_B", recursive = T)}

#Figures------------------------------------------------------------------------
#Energy-------------------------------------------------------------------------
p1 <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(VEMF %in% df_pri[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_pri[1,]))%>%
  ggplot(aes(x=year , y=IAMC_Template ))
p1 <- p1  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce))
p1 <- p1  + geom_bar_pattern(stat="identity",
                             aes(pattern = VEMF,
                                 fill = VEMF),
                             pattern_fill = "black",
                             color = "black",
                             pattern_spacing = 0.03,
                             pattern_size = 0.1,
                             pattern_key_scale_factor = 0.15) 
p1 <- p1  + scale_fill_manual(values = as_vector(df_pri[5,]), labels = as_vector(df_pri[2,]))
p1 <- p1  + scale_pattern_manual(values = as_vector(df_pri[6,]), labels = as_vector(df_pri[2,]))
p1 <- p1　+ geom_hline(yintercept=0,color = "grey")
p1 <- p1  + labs(x="Year", y="Primary energy (EJ/year)", fill = "Source")
p1 <- p1　+ my_theme + theme(legend.position = "none", axis.title.x = element_blank())



p2 <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015",SCENARIO==df_sce[2,1]|SCENARIO==df_sce[3,1])%>%
  select(-c(SCENARIO))%>%
  filter(VEMF %in% df_pri[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_pri[1,]))%>%
  pivot_wider(names_from = SCENARIO2, values_from = IAMC_Template)%>%
  replace(is.na(.), 0)
colnames(p2)<-c("REMF","VEMF","YEMF","year","SCE1","SCE2")
p2 <- mutate(p2, IAMC_Template = SCE2-SCE1, SCENARIO=paste(df_sce[3,2],df_sce[2,2], sep=" - "))%>%
  select(-c(SCE1,SCE2))%>%
  ggplot(aes(x=year , y= IAMC_Template))
p2 <- p2  + facet_grid( ~ SCENARIO ,scales="fixed")
p2 <- p2  + geom_bar_pattern(stat="identity",
                             aes(pattern = VEMF,
                                 fill = VEMF),
                             pattern_fill = "black",
                             color = "black",
                             pattern_spacing = 0.03,
                             pattern_size = 0.1,
                             pattern_key_scale_factor = 0.15) 
p2 <- p2  + scale_fill_manual(values = as_vector(df_pri[5,]), labels = as_vector(df_pri[2,]))
p2 <- p2  + scale_pattern_manual(values = as_vector(df_pri[6,]), labels = as_vector(df_pri[2,]))
p2 <- p2　+ geom_hline(yintercept=0,color = "grey")
p2 <- p2  + labs(x="Year", y="Primary energy (EJ/year)", fill = "Source",pattern = "Source")
p2 <- p2　+ my_theme + theme(axis.title = element_blank())

png(paste(df_path[df_path$name=="figure",]$path,"GEER_B/energy1.png",sep="/"), width = 3000, height = 4000,res = 300)
print(p2)
dev.off()

p3 <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(!(str_detect(VEMF, "^Fin_Ene_Ind_|^Fin_Ene_Tra_|^Fin_Ene_Bui_")))%>%
  filter(VEMF %in% df_fin[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_fin[1,]))%>%
  ggplot(aes(x=year , y=IAMC_Template ))
p3 <- p3  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce))
p3 <- p3  + geom_bar_pattern(stat="identity",
                             aes(pattern = VEMF,
                                 fill = VEMF),
                             pattern_fill = "black",
                             color = "black",
                             pattern_spacing = 0.03,
                             pattern_size = 0.1,
                             pattern_key_scale_factor = 0.15) 
p3 <- p3  + scale_fill_manual(values = as_vector(df_fin[5,]), labels = as_vector(df_fin[2,]))
p3 <- p3  + scale_pattern_manual(values = as_vector(df_fin[6,]), labels = as_vector(df_fin[2,]))
p3 <- p3　+ geom_hline(yintercept=0,color = "grey")
p3 <- p3  + labs(x="Year", y="Final energy (EJ/year)", fill = "Fuel")
p3 <- p3　+ my_theme + theme(legend.position = "none", axis.title.x = element_blank())

p4 <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015",SCENARIO==df_sce[2,1]|SCENARIO==df_sce[3,1])%>%
  select(-c(SCENARIO))%>%
  filter(!(str_detect(VEMF, "^Fin_Ene_Ind_|^Fin_Ene_Tra_|^Fin_Ene_Bui_")))%>%
  filter(VEMF %in% df_fin[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_fin[1,]))%>%
  pivot_wider(names_from = SCENARIO2, values_from = IAMC_Template)%>%
  replace(is.na(.), 0)
colnames(p4)<-c("REMF","VEMF","YEMF","year","SCE1","SCE2")
p4 <- mutate(p4, IAMC_Template = SCE2-SCE1, SCENARIO=paste(df_sce[3,2],df_sce[2,2], sep=" - "))%>%
  select(-c(SCE1,SCE2))%>%
  ggplot(aes(x=year , y= IAMC_Template))
p4 <- p4  + facet_grid( ~ SCENARIO ,scales="fixed")
p4 <- p4  + geom_bar_pattern(stat="identity",
                             aes(pattern = VEMF,
                                 fill = VEMF),
                             pattern_fill = "black",
                             color = "black",
                             pattern_spacing = 0.03,
                             pattern_size = 0.1,
                             pattern_key_scale_factor = 0.15) 
p4 <- p4  + scale_fill_manual(values = as_vector(df_fin[5,]), labels = as_vector(df_fin[2,]))
p4 <- p4  + scale_pattern_manual(values = as_vector(df_fin[6,]), labels = as_vector(df_fin[2,]))
p4 <- p4　+ geom_hline(yintercept=0,color = "grey")
p4 <- p4  + labs(x="Year", y="Final energy (EJ/year)", fill = "Fuel", pattern = "Fuel")
p4 <- p4　+ my_theme + theme(axis.title = element_blank())

png(paste(df_path[df_path$name=="figure",]$path,"GEER_B/energy.png",sep="/"), width = 5000, height = 3000,res = 300)
p <- grid.arrange(p1,p2,p3, p4, layout_matrix = rbind(c(1,1,1,1,1,1,2,2,2),c(3,3,3,3,3,3,4,4,4)))
print(p)
dev.off()

#Emission----------------------------------------------------------------------


p1 <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(VEMF %in% df_emi[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_emi[1,]))%>%
  ggplot(aes(x=year , y=IAMC_Template/1000 ))
p1 <- p1  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce))
p1 <- p1  + geom_bar_pattern(stat="identity",
                             aes(pattern = VEMF,
                                 fill = VEMF),
                             pattern_fill = "black",
                             color = "black",
                             pattern_spacing = 0.03,
                             pattern_size = 0.1,
                             pattern_key_scale_factor = 0.15) 
p1 <- p1  + scale_fill_manual(values = as_vector(df_emi[5,]), labels = as_vector(df_emi[2,]))
p1 <- p1  + scale_pattern_manual(values = as_vector(df_emi[6,]), labels = as_vector(df_emi[2,]))
p1 <- p1  + geom_line(data=filter(df_iamc,VEMF=="Emi_CO2",REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015"),aes(color=VEMF)) 
p1 <- p1  + geom_point(data=filter(df_iamc,VEMF=="Emi_CO2",REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015"),aes(color=VEMF)) 
p1 <- p1  + scale_color_manual(values = c("black"), labels =c("Net"))
p1 <- p1　+ geom_hline(yintercept=0,color = "grey")
p1 <- p1　+ my_theme +theme(legend.position = "none", axis.title.x = element_blank())
p1 <- p1 + labs(x="Year", y="CO2 emission (Gt/year)", fill = "source",pattern ="source")

png(paste(df_path[df_path$name=="figure",]$path,"GEER_B/emission1.png",sep="/"), width = 2000, height = 2000,res = 300)
print(p1)
dev.off()

p2 <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015",SCENARIO==df_sce[2,1]|SCENARIO==df_sce[3,1])%>%
  select(-c(SCENARIO))%>%
  filter(VEMF %in% df_emi[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_emi[1,]))%>%
  pivot_wider(names_from = SCENARIO2, values_from = IAMC_Template)%>%
  replace(is.na(.), 0)
colnames(p2)<-c("REMF","VEMF","YEMF","year","SCE1","SCE2")
p2 <- mutate(p2, IAMC_Template = SCE2-SCE1, SCENARIO=paste(df_sce[3,2],df_sce[2,2], sep=" - "))%>%
  select(-c(SCE1,SCE2))%>%
  ggplot(aes(x=year , y= IAMC_Template/1000))
p2 <- p2  + facet_grid( ~ SCENARIO ,scales="fixed")
p2 <- p2  + geom_bar_pattern(stat="identity",
                             aes(pattern = VEMF,
                                 fill = VEMF),
                             pattern_fill = "black",
                             color = "black",
                             pattern_spacing = 0.03,
                             pattern_size = 0.1,
                             pattern_key_scale_factor = 0.15) 
p2 <- p2  + scale_fill_manual(values = as_vector(df_emi[5,]), labels = as_vector(df_emi[2,]))
p2 <- p2  + scale_pattern_manual(values = as_vector(df_emi[6,]), labels = as_vector(df_emi[2,]))
p2 <- p2　+ geom_hline(yintercept=0,color = "grey")
p2 <- p2  + labs(x="Year", y="CO2 emission", fill = "Source",pattern ="Source")
p2 <- p2　+ my_theme + theme(axis.title = element_blank())

png(paste(df_path[df_path$name=="figure",]$path,"GEER_B/emission2.png",sep="/"), width = 3000, height = 2000,res = 300)
print(p2)
dev.off()

png(paste(df_path[df_path$name=="figure",]$path,"GEER_B/emission.png",sep="/"), width = 5000, height = 1800,res = 300)
p <- grid.arrange(p1,p2, layout_matrix = rbind(c(1,1,1,1,2,2,2)))
print(p)
dev.off()

#Landuse------------------------------------------------------------------------

p1 <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(VEMF %in% df_lan[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_lan[1,]))%>%
  ggplot(aes(x=year , y=IAMC_Template/1000 ))
p1 <- p1  + facet_grid( ~ SCENARIO ,scales="fixed",labeller = as_labeller(v_sce))
#p1 <- p1  + geom_bar(stat="identity",aes(fill=VEMF))
p1 <- p1  + geom_bar_pattern(stat="identity",
                           aes(pattern = VEMF,
                               fill = VEMF),
                           pattern_fill = "black",
#                           fill = "white",
                           color = "black",
                           pattern_spacing = 0.06,
                           pattern_size = 0.1,
                           pattern_key_scale_factor = 0.2) 
p1 <- p1  + scale_fill_manual(values = as_vector(df_lan[5,]), labels = as_vector(df_lan[2,]))
p1 <- p1  + scale_pattern_manual(values = as_vector(df_lan[6,]), labels = as_vector(df_lan[2,]))
p1 <- p1　+ geom_hline(yintercept=0,color = "grey")
p1 <- p1  + labs(x="Year", y="Landuse (billion ha)", fill = "land", pattern = "land")
p1 <- p1　+ my_theme + theme(legend.position = "none", axis.title.x = element_blank())
p2 <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015",SCENARIO==df_sce[2,1]|SCENARIO==df_sce[3,1])%>%
  select(-c(SCENARIO))%>%
  filter(VEMF %in% df_lan[1,])%>%
  mutate(VEMF = factor(VEMF, levels=df_lan[1,]))%>%
  pivot_wider(names_from = SCENARIO2, values_from = IAMC_Template)%>%
  replace(is.na(.), 0)
colnames(p2)<-c("REMF","VEMF","YEMF","year","SCE1","SCE2")
p2 <- mutate(p2, IAMC_Template = SCE2-SCE1, SCENARIO=paste(df_sce[3,2],df_sce[2,2], sep="\n - "))%>%
  select(-c(SCE1,SCE2))%>%
  ggplot(aes(x=year , y= IAMC_Template/1000))
p2 <- p2  + facet_grid( ~ SCENARIO ,scales="fixed")
p2 <- p2  + geom_bar_pattern(stat="identity",
                             aes(pattern = VEMF,
                                 fill = VEMF),
                             pattern_fill = "black",
                             color = "black",
                             pattern_spacing = 0.06,
                             pattern_size = 0.1,
                             pattern_key_scale_factor = 0.2) 
p2 <- p2  + scale_fill_manual(values = as_vector(df_lan[5,]), labels = as_vector(df_lan[2,]))
p2 <- p2  + scale_pattern_manual(values = as_vector(df_lan[6,]), labels = as_vector(df_lan[2,]))
p2 <- p2　+ geom_hline(yintercept=0,color = "grey")
p2 <- p2  + labs(x="Year", y="Landuse (billion ha)", fill = "land", pattern = "land")
p2 <- p2　+ my_theme + theme(legend.position = "right",axis.title = element_blank())

png(paste(df_path[df_path$name=="figure",]$path,"GEER_B/landuse.png",sep="/"), width = 3000, height = 2200,res = 300)
p <- grid.arrange(p1,p2, layout_matrix = rbind(c(1,1,1,2,2,2)))
print(p)
dev.off()








