
#Ttile: AIMHub parameter check
#Date: 2024/4/1
#Name: Osamu Nishiura

# ------------------------------------------------------------------------------
#Setting------------------------------------------------------------------------

#Data import--------------------------------------------------------------------

df_b_tra<-data.frame()
df_b_tra<-data.frame()
for (i in df_sce$SCENARIO){
  df_b_tra<-rgdx.param(paste(df_path[df_path$name=="cbnal0",]$path,
                             paste("global_17_",i,".gdx",sep=""),sep="/"),
                       "betatrsene_load")%>%
    mutate(i5 = i)%>%
    bind_rows(df_b_tra)
}

df_b_car<-data.frame()
df_b_car<-data.frame()
for (i in df_sce$SCENARIO){
  df_b_car<-rgdx.param(paste(df_path[df_path$name=="cbnal0",]$path,
                        paste("global_17_",i,".gdx",sep=""),sep="/"),
                  "betacarh_load")%>%
    mutate(i5 = i)%>%
  bind_rows(df_b_car)
}

df_b_ind<-data.frame()
df_b_ind<-data.frame()
for (i in df_sce$SCENARIO){
  df_b_ind<-rgdx.param(paste(df_path[df_path$name=="cbnal0",]$path,
                             paste("global_17_",i,".gdx",sep=""),sep="/"),
                       "betaindene_load")%>%
    mutate(i5 = i)%>%
    bind_rows(df_b_ind)
}

df_b_hh<-data.frame()
df_b_hh<-data.frame()
for (i in df_sce$SCENARIO){
  df_b_hh<-rgdx.param(paste(df_path[df_path$name=="cbnal0",]$path,
                             paste("global_17_",i,".gdx",sep=""),sep="/"),
                       "betaenec_load")%>%
    mutate(i5 = i)%>%
    bind_rows(df_b_hh)
}


  
#Figures------------------------------------------------------------------------
#beta transport
for (i in v_REMF2){
p <- df_b_tra%>%
  filter(i2==i, i5 == "SSP2_500C_CACN_Synf_NoCC"| i5 == "SSP2_500C_CACN_NoCC")%>%
  ggplot(aes(x=i1 , y=value ))
p <- p  + facet_grid(i4 ~ i5 ,scales="free")
p <- p  + geom_bar(stat="identity",aes(fill=i3)) 
#p <- p  + scale_fill_fue()
p <- p　+ geom_hline(yintercept=0,color = "grey")
#p <- p + labs(x="Year", y="Primary energy (EJ/year)", fill = "Source")
p <- p　+ my_theme
png(paste(df_path[df_path$name=="figure",]$path,"/other/",i,"_beta_trs.png",sep=""), width = 1000, height = 3000,res = 100)
print(p)
dev.off()
}

#beta industry
for (i in v_REMF2){
p <- df_b_ind%>%
  filter(i2==i, i5 == "SSP2_500C_CACN_Synf_NoCC"| i5 == "SSP2_500C_CACN_NoCC")%>%
  ggplot(aes(x=i1 , y=value ))
p <- p  + facet_grid(i4 ~ i5 ,scales="free")
p <- p  + geom_bar(stat="identity",aes(fill=i3)) 
#p <- p  + scale_fill_fue()
p <- p　+ geom_hline(yintercept=0,color = "grey")
#p <- p + labs(x="Year", y="Primary energy (EJ/year)", fill = "Source")
p <- p　+ my_theme
png(paste(df_path[df_path$name=="figure",]$path,"/other/",i,"_beta_ind.png",sep=""), width = 1000, height = 5000,res = 100)
print(p)
dev.off()
}

#beta hh



#beta hh car
for (i in v_REMF2){
  p <- df_b_car%>%
    filter(i2==i, i5 == "SSP2_500C_CACN_Synf_NoCC"| i5 == "SSP2_500C_CACN_NoCC")%>%
    ggplot(aes(x=i1 , y=value ))
  p <- p  + facet_grid(i4 ~ i5 ,scales="free")
  p <- p  + geom_bar(stat="identity",aes(fill=i3)) 
  #p <- p  + scale_fill_fue()
  p <- p　+ geom_hline(yintercept=0,color = "grey")
  #p <- p + labs(x="Year", y="Primary energy (EJ/year)", fill = "Source")
  p <- p　+ my_theme
  png(paste(df_path[df_path$name=="figure",]$path,"/other/",i,"_beta_car.png",sep=""), width = 1000, height = 1000,res = 100)
  print(p)
  dev.off()
}


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



