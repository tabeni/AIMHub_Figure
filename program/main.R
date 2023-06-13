
#Ttile: visualize AIMHub output
#Date: 2023/6/13
#Name: Osamu Nishiura

#Package load-------------------------------------------------------------------

library(tidyverse)
library(gdxrrw)

#Directory Preparation----------------------------------------------------------

if (dir.exists("../output/figure/main") == "FALSE") {dir.create("../output/figure/main", recursive = T)}
if (dir.exists("../output/figure/other") == "FALSE") {dir.create("../output/figure/other", recursive = T)}
if (dir.exists("../module") == "FALSE") {dir.create("../module")}

#Setting import-----------------------------------------------------------------

df_path    <- read_csv("../define/path.csv")
df_filter  <- read_csv("../define/filter.csv")
df_pri     <- read_csv("../define/primary_energy.csv")
df_pri2     <- read_csv("../define/primary_energy2.csv")

my_theme<-theme(
  panel.background = element_rect(fill = "transparent", colour = "black"),
  panel.grid = element_blank(),
  strip.background = element_blank(),
  legend.key = element_blank()
)

#Data import--------------------------------------------------------------------

df_iamc<-rgdx.param(paste(df_path[df_path$name=="IAMC",]$path,"global_17_IAMC.gdx",sep="/"), "IAMC_Template")%>%
  filter(str_detect(VEMF, paste(df_filter$VEMF,collapse="|")),
         str_detect(YEMF, paste(df_filter$YEMF,collapse="|")),
         str_detect(REMF, paste(df_filter$REMF,collapse="|")))%>%
  mutate(year=as.numeric(as.character(df_iamc$YEMF)))

#Figuress-----------------------------------------------------------------------
#GDP and Consumption loss-------------------------------------------------------

p <- df_iamc%>%
  filter(VEMF=="Pol_Cos_GDP_Los_rat"|VEMF=="Pol_Cos_Cns_Los_rat",REMF=="World",YEMF!="2010",YEMF!="2015")%>%
  ggplot(aes(x=year , y = IAMC_Template, linetype = SCENARIO,group = SCENARIO))
p <- p + facet_wrap(. ~ VEMF, scales="free", nrow=1)
p <- p + geom_line(size=1.2)
p <- p + labs(x="Year", y="Economic loss (%)",linetype="Scenarios") 
p <- p + geom_hline(yintercept=0,color = "grey")
p <- p + my_theme
#p <- p + scale_x_discrete(label=lb_x_r)
p <- p + theme(legend.key.size = unit(1, 'cm'),
               legend.position ="right",
               strip.text.y = element_text(size = 20),
               strip.text.x = element_text(size = 20),
               axis.title = element_text(size = 20),
               legend.title= element_text(size = 20),
               legend.text= element_text(size = 20),
               axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5,size = 20),
               axis.text.y = element_text(size = 20))
png(paste(df_path[df_path$name=="figure",]$path,"GDP_HH_loss.png",sep="/"), width = 5000, height = 3000,res = 300)
print(p)
dev.off()

#Primary Energy-----------------------------------------------------------------

p <- df_iamc%>%
  filter(REMF=="World",YEMF!="2005",YEMF!="2010",YEMF!="2015")%>%
  filter(str_detect(VEMF, paste(df_pri$VEMF,collapse="|")))%>%
  mutate(VEMF = factor(VEMF, levels=df_pri$VEMF))%>%
  ggplot(aes(x=YEMF , y=IAMC_Template ))
p <- p  + facet_grid( ~ SCENARIO ,scales="fix")
p <- p　+ geom_hline(yintercept=0,color = "grey")
p <- p  + geom_bar(stat="identity",aes(fill=VEMF)) 
p <- p  + scale_fill_manual(values = names(df_pri$color) <- df_pri$VEMF)
p <- p　+ my_theme 
p <- p　+ theme(strip.text.y = element_text(size = 25), 
               strip.text.x = element_text(size = 25),
               axis.title = element_text(size = 25),
               legend.title= element_text(size = 20),
               legend.text= element_text(size = 20),
               axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5,size = 22),
               axis.text.y = element_text(size = 22),
               axis.title.x = element_blank())
p <- p + labs(x="Year", y="Primary energy (EJ/year)", fill = "Source")
png(paste(df_path[df_path$name=="figure",]$path,"primary_energy.png",sep="/"), width = 5000, height = 3000,res = 300)
print(p)
dev.off()


c(df_pri$VEMF = df_pri$source)

names(df_pri$source) <- df_pri$VEMF

df_pri2[1,]








































