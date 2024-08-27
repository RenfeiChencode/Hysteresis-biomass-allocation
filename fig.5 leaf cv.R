setwd( "C:/Users/Lenovo/Desktop")
library(ggplot2);library(gridExtra);library(scales);library(grid)
library(tseries)# test the stability of time series
library(sqldf);library(dplyr);library(patchwork)

RANDN=rnorm(100, mean = 5, sd = 1.5)#produce normally distributed random numbers
leafratio=c(0.1,0.2,0.6,0.7)

leaf1=RANDN*leafratio[1]
leaf2=c(RANDN[1:50]*leafratio[2],RANDN[51:100]*leafratio[3])
leaf3=RANDN*leafratio[4]
CVleaf1=sd(leaf1)/mean(leaf1)
CVleaf2=sd(leaf2)/mean(leaf2)
CVleaf3=sd(leaf3)/mean(leaf3)

df=data.frame(category=c("1Low","2Moderate","3High"),value=c(CVleaf1,CVleaf2,CVleaf3))
fig5A=ggplot(df, aes(x=category, y=value)) +geom_bar(stat="identity", fill="blue")+
  labs(title="A   Theoretical prediction",colour="",x="Plant interaction intensity",y= expression("CV"["leaf"]))+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0,size=20),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.75,0.8), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_y_continuous(breaks=seq(0,0.8,by=0.2),limits=c(0,0.8))
########################################
########################################
#fig.5B
F=read.csv("dcr.csv")#crown diameter
leafmass=F$m.lf
stemass=F$m.st
dcr=F$d.cr
NUM=floor(length(leafmass)/50)#????ȡ??
CVleaf=CVstem=Leafmass=0
for (ij in 1:NUM){
  LM=leafmass[(1+50*(ij-1)):(50+50*(ij-1))]
  CVleaf[ij]=sd(LM)/mean(LM)
}
x=1:length(CVleaf);y=log(CVleaf)

df=data.frame(crown=x,cvleaf=y)
fig5B=ggplot(data = df,aes(x=crown,y=cvleaf))+geom_point(size=3,colour = "blue")+
  labs(title="B   Empirical observation",colour="",x="Class of crown diameter",y= expression("CV"["leaf"]))+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.75,0.8), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,80,by=20),limits=c(0,80))+
  scale_y_continuous(breaks=seq(-1,2,by=1),limits=c(-1,2))

#ggsave("fig5.pdf",grid.arrange(fig5A,fig5B,ncol=2),width = 40, height = 20, units = "cm", dpi = 300) 



