setwd("C:/Users/Lenovo/Desktop")
library(ggplot2);library(gridExtra);library(scales);library(grid)
library(tseries)# test the stability of time series
library(sqldf);library(dplyr);library(patchwork)

#k-a*M+c*M^3+p*M^q/(M^q+x0)=0
M=seq(0,1,0.0001)
x0=0.002;
c=-1;
q=3;
k=0.125;
a=6.5;

p=-(k-a*M+c*M^3)/(M^q/(M^q+x0))
class=group=NA;LP1=0.029023619;LP2=0.1471133
point1=-(k-a*LP1+c*LP1^3)/(LP1^q/(LP1^q+x0))
point2=-(k-a*LP2+c*LP2^3)/(LP2^q/(LP2^q+x0))

for (i in 1:length(M)){
  if (LP1 <M[i] && M[i] <= LP2){class[i]="red" }
  else if( M[i] > LP2){class[i]="black" }
  else if (LP1 >= M[i]){class[i]="blue" }
}

df=data.frame(p=p,M=M,class=class)
colorv=c(rep("grey",8),"orange","orange")
leafratio1=c(0.02127,0.02299,0.0247,0.02641)
leafratio2=c(0.3018,0.4566,0.5962,0.7257)
point3=c(rep(2:5,2),point1,point2)
x_start=x_end=c(point1,point2)
y_start=c(LP1,LP2)
y_end=c(0.02013,0.7592)
labp1=c(rep(2.5,4),rep(-2,4),0.5,-1)
labp2=c(rep(0.5,8),-1,1.5)
label=c("p1","p2","p3","p4","p5","p6","p7","p8","LP1","LP2")
df2=data.frame(p=point3,M=c(leafratio1,leafratio2,LP1,LP2),label=label,labp1=labp1,labp2=labp2,colorv=colorv)
fig4= ggplot()+geom_path(data = df,aes(x=p,y=M,group=class,color = class),size=2)+scale_color_manual(values = c("red" = "red", "blue" = "blue","black"="black")) +
  geom_point(data = df2,aes(x =p, y = M),size=4,color=colorv)+
  geom_text(data = df2,aes(x =p, y = M,label = label,vjust = labp1,hjust=labp2),color=colorv)+
  geom_line(aes(x=rep(2:5,2),y=c(leafratio1,leafratio2),group=rep(c("1","2","3","4"),2)),colour="grey",lty="dashed",size=1)+
  labs(title="",colour="",x="Plant interaction",y="Leaf mass ratio (EQ)")+
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
        legend.position = "none", 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,8,by=2),limits=c(0,8))+
  scale_y_continuous(breaks=seq(0,1,by=0.25),limits=c(0,1))

  #ggsave("fig4.pdf",grid.arrange(fig4,ncol=1),width = 20, height = 20, units = "cm", dpi = 300) 
  

