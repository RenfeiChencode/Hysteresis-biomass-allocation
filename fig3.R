p=2:5

leafratio1=c(0.3018,0.02299,0.5962,0.02641)
leafratio2=c(0.3018,0.4566,0.5962,0.7257)

df=data.frame(p=p,leafratio=leafratio1)
fig3A=ggplot(df, aes(x=p, y=leafratio)) +geom_bar(stat="identity", fill=c("blue","red","blue","red"))+
  labs(title="A      Inconsistency",colour="",x="Light competition intensity",y= "Leaf mass ratio")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0,size=30),
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


df=data.frame(p=p,leafratio=leafratio2)
fig3B=ggplot(df, aes(x=p, y=leafratio)) +geom_bar(stat="identity", fill=c("blue","red","blue","red"))+
  labs(title="B       Consistency",colour="",x="Light competition intensity",y= "Leaf mass ratio")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0,size=30),
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

#ggsave("fig3.pdf",grid.arrange(fig3A,fig3B,ncol=2),width = 40, height = 20, units = "cm", dpi = 300) 

