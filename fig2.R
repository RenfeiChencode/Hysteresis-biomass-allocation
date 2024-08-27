library(deSolve)
#define the function
eqs <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dM <- k-a*M+c*M^3+p*M^q/(M^q+x0)
    list(c(dM)) # would be a vactor if more than two variables
  })
}


init <- c(M = 1)  # initial state # would be a vactor if more than two variables
times <- seq(0, 1, by = 0.01)  # time
params <- c(k=0.125,a=6.5,M=0.9,c=-1,q=3,x0=0.002,p=1)
out1 <- ode(y = init, times = times, func = eqs, parms = params)
params["p"]=2
out2 <- ode(y = init, times = times, func = eqs, parms = params)
params["p"]=3
out3 <- ode(y = init, times = times, func = eqs, parms = params)


tv=rep(times,3);class=c(rep("p=1",length(times)),rep("p=2",length(times)),rep("p=3",length(times)))
df=data.frame(MLT=c(out1[,2],out2[,2],out3[,2]),class=class,time=tv)

fig2A=ggplot(data = df,aes(x=time,y=MLT,group=class,color = class))+geom_path(size=2) +
  labs(title="A   Theoretical prediction",colour="",x="Time",y="Leaf mass ratio")+
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
        legend.position = c(0.8,0.8), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,1,by=0.25),limits=c(0,1))+
  scale_y_continuous(breaks=seq(0,1,by=0.25),limits=c(0,1))

##################################
#fig.2B
moall=read.csv("critical transition.csv")
leaf=moall$Foliage;total=moall$Stumproots+moall$Stemoverbark+moall$Foliage
leafratio=leaf/total
class=moall$Origin
time=moall$Standage
df=data.frame(MLT=leafratio,time=time,class=class)

fig2B=ggplot(data = df,aes(x=time,y=MLT,group=class,color = class))+geom_point(size=1)+geom_smooth(method="loess",span=1)+
  labs(title="B   Empirical observation",colour="",x="Time",y="Leaf mass ratio")+
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
        legend.position = c(0.8,0.8), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,480,by=120),limits=c(0,480))+
  scale_y_continuous(breaks=seq(0,1,by=0.25),limits=c(0,1))
#ggsave("fig2.pdf",grid.arrange(fig2A,fig2B,ncol=2),width = 40, height = 20, units = "cm", dpi = 300) 
