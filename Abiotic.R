library(readxl)
AbioticMCB <- read_excel("AbioticMCB.xlsx")
colnames(AbioticMCB)<-c("Year","Month","Site","Tide","Weather","WindDir","WindSp","SurfT","BotT","DepthS",
                        "DepthF","SurfDO","BotDO","SurfSal","BotSal","Secchi","BA","SP","CT","CTvol")

fit<-aov(SP ~ Month+Year, data=AbioticMCB)
summary(fit)


library(readxl)
Abdata <- read_excel("Copy of Organisms high and low.xlsx", 
                     range = "A1:Q2596")

Abdata$Date <- as.Date(paste(as.numeric(Abdata$Month), "01", Abdata$Year, sep="-"), 
                   format = "%m-%d-%Y")

st.err <- function(x) {
  sd(x)/sqrt(length(x))
}
DOma<- aggregate(Dob ~ Site , Abdata,na.rm=TRUE, mean)
DOmse<- aggregate(Dos ~ Site , Abdata, st.err)
DOmsd <-aggregate(Dos ~ Site , Abdata, na.rm=TRUE, sd)

Salma<- aggregate(SalS ~ Year , Abdata,na.rm=TRUE, mean)
Salmse<- aggregate(SalS ~ Year , Abdata, st.err)
Salmsd <-aggregate(SalS ~ Year , Abdata, na.rm=TRUE, sd)

sp<-split(Abdata, f=Abdata$Organism)
BA<-sp$`Bay anchovy`
SP<-sp$`Silver perch`
CT<-sp$Ctenophore
CT2<-subset(CT, Year!="2018")
CT2<-subset(CT2, Year!="2019")
  

lmDOb<-lm(Dob~Site, data=Abdata)
summary(lmDOb)
lmDOs<-lm(Dos~Site, data=Abdata)
summary(lmDOs)




lmBA<-lm(Count~Date, data=BA)
summary(lmBA)

lmSP<-lm(Count~Date, data=SP)
summary(lmSP)

lmCT<-lm(Count~Date, data=CT2)
summary(lmCT)

library(ggplot2)
library(ggpubr)
jpeg("Year compare.jpg",units="in", 
     width=6, 
     height=8, 
    # pointsize=12, 
     res=500)
     


ST<- ggplot(Abdata,aes(x=factor(Month, levels=c("4","5","6","7","8","9","10")), y=TempS))+
  geom_boxplot()+
  scale_y_continuous(limits=c(8,35))+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                                  strip.background = element_blank(),legend.position="right",
                                  axis.line = element_line(colour = "black"),
                                  strip.text.x = element_text(margin = margin(0, 0, 0, 0)))+
  labs(y= (expression(paste("Temperature (",degree ,"C)"))), x = NULL)

BT<-ggplot(Abdata,aes(x=factor(Month, levels=c("4","5","6","7","8","9","10")), y=TempB))+
  geom_boxplot()+
  scale_y_continuous(limits=c(8,35))+
                       theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                                  strip.background = element_blank(),legend.position="right",
                                  axis.line = element_line(colour = "black"),
                                  strip.text.x = element_text(margin = margin(0, 0, 0, 0)))+
  labs(y= NULL, x =NULL)
SDo<-ggplot(Abdata,aes(x=factor(Month, levels=c("4","5","6","7","8","9","10")), y=Dos))+
  geom_boxplot()+
  scale_y_continuous(limits=c(3,16.5))+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                                  strip.background = element_blank(),legend.position="right",
                                  axis.line = element_line(colour = "black"),
                                  strip.text.x = element_text(margin = margin(0, 0, 0, 0)))+
  labs(y= (expression(paste("Dissolved Oxygen " ,"(mg/L)"))), x = NULL)
BDo<-ggplot(Abdata,aes(x=factor(Month, levels=c("4","5","6","7","8","9","10")), y=Dob))+
  geom_boxplot()+
  scale_y_continuous(limits=c(3,16.5))+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                                  strip.background = element_blank(),legend.position="right",
                                  axis.line = element_line(colour = "black"),
                                  strip.text.x = element_text(margin = margin(0, 0, 0, 0)))+
  labs(y= NULL, x =NULL)
SS<-ggplot(Abdata,aes(x=factor(Month, levels=c("4","5","6","7","8","9","10")), y=SalS))+
  geom_boxplot()+
  scale_y_continuous(limits=c(7,35))+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                                  strip.background = element_blank(),legend.position="right",
                                  axis.line = element_line(colour = "black"),
                                  strip.text.x = element_text(margin = margin(0, 0, 0, 0)))+
  labs(y= (expression(paste("Salinity " ,"(ppt)"))), x = "Month")
BS<-ggplot(Abdata,aes(x=factor(Month, levels=c("4","5","6","7","8","9","10")), y=SalB))+
  geom_boxplot()+
  scale_y_continuous(limits=c(7,35))+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                                  strip.background = element_blank(),legend.position="right",
                                  axis.line = element_line(colour = "black"),
                                  strip.text.x = element_text(margin = margin(0, 0, 0, 0)))+
  labs(y= NULL,  
       x = "Month")
ggarrange(ST, BT, SDo, BDo, SS, BS , 
          labels = c("A", "B", "C", "D","E","F"),
          ncol = 2, nrow = 3)
dev.off()
