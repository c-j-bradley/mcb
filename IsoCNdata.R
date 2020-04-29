#read in data

library(readxl)

CNIso <- read_excel("IsoData SEC ALL.xlsx")
CNIso <- na.omit(CNIso)
DietIso <- read_excel("SIA Results.xlsx",sheet="Diet")
DietIso <- na.omit(DietIso)
FishIso <- read_excel("SIA Results.xlsx",sheet="Fish")
FishIso <- na.omit(FishIso)
FishIso$Date <- as.Date(paste(as.numeric(FishIso$Month), "01", FishIso$Year, sep="-"), 
                       format = "%m-%d-%Y")

CTIso <- read_excel("SIA Results.xlsx",sheet="CT",range = "C1:N3000")
CTIso <- na.omit(CTIso)

#separate samples
ys<-split(FishIso,f=FishIso$'Site')
IncSite<-rbind(ys$`T004`, ys$T008, ys$T011, ys$T014, ys$T016)
IncSite$Month<-as.factor(IncSite$Month)
IncSite$Year<-as.factor(IncSite$Year)

CN2019<-ys$`2019`
sep<- split(FishIso, f=FishIso$'Species')
sep<- split(IncSite, f=IncSite$'Species')
BA <- sep$BA
SP <- sep$SP
site<-split(CTIso, f=CTIso$'Site')
CT <- rbind(site$T004, site$T008, site$T011, site$T014, site$T016)
fr<-split(IncSite, f=IncSite$Sp)
ZPmacr <- fr$MAC
ZPmeso <- fr$MES
ZPmic <- fr$MIC
ZP <- rbind(ZPmacr,ZPmeso,ZPmic)

SP2<-subset(SP, Year!="2018")

CNcomp<-rbind(BA,SP,CT)

shapiro.test(CTIso$d13C)
t.test(d13C~Year, data=BA)
lmMIC<-lm(d13C~Site+Month, data=ZPmacr)
summary(lmMIC)

#monthly averages by site -- Bay Anchovies
BAmaN<- aggregate(d15N ~ Site + Month +Year, BA,na.rm=TRUE, mean)
BAmsdN<- aggregate(d15N ~ Loc + Season, BA,na.rm=TRUE, sd)
BAmsdN$SE <-BAmsdN$d15N / sqrt(length(BA$d15N))
BAmaC<- aggregate(d13C ~ Site+Season, BA,na.rm=TRUE, mean)
BAmsdC<- aggregate(d13C ~ Loc + Season, BA,na.rm=TRUE, sd)
BAmsdC$SE <-BAmsdC$d13C / sqrt(length(BA$d13C))
BAmonAll <- cbind(BAmaN,BAmsdN$d15N,BAmsdN$SE,BAmaC$d13C,BAmsdC$d13C,BAmsdC$SE)
colnames(BAmonAll) <- c("Loc","Season","dN","Nsd","Nse","dC","Csd","Cse")
#add a species column
BAmonAll$Species <- c("Bay Anchovy")

#monthly averages by site -- Silver Perch
SPmaN<- aggregate(d15N ~ Loc + Season, SP,na.rm=TRUE, mean)
SPmsdN<- aggregate(d15N ~ Loc + Season, SP,na.rm=TRUE, sd)
SPmsdN$SE <-SPmsdN$d15N / sqrt(length(SP$d15N))
SPmaC<- aggregate(d13C ~ Site+Month+Year, SP,na.rm=TRUE, mean)
SPmsdC<- aggregate(d13C ~ Loc + Season, SP,na.rm=TRUE, sd)
SPmsdC$SE <-SPmsdC$d13C / sqrt(length(SP$d13C))
SPmonAll <- cbind(SPmaN,SPmsdN$d15N,SPmsdN$SE,SPmaC$d13C,SPmsdC$d13C,SPmsdC$SE)
colnames(SPmonAll) <- c("Loc","Season","dN","Nsd","Nse","dC","Csd","Cse")
#add a species column
SPmonAll$Species <- c("Silver Perch")


#monthly averages by site -- Ctenophores
CTmaN<- aggregate(d15N ~ Site + Month + Year, CT,na.rm=TRUE, mean)
CTmsdN<- aggregate(d15N ~ Site + Month + Year, CT,na.rm=TRUE, sd)
CTmsdN$SE <-CTmsdN$d15N / sqrt(length(CT$d15N))
CTmaC<- aggregate(d13C ~ Site + Month + Year, CT,na.rm=TRUE, mean)
CTmsdC<- aggregate(d13C ~ Site + Month + Year, CT,na.rm=TRUE, sd)
CTmsdC$SE <-CTmsdC$d13C / sqrt(length(CT$d13C))
CTmonAll <- cbind(CTmaN,CTmsdN$d15N,CTmsdN$SE,CTmaC$d13C,CTmsdC$d13C,CTmsdC$SE)
colnames(CTmonAll) <- c("Loc","Month","dN","Nsd","Nse","dC","Csd","Cse")
#add a species column
CTmonAll$Species <- c("Ctenophora")


#monthly averages by site -- Zooplankton
ZPmaN<- aggregate(d15N ~ Type + Loc + Season, ZP,na.rm=TRUE, mean)
ZPmsdN<- aggregate(d15N ~ Type + Loc + Season, ZP,na.rm=TRUE, sd)
ZPmsdN$SE <-ZPmsdN$d15N / sqrt(length(ZP$d15N))
ZPmaC<- aggregate(d13C ~ Type + Loc + Season, ZP,na.rm=TRUE, mean)
ZPmsdC<- aggregate(d13C ~ Type + Loc + Season, ZP,na.rm=TRUE, sd)
ZPmsdC$SE <-ZPmsdC$d13C / sqrt(length(ZP$d13C))
ZPmonAll <- cbind(ZPmaN,ZPmsdN$d15N,ZPmsdN$SE,ZPmaC$d13C,ZPmsdC$d13C,ZPmsdC$SE)
colnames(ZPmonAll) <- c("Size","Loc","Season","dN","Nsd","Nse","dC","Csd","Cse")

#aggregate competitors
COMPall <- rbind(BAmonAll, SPmonAll, CTmonAll)

#scatterplot with all data
library(ggplot2)
cbPalette <- c("#999999", "#56B4E9","#E69F00",  "#009E73",  "#CC79A7","#D55E00",
               "#0072B2", "#F0E442", "#000000")
png("2018FishCN.png",units="in",width=6,height=4, res=300)
y1<-ggplot(CNcomp, aes(x=d13C, 
                       y=d15N, 
                       #fill=factor(Loc, levels=c("T004","T008","T011","T016")),
                       fill=factor(Lot, levels=c("4","5")),
                       color=factor(Season, levels=c("SP","SU","FA")),
                       shape=factor(Type, levels=c("BA","SP",
                                                   "CT"))))+ 
  geom_point(size=4, stroke=2)+ 
  scale_shape_manual(values=c(21,22,23))+
  xlab(expression(paste(delta ^13,"C")))+
  ylab(expression(paste(delta ^15,"N")))+
  scale_color_manual(values=c("#0c0c0c","#949391","#3c4a46"))+
  scale_fill_manual(values=cbPalette)+
  scale_y_continuous(limits=c(10,50))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_blank(), legend.position="top",
                   axis.line = element_line(colour = "black"))
y1
dev.off()

png("2018FishCTCN.png",units="in",width=6,height=4, res=300)
y1<-ggplot(IncSite, aes(x=d13C, 
                       y=d15N, 
                       fill=factor(Loc, levels=c("T004","T008","T011","T016")),
                       color=factor(Season, levels=c("SP","SU","FA")),
                       shape=factor(Type, levels=c("MAC","MES",
                                                   "MIC"))))+ 
  geom_point(size=4, stroke=2)+ 
  scale_shape_manual(values=c(21,22,23))+
  xlab(expression(paste(delta ^13,"C")))+
  ylab(expression(paste(delta ^15,"N")))+
  scale_color_manual(values=c("#0c0c0c","#949391","#3c4a46"))+
  scale_fill_manual(values=cbPalette)+
  scale_y_continuous(limits=c(10,50))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_blank(), legend.position="top",
                   axis.line = element_line(colour = "black"))
y1
dev.off()
#barplot of seasonal means -- replace site and run, then change to carbon and run
st<- split(COMPall, f=COMPall$'Loc')

library(ggplot2)
cbPalette <- c("#999999", "#56B4E9","#E69F00",  "#009E73",  "#CC79A7","#D55E00",
               "#0072B2", "#F0E442", "#000000")
png("SPdC",units="in",width=6,height=4, res=300)
y2<-ggplot(SP, aes(x=factor(Site, levels=c("T004", "T008","T011","T014","T016")), 
                          y=d13C, 
                        #fill=factor(Site, levels=c("T004", "T008","T011","T014","T016"))))+
                          fill=factor(Month, levels=c("4","5","6","7","8","9","10"))))+
  geom_boxplot()+
  #geom_point(size=5)+
  #geom_bar(position=position_dodge(), stat="identity")+
  #geom_errorbar(data=ZP, aes(ymin = d13C - Cse, ymax = dC + Cse), width=0.5,
               # position=position_dodge(0.9))+
  ylab(expression(paste("SP ",delta ^13,"C")))+
  scale_fill_manual(values=cbPalette)+
  #scale_y_continuous(limits=c(-25,0))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   strip.background = element_blank(), legend.position="top",
                   axis.line = element_line(colour = "black"))
y2
dev.off()

# stats -------------------------------------------------------------------
t.test(d15N ~ Lot, data=BA)

AN<-aov(d13C~Species+Site+Month+Year,data=IncSite)
summary(AN)
TukeyHSD(AN,"Month",ordered=TRUE)
plot(TukeyHSD(AN, "Sp"))
library(agricolae)
tukey.test2 <- HSD.test(AN, trt = 'Site')
tukey.test2

st.err <- function(x) {
  sd(x)/sqrt(length(x))
}
Dietave<-aggregate(d13C ~ Species, data=IncSite,na.rm=TRUE,mean)
Dietse<-aggregate(d13C ~ Species, data=IncSite,sd)
DietMean<-cbind(Dietave, Dietse$d13C)

Fishave<-aggregate(d13C ~ Species+Season, data=IncSite,na.rm=TRUE,mean)
Fishse<-aggregate(d13C ~ Species+Season, data=IncSite,sd)
FishMean<-cbind(Fishave, Fishse$d13C)

zs<-split(IncSite, f=IncSite$Species)
t.test(d13C~Year, data=zs$ZOO5)

