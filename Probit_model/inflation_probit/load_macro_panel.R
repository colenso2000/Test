# ---------------------------------------------------------------------------
# House keeping:
rm(list=ls())


#------------------------------------------------------------------------------
# Load libraries:
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(haven))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))


#------------------------------------------------------------------------------
# Set directory
setwd("...")


#------------------------------------------------------------------------------
# Load Panel
"./Macro_panel1.dta"

paneldir <- "/mq/home/m1dxv01/1workdocs/TFS/winf/"

macro_panel <- read_dta(file.path(paneldir, "Macro_panel_data1.dta")) %>%
  mutate(quarter = (quarter %% 4) + 1) %>%
  mutate(quarter = as.yearqtr(paste0(year, "Q", quarter)))


#------------------------------------------------------------------------------
# Look at data:
#as_tibble((macro_panel))

# Opens table:
#View(macro_panel)


#------------------------------------------------------------------------------
# Extract data for one country:

# Pull all data for one country 
data_usa <- macro_panel[macro_panel$countryname=="US",]

# Pull one series for one country:
data_usa <- macro_panel[macro_panel$countryname=="US",]$cpi_gp


# Aside: Remove one object
# rm(x)


#------------------------------------------------------------------------------
# Extract one series for ALL countries

# Create Factor Variable with names:
country_lables<-levels(as.factor(macro_panel$countryname))
names(x)<-country_lables



#------------------------------------------------------------------------------
# Extract loop: extracts one series for all countries.

# Create Null object
x<-cbind()
#x<-mat.or.vec(250,1)

# Loop via Country code
cd1 <-c(2,3,4,7,12,17,18,19,26,28,29,36,37,38,42,49,51,55) #20 AFE
for(i in 1:length(cd1)){ 
  y <- macro_panel %>% select(cpi_gp,country) %>% filter(country==cd1[i]) %>% select(-country)
  y <- ts(data=y, start=c(1960,1), frequency =4)
  x <-cbind(x,y)
}
colnames(x)<-cd1





# Loop via Country name.
cd2 <-c("AL","AU","BE","CA","DN","US") #4 AFE
t2<-cbind()
for(i in 1:length(cd2)){
  t1 <- macro_panel %>% select(cpi_gp,countryname) %>% filter(countryname==cd2[i]) %>% select(-countryname)
  t1 <- ts(data=t1, start=c(1960,1), frequency =4)
  t2 <-cbind(t1,t2)
}
colnames(t2)<-cd2







# Manual extract (individual series)
#------------------------------------------------------------------------------
# Inflation data:

# Australia:
x2 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==2) %>% select(-country)
x2 <- ts(data=x2, start=c(1960,1), frequency =4)

# Austria:
x3 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==3) %>% select(-country)
x3 <- ts(data=x3, start=c(1960,1), frequency =4)

# Belgium:
x4 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==4) %>% select(-country)
x4 <- ts(data=x4, start=c(1960,1), frequency =4)

# Canada:
x7 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==7) %>% select(-country)
x7 <- ts(data=x7, start=c(1960,1), frequency =4)

# Denmark:
x12 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==12) %>% select(-country)
x12 <- ts(data=x12, start=c(1960,1), frequency =4)

# France:
x17 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==17) %>% select(-country)
x17 <- ts(data=x17, start=c(1960,1), frequency =4)

# Germany:
x18 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==18) %>% select(-country)
x18 <- ts(data=x18, start=c(1960,1), frequency =4)

# Greece:
x19 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==19) %>% select(-country)
x19 <- ts(data=x19, start=c(1960,1), frequency =4)

# Ireland:
x26 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==26) %>% select(-country)
x26 <- ts(data=x26, start=c(1960,1), frequency =4)

# Italy:
x28 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==28) %>% select(-country)
x28 <- ts(data=x28, start=c(1960,1), frequency =4)

# Japan:
x29 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==29) %>% select(-country)
x29 <- ts(data=x29, start=c(1960,1), frequency =4)

# Netherlands:
x36 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==36) %>% select(-country)
x36 <- ts(data=x36, start=c(1960,1), frequency =4)

# New Zealand:
x37 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==37) %>% select(-country)
x37 <- ts(data=x37, start=c(1960,1), frequency =4)

# Norway:
x38 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==38) %>% select(-country)
x38 <- ts(data=x38, start=c(1960,1), frequency =4)

# Portugal:
x42 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==42) %>% select(-country)
x42 <- ts(data=x42, start=c(1960,1), frequency =4)

# Spain:
x49 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==49) %>% select(-country)
x49 <- ts(data=x49, start=c(1960,1), frequency =4)

# Sweden:
x50 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==50) %>% select(-country)
x50 <- ts(data=x50, start=c(1960,1), frequency =4)

# Switzerland:
x51 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==51) %>% select(-country)
x51 <- ts(data=x50, start=c(1960,1), frequency =4)

# UK:
x55 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==55) %>% select(-country)
x55 <- ts(data=x55, start=c(1960,1), frequency =4)

# USA:
x56 <- macro_panel %>% select(cpi_gy,country) %>% filter(country==56) %>% select(-country)
x56 <- ts(data=x56, start=c(1960,1), frequency =4)


# Bind inflation data together:
data_pi<-cbind(x2,x3,x4,x7,x12,x17,x18,x19,x26,x28,x29,x36,x37,x38,x42,x49,x50,x51,x55,x56)




#-------------------------------------------------------------------------------
# Inflation plots to pdf:
pdf(file="inflation.pdf")
par(mfrow=c(2,2))
plot.ts(x56, ylab="yoy Inflaiton", xlab="Date", main="US")
plot.ts(x55, ylab="yoy Inflaiton", xlab="Date", main="UK")
plot.ts(x2, ylab="yoy Inflaiton", xlab="Date", main="Australia")
plot.ts(x3, ylab="yoy Inflaiton", xlab="Date", main="Austria")
plot.ts(x4, ylab="yoy Inflaiton", xlab="Date", main="Belgium")
plot.ts(x7, ylab="yoy Inflaiton", xlab="Date", main="Canada")
plot.ts(x12, ylab="yoy Inflaiton", xlab="Date", main="Denmark")
plot.ts(x17, ylab="yoy Inflaiton", xlab="Date", main="France")
plot.ts(x18, ylab="yoy Inflaiton", xlab="Date", main="Germany")
plot.ts(x19, ylab="yoy Inflaiton", xlab="Date", main="Greece")
plot.ts(x26, ylab="yoy Inflaiton", xlab="Date", main="Ireland")
plot.ts(x28, ylab="yoy Inflaiton", xlab="Date", main="Italy")
plot.ts(x29, ylab="yoy Inflaiton", xlab="Date", main="Japan")
plot.ts(x36, ylab="yoy Inflaiton", xlab="Date", main="Netherlands")
plot.ts(x37, ylab="yoy Inflaiton", xlab="Date", main="New Zealand")
plot.ts(x38, ylab="yoy Inflaiton", xlab="Date", main="Norway")
plot.ts(x42, ylab="yoy Inflaiton", xlab="Date", main="Portugal")
plot.ts(x49, ylab="yoy Inflaiton", xlab="Date", main="Spain")
plot.ts(x50, ylab="yoy Inflaiton", xlab="Date", main="Sweden")
plot.ts(x51, ylab="yoy Inflaiton", xlab="Date", main="Switzerland")
dev.off()






#-------------------------------------------------------------------------------
# Condition filters: greater or equal to 3% growth between 8 periods
p<-cbind()
for(i in 9:length(x56)){p<-rbind(p, ifelse(x56[i]-abs(x56[i-8])<=3,0,1))}
p <- ts(data=p, start=c(1960,1), frequency =4)
plot.ts(p)

# with extra condition
p<-cbind()
for(i in 9:length(x56)){p<-rbind(p, ifelse(x56[i]-x56[i-8]>=3&x56[i-8]>0,1,0))}
p <- ts(data=p, start=c(1960,1), frequency =4)
plot.ts(p)



# Several Countries:

p56<-cbind()
for(i in 9:length(x56)){p56<-rbind(p56, ifelse(x56[i]-x56[i-8]>=3 & x56[i-8]>0,1,0))}
#for(i in 9:length(x56)){p56<-rbind(p56, ifelse(x56[i]-abs(x56[i-8])<=3,0,1))}
p56 <- ts(data=p56, start=c(1960,1), frequency =4)
plot.ts(p56, main="US")

p2<-cbind()
for(i in 9:length(x2)){p2<-rbind(p2, ifelse(x2[i]-x2[i-8]>=3 & x2[i-8]>0,1,0))}
#for(i in 9:length(x2)){p2<-rbind(p2, ifelse(x2[i]-abs(x2[i-8])<=3,0,1))}
p2 <- ts(data=p2, start=c(1960,1), frequency =4)
plot.ts(p2, main="AL")

p3<-cbind()
for(i in 9:length(x3)){p3<-rbind(p3, ifelse(x3[i]-x3[i-8]>=3 & x3[i-8]>0,1,0))}
#for(i in 9:length(x3)){p3<-rbind(p3, ifelse(x3[i]-abs(x3[i-8])<=3,0,1))}
p3 <- ts(data=p3, start=c(1960,1), frequency =4)
plot.ts(p3, main="AU")

p4<-cbind()
for(i in 9:length(x4)){p4<-rbind(p4, ifelse(x4[i]-x4[i-8]>=3 & x4[i-8]>0,1,0))}
#for(i in 9:length(x4)){p4<-rbind(p4, ifelse(x4[i]-abs(x4[i-8])<=3,0,1))}
p4 <- ts(data=p4, start=c(1960,1), frequency =4)
plot.ts(p4, main="BE")

p7<-cbind()
for(i in 9:length(x7)){p7<-rbind(p7, ifelse(x7[i]-x7[i-8]>=3 & x7[i-8]>0,1,0))}
#for(i in 9:length(x7)){p7<-rbind(p7, ifelse(x7[i]-abs(x7[i-8])<=3,0,1))}
p7 <- ts(data=p7, start=c(1960,1), frequency =4)
plot.ts(p7, main="CA")

p12<-cbind()
for(i in 9:length(x12)){p12<-rbind(p12, ifelse(x12[i]-x12[i-8]>=3 & x12[i-8]>0,1,0))}
#for(i in 9:length(x12)){p12<-rbind(p12, ifelse(x12[i]-abs(x12[i-8])<=3,0,1))}
p12 <- ts(data=p12, start=c(1960,1), frequency =4)
plot.ts(p12, main="DE")

p17<-cbind()
for(i in 9:length(x17)){p17<-rbind(p17, ifelse(x17[i]-x17[i-8]>=3 & x17[i-8]>0,1,0))}
#for(i in 9:length(x17)){p17<-rbind(p17, ifelse(x17[i]-abs(x17[i-8])<=3,0,1))}
p17 <- ts(data=p17, start=c(1960,1), frequency =4)
plot.ts(p17, main="FR")

p18<-cbind()
for(i in 9:length(x18)){p18<-rbind(p18, ifelse(x18[i]-x18[i-8]>=3 & x18[i-8]>0,1,0))}
#for(i in 9:length(x18)){p18<-rbind(p18, ifelse(x18[i]-abs(x18[i-8])<=3,0,1))}
p18 <- ts(data=p18, start=c(1960,1), frequency =4)
plot.ts(p18, main="GE")

p19<-cbind()
for(i in 9:length(x19)){p19<-rbind(p19, ifelse(x19[i]-x19[i-8]>=3 & x19[i-8]>0,1,0))}
#for(i in 9:length(x19)){p19<-rbind(p19, ifelse(x19[i]-abs(x19[i-8])<=3,0,1))}
p19 <- ts(data=p19, start=c(1960,1), frequency =4)
plot.ts(p19, main="GR")

p26<-cbind()
for(i in 9:length(x26)){p26<-rbind(p26, ifelse(x26[i]-x26[i-8]>=3 & x26[i-8]>0,1,0))}
#for(i in 9:length(x26)){p26<-rbind(p26, ifelse(x26[i]-abs(x26[i-8])<=3,0,1))}
p26 <- ts(data=p26, start=c(1960,1), frequency =4)
plot.ts(p26, main="IR")

p28<-cbind()
for(i in 9:length(x28)){p28<-rbind(p28, ifelse(x28[i]-x28[i-8]>=3 & x28[i-8]>0,1,0))}
#for(i in 9:length(x28)){p28<-rbind(p28, ifelse(x28[i]-abs(x28[i-8])<=3,0,1))}
p28 <- ts(data=p28, start=c(1960,1), frequency =4)
plot.ts(p28, main="IT")

p29<-cbind()
for(i in 9:length(x29)){p29<-rbind(p29, ifelse(x29[i]-x29[i-8]>=3 & x29[i-8]>0,1,0))}
#for(i in 9:length(x29)){p29<-rbind(p29, ifelse(x29[i]-abs(x29[i-8])<=3,0,1))}
p29 <- ts(data=p29, start=c(1960,1), frequency =4)
plot.ts(p29, main="JP")

p36<-cbind()
for(i in 9:length(x36)){p36<-rbind(p36, ifelse(x36[i]-x36[i-8]>=3 & x36[i-8]>0,1,0))}
#for(i in 9:length(x36)){p36<-rbind(p36, ifelse(x36[i]-abs(x36[i-8])<=3,0,1))}
p36 <- ts(data=p36, start=c(1960,1), frequency =4)
plot.ts(p36, main="ND")

p37<-cbind()
for(i in 9:length(x37)){p37<-rbind(p37, ifelse(x37[i]-x37[i-8]>=3 & x37[i-8]>0,1,0))}
#for(i in 9:length(x37)){p37<-rbind(p37, ifelse(x37[i]-abs(x37[i-8])<=3,0,1))}
p37 <- ts(data=p37, start=c(1960,1), frequency =4)
plot.ts(p37, main="NZ")

p38<-cbind()
for(i in 9:length(x38)){p38<-rbind(p38, ifelse(x38[i]-x38[i-8]>=3 & x38[i-8]>0,1,0))}
#for(i in 9:length(x38)){p38<-rbind(p38, ifelse(x38[i]-abs(x38[i-8])<=3,0,1))}
p38 <- ts(data=p38, start=c(1960,1), frequency =4)
plot.ts(p38, main="NR")

p42<-cbind()
for(i in 9:length(x42)){p42<-rbind(p42, ifelse(x42[i]-x42[i-8]>=3 & x42[i-8]>0,1,0))}
#for(i in 9:length(x42)){p42<-rbind(p42, ifelse(x42[i]-abs(x42[i-8])<=3,0,1))}
p42 <- ts(data=p42, start=c(1960,1), frequency =4)
plot.ts(p42, main="PT")

p49<-cbind()
for(i in 9:length(x49)){p49<-rbind(p49, ifelse(x49[i]-x49[i-8]>=3 & x49[i-8]>0,1,0))}
#for(i in 9:length(x49)){p49<-rbind(p49, ifelse(x49[i]-abs(x49[i-8])<=3,0,1))}
p49 <- ts(data=p49, start=c(1960,1), frequency =4)
plot.ts(p49, main="SP")

p50<-cbind()
for(i in 9:length(x50)){p50<-rbind(p50, ifelse(x50[i]-x50[i-8]>=3 & x50[i-8]>0,1,0))}
#for(i in 9:length(x50)){p50<-rbind(p50, ifelse(x50[i]-abs(x50[i-8])<=3,0,1))}
p50 <- ts(data=p50, start=c(1960,1), frequency =4)
plot.ts(p50, main="SW")

p51<-cbind()
for(i in 9:length(x51)){p51<-rbind(p51, ifelse(x51[i]-x51[i-8]>=3 & x51[i-8]>0,1,0))}
#for(i in 9:length(x51)){p51<-rbind(p51, ifelse(x51[i]-abs(x51[i-8])<=3,0,1))}
p51 <- ts(data=p51, start=c(1960,1), frequency =4)
plot.ts(p51, main="SW")

p55<-cbind()
for(i in 9:length(x55)){p55<-rbind(p55, ifelse(x55[i]-x55[i-8]>=3 & x55[i-8]>0,1,0))}
#for(i in 9:length(x55)){p55<-rbind(p55, ifelse(x55[i]-abs(x55[i-8])<=3,0,1))}
p55 <- ts(data=p55, start=c(1960,1), frequency =4)
plot.ts(p55, main="UK")


#-------------------------------------------------------------------------------
# Episodes plots to pdf:
pdf(file="episodes.pdf")
par(mfrow=c(2,2))
plot.ts(p56, ylab="Inflation Episodes", xlab="Date", main="US")
plot.ts(p55, ylab="Inflation Episodes", xlab="Date", main="UK")
plot.ts(p2, ylab="Inflation Episodes", xlab="Date", main="Australia")
plot.ts(p3, ylab="Inflation Episodes", xlab="Date", main="Austria")
plot.ts(p4, ylab="Inflation Episodes", xlab="Date", main="Belgium")
plot.ts(p7, ylab="Inflation Episodes", xlab="Date", main="Canada")
plot.ts(p12, ylab="Inflation Episodes", xlab="Date", main="Denmark")
plot.ts(p17, ylab="Inflation Episodes", xlab="Date", main="France")
plot.ts(p18, ylab="Inflation Episodes", xlab="Date", main="Germany")
plot.ts(p19, ylab="Inflation Episodes", xlab="Date", main="Greece")
plot.ts(p26, ylab="Inflation Episodes", xlab="Date", main="Ireland")
plot.ts(p28, ylab="Inflation Episodes", xlab="Date", main="Italy")
plot.ts(p29, ylab="Inflation Episodes", xlab="Date", main="Japan")
plot.ts(p36, ylab="Inflation Episodes", xlab="Date", main="Netherlands")
plot.ts(p37, ylab="Inflation Episodes", xlab="Date", main="New Zealand")
plot.ts(p38, ylab="Inflation Episodes", xlab="Date", main="Norway")
plot.ts(p42, ylab="Inflation Episodes", xlab="Date", main="Portugal")
plot.ts(p49, ylab="Inflation Episodes", xlab="Date", main="Spain")
plot.ts(p50, ylab="Inflation Episodes", xlab="Date", main="Sweden")
plot.ts(p51, ylab="Inflation Episodes", xlab="Date", main="Switzerland")
dev.off()



