# ---------------------------------------------------------------------------
# House cleaning:
rm(list=ls())
cat("\014")  

# ---------------------------------------------------------------------------
# Load libraries:
#suppressPackageStartupMessages(library(aod))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(fredr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(quantmod))


# ---------------------------------------------------------------------------
# Retrieve data series from FRED:

# Set FRED key to pull data from FRB STL:
fredr_set_key("a96399a2a52bfff6ea1f93aa73bb13d5")


# DGS10: Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity (daily into monthly)
x1 <- na.omit(fredr(series_id="DGS10", frequency = "q", observation_start = as.Date("1962-01-01"), observation_end = as.Date("2022-08-30")))

# DTB3: 3-Month Treasury Bill Secondary Market Rate (daily into monthly)
x2 <- na.omit(fredr(series_id="DTB3", frequency = "q", observation_start = as.Date("1962-01-01"), observation_end = as.Date("2022-08-30")))

# USREC: NBER Recession indicator (monthly)
x9 <- na.omit(fredr(series_id="USREC", frequency = "q", observation_start = as.Date("1962-01-01"), observation_end = as.Date("2022-08-30")))


# ---------------------------------------------------------------------------
# Create Term Spread variable and data set:
ts<-x1["value"]-x2["value"] #data_frame

my_data<-cbind(x9$value,ts)
names(my_data)<-c("rec","ts")


# ---------------------------------------------------------------------------
# Compute probit model via GLS: 12-month ahead recession

myprobit <- glm(rec ~ lag(ts, 4), family = binomial(link = "probit"), data = my_data)
summary(myprobit)

# Retrieve Predicted values
fit<-myprobit$fitted.values

# Put data into data frame structure:
fitdf<-cbind(x1$date[-240:-243],as.data.frame(fit)*100)  # DF format to be used by GGPlot.
names(fitdf)<-c("date","value")


# ----------------------------------------------------------------------------
# Plot estimated CFD:

cdf <- plot(x = my_data$ts, 
            y = my_data$rec,
            main = "Probit Model of the Probability of a 4q ahead recession given the TS",
            xlab = "Term Spread",
            ylab = "Recession Indicator",
            pch = 20,
            ylim = c(-0.4, 1.4),
            cex.main = 0.85)

# Add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Recession")
text(2.5, -0.1, cex= 0.8, "No Recession")

# Add estimated regression line
x <- seq(-10.0, 3, 0.01)
x = x*(-1)
y <- predict(myprobit, list(ts = x), type = "response")
lines(x, y, lwd = 1.5, col = "steelblue")



# ---------------------------------------------------------------------------
# Compute out-of-sample forecast: ####

# Create "new data":
# Note: first 4 numbers are actual data not used in the estimation. Second 4 are forecasted values from SPF or MA.

# Values from Macro Advisers forecast. 
forecast <- data.frame(ts = c(1.48,1.64, 1.84,0.43,0.3,-0.57,-1.04,-1.19,0,0,0,0))

# Predict probabilities:
forecast[,c("p", "se")] <- predict(myprobit, newdata = forecast, type = "response", se.fit = TRUE)[-3]


# Put into data frame with correct dates:
#xdf<-cbind(x1$date[240:243],as.data.frame(forecast$p[5:12]),as.data.frame(forecast$se[5:12]))  # DF format to be used by GGPlot.
xdf<-cbind(seq(as.Date("2021/10/01"), by="quarter", length.out=8),as.data.frame(forecast$p[5:12]),as.data.frame(forecast$se[5:12]))  # DF format to be used by GGPlot.
names(xdf)<-c("date","p", "se")


# -----------------------------------------------------------------------------
# Plot data + forecast on shaded aree plot: ####


# ----------------------------------------------------------------------------
# Step 1: Merge data sets

tmp <-xdf[c(-3)]                # drops 3rd variable
names(tmp)<-c("date", "value")  # changes names to those used in fitdf
tmp$value<-tmp$value*100
dv <- rbind(fitdf, tmp)



# Step 2: Define default layout
theme_am<-function (base_size = 12, base_family = ""){ 
  library(ggthemes)
  library(scales)
  library(extrafont)
  theme_hc(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.text.x = element_text(color = "grey20", size = 11,family="Calibri"),
      axis.text.y = element_text(color = "grey20", size = 11,family="Calibri"),
      axis.title.x = element_text(color = "grey20", size = 12,family="Calibri"),
      axis.title.y = element_text(color = "grey20", size = 12,family="Calibri"),
      plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri"),
      legend.text = element_text(color = "grey20", size = 12,family="Calibri")
    )
}


# Step 3: Recession shading function:
add_rec_shade<-function(st_date,ed_date,shade_color="darkgray"){ 
  library(ecm)
  
  #st_date<-as.Date("1962-01-01")
  #ed_date<-as.Date(Sys.Date())
  
  recession<-fredr(series_id = "USREC",observation_start = as.Date(st_date),observation_end = as.Date(ed_date))
  
  recession$diff<-recession$value-lagpad(recession$value,k=1)
  recession<-recession[!is.na(recession$diff),]
  recession.start<-recession[recession$diff==1,]$date
  recession.end<-recession[recession$diff==(-1),]$date
  
  if(length(recession.start)>length(recession.end))
  {recession.end<-c(recession.end,Sys.Date())}
  if(length(recession.end)>length(recession.start))
  {recession.start<-c(min(recession$date),recession.start)}
  
  recs<-as.data.frame(cbind(recession.start,recession.end))
  recs$recession.start<-as.Date(as.numeric(recs$recession.start),origin=as.Date("1970-01-01"))
  recs$recession.end<-as.Date(recs$recession.end,origin=as.Date("1970-01-01"))
  if(nrow(recs)>0)
  {
    rec_shade<-geom_rect(data=recs, inherit.aes=F, 
                         aes(xmin=recession.start, xmax=recession.end, ymin=-Inf, ymax=+Inf), 
                         fill=shade_color, alpha=0.5)
    return(rec_shade)
  }
}

library(extrafont)


# ----------------------------------------------------------------------------
# Step 4: Generate Plot from MERGED series
# I use a blue shaded are to denote the forecast period.
my_plot2<- ggplot(dv, aes(x=date)) +
  add_rec_shade(min(dv$date),max(dv$date)) +
  geom_line(aes(y=value/100),size = 0.8,color="#dd0400") +  #ORIGINAL
  annotate("rect", xmin = as.Date("2022-07-01"), xmax = as.Date("2023-07-01"), ymin = 0, ymax = 72/100, alpha = .2,fill = "blue") +
  scale_y_continuous(name="Prob of recession [%]",labels = scales::percent_format(accuracy = 1)) +
  theme_am()+
  scale_x_date(labels = date_format("%m-%Y")) +
  theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri"))+
  labs(title="Predicted Probabilities and NBER Recessions",x ="")

print(my_plot2)




# ----------------------------------------------------------------------------
# ALT Plot: Generate Plot from 2 DIFFERENT data sets.
# I use two series (data & forecast) in different colors. 

# To avoid the plot from jumping, I need to patch the xdf series to include
# the last data point from fitdf.
parche <- fitdf[239,]  #extract last row
parche <- cbind(parche, 0)
names(parche)<-c("date","p","se")
parche$p <- parche$p/100
xdf2 <- rbind(parche, xdf)



my_plot3<- ggplot(dv, aes(x=date)) +
  add_rec_shade(min(dv$date),max(dv$date)) +
  geom_line(data=fitdf, aes(x=date, y=value/100),size = 0.8,color="#dd0400") +
  geom_line(data=xdf2, aes(x=date, y=p),size = 0.8,color="blue") +
  geom_line(data=xdf2, aes(x=date, y=(p+se)), colour='blue', linetype="dotted") +
  geom_line(data=xdf2, aes(x=date, y=(p-se)), colour='blue', linetype="dotted") +  
  scale_y_continuous(name="Prob of recession [%]",labels = scales::percent_format(accuracy = 1)) +
  theme_am()+
  scale_x_date(labels = date_format("%m-%Y")) +
  theme(plot.title = element_text(color="#04103b", size=13, face="bold",family="Calibri"))+
  labs(title="Predicted Probabilities and NBER Recessions",x ="")

my_plot3


