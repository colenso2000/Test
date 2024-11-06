#-----------------------------------------------------------------------------
# This script compares the recent the Federal Reserve's tightening cycles going 
# from 1983 until 2023.



#-----------------------------------------------------------------------------
# Housekeeping - Clear work space:
rm(list=ls())


#-----------------------------------------------------------------------------
# Get data from FRED:
library(fredr) # load FRED R package
fredr_set_key("711d25f225b04d86b2159bca07e18cf8") # set personal key


# Retrieve data 
# library(tidyverse) # load data processing package

DFEDTAR <- fredr(series_id = "DFEDTAR", # select FRED data series
      frequency = "m" # select monthly frequency
)
DFEDTARU <- fredr(series_id = "DFEDTARU", # select FRED data series
                 frequency = "m" # select monthly frequency
)
DFEDTARU$value[length(DFEDTARU$value)] <- 5.25

start_1983 <- 6
stop_1983 <- 24
start_1987 <- 52
stop_1987 <- 58
start_1994 <- 137
stop_1994 <- 150
start_1999 <- 201
stop_1999 <- 214
start_2004 <- 261
stop_2004 <- 287
start_2015 <- 84
stop_2015 <- 122
start_2022 <- 159
stop_2022 <- 177

# Plot ----

# define data to plot
dat <- function(tbl, start, stop) {
  vals <- tbl$value[start:stop]
  y <- vals - vals[1]
  x <- 0:(length(y)[1]-1)
  return(list(x=x,y=y))
  
}

plt_dat <- dat(DFEDTAR,start_1983,stop_1983)

# set graphical parameters
par(mar = c(10,5,2.5,2.5)) 

# create an empty plot
ylim_max <- 5.5
plot(plt_dat$x, plt_dat$y,
     type = "n", # no points or lines
     xlim = c(0,38),
     ylim = c(0,ylim_max), # set y limits
     ylab = "Change in Federal Funds Rate (%)", # set x label
     xlab = "Months Since End of Previous Cycle" # set y label
)

# add x axis with custom tick marks and labels
axis(1,
     at = seq(0,38, by = 1), # set tick mark positions
     labels = 0:38) # set tick mark labels


# add horizontal grid lines
abline(h = seq(0,ylim_max,by=1), # set y values for grid lines
       col = "grey", # set color for grid lines
       lty = "dotted") # set line type for grid lines

# add lines and labels
clr <- "red"
plt_dat <- dat(DFEDTAR,start_1983,stop_1983)
lines(plt_dat$x,plt_dat$y,col=clr,lty="solid")
text(x = max(plt_dat$x)+0.5, y = max(plt_dat$y)-0.1, labels = "1983", col = clr)
clr <- "blue"
plt_dat <- dat(DFEDTAR,start_1987,stop_1987)
lines(plt_dat$x,plt_dat$y,col=clr,lty="solid")
text(x = max(plt_dat$x)+0.25, y = max(plt_dat$y)-0.1, labels = "1987", col = clr)
clr <- "green"
plt_dat <- dat(DFEDTAR,start_1994,stop_1994)
lines(plt_dat$x,plt_dat$y,col=clr,lty="solid")
text(x = max(plt_dat$x)+0.1, y = max(plt_dat$y)+0.1, labels = "1994", col = clr)
clr <- "orange"
plt_dat <- dat(DFEDTAR,start_1999,stop_1999)
lines(plt_dat$x,plt_dat$y,col=clr,lty="solid")
text(x = max(plt_dat$x)+0.1, y = max(plt_dat$y)+0.1, labels = "1999", col = clr)
clr <- "purple"
plt_dat <- dat(DFEDTAR,start_2004,stop_2004)
lines(plt_dat$x,plt_dat$y,col=clr,lty="solid")
text(x = max(plt_dat$x)+0.1, y = max(plt_dat$y)+0.1, labels = "2004", col = clr)
clr <- "grey"
plt_dat <- dat(DFEDTARU,start_2015,stop_2015)
lines(plt_dat$x,plt_dat$y,col=clr,lty="solid")
text(x = max(plt_dat$x)-0.1, y = max(plt_dat$y)+0.1, labels = "2015", col = clr)
clr <- "black"
plt_dat <- dat(DFEDTARU,start_2022,stop_2022)
lines(plt_dat$x,plt_dat$y,col=clr,lty="solid")
text(x = max(plt_dat$x)-0.1, y = max(plt_dat$y)+0.1, labels = "2023", col = clr)

# add caption
mtext("Note: Data is the short-term interest rate targeted by the Federal Reserve's Federal Open Market Committee (FOMC) as part of its monetary policy. 
    Lines represent the cumulative change in the Federal Funds Target Rate from the end of the previous cycle to the end of the cycle shown.
      \n Source:Monthly Data, Federal Funds Target Rate (DISCONTINUED) [DFEDTAR] and Federal Funds Target Range - Upper Limit [DFEDTARU], 
      retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/DFEDTAR and https://fred.stlouisfed.org/series/DFEDTARU, May 15, 2023.", # text for caption
      side = 1, # bottom side of plot
      line = -1, # distance from plot region
      outer = TRUE) # outside margins




