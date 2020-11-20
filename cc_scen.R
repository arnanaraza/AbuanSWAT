### CREATE CLIMATE CHANGE RAINFALL AND TEMPERATURE WEATHER INPUTS FROM CC PROJECTIONS
install.packages('plyr')
install.packages('dplyr')
install.packages('lubridate')

library(plyr)
library(dplyr)
library(lubridate)

pcp <- read.csv('D:/AbuanSWAT/pcp_cc.csv')

DJF_45 <- 0.115 #12,1,2
DJF_85 <- 0.124

MAM_45 <- 0.103 #3,4,5
MAM_85 <- 0.07

JJA_45 <- -0.172 #6,7,8
JJA_85 <- -0.026

SON_45 <- 0.03 #9,10,11
SON_85 <- 0.11

DJF <- subset(pcp, m > 11 | m < 3)
MAM <- subset(pcp, m > 2 & m < 6)
JJA <- subset(pcp, m  > 5 & m < 9)
SON <- subset(pcp, m > 8 & m < 12)

DJF$pcp85 <- DJF$pcp + (DJF$pcp*DJF_85)
MAM$pcp85 <- MAM$pcp + (MAM$pcp*MAM_85)
JJA$pcp85 <- JJA$pcp + (JJA$pcp*JJA_85)
SON$pcp85 <- SON$pcp + (SON$pcp*SON_85)

####### TEMP
DJF_45 <- 1.2 #12,1,2
DJF_85 <- 1.6

MAM_45 <- 1.2 #3,4,5
MAM_85 <-1.7

JJA_45 <- 1.3 #6,7,8
JJA_85 <- 1.6

SON_45 <- 1.1 #9,10,11
SON_85 <- 1.6

DJF$max <- DJF$max + DJF_85
MAM$max <- MAM$max + MAM_85
JJA$max <- JJA$max + JJA_85
SON$max <- SON$max + SON_85
DJF$min <- DJF$min + DJF_85
MAM$min <- MAM$min + MAM_85
JJA$min <- JJA$min + JJA_85
SON$min <- SON$min + SON_85

pcp1 <- rbind(DJF, MAM, JJA, SON)
pcp1$max <- ifelse(pcp1$max <0, -99, pcp1$max)
pcp1$min <- ifelse(pcp1$min <0, -99, pcp1$min)
pcp1$temp45 <- paste0(pcp1$max,',' ,pcp1$min)


pcp1 <- pcp1[with(pcp1, order(id)), ]
pcp1

setwd('D:/AbuanSWAT/data')
write.csv(pcp1, 'pcp_85.csv', row.names=F)

### Cal-val 
pcp <- read.csv('D:/AbuanSWAT/pcp_cc.csv') #pcp85.csv
cc <- read.csv('D:/AbuanSWAT/data/cc_scen.csv')

pcp$d <- strptime(as.character(pcp$d), "%m/%d/%Y")
pcp$d <-format(pcp$d, "%Y-%m-%d")
jul <- sprintf("%03d", yday(pcp$d))
df <- data.frame(pcp$m, pcp$y, jul)
names(df) <- c('mo', 'yr', 'jul')
df$lwr <- NA
df$upr <- NA
#2050-45 DJF
df$lwr <- ifelse (df$mo == 12 | df$mo == 1 | df$mo == 2, cc[1,3], df$lwr)
df$upr <- ifelse (df$mo == 12 | df$mo == 1 | df$mo == 2, cc[1,5], df$upr)
#2050-45 MAM
df$lwr <- ifelse (df$mo == 3 | df$mo == 4 | df$mo == 5, cc[5,3], df$lwr )
df$upr <- ifelse (df$mo == 3 | df$mo == 4 | df$mo == 5, cc[5,5], df$upr)
#2050-45 JJA
df$lwr <- ifelse (df$mo == 6 | df$mo == 7 | df$mo == 8, cc[9,3], df$lwr )
df$upr <- ifelse (df$mo == 6 | df$mo == 7 | df$mo == 8, cc[9,5], df$upr)
#2050-45 SON
df$lwr <- ifelse (df$mo == 9 | df$mo == 10 | df$mo == 11, cc[13,3], df$lwr )
df$upr <- ifelse (df$mo == 9 | df$mo == 10 | df$mo == 11, cc[13,5], df$upr)

un <- unique(df[c("yr", "lwr")])

#subsets unique year and month
l <- lapply(1:nrow (un), function(x) subset(df, df$mo == un[x,1] |df$lwr == un[x,2]))

ll <- ldply(lapply(l, function(x) t(data.frame(c(x[1,3],x[nrow(x),3] )))), data.frame)
un <- unique(df[c("yr", "lwr", 'upr')])
fin <- data.frame(un, ll)
names(fin) <- c('yr','lwr', 'upr', 'start', 'end')

fin$end1 <- ifelse(fin$end == fin$end[1], fin$start[2]-1, fin$end)

ext <-fin[c(1,1:19*4 + 1),]
ext$start <- fin$end[4] + 1
ext$end1 <- ext$end

fin1 <- rbind(ext,fin)
fin1$end <- fin1$end1
fin1$start <- sprintf("%03d", fin1$start )
fin1$end <- sprintf("%03d", fin1$end )
fin1$chr <- paste0('r__Precipitation(){',fin1$yr, fin1$start,'-', fin1$yr,fin1$end,'}.pcp  ',fin1$lwr,' ','', fin1$upr)
fin1
setwd('D:/AbuanSWAT')
write.csv(fin1,'precip_calval.csv', row.names=F)
