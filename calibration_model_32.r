library(tidyverse)
library(QCA)
library(venn)
library(readxl)
library(SetMethods)
library(psych)
library(scatterD3)



Excel_FS_M32 <- read_excel("EXCEL_SHEET_QCA_POPOLISMUS_FINAL_STAND_04.03.2021.xlsm", sheet = 6)

has_rownames(Excel_FS_M32)
Excel_FS_M32 <- Excel_FS_M32 %>% 
  column_to_rownames(var = "Country (Party)")
has_rownames(Excel_FS_M32)


Excel_RAW_M32 <- Excel_FS_M32


#safe raw file
write.csv(Excel_RAW_M32, "Excel_RAW_M32.csv")


#Kalibrierung


#MIGRANT

Xplot(Excel_FS_M32$MIGRANT, jitter = F)
findTh(Excel_FS_M32$MIGRANT, n = 3)
describe(Excel_FS_M32$MIGRANT)

Excel_FS_M32$MIGRANT_FS<- round(calibrate(Excel_FS_M32$MIGRANT, idm=0.99,
                                          thresholds = "i= 4.4, c= 5.3,  e= 6.1"), 2)

checkMIGRANT <- as.numeric(Excel_FS_M32$MIGRANT_FS == 0.5) 
sum(checkMIGRANT)

skewMIGRANT <- as.numeric(Excel_FS_M32$MIGRANT_FS > 0.5)
prop.table(table(skewMIGRANT))

tooltips <- paste(rownames(Excel_FS_M32)," x = ", Excel_FS_M32$MIGRANT, "; y = ",
                  Excel_FS_M32$MIGRANT_FS)
scatterD3(data = Excel_FS_M32, x = MIGRANT,
          y = MIGRANT_FS, 
          tooltip_text = tooltips,
          lines = data.frame(slope = c(0,0,Inf),
                             intercept = c(0.5,5.3,5.3),
                             stroke = "black",
                             stroke_width = 1,
                             stroke_dasharray = "3"))

Excel_FS_M32 <- Excel_FS_M32 %>% 
  select(-MIGRANT)


#TURNOUT

Xplot(Excel_FS_M32$TURNOUT, jitter = F)
findTh(Excel_FS_M32$TURNOUT, n = 3)
describe(Excel_FS_M32$TURNOUT)

Excel_FS_M32$TURNOUT_FS<- round(calibrate(Excel_FS_M32$TURNOUT,
                                          thresholds = "e= 50, c= 67, i= 80"), 2)

checkTURNOUT <- as.numeric(Excel_FS_M32$TURNOUT_FS == 0.5) 
sum(checkTURNOUT)

skewTURNOUT <- as.numeric(Excel_FS_M32$TURNOUT_FS > 0.5)
prop.table(table(skewTURNOUT))

tooltips <- paste(rownames(Excel_FS_M32)," x = ", Excel_FS_M32$TURNOUT, "; y = ",
                  Excel_FS_M32$TURNOUT_FS)
scatterD3(data = Excel_FS_M32, x = TURNOUT,
          y = TURNOUT_FS, 
          tooltip_text = tooltips,
          lines = data.frame(slope = c(0,0,Inf),
                             intercept = c(0.5,67,67),
                             stroke = "black",
                             stroke_width = 1,
                             stroke_dasharray = "3"))

Excel_FS_M32 <- Excel_FS_M32 %>% 
  select(-TURNOUT)


#ESM

Xplot(Excel_FS_M32$ESM, jitter = F)
findTh(Excel_FS_M32$ESM, n = 3)
describe(Excel_FS_M32$ESM)

Excel_FS_M32$ESM_FS<- round(calibrate(Excel_FS_M32$ESM, logistic = T, idm =0.99, 
                                      thresholds = "e= 1, c= 5, i= 20"), 2)

checkESM <- as.numeric(Excel_FS_M32$ESM_FS == 0.5) 
sum(checkESM)

skewESM <- as.numeric(Excel_FS_M32$ESM_FS > 0.5)
prop.table(table(skewESM))

tooltips <- paste(rownames(Excel_FS_M32)," x = ", Excel_FS_M32$ESM, "; y = ",
                  Excel_FS_M32$ESM_FS)
scatterD3(data = Excel_FS_M32, x = ESM,
          y = ESM_FS, 
          tooltip_text = tooltips,
          lines = data.frame(slope = c(0,0,Inf),
                             intercept = c(0.5,5,5),
                             stroke = "black",
                             stroke_width = 1,
                             stroke_dasharray = "3"))

Excel_FS_M32 <- Excel_FS_M32 %>% 
  select(-ESM)

#VOTRPOP

Xplot(Excel_FS_M32$VOTRPOP, jitter = F)
findTh(Excel_FS_M32$VOTRPOP, n = 3)
describe(Excel_FS_M32$VOTRPOP)

Excel_FS_M32$VOTRPOP_FS<- round(calibrate(Excel_FS_M32$VOTRPOP,idm =0.97, 
                                      thresholds = "e= 3, c= 7.5, i= 15"), 2)

checkVOTRPOP <- as.numeric(Excel_FS_M32$VOTRPOP_FS == 0.5) 
sum(checkVOTRPOP)

skewVOTRPOP <- as.numeric(Excel_FS_M32$VOTRPOP_FS > 0.5)
prop.table(table(skewVOTRPOP))

tooltips <- paste(rownames(Excel_FS_M32)," x = ", Excel_FS_M32$VOTRPOP, "; y = ",
                  Excel_FS_M32$VOTRPOP_FS)
scatterD3(data = Excel_FS_M32, x = VOTRPOP,
          y = VOTRPOP_FS, 
          tooltip_text = tooltips,
          lines = data.frame(slope = c(0,0,Inf),
                             intercept = c(0.5,7.5,7.5),
                             stroke = "black",
                             stroke_width = 1,
                             stroke_dasharray = "3"))

Excel_FS_M32 <- Excel_FS_M32 %>% 
  select(-VOTRPOP)


#ELECPERFEXIST

Xplot(Excel_FS_M32$ELECPERF, jitter = F)
findTh(Excel_FS_M32$ELECPERF, n = 3)
describe(Excel_FS_M32$ELECPERF)

Excel_FS_M32$ELECPERF_FS<- round(calibrate(Excel_FS_M32$ELECPERF,
                                           thresholds = "e= 3, c= 5, i= 15"), 2)

checkELECPERF <- as.numeric(Excel_FS_M32$ELECPERF_FS == 0.5) 
sum(checkELECPERF)

skewELECPERF <- as.numeric(Excel_FS_M32$ELECPERF_FS > 0.5)
prop.table(table(skewELECPERF))

tooltips <- paste(rownames(Excel_FS_M32)," x = ", Excel_FS_M32$ELECPERF, "; y = ",
                  Excel_FS_M32$ELECPERF_FS)
scatterD3(data = Excel_FS_M32, x = ELECPERF,
          y = ELECPERF_FS, 
          tooltip_text = tooltips,
          lines = data.frame(slope = 0,
                             intercept = 0.5,
                             stroke = "black",
                             stroke_width = 1,
                             stroke_dasharray = "3"))



rownames(subset(Excel_FS_M32, ELECPERF_FS > 0.5 ))

Excel_FS_M32 <- Excel_FS_M32 %>% 
  select(-ELECPERF)

#Rename Conditions
Excel_FS_M32 <- Excel_FS_M32 %>% 
  rename(
    MIGRANT = MIGRANT_FS,
    ELECPERF = ELECPERF_FS,
    TURNOUT = TURNOUT_FS,
    ESM = ESM_FS,
    VOTRPOP = VOTRPOP_FS
  )

#Save file
write.csv(Excel_FS_M32, file = "Excel_FS_M32.csv")

