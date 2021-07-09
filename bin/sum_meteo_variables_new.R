library(data.table)
library(readr)
library(ggplot2)
library(lubridate)
library(writexl)
library(dplyr)  
library(zoo)

######### READ DATA ######################

Aval <- data.table(read_delim("./data/Aval_utf_8.txt", 
                              "\t", escape_double = FALSE, 
                              col_types = cols(A = col_character(), 
                              B = col_character(), C = col_character(), 
                              D = col_character(), E = col_character(), 
                              F = col_character(), G = col_character(), 
                              H = col_character(), J = col_character(),
                              K = col_number(), L = col_number(), 
                              M = col_number(), N = col_number(), 
                              O = col_character(), cadastr_letter = col_character(), 
                              cadastr_number = col_number(), date = col_character(), 
                              day = col_number(), event = col_number(), 
                              exposure = col_character(), locality = col_character(), 
                              month = col_number(), notes = col_character(), 
                              ranking = col_number(), season = col_character(), 
                              year = col_number()), trim_ws = TRUE))

                                                                          
dta <- data.table(read_delim("./data/data_hourly_2004_2020.txt", 
                             "\t", escape_double = FALSE, 
                             col_types = cols(D = col_double(), 
                             Fmax = col_double(), Fprum = col_double(), 
                             H = col_double(), RGLB1H = col_double(), 
                             SCE = col_double(), SNO = col_double(), 
                             SRA1H = col_double(), SSV1H = col_double(), 
                             SVH = col_double(), T = col_double(), 
                             T05 = col_double(), date = col_character(), 
                             datum = col_date(format = "%Y-%m-%d"), 
                             time = col_character()), trim_ws = TRUE))

lbou_daily <- data.table(read_delim("./data/LBOU_daily_1961_2020.txt", "\t", 
                                    escape_double = FALSE, 
                                    col_types = cols(date = col_date(format = "%d.%m.%Y"),
                                                     Hprum = col_double(), SCE = col_double(),
                                                     SNO = col_double(), SVH = col_double(), 
                                                     SRA = col_double(), SSV = col_double(), 
                                                     Tprum = col_double(), T05prum = col_double(),
                                                     Fprum = col_double(), Fmax = col_double(), 
                                                     Dprum = col_double()), trim_ws = TRUE))

dates <- gsub(pattern = " UTC", replacement = "", x = dta$date)
dta$date <- as.POSIXct(x = dates, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

times <- dta$time
dta$time <- as.POSIXct(x = times, format = "%Y-%m-%d %H:%M")

dta$DATE2 <- strtrim(x = as.character(dta$date), width = 10)
dta$DATE2 <- as.POSIXct(paste(dta$DATE2), format = "%Y-%m-%d")

dta$date <- dta$datum <-dta$time <- NULL
dta_melt <- melt(dta, id.vars = "DATE2")

hours <-  c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  sum_colname <- paste0("value", hours[i], "_sum")
  mean_colname <- paste0("value", hours[i], "_mean")
  colname <- paste0("value", hours[i])
  dta_melt[, c(sum_colname) := rollsum(value, hours[i], na.rm = T, align = "right", fill = NA), by = variable]
  dta_melt[, c(mean_colname) := rollmean(value, hours[i], na.rm = T, align = "right", fill = NA), by = variable]
  dta_melt[, c(colname) := ifelse(variable %in% c("SRA1H", "SSV1H"), get(sum_colname), get(mean_colname))]
  dta_melt[, c(sum_colname):= NULL ]
  dta_melt[, c(mean_colname):= NULL ]
  gc()
  print(hours[i])
}
saveRDS(object = dta_melt, file = "data/dta_melt.rds")
dta_melt <- readRDS(file = "data/dta_melt.rds")

Aval$DATE2 <- strtrim(x = Aval$date, width = 10)

Aval$DATE3 <- as.POSIXct(paste0(Aval$DATE2, " 07:00"), format = "%d.%m.%Y %H:%M")
Aval$DATE_OFF <- as.POSIXct(Aval$DATE3 + (17*60*60))

Aval_short <- Aval[DATE_OFF %between% c(dta_melt[complete.cases(dta_melt), DATE2][1], dta_melt[nrow(dta_melt), DATE2])]

Aval_short$ID <- paste0("Aval ", 1:nrow(Aval_short))
aval_list <- list()
for (i in 1:nrow(Aval_short)){
  id <- Aval_short[i,30]#30 sloupec Aval 1,2,3...
  start <- Aval_short[i,29]#29 sloupec pro ka?dou lavinu, datum
  stop <- Aval_short[i,29] - 5*24*60*60 #okno -5, form?t Posix pracuje se sekundami
  dta_aval <- dta_melt[DATE2 %between% c(stop[1], start[1]) , ]#vezmi ??dky start, stop a pro ty mi vykresli meteo prom?nn?
  dta_aval[, PLOT:= c(1:.N), by = variable] # pro ka?dou ud?lost graf? kv?li ?emu? pro lep?? vykreslen? dat # K ?EMU PLOT?
  dta_aval$ID <- id # sloupec id asi definov?n? parametru?
  aval_list[[i]] <- dta_aval
  print(i)
}
#6183 lavin od roku 2004-2020
aval_dtafr <- rbindlist(aval_list)
aval_dtafr <- merge(x = aval_dtafr, y = Aval_short[,.(event,ID)], by = "ID")

saveRDS(object = aval_dtafr, file = "data/aval_dtafr.rds")
aval_dtafr <- readRDS(file = "data/aval_dtafr.rds")

colnames(aval_dtafr) <- c(colnames(aval_dtafr)[1:2], "var", colnames(aval_dtafr)[4:length(colnames(aval_dtafr))])
aval_melt <- melt(data = aval_dtafr, id.vars = c("ID", "DATE2", "PLOT", "event", "var"))
aval_melt[, var_name:= paste0(var, "_", variable)]

aval_dcast <- dcast.data.table(aval_melt, DATE2 + ID + PLOT + event ~ var_name, value.var = 'value')

#aval_melt$var <- aval_melt$variable <- NULL
library(dplyr)
aval_nonaval <- Aval_short[,.(event, ID)]
#aval_melt_ID <- data.table(full_join(x = aval_melt, y = aval_nonaval, by = "ID"))
# denn? data - je zahrhnuto count, za??n? od r. 2004
#Aval_short[, AV_count := sum(event), by = date]

####### MIN MAX pro T FMAX ###############
fmax_t <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("T", "Fmax") & variable == "value", {min = min(value, na.rm = T); max = max(value, na.rm = T); T0 = value[1]; list(min = min, max = max, T0 = T0)}, by = .(ID, event, var)]
  a$H <- hours[i]
  fmax_t[[i]] <- a
  print(hours[i])
}
fmax_t <- rbindlist(fmax_t)


########### SUM pro PRECIP ###########

precip <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("SRA1H") & variable == "value", {sum = sum(value, na.rm = T); T0 = value[1] ; list(sum = sum, T0 = T0)}, by = .(ID, event, var)]
  a$H <- hours[i]
  precip[[i]] <- a
  print(hours[i])
}
precip <- rbindlist(precip)

############ AVG pro zbytek ###########

avg_rest <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & !var %in% c("SRA1H", "T", "Fmax") & variable == "value", {avg = mean(value, na.rm = T); T0 = value[1] ; list(avg = avg, T0 = T0)}, by = .(ID, event, var)]
  a$H <- hours[i]
  avg_rest[[i]] <- a
  print(hours[i])
}

avg_rest <- rbindlist(avg_rest)

#################### ? SNOW DRIFT ? ######

snow_drift <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("SRA1H") & variable == "value", {sum_p = sum(value, na.rm = T); list(sum_p = sum_p)}, by = .(ID, event)]
  b <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("Fprum") & variable == "value", {mean_f = mean(value, na.rm = T); list(mean_f = mean_f)}, by = .(ID, event)]
  a$H <- hours[i]
  b$H <- hours[i]
  c <- merge(x = a, y = b, by = c("ID", "event", "H"))
  c$f4 <- c$mean_f^4  ##########  opravdu ????
  avg_rest[[i]] <- a
  print(hours[i])
}


########### DELTA T ######################

delta_t <- fmax_t[var == "T", ]

delta_t[,delta_min:= T0 - min]
delta_t[,delta_max:= T0 - max]

########### DELTA SCE ####################

delta_sce <- list()
hours <- c(24,48,72,96,120,144)
for (i in c(1:length(hours))){
  a <- aval_melt[PLOT %between% c(1, hours[i]) & var %in% c("SCE") & variable == "value", {max_sce = max(value, na.rm = T); T0 = value[1] ; list(max_sce = max_sce, T0 = T0)}, by = .(ID, event, var)]
  a$H <- hours[i]
  delta_sce[[i]] <- a
  print(hours[i])
}

delta_sce <- rbindlist(delta_sce)

delta_sce[,delta_sce:=max_sce - T0]

############ FIVE DAY SUM of SNOW ##########################

sno_sum <- aval_melt[PLOT %between% c(1, 120) & var %in% c("SNO") & variable == "value", .(sum = sum(unique(value), na.rm = T)), by = .(ID, event, var)]


############################################################
############################################################


precip[,delta:= T0 - sum]

avg_rest[,delta:= T0 - avg]

unique(avg_rest$var)

ggplot(fmax_t)+
  geom_boxplot(aes(x = event, y = min, group = event, fill = as.factor(event)))+
  facet_grid(variable~H)+
  labs(fill = "Event")

ggplot(fmax_t)+
  geom_boxplot(aes(x = event, y = max, group = event, fill = as.factor(event)))+
  facet_grid(variable~H)+
  labs(fill = "Event")

ggplot(precip)+
  geom_boxplot(aes(x = event, y = sum, group = event, fill = as.factor(event)))+
  facet_grid(variable~H)+
  labs(fill = "Event")

ggplot(avg_rest)+
  geom_boxplot(aes(x = event, y = avg, group = event, fill = as.factor(event)))+
  facet_grid(variable~H, scales = "free_y")+
  labs(fill = "Event")



# suma promennych - jedna průmerna hodnota je uvedene na koni skriptu
install.packages("reshape2")
library(reshape2)

# rolling mean, I need mean for: "H", "SCE", "SVH", "T", "Fprum", "Fmax", "T05", "D")
aval_melt_ID[, value24 := frollmean(value, 24, na.rm = T), by = .(variable, ID)]
aval_melt_ID[, value48 := frollmean(value, 48, na.rm = T), by = .(variable, ID)]
aval_melt_ID[, value72 := frollmean(value, 72, na.rm = T), by = .(variable, ID)]
aval_melt_ID[, value96 := frollmean(value, 96, na.rm = T), by = .(variable, ID)]
aval_melt_ID[, value120 := frollmean(value, 120, na.rm = T), by = .(variable, ID)]

# also I wanna establish min and max 24,48,72,96,120 of "T", "Fmax" and mark them as TAmin, TAmax
#??aval_melt_ID[, value24 := froll(value, 24, na.rm = T), by = .(c(T,Fmax) variable, ID)]
# sum of precipitation 24, 48, 72, 96, 120  
#aval_melt_ID[, value24 := frollsum(value, 24, na.rm = T), by = .(variable, ID)]
#also I wanna establish the difference (delta) in 48,72,96,120 hours of snow depth SDdif - BE AWARE that snow daity are just daily data and I copied the same value to every hour - DON'T use sum, Tmean, and Tmax 
#so called "48, 72, 96, 120 difference in snow heigh" and "24h differencence in mean, maximum air temperature"
# 5 day sum of new snow height - we have daily data so just sum one daily value of 5 days
# establish snow drift parameter: product of sum of hourly precipitation and avg. wind speed to the fourth power - Sdrift 0,24,48,72,96,120

adta = dcast.data.table(aval_melt_ID, DATE2 + event + ID + PLOT ~ variable, value.var = c('value', 'value24', 'value48', 'value72', 'value96', 'value120'))

#adta = adta[complete.cases(DATE2, event, value48_SCE, value48_T)]
# complete.cases: return a logical vector indicating which cases are complete, i.e., have no missing values.
g = glm(event ~ value24_SCE + value24_SNO +value24_SVH + value24_H + value24_D + value24_Fprum + value24_Fmax + value24_T +  value24_SRA1H + value24_SSV1H + value24_T05, data = adta, family = 'binomial')
#g1 = glm(event ~ value48_SCE + value48_SNO +value48_SVH + value48_H + value48_D + value48_Fprum + value48_Fmax + value48_T + value48_SRA1H + value48_SSV1H + value48_T05, data = adta, family = 'binomial')

summary(g)
library(ggplot2)
# Basic box plot
#p <- ggplot(g, aes(x=event, y=value24_SCE)) + 
  geom_boxplot()
# Rotate the box plot
  #p + coord_flip()
# Notched box plot
  #ggplot(g, aes(x=event, y=value24_SCE)) + 
  # geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
  #ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  #geom_boxplot(outlier.colour="red", outlier.shape=8,
  #outlier.size=4)

#load("my_work_space.RData")

setwd("home/marketa/R/Krkonose/LBOU")
LBOU_daily = fread('LBOU_daily_1961_2020.txt', sep = "\t", data.table = T)
LBOU_daily$Date2 <- as.Date(LBOU_daily$date, format = "%d.%m.%Y")
class(LBOU_daily)                              
#LBOU_daily <- data.table(read_delim("C:/Users/marketa.souckova/Documents/laviny/LBOU/LBOU_daily_1961_2020.txt", 
# "\t", escape_double = FALSE, col_types = cols(Dprum = col_double(), 
# Fmax = col_double(), Fprum = col_double(), 
#Hprum = col_double(), RGLB1H = col_double(), 
#SCE = col_double(), SNO = col_double(), 
#SRA = col_double(), SSV = col_double(), 
#SVH = col_double(), Tprum = col_double(), 
#T05prum = col_double(), date = col_character(), 
#trim_ws = TRUE)) 



#rbind (Aval_short, dta, fill = TRUE)
Aval_short$Date_new <- as.Date(Aval_short$DATE2, format = "%d.%m.%Y")
AVmeteo_2004_2020 <- merge(x = Aval_short[,.(Date_new, AV_count)], y = LBOU_daily, by.x = "Date_new", by.y = "Date2")
#Vmeteo_2004_2020 <- merge(x = Aval_short[,.(Date_new, AV_count)], y = LBOU_daily, by = .(year(Date2), month(Date2)))# AGREGOVANE mesice?
AVmeteo_2004_2020[,YEAR:= year(Date_new)]
c(10,11, 12, 1, 2, 3, 4, 5)
AVmeteo_2004_2020[,MONTH:=month(Date_new)]
AVmeteo_2004_2020[MONTH %in% c(10,11, 12, 1, 2, 3, 4, 5)]
AVmeteo_2004_2020[month(Date_new) %in% c(10,11, 12, 1, 2, 3, 4, 5)]
AVmeteo_WINTER<- AVmeteo_2004_2020[MONTH %in% c(10,11, 12, 1, 2, 3, 4, 5)]

#Vmeteo_2004_2020[month(Date_new) %in% c(10,11, 12, 1, 2, 3, 4, 5), WINTER:= TRUE]
#Vmeteo_2004_2020[,aa:= lag(WINTER)]

coeff = 0.1 
(p2<-ggplot(data = AVmeteo_2004_2020[year(Date_new)==2004]) +
  geom_line(aes(x = Date_new, y=Tprum, colour ="Tavg", group = MONTH))+
  geom_bar(x = Date_new, y=AV_count/coeff, group = MONTH))+ # Divide by 10 to get the same range than the temperature
    facet_wrap(~YEAR, scales = "free_y")+
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Tavg",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Aval count")
  ))
#TRYING TO DISPLAY JUST WINTER SEASON, Av count by mel brat jen 1. výskyt datumu 2004/2005 je 77 lavin za zimu
(p2<-ggplot(data = AVmeteo_WINTER[year(Date_new==2004]) +
    geom_line(aes(x = Date_new, y=Tprum, colour ="Tavg", group = YEAR))+
    geom_line(aes(x = Date_new, y=AV_count/coeff, group = YEAR))+
    facet_wrap(~YEAR, scales = "free_y")+
    
    scale_y_continuous(
      
      # Features of the first axis
      name = "Tavg",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coeff, name="Aval count")
    ))

# correlation of the avalanche data
  A_lenght_crown <- ggplot(data = Aval, aes(y = L, x = N)) + 
    geom_point()+
    geom_smooth(method = "glm", se = FALSE)+
    xlab ("Avalanche slide distance [m]") + ylab ("Crown width [m]")

  
  
  #day1 <- aval_melt_ID[PLOT %between% c(120-24,120), ifelse(test = variable %in% c("H", "SCE", "SVH", "T", "Fprum", "Fmax", "T05", "D"), 
  #                                                     yes = ifelse(test = variable == "Fmax",
  #                                                                 yes = max(value, na.rm = T), 
  #                                                                no = mean(value, na.rm = T)), 
  #                                                  no = sum(value, na.rm = T)), by = .(ID, variable, event)]
  #day1[V1 == -Inf, unique(variable)]
  #day1[V1 == -Inf, V1:= NA]
  
  #day1_mean <- day1[,mean(V1, na.rm = T), by = .(variable, event)]
  
  #colnames(day1_mean) <- c("variable", "event", "24h")    
  
  #day2 <- aval_melt_ID[PLOT %between% c(120-(24*2),120), ifelse(test = variable %in% c("H", "SCE", "SVH", "T", "Fprum", "Fmax", "T05", "D"), 
  #          yes = ifelse(test = variable == "Fmax",
  #                      yes = max(value, na.rm = T), 
  #                     no = mean(value, na.rm = T)), 
  #       no = sum(value, na.rm = T)), by = .(ID, variable, event)]
  
  #day2[V1 == -Inf, unique(variable)]
  #day2[V1 == -Inf, V1:= NA]
  
  #day2_mean <- day2[,mean(V1, na.rm = T), by = .(variable, event)]
  
  #colnames(day2_mean) <- c("variable", "event", "48h") 
  
  #variables_48<-full_join(x = day1_mean, y = day2_mean, by = c("variable", "event"))
  
  #day3 <- aval_melt_ID[PLOT %between% c(120-(24*3),120), ifelse(test = variable %in% c("H", "SCE", "SVH", "T", "Fprum", "Fmax", "T05", "D"), 
  #                      yes = ifelse(test = variable == "Fmax",
  #                                  yes = max(value, na.rm = T), 
  #                                 no = mean(value, na.rm = T)), 
  #                   no = sum(value, na.rm = T)), by = .(ID, variable, event)]
  
  #day3_mean <- day3[,mean(V1, na.rm = T), by = .(variable, event)]
  
  #colnames(day3_mean) <- c("variable", "event", "72h") 
  #variables_72<-full_join(x = variables_48, y = day3_mean, by = c("variable", "event"))
  
  #day4 <- aval_melt_ID[PLOT %between% c(120-(24*4),120), ifelse(test = variable %in% c("H", "SCE", "SVH", "T", "Fprum", "Fmax", "T05", "D"), 
  #                  yes = ifelse(test = variable == "Fmax",
  #                              yes = max(value, na.rm = T), 
  #                             no = mean(value, na.rm = T)), 
  #               no = sum(value, na.rm = T)), by = .(ID, variable, event)]
  
  #day4_mean <- day4[,mean(V1, na.rm = T), by = .(variable, event)]
  
  #colnames(day4_mean) <- c("variable", "event", "96h") 
  #variables_96<-full_join(x = variables_72, y = day4_mean, by = c("variable", "event"))
  
  #day5 <- aval_melt_ID[PLOT %between% c(120-(24*5),120), ifelse(test = variable %in% c("H", "SCE", "SVH", "T", "Fprum", "Fmax", "T05", "D"), 
  ##                          yes = max(value, na.rm = T), 
  #                         no = mean(value, na.rm = T)), 
  #           no = sum(value, na.rm = T)), by = .(ID, variable, event)]
  #
  #day5_mean <- day5[,mean(V1, na.rm = T), by = .(variable, event)]
  
  #colnames(day5_mean) <- c("variable", "event", "120h") 
  #variables_120<-full_join(x = variables_96, y = day5_mean, by = c("variable", "event"))
  
  #saveRDS(variables_120, file = "my_data.rds")
  #save.image(file = "cumulative_variables.RData")
  #load("cumulative_variables.RData")
  