setwd("C:/Users/zasha/OneDrive/Documents/Lake Mattamuskeet R/Etheridge - Water & Nutrient over time")
library(plyr)
library(dplyr)
library(zoo)
library(lubridate)

# import pump condition file

pathD<-"" #Specify folder where  data is located - No change bc it is in the directory
filename<-"MI9_Pump.csv"  #Specify file where data is located
pump<-read.table(file=paste(pathD,filename,sep=""),sep=",",skip=1)
colnames(pump) <- c( 'Date', 'WL','Cond')

#head(pump)

# import WQ file

pathD<-"" #Specify folder where  data is located
filename<-"MI9_INE.csv"  #Specify file where data is located
WQ<-read.table(file=paste(pathD,filename,sep=""),sep=",",header=TRUE)

#head(WQ)

pump$DT=strptime(pump$Date,format="%m/%d/%Y %k:%M",tz="UTC") #create column DT, define format
pump$Date=pump$DT #copy DT back to FlowDate
pump$Date=as.POSIXlt(pump$Date) #change format (date)
pump=subset(pump,select=c(-DT)) #remove column DT

#head(pump)

WQ$DT=strptime(WQ$Date,format="%m/%d/%Y %k:%M",tz="UTC") #create column DT, define format
WQ$Date=WQ$DT #copy DT back to FlowDate
WQ$Date=as.POSIXlt(WQ$Date) #change format (date)
WQ=subset(WQ,select=c(-DT)) #remove column DT


WQ$RDate <- strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + round(as.numeric(WQ$Date)/(60*15))*(60*15) #rounding date table to nearest 15 min
WQ$Date <- WQ$RDate
WQ=subset(WQ,select=c(-RDate)) #remove column RDate

mass <- join(pump,WQ,by="Date",type="left",match="first") #merging tables

#head(mass)

mass$NO3I <- na.approx(mass$NO3,maxgap = 5)
mass$NO3 <- mass$NO3I
mass=subset(mass,select=c(-NO3I)) #remove column
#mass$DONI <- na.interpolation(mass$DON)
#mass$DON <- mass$DONI
#mass=subset(mass,select=c(-DONI)) #remove column
mass$NH4I <- na.approx(mass$NH4,maxgap = 5)
mass$NH4 <- mass$NH4I
mass=subset(mass,select=c(-NH4I)) #remove column
#mass$TDNI <- na.interpolation(mass$TDN)
#mass$TDN <- mass$TDNI
#mass=subset(mass,select=c(-TDNI)) #remove column

mass <- mass[format(mass$Date,'%Y') == "2017", ] #Choose year for analysis
#mass <- mass[format(mass$Date,'%m/%Y') == "05/2017", ] #Choose year for analysis


#1 cfs = 28.317 L/s
#MI9 = 32 cfs, AG = 3.3 cfs

mass$Flow <- ifelse(mass$Cond == "ON",32,0)
mass$NO3E <- mass$NO3*mass$Flow*28.317*60*15/1000/1000
#mass$DONE <- mass$DON*mass$Flow*28.317*60*15/1000/1000
mass$NH4E <- mass$NH4*mass$Flow*28.317*60*15/1000/1000
#mass$TDNE <- mass$TDN*mass$Flow*28.317*60*15/1000/1000
mass$water <- mass$Flow*60*15 #cubic feet


massC<-na.omit(mass)
massC$NO3C <- cumsum(massC$NO3E)
#mass$DONC <- cumsum(massC$DONE)
massC$NH4C <- cumsum(massC$NH4E)
#mass$TDNC <- cumsum(massC$TDNE)

sum(massC$NO3E) #total KG of export double check?
sum(massC$NH4E) #total KG of export double check
#sum(mass$DONE)
#sum(mass$TDNE)


#2017 rainfall = 43.76"
#Ag 42.9 ha (106 acres)
# sum(mass$water)/43560/106*12 #inches
# mass$waterin <- mass$water/43560/106*12
# mass$waterin2 <- cumsum(mass$waterin)
# sum(massC$water)/43560/106*12 #inches
# massC$waterin <- massC$water/43560/106*12
# massC$waterin2 <- cumsum(massC$waterin)
# massC$watercm2 <- massC$waterin2*2.54


#MI9 117 ha (289 acres)
sum(mass$water)/43560/289*12 #inches
mass$waterin <- mass$water/43560/289*12
mass$waterin2 <- cumsum(mass$waterin)
sum(massC$water)/43560/289*12 #inches
massC$waterin <- massC$water/43560/289*12
massC$waterin2 <- cumsum(massC$waterin) #this is just the sum of the water
massC$watercm2 <- massC$waterin2*2.54

#head(massC)

#Write CSV file with results
write.csv(massC,file=paste("MI9_mass",".csv"),row.names=TRUE)

#Plot

#START OF OLD CONCENTRATION INFO. THIS IS EXPERIMENTAL!!! This is  mg/L
massC$NO3con <- ifelse(massC$Flow >0, massC$NO3, 0) #checks if there is flow, if yes, then multiplies the NO3/15min by 15min/liters.   The liters come from the ft^3/15min conversion from the pump. this is in mg/L.
massC$NH4con <- ifelse(massC$Flow >0, massC$NH4, 0)
mean(massC$NO3con[massC$NO3con !=0]) #This will give the mean NO3 concentration when pump is on in mg/l
mean(massC$NH4con[massC$NH4con !=0]) #This will give the mean NO3 concentration when pump is on in mg/l
#(sum(massC$water)*28.317) * (mean(massC$NH4con[massC$NH4con !=0]))/1000000 #check for validity

#Start of new LOAD info. THIS IS EXPERIMENTAL!! THIS IS KG/HA
massC$NO3loadSum <- massC$NO3C/117
massC$NH4loadSum <- massC$NH4C/117
sum(massC$NO3E/117) #kg/ha
sum(massC$NH4E/117) #mg/ha


  precip <- read.csv("USGS_RAIN_GAUGE.csv", stringsAsFactors = FALSE, skip = 1)

# Fix the comma and parse datetime
precip$datetime_clean <- gsub(",", " ", precip$X20d)

precip$Date <- parse_date_time(
  precip$datetime_clean,
  orders = c("Y-m-d H:M", "Y-m-d H:M:S")
)

# Extract the precipitation value column
precip <- precip %>%
  select(Date, Precip = 'X14n')

massC$Date <- as.POSIXct(massC$Date, tz = "EST")
massC <- massC %>%
  left_join(precip, by = "Date")


#Relevant info for publication
#INE concentration mean when raining (excluding when pump off)

message("MSM mean pumping concentration NO3:\n", round(mean(massC$NO3con[massC$NH4con != 0], na.rm = TRUE), digits =4),
        "\nMSM mean pumping concentration NH4:\n", round(mean(massC$NH4con[massC$NH4con != 0], na.rm = TRUE), digits =4),
        "\nMSM mean concentration raining, pumping NO3:\n", round(mean(massC$NO3con[massC$Precip > 0 & massC$NH4con != 0], na.rm = TRUE), digits =4),
        "\nMSM mean concentration raining, pumping NH4:\n", round(mean(massC$NH4con[massC$Precip > 0 & massC$NH4con != 0], na.rm = TRUE), digits =4),
        "\nMSM total load in kg/ha of NO3:\n", (round((tail(massC$NO3loadSum, 1)), digits = 4)),
        "\nMSM total load in kg/ha of NH4:\n", (round((tail(massC$NH4loadSum, 1)), digits = 4)))




##---------------------------THIS IS THE BREAK FOR PLOTTING THINGS Alt+Ctrl+B-----------------------------
require("dplyr")
require("reshape2")
require("ggplot2")
require("knitr")
require("grid")
#variable for consistent theme
LM_THEME <- theme(  legend.position = "top",
                    legend.text = element_text(size = 16),
                    axis.title = element_text(size = 18),
                    axis.text = element_text(size = 14, colour = "black"),
                    axis.text.x = element_text(angle = 30, hjust = 1),
                    axis.line = element_line(colour = "black"),
                    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                    axis.ticks.length = unit(0.2, "cm"))
#This is to generalize the plots



#water export CM
p <- ggplot(massC, aes(x = as.POSIXct(Date, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"))) +
  geom_line(aes(y = NH4C, colour = "Ammonium-N"), size = 1.2) +
  geom_line(aes(y = NO3C, colour = "Nitrate-N"), size = 1.2) +
  geom_line(aes(y = watercm2*8, colour = "Water"), size = 1.2) +
  scale_y_continuous(
    limits = c(0,640),
    breaks = seq(0, 600, by = 200),          # ← left‑axis tick positions
    labels = seq(0, 600, by = 200),
    sec.axis = sec_axis(~ .*(1/8),breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80), name = "Water Export (cm)")
  ) +
  scale_colour_manual(values = c("Ammonium-N" = "black",
                                 "Nitrate-N" = "green",
                                 "Water" = "blue")) +
  scale_x_datetime(
    date_breaks = "2 months",
    date_labels = "%m/%d/%y"
  ) +
  labs(y = "N Export (kg)", x = "Date", colour = "") +
  theme_bw() +
  LM_THEME #This is to generalize the plots


p

ggsave("MI9_cm.jpeg", plot=last_plot(), device=NULL, path="C:/Users/zasha/OneDrive/Documents/Lake Mattamuskeet R/Etheridge - Water & Nutrient over time/PLOT_OUTPUTS", scale=1, width=NA, height=NA, dpi=600, limitsize=TRUE)




#Start of load graph, THIS IS EXPERIMENTAL. THIS IS IN kg/ha

p <- ggplot(massC, aes(x = as.POSIXct(Date, format="%m/%d/%Y %H:%M:%S", tz="UTC"))) +
  geom_line(aes(y = NH4loadSum , colour = "Ammonium-N"), size = 1.2) +
  geom_line(aes(y = NO3loadSum , colour = "Nitrate-N"), size = 1.2) +
  geom_line(aes(y = watercm2*(1/8), colour = "Water"), size = 1.2) +
  scale_y_continuous(
    limits = c(0,5),
    #breaks = seq(0, 600, by = 200),          # ← left‑axis tick positions : STORE FOR LATER:breaks = c(0,20,40,60,80), labels = c(0,20,40,60,80)
    #labels = seq(0, 600, by = 200),
    sec.axis = sec_axis(~ .*(8),  c(0,10,20,30,40), labels = c(0,10,20,30,40),  name = "Water Export (cm)")
  ) +
  scale_colour_manual(values = c("Ammonium-N" = "black",
                                 "Nitrate-N" = "green",
                                 "Water" = "blue")) +
  scale_x_datetime(
    date_breaks = "2 months",
    date_labels = "%m/%d/%y"
  ) +
  labs(y = expression(paste("N concentration (kg ha"^-{1},")")), x = "Date", colour = "") +
  theme_bw() +
  LM_THEME #This is to generalize the plots


p

ggsave("MSM_Load.jpeg", plot=last_plot(), device=NULL, path="C:/Users/zasha/OneDrive/Documents/Lake Mattamuskeet R/Etheridge - Water & Nutrient over time/PLOT_OUTPUTS", scale=1, width=NA, height=NA, dpi=600, limitsize=TRUE)





#start of "zoom in" period !!!! this is not the full time frame.THIS NEEDS TO BE SPECIFIED IF THE CONCENTRATION WHEN PUM IS ACTIVE OR IF JUST THE CONCENTRATION
p <- ggplot(massC, aes(x = as.POSIXct(Date, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"))) +
  geom_line(aes(y = NO3, colour = "Nitrate-N"), size = 1.2) +
  geom_line(aes(y = NH4, colour = "Ammonium-N"), size = 1.2, alpha = .7) +
  geom_line(aes(y = (WL)*2, colour = "Water"), size = 1.2) +
  scale_x_datetime(limits = as.POSIXct(c("2017-04-08 00:00:00", "2017-04-15 23:59:59")), date_breaks = "1 day", date_labels = "%m/%d/%y")+
  scale_y_continuous(
    limits = c(0.0, 3.5), labels = scales::label_number(accuracy = 0.1),
    sec.axis = sec_axis(~ .*(1/2), name = "Water level (m)", labels = scales::label_number(accuracy = 0.1))
  ) +
  scale_colour_manual(values = c("Ammonium-N" = "black",
                                 "Nitrate-N" = "green",
                                 "Water" = "blue")) +
  labs(y = expression(paste("N concentration (mg L"^-{1},")")), x = "Date", colour = "") +
  theme_bw() +
  LM_THEME #This is to centralized theme

p

ggsave("MSM_Zoom_Conc0408_0415.jpeg", plot=last_plot(), device=NULL, path="C:/Users/zasha/OneDrive/Documents/Lake Mattamuskeet R/Etheridge - Water & Nutrient over time/PLOT_OUTPUTS", scale=1, width=NA, height=NA, dpi=600, limitsize=TRUE)



#FIX FOR MEAN EVERY DAY THIS DESTROYS PRECISION WARNING THESE LINES WILL DESTROY PRECISION

massC$Day <- as.Date(massC$Date)

daily_mean <- massC %>%
  group_by(Day) %>%
  summarise(
    NH4 = mean(NH4, na.rm = TRUE),
    NO3 = mean(NO3, na.rm = TRUE),
    WL = mean(WL, na.rm = TRUE),
  )

#THIS STARTS WL GRAPH, SMOOTHING MAY NOT BE NECESSARY FOR THIS GRAPH AS IT DIMINISHES THE COORELATION THAT CAN BE OBSERVED
p <- ggplot(daily_mean, aes(x = Day)) +
  geom_line(aes(y = NO3, colour = "Nitrate-N"), size = 1.2) +
  geom_line(aes(y = NH4, colour = "Ammonium-N"), size = 1.2, alpha = .7) +
  geom_line(aes(y = WL*1.25, colour = "Water"), size = 1.2) +
  scale_y_continuous(
    limits = c(0, 2.5), labels = scales::label_number(accuracy = 0.1),
    sec.axis = sec_axis(~ .*(1/1.25), name = "Water Level (m)", labels = scales::label_number(accuracy = 0.1))
  ) +
  scale_colour_manual(values = c("Ammonium-N" = "black",
                                 "Nitrate-N" = "green",
                                 "Water" = "blue")) +
  scale_x_date(
    limits = as.Date(c("2017-01-01", "2018-01-01")),
    date_breaks = "2 months",
    date_labels = "%m/%d/%y"
  ) +
  labs(y = expression(paste("N concentration (mg L"^-{1},")")),
       x = "Date",
       colour = "") +
  theme_bw() +
  LM_THEME #This is to generalize the plots
p

ggsave("MI9_WL_Daily_Mean.jpeg", plot=last_plot(), device=NULL, path="C:/Users/zasha/OneDrive/Documents/Lake Mattamuskeet R/Etheridge - Water & Nutrient over time/PLOT_OUTPUTS", scale=1, width=NA, height=NA, dpi=600, limitsize=TRUE)
