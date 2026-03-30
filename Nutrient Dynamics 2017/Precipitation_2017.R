library(dplyr)
library(zoo)
library(ggplot2)

setwd("C:/Users/zasha/OneDrive - East Carolina University/_Lake Mattamuskeet DOCs")

pathD <- "" #Specify folder where  data is located - No change bc it is in the directory
filename <- "USGS_RAIN_GAUGE.csv"  #Specify file where data is located
rainfall <- read.table(file=paste(pathD,filename,sep=""),sep=",",skip=2)
colnames(rainfall)

# Create a new table with only the 'name' and 'score' columns
rainfall <- rainfall[, c(3, 5)]

colnames(rainfall)[1] <- "Date"
colnames(rainfall)[2] <- "Precipitation"

rainfall <- rainfall %>%
  mutate(Sum_Precipitation = cumsum(rainfall$Precipitation))

rainfall$SI_Precipitation <- rainfall$Precipitation *  2.54 #inches to centimeters
rainfall$SI_Sum_Precipitation <- rainfall$Sum_Precipitation *  2.54


#head(rainfall$SI_Precipitation)


#GRAPH FOR PRECIPITATION IN SI
p <- ggplot(rainfall, aes(x = as.POSIXct(Date, format = "%Y-%m-%d, %H:%M", tz = "UTC"))) +
  geom_line(aes(y = SI_Precipitation, colour = "Precipitation"), linewidth = 1.2) + 
  scale_y_continuous(name = "Precipitation (cm)") +
  scale_colour_manual(values = c("Precipitation" = "blue")) + 
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m/%d/%y") +
  labs(y = "Precipitation (cm)", x = "Date", colour = NULL) +
  theme_bw() +
  theme(  legend.position = "top",
          legend.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14, colour = "black"),
          axis.text.x = element_text(angle = 30, hjust = 1),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
          axis.ticks.length = unit(0.2, "cm"))


p

ggsave("Precipitation(cm).jpeg", plot=last_plot(), device=NULL, path="C:/Users/zasha/OneDrive/Documents/Lake Mattamuskeet R/Etheridge - Water & Nutrient over time/PLOT_OUTPUTS", scale=1, width=NA, height=NA, dpi=600, limitsize=TRUE)


#GRAPH FOR SI UNITS FOR RAINFALL CUMSUM
p <- ggplot(rainfall, aes(x = as.POSIXct(Date, format = "%Y-%m-%d, %H:%M", tz = "UTC"))) +
  geom_line(aes(y = SI_Sum_Precipitation, colour = "Precipitation"), linewidth = 1.2) + 
  scale_y_continuous(limits=c(0,120), breaks=seq(0,120, by = 30), labels= seq(0,120, by = 30), name = "Precipitation (cm)") +
  scale_colour_manual(values = c("Precipitation" = "blue")) + 
  scale_x_datetime(date_breaks = "2 months", date_labels = "%m/%d/%y") +
  labs(y = "Precipitation (cm)", x = "Date", colour = NULL) +
  theme_bw() +
  theme(  legend.position = "top",
          legend.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 14, colour = "black"),
          axis.text.x = element_text(angle = 30, hjust = 1),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
          axis.ticks.length = unit(0.2, "cm"))

p

ggsave("Cumulative_Precipitation(cm).jpeg", plot=last_plot(), device=NULL, path="C:/Users/zasha/OneDrive/Documents/Lake Mattamuskeet R/Etheridge - Water & Nutrient over time/PLOT_OUTPUTS", scale=1, width=NA, height=NA, dpi=600, limitsize=TRUE)