## Set ups
setwd("C:/Users/bdfitzgerald/Desktop/Data Science Specialist/RepData_PeerAssessment2")
file <- list.files("data", full.names = TRUE)
        ## libraries used
options(scipen = 1)
library(lubridate)
library(ggplot2)
library(gridExtra)

## importing the data file
## NOTE the cache needs to be "cache = TRUE"
stormdata <- read.csv(file)

## Understanding basics of data before data clean up
        ## data dimensions
dim(stormdata)
        ## data class
sapply(stormdata, class)
        ## first two rows
head(stormdata, 2)

## Since using EVTYPE as the primiary incident
        ## Number of unique incidents
length(unique(stormdata$EVTYPE))
        ## examining notice no consistency in the EVTYPE...some are all caps, some not, some have a space before them
        ## adjusting the EVTYPE to have consistency in the data
evtypes <- tolower(stormdata$EVTYPE)
evtypes <- gsub("[[:blank:][:punct:]+]", " ", evtypes)
stormdata$EVTYPE <- evtypes



## BY year
stormdata$YEAR <- as.numeric(format(as.Date(stormdata$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))
fatal_year <- aggregate(stormdata$FATALITIES, by = list(DATE = stormdata$YEAR), sum, na.rm = TRUE)
colnames(fatal_year)[2] <- "FATALITIES"
injury_year <- aggregate(stormdata$INJURIES, by = list(DATE = stormdata$YEAR), sum, na.rm = TRUE)
colnames(injury_year)[2] <- "INJURIES"
prop_year <- aggregate(stormdata$PROPDMG, by = list(DATE = stormdata$YEAR), sum, na.rm = TRUE)
colnames(prop_year)[2] <- "PROP_DMG"
crop_year <- aggregate(stormdata$CROPDMG, by = list(DATE = stormdata$YEAR), sum, na.rm = TRUE)
colnames(crop_year)[2] <- "CROP_DMG"
        ## Charts
fat_year <- ggplot(fatal_year, aes(x = DATE, y = FATALITIES)) +
        geom_line(size = 1, color = "firebrick") +
        ylim(min(fatal_year$FATALITIES), max(fatal_year$FATALITIES)) +
        xlab("Year") +
        ylab("Number of of Fatalities") +
        ggtitle("Number of Fatalities from 1950 to 2011") +
        theme(plot.title = element_text(size = 10, face = "bold"))
inj_year <- ggplot(injury_year, aes(x = DATE, y = INJURIES)) +
        geom_line(size = 1, color = "firebrick") +
        ylim(min(injury_year$INJURIES), max(injury_year$INJURIES)) +
        xlab("Year") +
        ylab("Number of Injuries") +
        ggtitle("Number of Injuries from 1950 to 2011") +
        theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold"))
propdmg_year <- ggplot(prop_year, aes(x = DATE, y = PROP_DMG) +
        geom_line(size = 1, color = "firebrick") +
        ylim(min(prop_year$PROP_DMG), max(prop_year$PROP_DMG)) +
        xlab("Year") +
        ylab("total Property Damage") +
        ggtitle("Amount of Property Damage from 1950 to 2011") +
        theme(plot.title = element_text(size = 10, face = "bold"))
cropdmg_year <- ggplot(crop_year, aes(x = DATE, y = CROP_DMG)) +
        geom_line(size = 1, color = "firebrick") +
        ylim(min(crop_year$CROP_DMG), max(crop_year$CROP_DMG)) +
        xlab("Year") +
        ylab("Total Crop Damage") +
        ggtitle("Amount of Crop Damage from 1950 to 2011") +
        theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold"))

grid.arrange(fat_year, propdmg_year, inj_year, cropdmg_year, ncol = 2)


## Note this will need to be CACHE = TRUE
## creating a 15 yr trend 1997 to 20111
stormdata <- subset(stormdata, stormdata$YEAR >= 1997, )

## Totals in a year
total_fat <- aggregate(stormdata$FATALITIES, by = list(Year = stormdata$YEAR), sum, na.rm = TRUE)
total_inj <- aggregate(stormdata$INJURIES, by = list(Year = stormdata$YEAR), sum, na.rm = TRUE)
total_prop <- round(aggregate(stormdata$PROPDMG, by = list(Year = stormdata$YEAR), sum, na.rm = TRUE), 0)
total_crop <- round(aggregate(stormdata$CROPDMG, by = list(Year = stormdata$YEAR), sum, na.rm = TRUE), 0)
total <- cbind(total_fat, total_inj[,2], total_prop[,2], total_crop[,2])
colnames(total)[2:5] <- c("Fatalities", "Injuries", "Property Damage (in dollars)", "Crop Damage (in dollars)")
total[,2:5] <- format(total[,2:5], big.mark = ",")
total


## By EVTYPE charts
        ## fatal
fatal <- aggregate(stormdata$FATALITIES, by = list(EVTYPE = stormdata$EVTYPE), sum, na.rm = TRUE)
colnames(fatal)[2] <- "FATALITIES"
fatal <- subset(fatal, fatal$FATALITIES > 0,)
        ## injury
injury <- aggregate(stormdata$INJURIES, by = list(EVTYPE = stormdata$EVTYPE), sum, na.rm = TRUE)
colnames(injury)[2] <- "INJURIES"
injury <- subset(injury, injury$INJURIES > 0,)
        ## property damage
prop_damage <- aggregate(stormdata$PROPDMG, by = list(EVTYPE = stormdata$EVTYPE), sum, na.rm = TRUE)
colnames(prop_damage)[2] <- "PROP_DAMAGE"
prop_damage <- subset(prop_damage, prop_damage$PROP_DAMAGE > 0,)
        ## crop damage
crop_damage <- aggregate(stormdata$CROPDMG, by = list(EVTYPE = stormdata$EVTYPE), sum, na.rm = TRUE)
colnames(crop_damage)[2] <- "CROP_DAMAGE" 
crop_damage <- subset(crop_damage, crop_damage$CROP_DAMAGE > 0, )
        ## top 10
fatal_top10 <- head(arrange(fatal, -FATALITIES), 10)
injury_top10 <- head(arrange(injury, -INJURIES), 10)
prop_damage10 <- head(arrange(prop_damage, -PROP_DAMAGE), 10)
prop_damage10$PROP_DAMAGE <- round(prop_damage10$PROP_DAMAGE, 0)
crop_damage10 <- head(arrange(crop_damage, -CROP_DAMAGE), 10)
crop_damage10$CROP_DAMAGE <- round(crop_damage10$CROP_DAMAGE, 0)


## Top charts
fat_10 <- ggplot(fatal_top10, aes(x = reorder(EVTYPE, FATALITIES), y = FATALITIES)) +
        geom_bar(stat = "identity", fill = "firebrick", color = "firebrick") +
        coord_flip() +
        ylab("Number of Fatalities") +
        xlab("Event Type") +
        ggtitle("10 Highest Events Resulting in Fatalities") +
        theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold"))
inj_10 <- ggplot(injury_top10, aes(x = reorder(EVTYPE, INJURIES), y = INJURIES)) +
        geom_bar(stat = "identity", fill = "firebrick", color = "firebrick") +
        coord_flip() +
        ylab("Number of Injuries") +
        xlab("Event Type") +
        ggtitle("10 Highest Events Resulting in Injuries") +
        theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold"))
prop_10 <- ggplot(prop_damage10, aes(x = reorder(EVTYPE, PROP_DAMAGE), y = log10(PROP_DAMAGE))) +
        geom_bar(stat = "identity", fill = "firebrick", color = "firebrick") +
        coord_flip() +
        ylab("Amount of Property Damage (based on log-scale)") +
        xlab("Event Type") +
        ggtitle("10 Highest Events Resulting in Property Damage") +
        theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold"))
crop_10 <- ggplot(crop_damage10, aes(x = reorder(EVTYPE, CROP_DAMAGE), y = log10(CROP_DAMAGE))) +
        geom_bar(stat = "identity", fill = "firebrick", color = "firebrick") +
        coord_flip() +
        ylab("Amount of Property Damage (based on log-scale)") +
        xlab("Event Type") +
        ggtitle("10 Highest Events Resulting in Crop Damage") +
        theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold"))

grid.arrange(fat_10, prop_10, inj_10, crop_10, ncol = 2)
        





