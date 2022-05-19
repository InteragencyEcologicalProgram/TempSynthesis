library(dplyr)
library(base)
library(datasets)
library(ggplot2)
library(dunn.test)
tempdata <- readRDS("C:/Users/plehman/OneDrive - California Department of Water Resources/Documents/_NEW PROJECTS/Climate Change PWT/WT analysis/data/temp10yearsCalYr_20211021.rds")
#as.data.frame(tempdata)
names(tempdata)
#head (tempdata)
#  21 degree day heat map

tempdata1 <- filter(tempdata, Year >2009  & Month >5 & Month <11)
head(tempdata1)
#tempdata1
names(tempdata1)
summary(tempdata1)
# 
#collect max and min data for each day for all stations 2010 to 2019 and june to october
#maximum value
file1 <- with(tempdata1, aggregate(Temp, by=list(Region=Region, Station=Station, Year=Year, Month=Month, yDay=yDay), max)) 
#head(file1)
names(file1) 
names(file1)[6] <- "maxwt"
names(file1)
#head(file1)

#minimum value
file2 <- with(tempdata1, aggregate(Temp, by=list(Region=Region, Station=Station, Year=Year, Month=Month, yDay=yDay), min)) 
names(file2) 
names(file2)[6] <- "minwt"
names(file2)
#head(file2)

# merge max and min values into file
file3 <- merge(file1, file2)
names(file3)

#degree day for 21 degreee C
file4 <- with (file3, (((maxwt + minwt)/2)-21))


# merge DD value to file
file5 <- cbind(file3, file4)
names(file5)
names(file5)[8] <- "DD21"
names(file5)
#head(file5)

#change negative values to zero
file5$DD21[file5$DD21 < 0] <- 0
#head(file5)

# sum DD21 OVER MONTH for month by station
file11 <- with(file5, aggregate(DD21, by=list(Region=Region, Station=Station,  Year=Year, Month=Month), sum)) 
names(file11)
names(file11)[5] <- "SDD21mo"
names(file11)
# sum for year of monthly sums  
file12 <- with(file11, aggregate(SDD21mo, by=list(Region=Region,Station=Station,  Year=Year), sum)) 
names(file12)
names(file12)[4] <- "SDD21yr"
names(file12)
#file12
# mean for region of sum of year of monthly sums  
file13 <- with(file12, aggregate(SDD21yr, by=list(Region=Region, Year = Year), mean)) 
names(file13)
summary(file13)
names(file13)[3] <- "Degree_Days"
names(file13)
file13
head(file13)
summary(file13)
# make heat map
file13 <- as.data.frame(file13)
#tiff(filename=file13, units="in", type="cairo", bg="white", height=7, width=7, res=300, pointsize=10, compression="lzw")

#final plot
ggplot(file13, aes(y=Region, x= Year,fill=Degree_Days)) + labs(x = "Calendar Year", fill="Degree  Days") + geom_tile() + scale_x_continuous(breaks=seq(2010,2019,by = 3)) +  scale_y_discrete(labels=c(“Sac River” = “Confluence”, “San Joaquin” = “Central”)) + scale_fill_gradient2(limits=c(0,1000)) + theme(axis.text=element_text(size = 13) , axis.title=element_text(size=14),legend.text=element_text(size=13), legend.title=element_text(size=14))



#Degree day computation with heat graph 19 degree threshold


library(dplyr)
library(base)
library(datasets)
library(ggplot2)
library(dunn.test)
tempdata <- readRDS("C:/Users/plehman/OneDrive - California Department of Water Resources/Documents/_NEW PROJECTS/Climate Change PWT/WT analysis/data/temp10yearsCalYr_20211021.rds")
#as.data.frame(tempdata)
names(tempdata)
#head (tempdata)
#  21 degree day heat map

tempdata1 <- filter(tempdata, Year >2009  & Month >5 & Month <11)
head(tempdata1)
#tempdata1
names(tempdata1)
summary(tempdata1)
# 
#collect max and min data for each day for all stations 2010 to 2019 and june to october
#maximum value
file1 <- with(tempdata1, aggregate(Temp, by=list(Region=Region, Station=Station, Year=Year, Month=Month, yDay=yDay), max)) 
#head(file1)
names(file1) 
names(file1)[6] <- "maxwt"
names(file1)
#head(file1)

#minimum value
file2 <- with(tempdata1, aggregate(Temp, by=list(Region=Region, Station=Station, Year=Year, Month=Month, yDay=yDay), min)) 
names(file2) 
names(file2)[6] <- "minwt"
names(file2)
#head(file2)

# merge max and min values into file
file3 <- merge(file1, file2)
names(file3)

#degree day for 19oC threshold Microcystis
file4 <- with (file3, (((maxwt + minwt)/2)-19))


# merge DD value to file
file5 <- cbind(file3, file4)
names(file5)
names(file5)[8] <- "DD19"
names(file5)
#head(file5)

#change negative values to zero
file5$DD19[file5$DD19 < 0] <- 0
#head(file5)

# sum DD21 OVER MONTH for month by station
file11 <- with(file5, aggregate(DD19, by=list(Region=Region, Station=Station,  Year=Year, Month=Month), sum)) 
names(file11)
names(file11)[5] <- "SDD19mo"
names(file11)
# sum for year of monthly sums  
file12 <- with(file11, aggregate(SDD19mo, by=list(Region=Region,Station=Station,  Year=Year), sum)) 
names(file12)
names(file12)[4] <- "SDD19yr"
names(file12)
#file12
# mean for region of sum of year of monthly sums  
file13 <- with(file12, aggregate(SDD19yr, by=list(Region=Region, Year = Year), mean)) 
names(file13)
summary(file13)
names(file13)[3] <- "Degree_Days"
names(file13)
#file13a <- filter(file13, Region != "Far North" & Year >2010)
file13
head(file13)
summary(file13)
# make heat map
file13 <- as.data.frame(file13)
#tiff(filename=file13, units="in", type="cairo", bg="white", height=7, width=7, res=300, pointsize=10, compression="lzw")
ggplot(file13, aes(y=Region, x= Year,fill=Degree_Days)) + labs(x = "Calendar Year", fill="Degree  Days") + geom_tile() + scale_x_continuous(breaks=seq(2010,2019,by = 3)) +  scale_y_discrete(labels=c("Sac River" = "Confluence", "San Joaquin" = "Central")) +  scale_fill_gradient2(limits=c(0,1000)) + theme(axis.text=element_text(size = 13) , axis.title=element_text(size=14),legend.text=element_text(size=13), legend.title=element_text(size=14))
