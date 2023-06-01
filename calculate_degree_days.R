library(dplyr)
library(base)
library(datasets)
library(ggplot2)
library(dunn.test)
library(lubridate)
library(FSA)

# tempdata <- readRDS("C:/Users/plehman/OneDrive - California Department of Water Resources/Documents/_NEW PROJECTS/Climate Change PWT/WT analysis/data/temp10yearsCalYr_20211021.rds")
# #as.data.frame(tempdata)
tempdata <- readRDS(here::here("Data/temp10yearsCalYr_20230601.rds")) 
names(tempdata)
#head (tempdata)
#  21 degree day heat map

tempdata1 <- tempdata %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  filter(Year >2009  & Month >5 & Month <11)
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

#degree day for 21 degreee C --------------------------------------------------
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

file21 <- file13 %>%
  mutate(Region = factor(Region, 
                         levels = c("Suisun Marsh", "Suisun Bay","South", "Central","Confluence","North Delta")))

#final plot
(plot_21 <- ggplot(file21, aes(y=Region, x= Year,fill=Degree_Days)) + 
  labs(x = "Calendar Year", fill="Degree  Days", title = "B") + 
  geom_tile(color = "gray40") + 
  scale_x_continuous(breaks=seq(2010,2019,by = 1), expand = c(0,0)) +  
  scale_y_discrete(expand = c(0,0)) +
  # scale_y_discrete(labels=c(“Sac River” = “Confluence”, “San Joaquin” = “Central”)) + 
  scale_fill_gradient2(limits=c(0,1000)) + 
  theme_bw()+
  theme(axis.text=element_text(size = 13) , 
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title=element_text(size=14),
        legend.text=element_text(size=13), 
        legend.title=element_text(size=14)))
  # theme(legend.key.width=unit(1,"cm")))



#degree day for 19oC threshold Microcystis --------------------------
file4b <- with (file3, (((maxwt + minwt)/2)-19))


# merge DD value to file
file5b <- cbind(file3, file4b)
names(file5b)
names(file5b)[8] <- "DD19"
names(file5b)
#head(file5)

#change negative values to zero
file5b$DD19[file5b$DD19 < 0] <- 0
#head(file5)

# sum DD21 OVER MONTH for month by station
file11b <- with(file5b, aggregate(DD19, by=list(Region=Region, Station=Station,  Year=Year, Month=Month), sum)) 
names(file11b)
names(file11b)[5] <- "SDD19mo"
# sum for year of monthly sums  
file12b <- with(file11b, aggregate(SDD19mo, by=list(Region=Region,Station=Station,  Year=Year), sum)) 
names(file12b)
names(file12b)[4] <- "SDD19yr"
names(file12b)
#file12
# mean for region of sum of year of monthly sums  
file19 <- with(file12b, aggregate(SDD19yr, by=list(Region=Region, Year = Year), mean))  
names(file19)
summary(file19)
names(file19)[3] <- "Degree_Days"
names(file19)
#file13a <- filter(file13, Region != "Far North" & Year >2010)
file19
head(file19)
summary(file19)
# make heat map
file19 <- as.data.frame(file19) %>%
  mutate(Region = factor(Region, 
                         levels = c("Suisun Marsh", "Suisun Bay","South", "Central","Confluence","North Delta")))

#tiff(filename=file13, units="in", type="cairo", bg="white", height=7, width=7, res=300, pointsize=10, compression="lzw")
(plot_19 <- ggplot(file19, aes(y=Region, x= Year,fill=Degree_Days)) + 
  labs(x = "Calendar Year", fill="Degree  Days", title = "A") + geom_tile(color = "gray40") + 
  scale_x_continuous(breaks=seq(2010,2019,by = 1), expand = c(0,0)) +  
  scale_y_discrete(expand = c(0,0)) +
  # scale_y_discrete(labels=c(“Sac River” = “Confluence”, “San Joaquin” = “Central”)) + 
  scale_fill_gradient2(limits=c(0,1000)) + 
  theme_bw()+
  theme(axis.text=element_text(size = 13) , 
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank(),
        axis.title=element_text(size=14),
        legend.text=element_text(size=13), 
        legend.title=element_text(size=14)) )
  # theme(legend.key.width=unit(1,"cm")))


# plot combine -------------------------------------
library(patchwork)

(dd_plots <-( plot_19 + plot_21 )+ plot_layout(guides = "collect"))

png(filename=file.path("Figures/DegreeDays.png"), units="in",type="cairo", bg="white", height=5, 
    width=10, res=300, pointsize=10)
dd_plots
dev.off()

# summary stats -------------------------------------------------
mean(file19$Degree_Days)
sd(file19$Degree_Days)
mean(file21$Degree_Days)
sd(file21$Degree_Days)

# kruskal and dunn test ----------------------------------------------
kruskal.test(Degree_Days ~ Region, data = file19)
kruskal.test(Degree_Days ~ factor(Year), data = file19)
FSA::dunnTest(Degree_Days~Region, data = file19, method = "bonferroni")
# Central higher than Suisun Bay/Marsh, South higher than Suisun Bay, 

kruskal.test(Degree_Days ~ Region, data = file21)
kruskal.test(Degree_Days ~ factor(Year), data = file21)
FSA::dunnTest(Degree_Days~Region, data = file21, method = "bonferroni")
#Central>Confluence, SB,SM, ND / South > North Delta, SB, 


