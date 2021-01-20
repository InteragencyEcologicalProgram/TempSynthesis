########
#Let's look at some continuous data now

library(dataRetrieval)

USGSsites = readNWISdata(sites = c(11455280, 11337190, 11455508, 11455478,11336790, 11455315, 380631122032201), 
                         service = "iv", parameterCd = "32316",
                         startDate="2017-01-01T00:00Z",endDate="2020-01-01T12:00Z")

names(USGSsites) =  c("agency", "site_no","dateTime", "Chla", "Chla_cd", "Chl_intercal",  
                      "Chl_intercal_cd", "chl_BGC", "Chl_BGC_cd","tz_cd")

test = group_by(USGSsites, site_no) %>%
  summarise(nchla = length(!which(is.na(Chla))), nchlaIC = length(!which(is.na(Chl_intercal))), 
            nchlaBGC = length(!which(is.na(chl_BGC))))

ggplot(USGSsites, aes(x=dateTime, y = Chla)) + geom_point(aes(color = site_no)) + facet_wrap(~site_no)
            
ggplot(USGSsites, aes(x=dateTime, y = Chl_intercal)) + geom_point(aes(color = site_no))+ facet_wrap(~site_no)

ggplot(USGSsites, aes(x=dateTime, y = chl_BGC)) + geom_point(aes(color = site_no))+ facet_wrap(~site_no)

#I think the intercalibration study clorophyl values aren't worth keeping
USGSsites$Chl_intercal = NULL
USGSsites$Chl_intercal_cd = NULL

USGSsites$CHL = rowSums(USGSsites[,c(4,6)], na.rm = T)
USGSsites2 = filter(USGSsites, CHL >0) %>%
  mutate(Date = date(dateTime))

ggplot(USGSsites2, aes(x=dateTime, y = CHL)) + geom_point(aes(color = site_no))+ facet_wrap(~site_no)

USGSsites3 = group_by(USGSsites2, Date, site_no) %>%
  summarize(chla = mean(CHL, na.rm = T)) %>%
  mutate(logchla = log(chla), Month = month(Date)) %>%
  filter(Month %in% c(7,8,9, 10, 11)) %>%
  rename(date = Date) %>%
  filter(site_no %in% c(11337190, 11455508, 380631122032201))

smoke2sum = rename(smoke2sum, date = Time)
USGSsmoke = merge(USGSsites3, smoke2sum) 

ggplot(USGSsmoke, aes(x = AOD, y = chla)) + geom_point() +
  facet_grid(.~site_no)

fires_TS = read.csv("fire_TS_8K.csv")
fires_TS = mutate(fires_TS, date = ymd(date), X = NULL)
save(EMPc, USGSsites2, USGSsmoke, fires_TS, smoke2, smoke2sum, smokeEMP, file = "smoke.RData")
