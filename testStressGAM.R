glimpse(newdatag25abin)
predictions <- newdatag25abin$Prediction

range(highStress21$nDays) #0-185
range(newdatag21abin$Prediction*365) #44-130
range(noRecov21$nDays) #0-154
range(newdatag21NRabin$Prediction*365) #11-108

range(highStress25$nDays) #0-110
range(newdatag25abin$Prediction*365)
range(noRecov25$nDays) #0-76
range(newdatag25NRabin$Prediction*365)

summary(newdata_25a)


modelg25abin_predictions<-predict(g25abinre, newdata=newdata_25a, type="response", se.fit=TRUE, discrete=T) # Create predictions
# Edit data frame
newdatag25abin<-newdata_25a%>%
  mutate(Prediction=modelg25abin_predictions$fit)%>%
  mutate(SE=modelg25abin_predictions$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96)

head(newdatag25abin)
check <- dplyr::filter(newdatag25abin, Prediction > 0.3)

modelg25NRabin_predictions<-predict(g25NRabinQ, newdata=newdata_25NRa, type="response", se.fit=TRUE, discrete=T) # Create predictions
# Edit data frame
newdatag25NRabin<-newdata_25NRa%>%
  mutate(Prediction=modelg25NRabin_predictions$fit)%>%
  mutate(SE=modelg25NRabin_predictions$se.fit,
         L95=Prediction-SE*1.96,
         U95=Prediction+SE*1.96)


latlon25 <- left_join(highStress25, stas)
latlonA <- latlon25 %>% 
  filter(Anomaly >-0.5 & Anomaly <0.5)
latlonB <- latlon25%>%
  filter(Anomaly>0.5 & Anomaly<1.5)

ggplot() +
  geom_sf(data = latlonA, aes(geometry = geometry, color = nDays)) +
  viridis::scale_colour_viridis() + theme_bw()

ggplot() +
  geom_sf(data = latlonB, aes(geometry = geometry, color = nDays)) +
  viridis::scale_colour_viridis() + theme_bw()


 
ggplot(latlon25) + geom_point(aes(x = Latitude, y = Longitude, fill = nDays))