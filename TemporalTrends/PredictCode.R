summary(M.max1a <- lmer(maxAvTemp~Region + Month + I(Month^2) + fWY + fWY:(Month + I(Month^2)) + (1|Station), data = tempMonM, na.action = na.omit))

sjPlot::plot_model(M.max1a,
                   show.values = TRUE, show.p = TRUE,
                   title = "Max Average Temperature")

sjPlot::tab_model(M.max1a)

effects_region <- effects::effect(term="Region", mod = M.max1a)
x_region <- as.data.frame(effects_region)

region_plot <- ggplot() + 
  geom_point(data = subset(tempMonM, fMonth == "7"), aes(x = Region, y = maxAvTemp), color = "black") +
  geom_point(data = x_region, aes(x = Region, y = fit), color = "red") +
  geom_line(data = x_region, aes(x = Region, y = fit), color = "blue") + 
  geom_ribbon(data = x_region, aes(x = Region, ymin = lower, ymax = upper), 
              alpha = 0.3, fill = "blue") 

region_plot

visreg(M.max1a, "Region", by = "fWY")
