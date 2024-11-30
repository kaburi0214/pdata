#Clear all variables and load packages
rm(list = ls())
library(MASS)
library(ggplot2)
library(car)
library(dplyr)

#Load the bee data
dat <- read.csv("../../data/Eulaema.csv", fileEncoding = "latin1")

#Data preprocessing: mean centred MAT
dat$mcMAT <- dat$MAT - mean(dat$MAT, na.rm = TRUE)

#Fitted GLM with negative binomial error distribution and check the parameter estimate
pm <- glm.nb(Eulaema_nigrita ~ forest. + lu_het + mcMAT + effort + as.factor(method), data = dat)
summary(pm)

# VIF testing
m_lm <- lm(Eulaema_nigrita ~ forest. + mcMAT + effort, data = dat)  
vif_vals <- vif(m_lm)
print(vif_vals)

#Fitted a similar but better GLM, check the parameter estimate and evaluate this model
m <- glm.nb(Eulaema_nigrita ~ forest. + mcMAT + effort, data = dat)
summary(m)

#pseudo R^2
pr <- 1- m$deviance/m$null.deviance

# Calculate mean and standard errors, predict the expected abundance value with coefs of model
mean_mcMAT <- mean(dat$mcMAT, na.rm = TRUE)
sd_mcMAT <- sd(dat$mcMAT, na.rm = TRUE)

mean_effort <- mean(dat$effort, na.rm = TRUE)
sd_effort <- sd(dat$effort, na.rm = TRUE)


new_data_mcMAT <- expand.grid(
  forest. = seq(min(dat$forest.), max(dat$forest.), length.out = 100),  
  mcMAT = c(mean_mcMAT, 
            mean_mcMAT + sd_mcMAT,  
            mean_mcMAT - sd_mcMAT),  
  effort = mean_effort  
)

new_data_mcMAT$pred <- predict(m, newdata = new_data_mcMAT, type = "response")
coefs <- coef(summary(m))

new_data_mcMAT <- new_data_mcMAT %>%
  mutate(
    condition = case_when(
      mcMAT == mean_mcMAT ~ "Mean MAT and Effort",
      mcMAT == mean_mcMAT + sd_mcMAT ~ "+1 SD MAT",
      mcMAT == mean_mcMAT - sd_mcMAT ~ "-1 SD MAT"
    )
  )


new_data_effort <- expand.grid(
  forest. = seq(min(dat$forest.), max(dat$forest.), length.out = 100),  
  mcMAT = mean_mcMAT,  
  effort = c(
             mean_effort + sd_effort,  
             mean_effort - sd_effort)  
)


new_data_effort$pred <- predict(m, newdata = new_data_effort, type = "response")


new_data_effort <- new_data_effort %>%
  mutate(
    condition = case_when(
      effort == mean_effort + sd_effort ~ "+1 SD Effort",
      effort == mean_effort - sd_effort ~ "-1 SD Effort"
    )
  )

# Data combination
new_data_combined <- bind_rows(new_data_mcMAT, new_data_effort)

# With other variables at mean value, 2 lines give the expected abundance in case there is no or completed forest
h_line1 <- exp(coefs[1, 1] + coefs[4, 1]*mean(dat$effort))  
h_line2 <- exp(coefs[1, 1] + coefs[2, 1] + coefs[4, 1]*mean(dat$effort))  

# Ploting
p_combined <- ggplot() +
 
  geom_point(data = dat, aes(x = forest., y = Eulaema_nigrita, color = method, shape = method), 
             size = 2, alpha = 0.7) +


  geom_line(data = new_data_mcMAT, aes(x = forest., y = pred, color = condition, linetype = condition), size = 1.2) +
  geom_line(data = new_data_effort, aes(x = forest., y = pred, color = condition, linetype = condition), size = 1.2) +
  
  geom_hline(yintercept = h_line1, linetype = "dotted", color = "skyblue", size = 0.8) +
  geom_hline(yintercept = h_line2, linetype = "dotted", color = "skyblue", size = 0.8) +

  scale_color_manual(
    values = c(
      "Mean MAT and Effort" = "black", 
      "+1 SD MAT" = "blue", 
      "-1 SD MAT" = "pink", 
      "+1 SD Effort" = "purple", 
      "-1 SD Effort" = "orange"
    )) +
  scale_shape_manual(
    values = c(
      "NetTraps" = 15, 
      "Traps" = 17, 
      "Net" = 18
    )) +

  scale_linetype_manual(
    values = c(
      "Mean MAT and Effort" = "solid", 
      "+1 SD MAT" = "solid", 
      "-1 SD MAT" = "solid", 
      "+1 SD Effort" = "dashed", 
      "-1 SD Effort" = "dashed"
    )) +
  
  labs(
    x = "Forest Cover",
    y = expression(paste(italic("El. nigrita"), " Abundance")),
    color = "Prediction", 
    shape = "Method",  
    linetype = "Prediction"  
  ) +
  theme_classic() +

  theme(
    legend.position = c(0.95, 0.95),  
    legend.justification = c(1, 1),   
    legend.box = "vertical",          
    legend.text = element_text(size = 8),   
    legend.title = element_text(size = 10)  
  ) +
  
  guides(
    color = guide_legend(order = 1),  
    shape = guide_legend(order = 1),  
    linetype = guide_legend(order = 1)  
  )

print(p_combined)



