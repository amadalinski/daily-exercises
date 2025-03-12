# Ava Madalinski, February 25, 2025
# Daily Exercise 9

library(tidyverse)
library(visdat)
vis_dat(airquality)

airquality <- airquality |>
  distinct()

cleaned_data <- airquality |>
  drop_na()

model <- lm(Ozone ~ Temp, data = airquality)
  lm(formula = Ozone ~ Temp, data = airquality)
# I chose temp as the predictor value because I made an estimated guess that it would have a relationship to ozone. In addtion, the temp data did not have any NA values so I would not have to worry about cleaning the data before working with it. .

  summary(model)
  # Residuals:
  #  Min      1Q  Median      3Q     Max 
  #  -40.922 -17.459  -0.874  10.444 118.078 
  
  #  Coefficients:
  #    Estimate Std. Error t value Pr(>|t|)    
  #  (Intercept) -147.6461    18.7553  -7.872 2.76e-12 ***
  #    Temp           2.4391     0.2393  10.192  < 2e-16 ***
  ---
    #   Signif. codes:  
    #    0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    #  Residual standard error: 23.92 on 109 degrees of freedom
    #  Multiple R-squared:  0.488,	Adjusted R-squared:  0.4833 
    #  F-statistic: 103.9 on 1 and 109 DF,  p-value: < 2.2e-16
    
    # The R squared is 0.48, which means that 48% of the variability is explained by the regression model.
    # This does seem like a valid model, as the p-value is small.
    

 broom::augment(model)   

 ggplot(data = airquality,
        aes(x = Temp, y = Ozone)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, color = "red") +
   labs(title = "Actual vs. Predicted Ozone Related to Temperature",
        x = "Temperature",
        subtitle = "Correlation between actual and predicted ozone: 0.7") +
   theme_bw()
   
 
 
 a <- augment(model, newdata = cleaned_data)
 head(data)
 
 
 paste("Correlation:", round(cor(a$Ozone, a$.fitted),2))
 