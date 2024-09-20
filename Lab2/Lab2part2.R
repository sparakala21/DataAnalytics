# read data
epi.results <- read.csv("epi2024results06022024.csv")
populations_2023 <- read.csv("countries_populations_2023.csv")

# drop countries not in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

# sort populations by country
populations <- populations[order(populations$Country),]

# drop countries not in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

# sort epi results by country
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

# only keep necessary columns
epi.results.sub <- epi.results.sub[, c("country", "EPI.old", "EPI.new", "ECO.new", "ECO.old", "BDH.old", "BDH.new")]

# convert population to numeric
epi.results.sub$population <- as.numeric(populations$Population)

# compute population log base 10
epi.results.sub$population_log <- log10(epi.results.sub$population)

# Linear models
lin.mod.epinew <- lm(EPI.new ~ population_log, data = epi.results.sub)
lin.mod.epiold <- lm(EPI.old ~ population_log, data = epi.results.sub)
lin.mod.bdhold <- lm(BDH.old ~ population_log, data = epi.results.sub)
lin.mod.bdhnew <- lm(BDH.new ~ population_log, data = epi.results.sub)


# Basic plot with regression line for EPI.new
plot(epi.results.sub$population_log, epi.results.sub$EPI.new,
     xlab = "Population Log (Base 10)", ylab = "EPI New")
abline(lin.mod.epinew)

# Basic plot with regression line for EPI.old
plot(epi.results.sub$population_log, epi.results.sub$EPI.old,
     xlab = "Population Log (Base 10)", ylab = "EPI Old")
abline(lin.mod.epiold)

# Basic plot with regression line for BDH.old (previously MHP.old)
plot(epi.results.sub$population_log, epi.results.sub$BDH.old,
     xlab = "Population Log (Base 10)", ylab = "BDH Old")
abline(lin.mod.bdhold)

plot(epi.results.sub$population_log, epi.results.sub$BDH.new,
     xlab = "Population Log (Base 10)", ylab = "BDH New")
abline(lin.mod.bdhnew)

# Summaries of linear models
summary(lin.mod.epinew)
# Call:
#   lm(formula = EPI.new ~ population_log, data = epi.results.sub)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -21.017  -8.608  -2.396   6.046  29.789 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      62.340      7.587   8.216 6.17e-14 ***
#   population_log   -2.211      1.087  -2.035   0.0435 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 11.58 on 163 degrees of freedom
# Multiple R-squared:  0.02478,	Adjusted R-squared:  0.01879 
# F-statistic: 4.141 on 1 and 163 DF,  p-value: 0.04348
summary(lin.mod.epiold)
# Call:
#   lm(formula = EPI.old ~ population_log, data = epi.results.sub)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -24.672  -8.432  -2.082   5.436  29.375 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      65.142      7.408   8.794 1.97e-15 ***
#   population_log   -2.950      1.061  -2.780  0.00607 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 11.31 on 163 degrees of freedom
# Multiple R-squared:  0.04528,	Adjusted R-squared:  0.03942 
# F-statistic: 7.731 on 1 and 163 DF,  p-value: 0.006067
summary(lin.mod.bdhold)
# Call:
#   lm(formula = BDH.old ~ population_log, data = epi.results.sub)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -39.154 -10.430   2.298  12.605  40.085 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)      34.832     11.371   3.063  0.00256 **
#   population_log    1.718      1.628   1.055  0.29309   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 17.36 on 163 degrees of freedom
# Multiple R-squared:  0.006779,	Adjusted R-squared:  0.0006859 
# F-statistic: 1.113 on 1 and 163 DF,  p-value: 0.2931
# Diagnostic plots for each linear model
plot(lin.mod.epinew)

plot(lin.mod.epiold)
plot(lin.mod.bdhold)

# ggplot with linear model for EPI.new
library(ggplot2)
ggplot(epi.results.sub, aes(x = population_log, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(title="EPI vs. Population Log", x="Population Log (Base 10)", y="EPI New")

# Extract fitted values and residuals from the EPI.new linear model
model_diagnostics <- data.frame(Fitted = lin.mod.epinew$fitted.values,
                                Residuals = lin.mod.epinew$residuals)

# Residuals vs Fitted Values plot
ggplot(model_diagnostics, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color="red") +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

