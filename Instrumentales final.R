# Álvaro Amorós y Javier Carrero
# 05/11/22
# Asigment one. Instrumental variables


library(dplyr)
library(tidyverse)
library(lmtest)
library(AER)
library(Ecdat)
library(plm)
data("Schooling")

Schooling <- Schooling %>%
  mutate(wage = lwage76,
         edu = ed76,
         father_edu = daded,
         age = age76,
         iq = iqscore,
         experience = exp76)



# We want to measure the effect of education on wage. A classical endogeneity problem with this kind of measurement
# is related with the unmeasured ability of individuals, as those with high ability will perform better at school,
# as well as at work, leading to upward biased results wen measuring the effect of education on wage.
# The instrumental variable we will use to solve this endogeneity problem will be parental education, 
# as it correlates with childless ability, and it does not have any effect on the individuals wage if it is not 
# trough childless ability.

# The simplest model, withount instrumental vriable, yields this results

reg <- lm(wage ~ edu, data = Schooling)
summary(reg)

# Our variables will be
# y = log wage in 1976 (outliers trimmed) - lwage76
# x = education in 1976 (messured in years) - ed76
# z = dads education (imputed avg if missing) - daded

# For this model to be valid we need 2 asumptions
  # 1- X and Z are correlated 
  # 2- Z can not be correlated with the errror term

# We will start by manually computing the Two-Stage Least Squares Estimator with one regressor and one instrument

# We  start by separating the variation of our endogenous regressor X in two components
  # 1- A problem free component that is explained by our instrument Z
  # 2 - A problematic component which is correlated with our error.

# Fist stage regression (problem free component of X)
fs_reg <- lm(edu ~ father_edu, data = Schooling)
summary(fs_reg)

  # We store the fitted values of the model to use them in our second regression
  edu_hat <- fs_reg$fitted.values
  edu_hat
# Second Stage regression using edu_hat
ss_reg <- lm(wage ~ edu_hat, data = Schooling)
  
summary(ss_reg)

# Caluclating  our results manually, like we have done, we obtain correct coeffiecients for edu,
# but wrong standard erros because we are cot acounting for un certainty in the frist stage. For that we will use
# the R function ivreg

ivreg_reg_1 <- ivreg(wage ~ edu | father_edu, data = Schooling)
summary(ivreg_reg_1)


# Now we will add more variables to our model and using the  General IV Regression  Model
# WE will add one exogeneous variable, as well as a asecond instrument Z for our edogenous regressor
ivreg_reg_2 <- ivreg(wage ~ edu  | father_edu + nearc4, data = Schooling)
summary(ivreg_reg_2)


summary(ivreg_reg_2, diagnostics = TRUE)



#  test
Schooling <- na.omit(Schooling)

reg_2 <- ivreg(wage ~ edu | father_edu + nearc4, data = Schooling)
Schooling$u.hat = reg_2$residuals

mylm <- lm(u.hat ~ father_edu + nearc4, data = Schooling)

nR2 = nrow(Schooling) * summary(mylm)$r.squared

pchisq(nR2,1)



  