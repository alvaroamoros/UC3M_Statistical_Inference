# Propensity Score Matching
# We weill rely on the same data we used in our first assigment on instrumental variables.
library(MatchIt)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(Ecdat)
data("Schooling")

# In this assigment we will analyce the effects of starting to read book in a early age on individuals achivement in the labour market, as 
# we assume that early readers are different from non readers. To examining the effect of reading as a child "Treat" versus not reading
# "Control"


# We will use the loged wage in 1976 (lwage76) as our outcome variable of interest, the idependent variable of intereset,
# will be if the respondent had a library card at home at age 14 (libcrd14)

Schooling <- Schooling %>%
  filter(!is.na(libcrd14)) %>%
  mutate(race = ifelse(black == "yes", 1, 0),
         card = ifelse(libcrd14 == "yes", 1, 0),
         college = ifelse(nearc4 == "yes", 1, 0),
         city = ifelse(smsa66 == "yes", 1, 0))

# First we will test if the differences in mean between both groups is statistically significant.

Schooling %>%
  group_by(card) %>%
  summarise(n_respondents = n(),
            mean_log_wage = mean(lwage76),
            std_error = sd(lwage76) / sqrt(n_respondents))

with(Schooling, t.test(lwage76 ~ card))

# We will continue by adding a set of covariates to our model
  # dads education (daded)
  # IQ score (nearc4)
  # race, with 1 == beeing black, and 0 == being white
  # Living near a college (nearc4)
  #Living in a metropositan area (city)

# calculate the mean for each covariate by treatment status ant test if the difference in means is statistically significant
cov <- c("daded", "iqscore", "race", "college", "city")

Schooling %>%
  group_by(card) %>%
  select(one_of(cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

lapply(cov, function(v) {
  t.test(Schooling[, v] ~ Schooling[,'card'])
})



# Propensity score estimation  
# We will run a logt model where the otcome variable is a binary variable indicating the treatment status
ps <- glm(card ~ daded + iqscore + race + college+ city,
          family = binomial(), data = Schooling)

summary(ps)

# Now we can obtain the propensity score of each respondent, this is, the students predicted probability of being "treated", given the estimates
# of our logit model

prs_df <- data.frame(pr_score = predict(ps, type = "response"),
                     card = ps$model$card)
head(prs_df)

# WE can use histograms to of the estimate propensity scores by treatment groups
labs <-  c("Had library card at home: YES", "Had library card at home: No")

prs_df %>%
  mutate(library = ifelse(card == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~ library) +
  xlab("Probability of having a librery card")


# Now we can execute the matching algorithm algorithm to grupe paris of observations with simimillar propensity scores but different treatments

# First we aliminate all missing values
Schooling_nomiss <-Schooling %>%
  select(card, lwage76, one_of(cov)) %>%
  na.omit()

# Now we do the proper matching 
mod_match <- matchit(card ~ daded + iqscore + race + college + city,
                 method = "nearest", data = Schooling_nomiss)

dta_m <- match.data(mod_match)
dim(dta_m)

# 4 Examining covariate balance in the matched sample
# 4.1 Visual inspection  If matching is done well, the treatment and control groups will have (near) identical means of each covariate at each value of the propensity score.


# 4.2 Difference-in-means
# The means below indicate that we have attained a high degree of balance on the five covariates included in the model.
dta_m %>%
  group_by(card) %>%
  select(one_of(cov)) %>%
  summarise_all(funs(mean))
dta_m
