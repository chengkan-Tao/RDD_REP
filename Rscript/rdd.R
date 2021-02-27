## abstract
## The author tries to find the effect of punishments and deterrence on driving under
## the influence (DUI) through administrative records on 512,964 DUI BAC tests in the 
## state of Washington from 1995 to 2011 by discontinuity approach. Blood alcohol content 
## and previous offenses in the data set are used to estimate the effect.
## This paper finds having a BAC above either DUI or aggravated DUI threshold reduces repeat drunk
## driving. Having a BAC over the 0.08 DUI threshold is associated with a about 2 percent point 
## decline in recidivism over the next four years. And, having a BAC over the 0.15 DUI will leads to
## about 3 percentage point decline in recidivism.


install.packages("tidyverse")
install.packages("cli")
install.packages("haven")
install.packages("rmarkdown")
install.packages("learnr")
install.packages("haven")
install.packages("stargazer")
install.packages("KernSmooth")
install.packages("estimatr")
install.packages("rdd")
install.packages("rdrobust")
install.packages("rddensity")
install.packages("tidyfst")
install.packages("ggplot2")
install.packages("reshape")
install.packages("texreg")
install.packages("sandwich")

library(learnr)
library(haven)
library(tidyverse)
library(stargazer)
library(estimatr)
library(reshape)
library(rdd)
library(rdrobust)
library(rddensity)
library(tidyfst)
library(ggplot2)
library(texreg)
library(sandwich)
library(KernSmooth)


# read data
hansen_dwi <- read.csv("~/GitHub/CAUSAL INFERENCE CLASS/causal-inference-class/Data/hansen_dwi.csv", header=TRUE)
# Q3
BAC = hansen_dwi %>%
  mutate_vars(.cols = bac1, .func = function(bac1) ifelse(bac1<0.08,0,1))
BAC = data.frame(BAC,hansen_dwi$bac1)
BAC = rename(BAC,c(bac1="DUI"))
BAC = rename(BAC,c(hansen_dwi.bac1="bac1"))

# Q4
rdr = rdrobust(y = hansen_dwi$recidivism,x=hansen_dwi$bac1, c = 0.08)
summary(rdr)

ggplot(data = hansen_dwi,mapping = aes(bac1)) + 
  geom_histogram(binwidth = 0.001, fill = "lightblue", colour = "black") + 
  labs(title = 'BAC Histogram', y = 'Frequency', x = 'bac1')
## To see if there’s any evidence for manipulation, we would use density test. Through the 
## picture, I cannot find a obvious discontinuity.

# Q5
lm_q5_male <- lm_robust(male ~ DUI+bac1+bac1*DUI, data = BAC)
coef(lm_q5_male)

lm_q5_white <- lm_robust(white ~ DUI+bac1+bac1*DUI, data = BAC)
coef(lm_q5_white)

lm_q5_acc <- lm_robust(acc ~ DUI+bac1+bac1*DUI, data = BAC)
coef(lm_q5_acc)

texreg::screenreg(list(lm_q5_male,lm_q5_white,lm_q5_acc),type="text")
## They are stable across the DUI punishment thresholds. The coefficients
## are not statistically significant.


# Q6
Q6 <- BAC %>% 
  mutate(gg_group = case_when(bac1 > 0.08 ~ 1, TRUE ~ 0))

ggplot(Q6, aes(bac1, acc)) +
  geom_point(aes(x = bac1, y = acc), data = Q6) +
  labs(title = 'Panal A', y = 'accident', x = 'bac1') +
  stat_smooth(aes(bac1, acc, group = gg_group), method = "lm") +
  xlim(0,0.5) + ylim(0,1) +
  geom_vline(xintercept = 0.08)

ggplot(Q6, aes(bac1, acc)) +
  geom_point(aes(x = bac1, y = acc), data = Q6) + 
  labs(title = 'Panal A Quadratic', y = 'accident', x = 'bac1')+
  stat_smooth(aes(bac1, acc, group = gg_group), method = "lm", 
              formula = y ~ x + I(x^2)) +
  xlim(0,0.5) + ylim(0,1) +
  geom_vline(xintercept = 0.08)

ggplot(Q6, aes(bac1, male)) +
  geom_point(aes(x = bac1, y = male), data = Q6) +
  labs(title = 'Panal B', y = 'male', x = 'bac1') +
  stat_smooth(aes(bac1, male, group = gg_group), method = "lm") +
  xlim(0,0.5) + ylim(0,1) +
  geom_vline(xintercept = 0.08)

ggplot(Q6, aes(bac1, male)) +
  geom_point(aes(x = bac1, y = male), data = Q6) + 
  labs(title = 'Panal B Quadratic', y = 'male', x = 'bac1')+
  stat_smooth(aes(bac1, male, group = gg_group), method = "lm", 
              formula = y ~ x + I(x^2)) +
  xlim(0,0.5) + ylim(0,1) +
  geom_vline(xintercept = 0.08)

ggplot(Q6, aes(bac1, aged)) +
  geom_point(aes(x = bac1, y = aged), data = Q6) +
  labs(title = 'Panal C', y = 'aged', x = 'bac1') +
  stat_smooth(aes(bac1, aged, group = gg_group), method = "lm") +
  xlim(0,0.5) + ylim(0,80) +
  geom_vline(xintercept = 0.08)

ggplot(Q6, aes(bac1, aged)) +
  geom_point(aes(x = bac1, y = aged), data = Q6) + 
  labs(title = 'Panal C Quadratic', y = 'aged', x = 'bac1')+
  stat_smooth(aes(bac1, aged, group = gg_group), method = "lm", 
              formula = y ~ x + I(x^2)) +
  xlim(0,1) + ylim(0,80) +
  geom_vline(xintercept = 0.08)

ggplot(Q6, aes(bac1, white)) +
  geom_point(aes(x = bac1, y = white), data = Q6) +
  labs(title = 'Panal D', y = 'white', x = 'bac1') +
  stat_smooth(aes(bac1, white, group = gg_group), method = "lm") +
  xlim(0,1) + ylim(0,1) +
  geom_vline(xintercept = 0.08)

ggplot(Q6, aes(bac1, white)) +
  geom_point(aes(x = bac1, y = white), data = Q6) + 
  labs(title = 'Panal D Quadratic', y = 'white', x = 'bac1')+
  stat_smooth(aes(bac1, white, group = gg_group), method = "lm", 
              formula = y ~ x + I(x^2)) +
  xlim(0,1) + ylim(0,1) +
  geom_vline(xintercept = 0.08)

## Through these figures, I find these predetermined characteristics remain unchanged
## across the punishment thresholds. It's same with what hansen has found.













# Q7
## simple linear regression
Q7_wide = BAC %>% 
  filter(bac1 > .03 & bac1 < 0.13) %>%
  mutate(bac1_sq = bac1^2)

lm_q7_wide1 = lm_robust(recidivism ~ bac1, data = Q7_wide)
coef(lm_q7_wide1)

lm_q7_wide2 = lm_robust(recidivism ~ bac1*DUI, data = Q7_wide)
coef(lm_q7_wide2)

lm_q7_wide3 = lm_robust(recidivism ~ bac1*DUI+bac1_sq*DUI, data = Q7_wide)
coef(lm_q7_wide3)

Q7_narrow = BAC %>% 
  filter(bac1 > 0.055 & bac1 < 0.105) %>%
  mutate(bac1_sq = bac1^2)

lm_q7_narrow1 = lm_robust(recidivism ~ bac1, data = Q7_narrow)
coef(lm_q7_narrow1)

lm_q7_narrow2  = lm_robust(recidivism ~ bac1*DUI, data = Q7_narrow)
coef(lm_q7_narrow2)

lm_q7_narrow3  = lm_robust(recidivism ~ bac1*DUI + bac1_sq*DUI, data = Q7_narrow)
coef(lm_q7_narrow3)

texreg::screenreg(list(lm_q7_wide1,lm_q7_wide2,lm_q7_wide3),type="text")
texreg::screenreg(list(lm_q7_narrow1,lm_q7_narrow2,lm_q7_narrow3),type="text")


lm_q7_localnarrow  = lowess(Q7_narrow$recidivism ~ Q7_narrow$bac1)
summary(lm_q7_localnarrow)

rse1<-vcovHAC(lm_q7_wide1)
rse2<-sqrt(diag(rse1))

loc_fit_1 <- rdrobust(Q7_narrow$recidivism , Q7_narrow$bac1, c = 0.08, p = 1,
                      kernel = 'triangular', bw = 0.05)



# Q8
Q8 = BAC %>% 
  filter(bac1 < 0.15) %>%
  mutate(gg_group = case_when(bac1 > 0.08 ~ 1, TRUE ~ 0))

ggplot(Q8, aes(bac1, recidivism)) +
  geom_point(aes(x = bac1, y = recidivism), data = Q8) +
  stat_smooth(aes(bac1, recidivism, group = gg_group), method = "lm") +
  xlim(0,0.15) + ylim(0,1) +
  geom_vline(xintercept = 0.08)

ggplot(Q8, aes(bac1, recidivism)) +
  geom_point(aes(x = bac1, y = recidivism), data = Q8) +
  stat_smooth(aes(bac1, recidivism, group = gg_group), method = "lm",
              formula = y ~ x + I(x^2)) +
  xlim(0,0.15) + ylim(0,1) +
  geom_vline(xintercept = 0.08)

## Q9
## Regression discontinuity design gives causal effect of treatment on the outcome.
## In this exercise, I use BAC=0.08 as a cutoff and use density test to find whether 
## agents can manipulate running variable score. By recreating Table 2 and Figure, 
## I fail to reject the null hypothesis that predetermined characters are unrelated 
## to DUI. That's the same with hansen's paper.
