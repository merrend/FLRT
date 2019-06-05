library(nlme)
library(dplyr)
library(tidyr)
library(ggplot2)

nips <- read.csv('data/nip_data.csv')


# Fitting GLM w/ poisson distribution. Model output related to nip count by link function.
# Needs to be transformed before interpreting

nipmod <- glm(nips ~ broad_class, data  = nips, family = "poisson") #Predictor is road class
nipnull <- glm(nips ~ 1, data  = nips, family = "poisson") #Modeling the mean for all samples

# 
# summary(nipnull)
# hist(nips$nips, breaks = 1)

#Boxplots of raw count data
ggplot(nips) +
  geom_boxplot(aes(x = broad_class, y = nips)) +
  geom_jitter(aes(x = broad_class, y = nips), width = 0.2, color = "lightblue4") +
  ylab("Nips per 0.1 mile") +
  xlab("Road class") +
  theme_bw() +
  ggtitle("Nip prevalence by road type in Falmouth, MA") +
  annotate("text", label = "p < 0.001", x = 1, y = 57)


#Get inverse of link function to transform model output back to interpretable data
ilink <- family(nipmod)$linkinv


## add fit and se.fit on the link scale
ndata <- nips
ndata <- bind_cols(nips, 
                   setNames(as_tibble(predict(nipmod, ndata, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))

## create the confidence interval for the prediction (fit_resp)
ndata2 <- ndata %>% 
          mutate(fit_resp  = ilink(fit_link),
                right_upr = ilink(fit_link + (2 * se_link)),
                right_lwr = ilink(fit_link - (2 * se_link))
                )
