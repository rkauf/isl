#' # Chapter 4 Lab

#+ setup
knitr::opts_chunk$set(dpi = 200, fig.width = 8, fig.height = 5, message = F, warning = F)

library(MASS)
library(ISLR)
library(tidyverse)
library(tidymodels)
library(expappr)
library(modelr)

options(scipen=999)

#ezspin_pt(file_name = "ch4", project_directory = ".", file_folder = "munge", keep_html = F)

#' ## 4.6.1 Stock Market Data
names(Smarket)
pairs(Smarket)
cor(Smarket %>% select(-Direction))

Smarket %>% 
  ggplot(aes(x = 1, Volume)) +
  geom_jitter()

#' ## 4.6.2 Logistic Regression

smarket_train <- Smarket %>% filter(Year < 2005)
smarket_test <- Smarket %>% anti_join(smarket_train %>% distinct(Year), by = "Year")

glm_fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = Smarket, subset = smarket_train)
summary(glm_fit)

smarket_binom <- smarket_test %>% 
  as.tibble() %>% 
  augment(glm_fit, newdata = ., type.predict = "response") %>% 
  mutate(pred_direction = ifelse(.fitted > .5, "Up", "Down"))

smarket_binom %>%
  count(Direction, pred_direction)

smarket_binom %>% 
  summarise(correct_pred_pct = mean(Direction == pred_direction))

#' Remove some variables
glm_fit2 <- glm(Direction ~ Lag1 + Lag2, data = smarket_train, family = binomial)

smarket_binom2 <- smarket_test %>% 
  as.tibble() %>% 
  augment(glm_fit2, newdata = ., type.predict = "response") %>% 
  mutate(pred_direction = ifelse(.fitted > .5, "Up", "Down"))

smarket_binom2 %>%
  count(Direction, pred_direction)

smarket_binom2 %>% 
  summarise(correct_pred_pct = mean(Direction == pred_direction))
