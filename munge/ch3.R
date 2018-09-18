#' # Chapter 3 Lab

#+ setup
knitr::opts_chunk$set(dpi = 200, fig.width = 8, fig.height = 5, message = F, warning = F)

library(MASS)
library(ISLR)
library(tidyverse)
library(tidymodels)
library(expappr)
library(ggfortify)
library(gridExtra)
library(car)

options(scipen=999)

#ezspin_pt(file_name = "ch3", project_directory = ".", file_folder = "munge", keep_html = F)

head(Boston)

lm_fit <- lm(medv ~ lstat, data = Boston)

Boston %>% lm(medv ~ lstat, .) %>% glance()

## base R model metrics
summary(lm_fit)
names(lm_fit)

## broom methods
tidy(lm_fit)
glance(lm_fit)

## base (can transform to a datafram)
confint(lm_fit) 

## tidy
confint_tidy(lm_fit)

Boston %>% 
  lm(medv ~ lstat, .) %>% 
  augment()

ggplot(Boston, aes(x=lstat, y=medv)) +
  geom_smooth(method = "lm") +
  geom_point()


#' ## 3.6.3 Multiple Linear Regression

ml_fit <- Boston %>% 
  lm(medv ~ ., .)

ml_fit %>% summary()

vif(ml_fit)

#' ## 3.6.4 Interaction Terms

## this includes lstat, age, and the interaction between the 2 variable
summary(lm(medv~lstat*age, data=Boston))
