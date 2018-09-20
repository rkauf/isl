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
library(modelr)

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

## this includes lstat, age, and the interaction between the 2 variables
summary(lm(medv~lstat*age, data=Boston))

#' ## 3.6.5 Non-linear transformations of the predictors

ml_quad <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(ml_quad)


anova(lm_fit, ml_quad)
#+ autoplot, fig.height = 10, fig.width = 12
autoplot(ml_quad)

## 5th polynomial
ml_poly5 <- lm(medv ~ poly(lstat, 5), data = Boston)
summary(ml_poly5)


## log
summary(lm(medv ~ log(rm), data = Boston))

#' # Excercies
#' #### 8
cars_lm <- lm(mpg ~ horsepower , data = Auto)
summary(cars_lm)

#' There is a strong negative relationship between the predictor and the response

predict(cars_lm, tibble(horsepower = 98))
predict(cars_lm, tibble(horsepower = 98), interval = "confidence")
predict(cars_lm, tibble(horsepower = 98), interval = "prediction")

tibble(horsepower = 98) %>% 
  augment(cars_lm, data = .)

Auto %>% 
  augment(cars_lm, .) %>% 
  ggplot(aes(horsepower, mpg)) +
  geom_point() +
  geom_line(aes(x = horsepower, y = .fitted), color = "red")
  
autoplot(cars_lm)

#' Seems like the relationship is non-linear, residuals vs fitted shows a U-shaped pattern 
#' 
#' #### 9
#+ cor_plot, fig.height = 12, fig.width = 12
pairs(Auto)

cor(Auto %>%select(-name)) %>% 
  as.tibble()

lm_all <- lm(mpg ~ .-name, data = Auto)
summary(lm_all)

#' Displacement, weight, year and origin are all significant predictors

autoplot(lm_all)

#' Residuals are a bit fan-shaped and u-shaped, so there's some evidence of a non-linear relationship, but the linear relationship seems to fit fairly well.

## All interactions
summary(lm(mpg ~ (.-name)^2, data = Auto))
