#' # Chapter 3 Lab

#+ setup
knitr::opts_chunk$set(dpi = 200, fig.width = 8, fig.height = 5, message = F, warning = F)

library(MASS)
library(ISLR)
library(tidyverse)
library(tidymodels)
library(expappr)

options(scipen=999)

#ezspin_pt(file_name = "ch3", project_directory = ".", file_folder = "munge", keep_html = F)

head(Boston)

lm_fit <- lm(medv ~ lstat, data = Boston)

## base R model metrics
summary(lm_fit)

## broom methods
tidy(lm_fit)
glance(lm_fit)

## base (can transform to a datafram)
confint(lm_fit) 

## tidy
confint_tidy(lm_fit)
