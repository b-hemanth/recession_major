library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)
library(ggthemes)
library(plm)
library(broom)

x <- read_csv("data.csv") %>% 
  clean_names()

x <- x %>% 
  select(duration_in_seconds, response_id, q1:q20) %>% 
  filter(duration_in_seconds != '{"ImportId":"duration"}')

# Making new variables
x <- x %>% 
  mutate(
    poc = ifelse(q1=="White", 0, 1),
    class = q4,
    gm = ifelse(q5 == "Female" | q5 == "Other", 1, 0),
    intnl = ifelse(q8=="Yes", 1, 0),
    legacy = ifelse(q7=="No one in my family has attended Harvard College or been on the faculty of Harvard",0,1))

x <- x %>% 
  select(-q1, -q4, -q1_10_text, -q4_5_text, -q5, -q8, -q7)

x[x$q6 == "Below $40k" & !is.na(x$q6), "q6"] <- 20
x[x$q6 == "Between $125k and $250k" & !is.na(x$q6), "q6"] <- 125
x[x$q6 == "Between $250k and $500k" & !is.na(x$q6), "q6"] <- 250
x[x$q6 == "Between $40k and $80k" & !is.na(x$q6), "q6"] <- 40
x[x$q6 == "Between $80k and $125k" & !is.na(x$q6), "q6"] <- 80
x[x$q6 == "Over $500k" & !is.na(x$q6), "q6"] <- 500


x <- x %>% 
  mutate(exp_inc = q6) %>% 
  select(-q6)

x <- x %>% 
  mutate(stem = ifelse(q30=="Applied Mathematics" | q30=="Computer Science" | q30=="Economics" | q30=="Statistics", 1, 0)) %>% 
  select(-q30)

x <- x %>% 
  select(q39:stem)

x <- x %>% 
  mutate(money_imp = substr(x$q17, start = 1, stop = 1),
         econ_strength = substr(x$q40, start = 1, stop = 1),
         change_major = ifelse(q12=="No",0,1))

x <- x %>% 
  select(-q17, -q40, -q12)

x <- x %>% 
  filter(exp_inc!="What is your approximate family income?")

x <- x %>% 
  select(poc:change_major)

# Sample distribution
y <- x %>% 
  group_by(poc) %>% 
  count()

z <- x %>% 
  group_by(gm) %>% 
  count()

class <- x %>% 
  group_by(class) %>% 
  count()

# Summary stats
m <- x %>% 
  group_by(money_imp) %>% 
  count()

e <- x %>% 
  group_by(econ_strength) %>% 
  count()

c <- x %>% 
  group_by(change_major) %>% 
  summarise(n = n())

# Boxplots
c %>% ggplot(aes(change_major, n)) +
  geom_bar(stat="identity") +
  theme_economist() +
  xlab("Change Major? 0 = No, 1 = Yes") +
  ylab("Frequency")

# Haussman Test
data(x, package = "plm")
form <- change_major ~ econ_strength + money_imp
wi <- plm(form, data = x, model = "within")
re <- plm(form, data = x, model = "random")
phtest(wi, re)

# Linear Regression
reg1 <- x %>% 
  filter(!is.na(econ_strength) & !is.na(change_major))

fit <- lm(change_major ~ econ_strength, data=reg1)
summary(fit)

fit2 <- lm(change_major ~ money_imp, data=reg1)
summary(fit2)

fit3 <- lm(change_major ~ poc + gm + intnl + legacy + exp_inc, data=reg1)
summary(fit3)

anova(fit, fit2)

ggplot(x, aes(legacy, change_major)) +
  geom_smooth(method='lm', formula=y~x) +
  theme_economist() +
  xlab("Legacy status: 0=no, 1=yes") +
  ylab("Change major if there was a recession")

ggplot(x, aes(poc, change_major)) +
  geom_smooth(method='lm', formula=y~x) +
  theme_economist() +
  xlab("Person of Color Status: 0=no, 1=yes") +
  ylab("Change major if there was a recession")

# Confidence intervals
x %>% 
  filter(!is.na(change_major)) %>% 
  summarise(mean = mean(change_major), sd = sd(change_major))

me <- 1.644*(0.335/sqrt(10))
me

ci_u <- 0.125 + me
ci_l <- 0.125 - me

# Central tendency