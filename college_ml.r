## Run this code but do not edit it. Hit Ctrl+Enter to run the code.
# This command downloads a useful package of R commands
library(coursekata)

## Run this code but do not edit it. Hit Ctrl+Enter to run the code.
# This command downloads data from the file 'colleges.csv' and stores it in an object called `dat`
dat <- read.csv('https://skewthescript.org/s/four_year_colleges.csv')

## Run this code but do not edit it
# create a data set to train the model with 20 randomnly selected observations
set.seed(2)
sample_dat <- sample(dat, size = 20)

## Your code here
head(sample_dat)

## Your code here
dim(sample_dat)

## Run this code but do not edit it
# create scatterplot: default_rate ~ SAT_avg
gf_point(default_rate ~ SAT_avg, data = sample_dat)

## Your code here
gf_point(default_rate~SAT_avg, data=sample_dat) %>% gf_lm(color="red")

## Your code here
sat_model_1 <- lm(default_rate~SAT_avg, data=dat)
summary(sat_model_1)

## Run this code but do not edit it
# create scatterplot: default_rate ~ SAT_avg, with degree 2 polynomial model overlayed
gf_point(default_rate ~ SAT_avg, data = sample_dat) %>% gf_lm(formula = y ~poly(x, 2), color = "orange")

## Run this code but do not edit it
# degree 2 polynomial model for default_rate ~ SAT_avg
sat_model_2 <- lm(default_rate ~ poly(SAT_avg, 2), data = sample_dat)
sat_model_2

## Your code here
summary(sat_model_2)

## Run this code but do not edit it
# create scatterplot: default_rate ~ SAT_avg, with degree 3 polynomial model overlayed
gf_point(default_rate ~ SAT_avg, data = sample_dat) %>% gf_lm(formula = y ~poly(x, 3), color = "orange") + ylim(-4,12)

## Run this code but do not edit it
# create scatterplot: default_rate ~ SAT_avg, with degree 5 polynomial model overlayed
gf_point(default_rate ~ SAT_avg, data = sample_dat) %>% gf_lm(formula = y ~poly(x, 5), color = "orange") + ylim(-4,12)

## Run this code but do not edit it
# create scatterplot: default_rate ~ SAT_avg, with degree 12 polynomial model overlayed
# gf_point(default_rate ~ SAT_avg, data = sample_dat) %>% gf_smooth(method = "lm", formula = y ~poly(x,12), color = "orange") + ylim(-4,14)

## Run this code but do not edit it
# degree 3, 5, and 12 polynomial models for default_rate ~ SAT_avg
sat_model_3 <- lm(default_rate ~ poly(SAT_avg, 3), data = sample_dat)
sat_model_5 <- lm(default_rate ~ poly(SAT_avg, 5), data = sample_dat)
sat_model_12 <- lm(default_rate ~ poly(SAT_avg, 12), data = sample_dat)

## Run this code but do not edit it
# r-squared value for each model
r2_sat_model_1 <- summary(sat_model_1)$r.squared
r2_sat_model_2 <- summary(sat_model_2)$r.squared
r2_sat_model_3 <- summary(sat_model_3)$r.squared
r2_sat_model_5 <- summary(sat_model_5)$r.squared
r2_sat_model_12 <- summary(sat_model_12)$r.squared

# print each model's r-squared value
print(paste("The R squared value for the degree 1 model is", r2_sat_model_1))
print(paste("The R squared value for the degree 2 model is", r2_sat_model_2))
print(paste("The R squared value for the degree 3 model is", r2_sat_model_3))
print(paste("The R squared value for the degree 5 model is", r2_sat_model_5))
print(paste("The R squared value for the degree 12 model is", r2_sat_model_12))

## Run this code but do not edit it
# create a data set to test the model with 10 new, randomnly selected observations
# not used to train the model
set.seed(23)
test_dat <- sample(dat, size = 10)

## Your code here
head(test_dat)

## Run this code but do not edit it
# label train and test sets
sample_dat$phase <- "train"
test_dat$phase <- "test"

# concatenate two datasets
full_dat <- rbind(sample_dat, test_dat)

# create scatterplot: default_rate ~ SAT_avg, with degree 5 polynomial model overlayed
gf_point(default_rate ~ SAT_avg, data = full_dat, color = ~phase, shape = ~phase)

## Run this code but do not edit it
# get predictions for degree 5 mdoel
pred_deg5 = predict(sat_model_5, newdata = data.frame(SAT_avg = test_dat$SAT_avg))
pred_deg5

### Run this code but do not edit it
## create scatterplot: default_rate ~ SAT_avg, with degree 5 polynomial model overlayed
#gf_point(default_rate ~ SAT_avg, data = sample_dat, color = ~phase, shape = ~phase) %>% gf_lm(formula = y ~poly(x, 5), color = "orange") %>% gf_point(default_rate ~ SAT_avg, data = full_dat, color = ~phase, shape = ~phase) + ylim(0,19)

### Run this code but do not edit it
## create scatterplot: default_rate ~ SAT_avg, with degree 5 polynomial model overlayed
#gf_point(default_rate ~ SAT_avg, data = sample_dat, color = ~phase, shape = ~phase) %>% gf_lm(formula = y ~poly(x, 5), color = "orange") %>% gf_point(default_rate ~ SAT_avg, data = full_dat, color = ~phase, shape = ~phase) + ylim(0,19)

## Run this code but do not edit it
# Get correlation between predicted and actual default rates in test set
cor(test_dat$default_rate, pred_deg5) ^ 2

## Run this code but do not edit it
# Storing test set predictions for all models
pred_deg1 = predict(sat_model_1, newdata = data.frame(SAT_avg = test_dat$SAT_avg))
pred_deg2 = predict(sat_model_2, newdata = data.frame(SAT_avg = test_dat$SAT_avg))
pred_deg3 = predict(sat_model_3, newdata = data.frame(SAT_avg = test_dat$SAT_avg))
pred_deg5 = predict(sat_model_5, newdata = data.frame(SAT_avg = test_dat$SAT_avg))
pred_deg12 = predict(sat_model_12, newdata = data.frame(SAT_avg = test_dat$SAT_avg))

# print each model's r-squared value
print(paste("The test R squared value for the degree 1 model is", cor(test_dat$default_rate, pred_deg1) ^ 2))
print(paste("The test R squared value for the degree 2 model is", cor(test_dat$default_rate, pred_deg2) ^ 2))
print(paste("The test R squared value for the degree 3 model is", cor(test_dat$default_rate, pred_deg3) ^ 2))
print(paste("The test R squared value for the degree 5 model is", cor(test_dat$default_rate, pred_deg5) ^ 2))
print(paste("The test R squared value for the degree 12 model is", cor(test_dat$default_rate, pred_deg12) ^ 2))

## Run but do not edit this code

# set training data to be 80% of all colleges
train_size <- floor(0.8 * nrow(dat))

## sample row indeces
set.seed(123)
train_ind <- sample(seq_len(nrow(dat)), size = train_size)

train <- dat[train_ind, ]
test <- dat[-train_ind, ]

dim(train)

dim(test)

sat_model <- lm(default_rate~poly(SAT_avg, 2), data=train)
gf_point(default_rate~SAT_avg, data=train) %>% gf_lm(formula=y~poly(x,2), color="red")
summary(sat_model)

print(cor(test$default_rate, predict(sat_model, newdata=test)^2))

grad_model <- lm(default_rate~poly(grad_rate,2), data=train)
gf_point(default_rate~grad_rate, data=train) %>% gf_lm(formula=y~poly(x,2), color="red")
summary(grad_model)

print(cor(test$default_rate, predict(grad_model, newdata=test))^2)

alum_earnings_model <- lm(default_rate~poly(med_alum_earnings,3), data=train)
gf_point(default_rate~med_alum_earnings, data=train) %>% gf_lm(formula=y~poly(x,3), color="red")
summary(alum_earnings_model)
print(cor(test$default_rate, predict(alum_earnings_model, newdata=test))^2)

test_model <- lm(default_rate~poly(pct_PELL,2), data=train)
gf_point(default_rate~pct_PELL, data=train) %>% gf_lm(formula=y~poly(x,2), color="red")
summary(test_model)
print(cor(test$default_rate, predict(test_model, newdata=test))^2)

test_model <- lm(default_rate~poly(avg_faculty_salary,3), data=train)
gf_point(default_rate~avg_faculty_salary, data=train) %>% gf_lm(formula=y~poly(x,3), color="red")
summary(test_model)
print(cor(test$default_rate, predict(test_model, newdata=test))^2)

my_model <- lm(default_rate~poly(SAT_avg,2)+grad_rate+pct_PELL+poly(avg_faculty_salary,3)+poly(med_alum_earnings,3)+highest_degree+region+hbcu, data=train)
summary(my_model)

# run this code to get the R^2 value on the test set from your model
test_predictions = predict(my_model, newdata = test)
print(paste("The test R^2 value was: ", cor(test$default_rate, test_predictions) ^ 2))
