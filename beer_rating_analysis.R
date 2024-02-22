library(reshape)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(caret)
library(aod)
library(cvms)
library(pROC)

beer <- read.csv('beer_reviews.csv')

#Data Exploration
sapply(beer, function(x) sum(is.na(x)))

#The column that gives us the alcohol by volume for the beer has 67k null values.
#I will make use of the subset of the dataset
#which does not have missing values for this column.
#I am doing this since the number of rows we have in the dataset
#is comparatively large. 
#The missing values contribute to only 4% of the entire dataset.

beer <- na.omit(beer)

sapply(beer, function(x) sum(is.na(x)))

nrow(beer)

colnames(beer)

#The following columns are not important for our research scenario:
#a)review_time
#b)review_profilename
#We can just get rid of these column.
beer <- beer %>%
  subset(select = -c(3, 7))

##Data cleanup
#Since the ratings for each individual aspect of the beer can range from
#only 0 to 5, the presence of an outlier in these indicates that there
#are less number of beers with that particular rating. 
#Hence, we will not be removing these outliers.

#Let us check for outliers in the alcohol by volume column
boxplot(beer$beer_abv, 
        main = 'Alcohol by Volume in the beers being reviewed',
        col = 'lightcyan4',
        border = 'grey',
        horizontal = TRUE)
#Since there seems to be a lot of outliers,
#let us find out the outliers and treat them.

hist(beer$beer_abv)
#The histogram for alcohol by volume appears to be skewed.
#Hence, we make use of the IQR method to find the outliers.

q1 = quantile(beer$beer_abv, prob = 0.25)
q3 = quantile(beer$beer_abv, prob = 0.75)

iqr = IQR(beer$beer_abv)

lower = q1 - 1.5 * iqr
upper = q3 + 1.5 * iqr

x <- beer[((beer$beer_abv < lower) | (beer$beer_abv > upper)),]

nrow(x)/nrow(beer)
#Since the number of outlier makes up only 1.25% of the entire data
#we can simply remove these outliers.

beer <- beer[!rownames(beer) %in% rownames(x),]

write.csv(beer, "beer_cleaned.csv", row.names = FALSE)


#creating a data frame which contains the data for individual beers
beers <- beer %>%
  group_by(beer_name, brewery_name, beer_style) %>%
  summarise(across(everything(), mean))

beer_count <- beer %>%
  group_by(beer_name, beer_beerid) %>%
  summarise(review_overall = n())

beer_df <- merge(beers, beer_count, by = c('beer_beerid')) %>%
  subset(select = -c(12))
  
colnames(beer_df)[colnames(beer_df) == 'beer_name.x'] <- 'beer_name'
colnames(beer_df)[colnames(beer_df) == 'review_overall.x'] <- 'review_overall'
colnames(beer_df)[colnames(beer_df) == 'review_overall.y'] <- 'review_count'

head(beer_df)

beer_df <- beer_df[beer_df$review_count > 10,]
nrow(beer_df)
#the above dataframe will be used to test out first research question.

write.csv(beer_df, "beer_name_data.csv", row.names = FALSE)


#Research Questions 
#1)
#For the first question we are trying to find out what property of a beer
#gives it the the best rating.
#For this purpose we will perform simple linear regression models.

#Linear regression for review for aroma vs. overall review 
set.seed(31)
x = beer_df$review_aroma
y = beer_df$review_overall

aroma <- lm(y~x)
summary(aroma)
anova(aroma)

qf(0.05, 1, nrow(beer_df) - 2, lower.tail = FALSE)

ggplot(beer_df, aes(x = review_aroma, y = review_overall)) +
  geom_point(size = 0.1, alpha = 0.7, color = 'rosybrown4') +
  geom_abline(aes(intercept = aroma$coefficients[1],
                  slope = aroma$coefficients[2])) +
  ggtitle(label = paste('Effect of Aroma Rating on Overall Rating (R-squared : ', 
                        round(summary(aroma)$r.squared,4), ')')) +
  ylab('Overall Beer Rating') +
  xlab('Beer Aroma Rating')

#Linear regression for review for appearance vs. overall review 
set.seed(31)
x = beer_df$review_appearance
y = beer_df$review_overall

appearance <- lm(y~x)
summary(appearance)
anova(appearance)

qf(0.05, 1, nrow(beer_df) - 2, lower.tail = FALSE)

ggplot(beer_df, aes(x = review_appearance, y = review_overall)) +
  geom_point(size = 0.1, alpha = 0.7, color = 'cadetblue4') +
  geom_abline(aes(intercept = appearance$coefficients[1],
                  slope = appearance$coefficients[2])) +
  ggtitle(label = paste('Effect of Appearance Rating on Overall Rating (R-squared : ', 
                        round(summary(appearance)$r.squared,4), ')')) +
  ylab('Overall Beer Rating') +
  xlab('Beer Appearance Rating')

#Linear regression for review for taste vs. overall review 
x = beer_df$review_taste
y = beer_df$review_overall

taste <- lm(y~x)
summary(taste)
anova(taste)

qf(0.05, 1, nrow(beer_df) - 2, lower.tail = FALSE)

ggplot(beer_df, aes(x = review_taste, y = review_overall)) +
  geom_point(size = 0.1, alpha = 0.7, color = 'lavenderblush4') +
  geom_abline(aes(intercept = taste$coefficients[1],
                  slope = taste$coefficients[2])) +
  ggtitle(label = paste('Effect of Taste Rating on Overall Rating (R-squared : ', 
                        round(summary(taste)$r.squared,4), ')')) +
  ylab('Overall Beer Rating') +
  xlab('Beer Taste Rating')

#Linear regression for review for palate vs. overall review 
set.seed(31)
x = beer_df$review_palate
y = beer_df$review_overall

palate <- lm(y~x)
summary(palate)
anova(palate)

qf(0.05, 1, nrow(beer_df) - 2, lower.tail = FALSE)

ggplot(beer_df, aes(x = review_palate, y = review_overall)) +
  geom_point(size = 0.1, alpha = 0.7, color = 'lightsteelblue3') +
  geom_abline(aes(intercept = palate$coefficients[1],
                  slope = palate$coefficients[2])) +
  ggtitle(label = paste('Effect of Palate Rating on Overall Rating (R-squared : ', 
                        round(summary(palate)$r.squared,4), ')')) +
  ylab('Overall Beer Rating') +
  xlab('Beer Palate Rating')

#Linear regression for alcohol by volume vs. overall review 
set.seed(31)
x = beer_df$beer_abv
y = beer_df$review_overall

abv <- lm(y~x)
summary(abv)
anova(abv)

qf(0.05, 1, nrow(beer_df) - 2, lower.tail = FALSE)

ggplot(beer_df, aes(x = beer_abv, y = review_overall)) +
  geom_point(size = 0.1, alpha = 0.7, color = 'darkseagreen4') +
  geom_abline(aes(intercept = abv$coefficients[1],
                  slope = abv$coefficients[2])) +
  ggtitle(label = paste('Beer Overall Rating against Alcohol by Volume (R-squared : ', 
                        round(summary(abv)$r.squared,4), ')')) +
  ylab('Overall Beer Rating') +
  xlab('Alcohol by Volume')

#Based on the critical f-value and comparing it with the f-value of each model
#we can state that each of the linear regression models is significant. 

#Hence, based on r-squared values of the models we can determine that
#the rating for taste is the most effective
#in predicting the overall rating of the beer.

#2)
#What style/type of beer is the most popular.
#Let us now try to find which style of beer is the most popular across all the breweries.

#Let us start by sorting and grouping the data according to the beer style.

#Let us first count the number of beers in each style
beer_style <- beer_df %>%
  group_by(beer_style) %>%
  summarise(beer_name = n()) %>%
  arrange(desc(beer_name))

colnames(beer_style)[(colnames(beer_style) == 'beer_name')] <- 'beer_count'

#Let us now count the number of reviews for each beer style
beer_style_review <- beer_df %>%
  group_by(beer_style) %>%
  summarise(review_freq = sum(review_count)) %>%
  arrange(desc(review_freq))


#Let us now find the average overall rating for each style of beer
beer_style_rating <- beer_df %>%
  group_by(beer_style) %>%
  summarise(review_overall = mean(review_overall)) %>%
  arrange(desc(review_overall))

#Top 10 beer styles with most number of beers
ggplot(beer_style[1:10,], aes(x = fct_inorder(beer_style), y = beer_count)) +
  geom_bar(stat = 'identity', fill = 'thistle4', width = 0.5) +
  coord_flip() +
  ggtitle('Top 10 Beer Styles') +
  labs(subtitle = 'Number of Beers by Style') +
  ylab('Number of Beers') +
  xlab('Beer Style') +
  theme(aspect.ratio = 9/7)

#Top 10 beer styles with most number of reviews
ggplot(beer_style_review[1:10,], aes(x = fct_inorder(beer_style), y = review_freq)) +
  geom_bar(stat = 'identity', fill = 'cornflowerblue', width = 0.5) +
  coord_flip() +
  ggtitle('Top 10 Beer Styles') +
  labs(subtitle = 'Number of Reviews by Style') +
  ylab('Number of Reviews') +
  xlab('Beer Style') +
  theme(aspect.ratio = 9/7)


#Top 10 beer styles according to their average overall rating
ggplot(beer_style_rating[1:10,], aes(y = fct_inorder(beer_style), x = review_overall)) +
  geom_bar(stat = 'identity', fill = 'slategray2', width = 0.5) +
  coord_cartesian(xlim = c(3.8, 4.15)) +
  ggtitle('Top 10 Beer Styles') +
  labs(subtitle = 'Ordered by Average Overall Beer Rating') +
  ylab('Average Overall Beer Rating') +
  xlab('Beer Style') +
  theme(aspect.ratio = 9/7, 
        axis.text.x = element_text(angle = 70, vjust = 1, hjust = 1))
  

#3)
#Comparing the average ratings for the top 3 breweries
#with the highest number of reviews.

#First we will need to find the top 3 breweries with the highest number of reviews
brewery <- beer %>%
  group_by(brewery_name) %>%
  summarise(review_overall = n()) %>%
  arrange(desc(review_overall)) %>%
  head(3)
brewery$brewery_name

#Now that I have the top 3 breweries I will compare their average ratings
#Taking a sample of 100 data points for each of the top 3 breweries
brewery_1 <- beer[beer$brewery_name == brewery$brewery_name[1], ][sample(nrow(beer[beer$brewery_name == brewery$brewery_name[1], ]), 100),]
brewery_2 <- beer[beer$brewery_name == brewery$brewery_name[2], ][sample(nrow(beer[beer$brewery_name == brewery$brewery_name[2], ]), 100),]
brewery_3 <- beer[beer$brewery_name == brewery$brewery_name[3], ][sample(nrow(beer[beer$brewery_name == brewery$brewery_name[3], ]), 100),]

rownames(brewery_1) <- NULL
rownames(brewery_2) <- NULL
rownames(brewery_3) <- NULL

brewery_data <- rbind(brewery_1, brewery_2,brewery_3)
head(brewery_data)

write.csv(brewery_data, "brewery.csv", row.names = FALSE)


#Hypotheses:
#Step 1: Set up the hypotheses and select the alpha level
#H0: mean of the three groups is the same
#H1: mean of at least one group is different
#alpha = 0.05

#Step 2: Select the appropriate test statistic
#We make use of the F-statistic in our case
# F = MSB / MSW

#Step 3: State the decision rule
#Reject the null hypothesis if F >= F_critical
#We have sampled 30 data points from each group
#Degrees of freedom are:
#k-1 = 3-1 = 2
#n-k = 300-3 = 297

qf(0.05, 2, 297, lower.tail = FALSE)

#We reject the null hypothesis if F >= 3.026153

#Step 4: Compute the test statistic
set.seed(31)
m <- aov(review_overall ~ brewery_name, data = brewery_data)
summary(m)

#Step 5: Conclusion
#Since F-statistic has a value greater than F_critical 
#we reject the null hypothesis

#Performing pairwise t-test
pairwise.t.test(brewery_data$review_overall, brewery_data$brewery_name, p.adj = 'bonferroni')

TukeyHSD(m)

#The above test results indicate that there is a significant difference in the
#overall rating of the beers from Stone Brewing Co and Boston Beer Company. However, since the
#p-value is > 0.05 for Stone Brewing Co and Dogfish Head Brewery & for Dogfish Head Brewery and Boston Beer Company,
#their average overall ratings are not significantly different from one another.



#Confirming the results received through ANOVA by performing ANCOVA
library(car)
library(emmeans)

### Predicting the overall rating based on the breweries - Dummy variables
set.seed(31)
model <- lm(review_overall ~ brewery_name, data = brewery_data)
summary(model)

qf(0.05, 2, nrow(brewery_data)-3, lower.tail = FALSE)

#The above model is the same as the test that we have done previously for beer styles. 
#The model has taken Boston Beer Company as the reference group.
#It tells us whether there is a significant difference between the reference group
#and the remainder groups.

#Since the F-statistic has a value greater than the critical value,
#there is a significant difference between the average overall rating of beers
#from the three different breweries.

##ANCOVA
Anova(lm(review_overall ~ brewery_name + 
           review_aroma +
           review_appearance +
           review_palate + 
           review_taste, data = brewery_data), type = 3)

model1 <- lm(review_overall ~ brewery_name +
               review_aroma +
               review_appearance +
               review_taste +
               review_palate, data = brewery_data)
summary(model1)

emm_options(contrast = c('contr.treatment', 'contr.poly'))

emmeans(model1, specs = 'brewery_name', contr = 'pairwise', adjust = 'none')

#The least square means for the 3 groups were found to be: 3.97, 3.85, and 3.76 after
#adjusting for the other numerical features.
#It can be seen that differences between the Dogfish Head brewery and Stone Brewing Co. were due to the
#differences in the numerical features across the Dogfish Head brewery and Stone Brewing Co. breweries 
#as opposed to the true difference in the overall ratings for these two breweries.


#4)
#Comparing the average rating for the top 3 beer styles
#with the highest number of reviews.

#Find the top 3 beer styles with the highest number of reviews
beer_style <- beer %>%
  group_by(beer_style) %>%
  summarise(review_overall = n()) %>%
  arrange(desc(review_overall)) %>%
  head(3)
beer_style$beer_style

#Now that I have the top 3 beer styles I will compare their average ratings
#Taking a sample of 100 data points for each of the top 3 beer styles
style_1 <- beer[beer$beer_style == beer_style$beer_style[1], ][sample(nrow(beer[beer$beer_style == beer_style$beer_style[1], ]), 100),]
style_2 <- beer[beer$beer_style == beer_style$beer_style[2], ][sample(nrow(beer[beer$beer_style == beer_style$beer_style[2], ]), 100),]
style_3 <- beer[beer$beer_style == beer_style$beer_style[3], ][sample(nrow(beer[beer$beer_style == beer_style$beer_style[3], ]), 100),]

rownames(style_1) <- NULL
rownames(style_2) <- NULL
rownames(style_3) <- NULL

style_data <- rbind(style_1, style_2, style_3)
head(style_data)

write.csv(style_data, "beer_style.csv", row.names = FALSE)


#Hypotheses:
#Step 1: Set up the hypotheses and select the alpha level
#H0: mean of the three groups is the same
#H1: mean of at least one group is different
#alpha = 0.05

#Step 2: Select the appropriate test statistic
#We make use of the F-statistic in our case
# F = MSB / MSW

#Step 3: State the decision rule
#Reject the null hypothesis if F >= F_critical
#We have sampled 30 data points from each group
#Degrees of freedom are:
#k-1 = 3-1 = 2
#n-k = 300-3 = 297
qf(0.05, 2, 297, lower.tail = FALSE)

#We reject the null hypothesis if F >= 3.026153

#Step 4: Compute the test statistic
m <- aov(review_overall ~ beer_style, data = style_data)
summary(m)

#Step 5: Conclusion
#Since F-statistic has a value lesser than F_critical 
#we fail to reject the null hypothesis

#5)
#The average review score of beers with higher alcohol content 
#is significantly different from that of beers with a lower alcohol content.

#Getting a subset of the original data with only the two target columns
sub <- subset(beer, select = c('review_overall', 'beer_abv'))

#Based on the hypotheses we need to divide the data according to the alcohol content of the beers
high_abv <- subset(sub, beer_abv >=8)
low_abv <- subset(sub, beer_abv < 8)

#We can make use of two-sample t-test to compare the two groups
#and test the hypotheses.

#Step 1: Set up the hypotheses and select the alpha level
#H0: mean(high_abv) = mean(low_abv)
#This indicates that there is no relationship between the alcohol content
#in the beer and the overall rating
#H1: mean(high_abv) != mean(low_abv)
#This indicates that there exists a relationship between the alcohol content
#in the beer and the overall rating
#alpha = 0.05

#Step 2: Select the appropriate test statistic
#We make use of two-sample t-test for this instance
#t = (mean(high_abv) - mean(low_abv)) / (sqrt((std(high_abv)^2 / nrow(high_abv) + (std(low_abv)^2 / nrow(low_abv))))

#Step 3: State the decision rule
#We reject the null hypothesis if the |t-statistic| >= t-critical value
#Otherwise, we fail to reject the null hypothesis

t_critical = qt(0.05/2, min(nrow(high_abv) - 1, nrow(low_abv) - 1), lower.tail = FALSE)
t_critical
#The null hypothesis is rejected if |t-statistic| >= 3.919938

#Step 4: Compute the test statistic
numerator = mean(high_abv$review_overall) - mean(low_abv$review_overall)
denominator = sqrt((sd(high_abv$review_overall)^2 / nrow(high_abv)) + (sd(low_abv$review_overall)^2 / nrow(low_abv)))

t = numerator / denominator
t

#Step 5: Conclusion
#Since the value of t-statistic is greater than the critical value
#We reject the null hypothesis
#This indicates there is evidence to show that the mean overall rating
#for beer with higher alcohol content is significantly different
#from beers with lower alcohol content.

#The 95% confidence interval for the difference between the average
#overall rating between beers with higher alcoholic content and those
#with lower alcoholic content is:
ci_lower <- numerator - t_critical * denominator
ci_upper <- numerator + t_critical * denominator

#The confidence interval is (0.1618, 0.1712)


##Multiple Linear Regression
# Predicting the overall rating based on the ratings for individual
#aspects of the beer

corr_matrix = cor(beer[c('review_aroma', 
                         'review_taste', 
                         'review_appearance',
                         'review_palate',
                         'beer_abv')])
corrplot(corr_matrix, method = 'number')

model1 <- lm(review_overall ~ review_aroma +
               review_appearance +
               review_palate +
               review_taste,
             data = beer)
summary(model1)

#Hypotheses Testing:
#Step 1: Set up hypotheses and select alpha level
#H0: beta(review_aroma) = beta(review_appearance) = 
#beta(review_palate) = beta(review_taste) = 0
#This indicates that none of the predictors are significant
#H1: beta(review_aroma) != beta(review_appearance) != 
#beta(review_palate) != beta(review_taste) != 0
#This indicates that at least one of the predictors is significant
#alpha = 0.05

#Step 2: Select the appropriate test statistic
#For our hypotheses test we make use of the F-statistic
#F = MS Reg/MS Res
#df = k, n-k-1
#df = 4, nrow(beer) - 5

#Step 3: State the decision rule
#Reject the null hypothesis if F-statistic >= F-critical value
#F critical value:
qf(0.05, 3, nrow(beer) - 5, lower.tail = FALSE)
#Reject the null hypothesis if F-statistic >= 2.604909
#Otherwise, we fail to reject the null hypothesis

#Step 4: Compute the test statistic
#F = MS Reg/MS Res
pred = predict(model1)

regss = sum((pred - mean(beer$review_overall))^2)
resss = sum((beer$review_overall - pred)^2)

regms = regss/4
resms = resss/(nrow(beer)-4-1)

f = regms/resms
f

#Step 5: Conclusion
#Since the value of F-statistic is greater than the critical value
#We can conclude that at least one of the predictors is significant.
#We have significant evidence at alpha = 0.05 level that at least one
#of the predictors when taken together is significant to predict the
#overall review.
#This means that there is evidence of a linear association between the
#ratings for aroma, appearance, palate and taste and the overall rating.

#Let us proceed to perform t-tests and check which predictor is
#significant in the prediction of the overall rating.

#T-tests
#Calculating the critical value:
qt(0.05/2, nrow(beer)-4-1, lower.tail = FALSE)

#A predictor is significant if its t-statistic >= 3.919931

#Since the t-statistics, as shown by the summary(model2), are all
#greater than the critical value, we can conclude that all the predictors
#are significant in predicting the overall rating for the beers.

#Let's calculate the R-squared for the model:
r2 <- regss/(regss+resss)
r2
#R-squared represents the proportion of variation in the response
#variable that is explained by the regression model.
#This means that 65% of the variation in the response variable is 
#explained by the multiple linear regression model.

r2adj = 1 - (resms/sd(beer$review_overall)^2)
r2adj

#Since we have 1.5m data points in our dataset our adjusted R-squared
#has a similar value as R-squared.
#Which is consistent with what we have studied.


mlr <- lm(review_overall ~ review_aroma +
               review_appearance +
               review_palate +
               beer_abv,
             data = beer)
summary(mlr)

mlr2 <- lm(review_overall ~ review_appearance +
               review_taste +
             beer_abv,
             data = beer)
summary(mlr2)

mlr3 <- lm(review_overall ~ review_aroma +
               review_appearance +
               review_palate +
               review_taste +
               beer_abv,
             data = beer)
summary(mlr3)

#Logistic Regression
#Converting the overall rating column to binary classes
beer$class <- sapply(beer$review_overall, function(x) ifelse(x >= 3.5, 1, 0))

#If the overall rating is greater than 2.5 the class is 1 else 0
#Now that we have a binary classification problem let us perform Logistic regression on our data.

#Splitting the data into train and test
s <- sample(c(TRUE, FALSE), size = nrow(beer), replace = TRUE, prob = c(0.7, 0.3))

train <- beer[s,]
test <- beer[!s,]

model4 <- glm(class ~ review_aroma +
                review_appearance +
                review_taste +
                review_palate +
                beer_abv,
              data = train,
              family = 'binomial')
summary(model4)

#Global test of significance
wald.test(b = coef(model4), Sigma = vcov(model4), Terms = 2:6)
#Since our p-value is < 0.05 we reject the null hypothesis
#indicating that the model is significant.

pred <- predict(model4, test, type='response')

new_pred <- sapply(pred, function(x) ifelse(x > 0.5, 1, 0))

new_pred <- factor(new_pred)
test$class <- factor(test$class)

cm <- confusionMatrix(new_pred, test$class)

x <- tibble('target' = test$class,
            'prediction' = new_pred)

confusion_matrix <- as_tibble(table(x))
plot_confusion_matrix(confusion_matrix,
                      target_col = 'target',
                      prediction_col = 'prediction',
                      counts_col = 'n')

logistic_roc <- roc(test$class, pred)

print(logistic_roc)
plot(logistic_roc, main = 'ROC curve for Multiple Logistic Regression')
