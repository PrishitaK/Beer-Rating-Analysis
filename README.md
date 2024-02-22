# Investigating the Aspects That Affect Beer Reviews

## Project Summary
The project is aimed at investigating the various factors that contribute the ratings given to various kinds of beers from multiple different breweries. This has been done by making use of various statistical analysis techniques like teo sample F-tests, T-test, , ANOVA, ANCOVA, Simple Linear Regression, Multiple Linear Regression, and Logistic Regression.

## Tools and Technologies used
1. R Programming
2. RStudio
3. ggplot
4. corrplot
5. caret

## Research Questions
The dataset consists of the ratings provided to the beers by various users along with a rating for 4 aspects of each beer namely, it's arom, taste, palate, and appearance. The following research questions have been answered by making use of these ratings:
1. *What property of a beer gives it the best rating?*
2. *What style of beer is te most popular?*
3. *Is there a difference amongst the average ratings for the top 3 breweries?*
4. *Is there a significant difference amongst the average ratings of the top 3 beer styles?*
5. *Is there a significant difference between the average rating for beers with a higher alcohol content as compared to those with a lower alcohol content?*

Once these research questions are answered a Multiple Linear Regression model has been trained to predict the overall review rating based on all the aspects of a beer. Following this a Logistic Regression model is trained to identify beers with a 'high' or a 'low' rating by converting the ratings to a categorical value and using the significant aspects, as found through Multiple Linear Regression, to train the Logistic Regression model.

### What property of a beer givers it the best rating?
A number of Simple Linear Regression models were performed to predict the overall rating of the beer based on the ratings it received for one of its aspects, i.e. aroma, appearance, taste and palate. Taste was found to have the highest impact on the overall rating with an r-squared value of 0.9026
**This indicates that the model built with tast as a predictor was able to explain 90% of the variation in the overall reviews.**

### What style of beer is te most popular?
To answer this question the data was first sorted to retrieve the data for the total reviews each style received, the number of beers under each style and the average overall rating each style received. The analysis involved the creation of multiple bar charts for each of the three categories for the top 10 beer styles in each. It was found that American IPA style of beers are the most reviewed and also the most common style of beer. The highest overall rating was received by Lambic-Unblended beer style. It can also be seen that America Double/Imperial Stout is amongst the top 10 beer styles in each of the 3 categories. 
The three barplots can be seen below:
![Barplot - Ratings](https://github.com/PrishitaK/Beer-Rating-Analysis/assets/126426638/180119b0-4290-4be4-95f6-bb586087a38e)
![Barplot - Reviews](https://github.com/PrishitaK/Beer-Rating-Analysis/assets/126426638/f795a34a-03ba-4dc6-b69a-dfda2a5214bc)
![Barplot-Beers](https://github.com/PrishitaK/Beer-Rating-Analysis/assets/126426638/f0e8fb44-6b22-42ef-93c4-b63bb8781557)

### Is there a difference amongst the average ratings for the top 3 breweries?
ANOVA is used to compare the overall average rating for the top 3 breweries. Obtaining significant results using ANOVA I made use of a Linear Regression model to validate these results. The two models gave similar results indicating the presence of a significant difference in the average overall ratings of the top 3 breweries. 
ANCOVA was then used to check whether these results are consistent after controlling for the other numerical features. The results indicate that a significant difference exists between the average ratings for 2 of the top 3 breweries. This indicates that the overall rating for a beer is solely dependant on its different aspects and has nothing to do with the brewery it was brewed in.

### Is there a significant difference amongst the average ratings of the top 3 beer styles?
ANOVA is used again to compare the average ratings for the top 3 beer styles, the results of which suggest the absence of a significant difference between the average overall ratings of the 3 groups. This indicats that the overal rating of a beer has nothing to do with the style of the beer.

### Is there a significant difference between the average rating for beers with a higher alcohol content as compared to those with a lower alcohol content?
The alcohol by volume feature was first converted to a categorical feature to split the data into two groups. The two new subsets were used to perform two-sample t-test. The test showed evidence that the mean overal rating for beer with higher alcohol content is significantly different from beers with lower alcohol content.
