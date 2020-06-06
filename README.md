# MovieLens User Ratings Predictions Project

In this project, we will study the MovieLens Dataset - a collection of user ratings of movies covering a period of several years created by the GroupLens Project (https://grouplens.org/datasets/movielens/) - and will create a movie recommendation system using machine learning techniques.

We will be using the MovieLens 10M Dataset, a subset of the full MovieLens Dataset containing approximately 10 million user ratings, compared to 27 million ratings in the current version of the full set, due to its more manageable size and lower computing time required.

We will first split the MovieLens 10M Dataset into training and validation sets, and will examine the data contained in the training set only. Our goal is to create an algorithm to predict movie ratings in the the validation set by utilizing the data in the training set. Various factors and their relationships to movie ratings in the training dataset will be explored and visualized to determine their relevance for inclusion in our final algorithm.

A linear regression algorithm will then be developed utilizing the training set, which itself will be split into training and test subsets. The linear model is built-up in steps, to measure the effect on performance provided by each incremental factor added. The algorithm's performance will be measured by calculating the Root Mean Square Error (RMSE) when comparing our algorithm's predicted ratings for the test set to the set's actual ratings. We will then look at the effects of regularization on the model's performance and implement cross validation to find the optimal tuning parameter.

Finally, we will test our final optimized linear regression algorithm with the validation set and measure the RMSE between our predicted ratings and the actual ratings.
