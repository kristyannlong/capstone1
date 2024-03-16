# For this assignment, you will be able to choose a set of data on Kaggle or
# similar and do your own analysis of it! This project is meant to highlight the
# analysis process as you use multiple languages and platforms to analyze the
# same set of data.
#
# Choosing a Data Set Below, I have added a few common resources for finding
# data. Some of these websites change regularly. Some of them are not formatted
# ideally for what we do. Before you choose a set, try and make sure that it is
# a data set you have the tools to analyze. For example, some soccer data is
# arranged into graph data, which is not a normal data-analysis format; if
# there's a tabular or noSQL format, that's more common and will be a better
# resume piece anyways; so don't try and reinvent the wheel just because you
# found a cool data set. I recommend you find a topic you like and search
# elsewhere for similar data sets.
#
# Imagine you are working for a business and your boss doesn't understand data.
# Your mission is to create a report explaining what is going on with the data
# and what conclusions you have made from the data. (The data set you choose
# doesn't have to be related to a business at all).
#
#
# Before you start, I want you to write out a hypothesis about the data, even if
# you have no clue what the data means. You may also add hypotheses as you go
# and test them - often you will have many, many questions about your data that
# you'll want to answer. Then, you'll be using R and Python to analyze whatever
# data you find. I recommend going through the entire process with one language
# and then trying it with the other. Googling things is totally encouraged if
# you need to find specific tools for Python or R, particularly if there are
# specific tools from the tidyverse to help out; this won't be needed for every
# project. This project is intentionally open-ended, as an important part of
# data analysis is knowing your data.
#
# After you've looked at a lot of the data, you will write a report detailing
# your process and showing the results of your analyses. For each major
# hypothesis you look into, you should cover that in the report and include
# charts, graphs, anything necessary to communicate the data. I am much less
# concerned with things like grammar and styling and very interested in seeing
# how you got the conclusions you made in a way that you can explain to someone
# who doesn't speak data.
#
# Have fun with this project! There is no length requirement on the report; I
# just want to see you communicate well about what you've found and what the
# data means. I anticipate the project takes around 10 hours; if it takes more
# or less, that's okay, but you should be very familiar with your data and
# writing a good report on it.
#
# The submission should include all your technical work, and the report itself.
# Put them all in a GitHub repo and post the repo here. Please book here after
# submission: https://calendly.com/d/48z-p4c-5kb/data-capstone-presentation


# Hypothesis:
# Research Question 1: Are the rating means different?
# Research Question 2: Is the rating mean in Samsung greater than the mean in Apple?
# Research Question 3: Is the rating mean in Samsung less than the mean in Apple?
# Null Hypothesis: There is no difference in the rating means of Samsung and Apple.
# Alternative Hypothesis: Apple has a higher mean for ratings than Samsung.
# I wanted to show that the Apple brand is, on average, higher rated than any 
# other mobile phone brand even with the cost generally being substantially 
# higher for Apple.


# What type of variation occurs within my variables?
# What type of covariation occurs between my variables?


library(tidyverse)
library(tidyselect)
library(tidyr)
library(dplyr)
library(tidyselect)
library(dtplyr)
library(dbplyr)
library(janitor)
library(shiny)
library(readr)



sales <- read_csv("Sales.csv")

glimpse(sales)

# Summarize all data for easy viewing
summary(sales)

# Count total missing values in each column
sapply(sales, function(x) sum(is.na(x)))

# Missing data from Rating(144), Memory(43), Storage(39)

# brands of phones first, then break down by models?
# what colors, memory, storage are most popular?
# how do the most popular phones compare to each other?
# which is the most popular and the second most?

# for charting -- 
#   phone brand and rating?
#   can we break down into brand and model?
#   selling price vs original price?
#   rating and selling prices?




# Checking to see what the min and max ratings are
sales |>
  summarize(
    max(Rating, na.rm = TRUE),
    min(Rating, na.rm = TRUE))
# Min rating is NA or 2.3.  Max is 5

# This is my original code.  I moved it to a function so I could pull other 
# Brands without having to rewrite all this code.

# # Samsung data grouped by model
# # Samsung average sales info
# samsung_sales <- sales |>
#   filter(Brands == "SAMSUNG") |>
#   group_by(Models) |>
#   summarize(
#     avg_sales_price = mean(`Selling Price`)
#   )
# view(samsung_sales)
# 
# # Samsung average rating info
# samsung_ratings <- sales |>
#   filter(Brands == "SAMSUNG") |>
#   group_by(Models) |>
#   summarize(
#     ratings_mean = mean(Rating)
#   )
# view(samsung_ratings)
# 
# 
# # Combine the two queries into one
# samsung_combined <- merge(samsung_sales, samsung_ratings, by = "Models")
# 
# # View the data sorted by rating
# samsung_combined <- samsung_combined |>
#   arrange(desc(ratings_mean))
# 
# # View the combined data frame
# view(samsung_combined)
# 
# # Samsung mean ratings not grouped by model
# samsung_ratings_all <- sales %>%
#   filter(Brands == "SAMSUNG") %>%
#   summarize(
#     mean_rating_all = mean(Rating, na.rm = TRUE)  
#   )
# 
# view(samsung_ratings_all)

# Function to pull rating and sales means for any brand
get_brand_info <- function() {
  brand_info <- sales |>
    group_by(Brands) |>
    summarize(
      Brand = unique(Brands),  # Keep the brand name intact
      avg_sales_price = mean(`Selling Price`, na.rm = TRUE),
      mean_rating = mean(Rating, na.rm = TRUE)
    ) |>
    arrange(desc(mean_rating))  # Sort by mean_rating in descending order
  
  return(brand_info)
}

brand_info <- get_brand_info()
view(brand_info)


# Check the number of unique brands available in the data
sales |> count(Brands, sort = TRUE)

#  How many models per brand?
models_per_brand <- sales |> 
  group_by(Brands) |> 
  summarize(Models = n_distinct(Models)) |> 
  arrange(desc(Models))

view(models_per_brand)



# These are charts I created before I decided what I wanted to find.

# Scatter based on selling vs original pricing with brands being colored.
# very congested charting.  Which brands offer the biggest discounts?
ggplot(
  data = sales,
  mapping = aes(x = `Original Price`, y = `Selling Price`, color = Brands)
) +
  geom_point() +
  # adding geom_smooth to create a nice line in each color 
  geom_smooth(method = "lm")

# changing the color mapping to the geom_point
ggplot(
  data = sales,
  mapping = aes(x = `Original Price`, y = `Selling Price`)
) +
  geom_point(mapping = aes(color = Brands)) +
  # adding geom_smooth to create a nice line in each color 
  geom_smooth(method = "lm")


# These are the charts for the ratings for all Brands
brand_info_rating_filtered <- brand_info[!is.nan(brand_info$mean_rating), ]

brand_info$Brand <- factor(brand_info$Brand, levels = brand_info_rating_filtered$Brand[order(brand_info_rating_filtered$mean_rating)])

ggplot(
  data = brand_info,
  mapping = aes(x = Brand, y = mean_rating)
) +
  geom_point(mapping = aes(color = Brands)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Charts for the selling price for all brands
brand_info_sale_price_filtered <- brand_info[!is.nan(brand_info$avg_sales_price), ]

brand_info$Brand <- factor(brand_info$Brand, levels = brand_info_sale_price_filtered$Brand[order(brand_info_sale_price_filtered$avg_sales_price)])

ggplot(
  data = brand_info,
  mapping = aes(x = Brand, y = avg_sales_price)
) +
  geom_point(mapping = aes(color = Brands)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# testing overlaying of graphs.  
# I like how this one turned out.  Shows the avg sale price and the avg rating on one.
brand_info |>
  ggplot(aes(x = avg_sales_price, y = mean_rating, color = Brands)) +
  geom_point(size=2) +
  geom_smooth(se=F, color="black")

# second test
# not a good fit.
brand_info |>
  ggplot(aes(x = avg_sales_price, y = mean_rating, color = Brands)) +
  geom_point(size=2) +
  geom_smooth(se=F, color="black") +
  facet_grid(cols = )

# different chart
brand_info |>
  ggplot(aes(x = avg_sales_price, y = mean_rating, color = Brands)) +
  geom_point(size=2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # geom_smooth(se=F, color="black") +
  facet_grid(rows = vars(Brands))  #This was ugly.  trying cols instead
  # facet_grid(cols = vars(Brands))













