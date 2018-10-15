# Linear Regression Assignment 2
# Big Data Science
# Joshua Robinson, Isaac Whittaker, Jacob Braswell, Tyki Wada

library(dplyr)
library(data.table)
library(ggplot2)
library(glmnet)
library(stringr)
library(tibble)

###########################
# Problem One
###########################

##### PART A #####
# Loading the data
as.tibble(fread("~/Desktop/icudata.csv", header = TRUE)) %>%
  dplyr::select(STA, AGE, RACE, CPR, SYS, HRA, TYP) -> icu

# Fitting logistic regression model
model <- glm('STA ~ .', data = icu, family = binomial)

# Printing table of coefficients
coef(model)

##### PART B #####
# The coefficient for CPR, 1.39, is the expected change in log odds
# if a patient had CPR admnistered. That is, we expect to see about a
# 400% increase in the odds of having STA be true if CPR was administered.

##### PART C #####
# Fitting LASSO model
x <- model.matrix(STA ~ ., data = icu)[,-1]
y <- icu$STA

# Using cross validation to determine optimal alpha
k <- 150

possible_alphas <- seq(0, 1, 0.005)

alpha_accuracies <- c()

for (l in possible_alphas) {
  accuracies <- rep(0, k)
  for (cv in 1:k) {
    train_rows <- sample.int(n = nrow(icu), size = floor(.8 * nrow(icu)), replace = F)
    
    lasso_model <- glmnet(x[train_rows,], y[train_rows], alpha = l)
    preds <- predict(lasso_model, s = l, newx = x[-train_rows,])
    
    accuracies[cv] <- mean(round(preds) == y[-train_rows])
  }
  alpha_accuracies <- c(alpha_accuracies, mean(accuracies))
}

# Looking at potential alpha values and associated rpmse
data.frame("alpha" = possible_alphas, "accuracy" = alpha_accuracies) %>%
  ggplot(aes(alpha, accuracy)) +
  geom_point()

# Getting the alpha that minimizes rpmse
best_alpha <- possible_alphas[which.max(alpha_accuracies)]

##### PART D #####
lasso_model <- glmnet(x[train_rows,], y[train_rows], alpha = best_alpha)

# Here are the coefficients
coef(lasso_model)

###########################
# Problem Two
###########################

##### PART A #####
ted <- read.csv('~/Desktop/ted.csv', stringsAsFactors=FALSE) %>% as.tibble

sapply(
  unname(unlist(sapply(1:nrow(ted), function(row) ted[row, 'tags']))),
  function(x) strsplit(gsub("[^[:alnum:|, ]", "", x), ",")[[1]] %>% trimws
) %>% unlist %>% unique %>% sort -> tag_names

length(tag_names)

# Go through the tag_names and for each add a column to the ted dataset
# and make it true or false based on whether string is present in tag list
new_ted <- ted

sapply(
  unname(unlist(sapply(1:nrow(ted), function(row) ted[row, 'tags']))),
  function(x) paste(strsplit(gsub("[^[:alnum:] ]", ",", x), ",")[[1]], collapse=',')
) -> new_ted$tags

for (tag_name in tag_names) {
  new_col <- rep(FALSE, nrow(new_ted))
  for (row in 1:nrow(new_ted)) {
    if (length(grep(tag_name, new_ted[row, 'tags'])) != 0) {
      new_col[row] <- TRUE
    }
  }
  new_ted <- cbind(new_ted, new_col)
  colnames(new_ted)[length(colnames(new_ted))] <- paste0("TAG_", tag_name)
}

a_done <- new_ted

##### PART B #####

# Splitting up ratings
rating_names <- c("Inspiring", "Persuasive", "Courageous", "Fascinating", "Informative",
                  "Ingenious", "Unconvincing", "Obnoxious", "Longwinded", "Jaw-dropping",
                  "Confusing", "Funny", "Beautiful", "OK")

for (rat_name in rating_names) {
  new_col <- rep(FALSE, nrow(a_done))
  for (row in 1:nrow(a_done)) {
    q <- str_locate(a_done[row, 'ratings'], rat_name)[2] + 13
    r <- substr(a_done[row, 'ratings'], q, nchar(a_done[row, 'ratings']))
    s <- str_locate(r, '\\}')[2]
    
    new_col[row] <- as.integer(substr(r, 1, s - 1))
  }
  a_done <- cbind(a_done, new_col)
  colnames(a_done)[length(colnames(a_done))] <- paste0("RATINGS_", rat_name)
}

b_done <- a_done

##### PARTS C & D #####

# Limiting data down to just what is asked for in problem
b_done %>%
  select(-description, -event, -film_date, -languages, 
         -main_speaker, -name, -published_date,
         -ratings, -related_talks, -speaker_occupation, -tags,
         -title, -url) -> lasso_part

x <- model.matrix(views ~ ., data = lasso_part)[,-1]
y <- lasso_part$views

# Using cross validation to determine optimal lambda
k <- 3

possible_lambdas <- seq(0, 200, 1)

lambda_rpmses <- c()

for (l in possible_lambdas) {
  print(l/length(possible_lambdas)*100)
  rpmses <- rep(0, k)
  for (cv in 1:k) {
    train_rows <- sample.int(n = nrow(lasso_part), size = floor(.8 * nrow(lasso_part)), replace = F)
    lasso_model <- glmnet(x[train_rows,], y[train_rows], lambda = l)
    
    preds <- predict(lasso_model, s = l, newx = x[-train_rows,])
    
    rpmses[cv] <- sqrt(mean((preds - y[-train_rows])^2))
  }
  lambda_rpmses <- c(lambda_rpmses, mean(rpmses))
}

# Looking at potential alpha values and associated rpmse
data.frame("lambda" = possible_lambdas, "rpmse" = lambda_rpmses) %>%
  ggplot(aes(lambda, rpmse)) +
  geom_point()

# Getting the alpha that minimizes rpmse as required by part d
best_lambda <- possible_lambdas[which.min(lambda_rpmses)]

# Fitting the model as required by part c
lasso_model <- glmnet(x[train_rows,], y[train_rows], lambda = best_lambda)

##### PARTS E & F #####
coef(lasso_model) 

# 10 best tags (starting from best)
  # Magic
  # Body language
  # Addiction
  # Epidemiology
  # Exoskeleton
  # Fashion
  # Testing
  # Speech
  # Prison
  # TED Residency

# 10 worst tags (starting from worst)
  # Meme
  # Novel
  # Primates
  # Atheism
  # Archaeology
  # Suicide
  # Failure
  # Google
  # Nobel Prize
  # Presentation

# Least important characteristic is "Longwinded"

