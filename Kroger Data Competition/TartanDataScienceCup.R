## Tartan Data Science Cup
## EDA

## Check for useful packages and install missing ones
packs = c("ggplot2", "knitr", "rmarkdown", "dplyr", 
          "magrittr", "gridExtra", "lubridate", 
          "forecast", "stargazer", "tidyr", "reshape2",
          "car", "data.table", "sqldf", "caret")
## Compares new packages to installed packages
new_packs = packs[!(packs %in% installed.packages())]
## If new packages not installed, install them
if(length(new_packs)) {install.packages(new_packs)}
## Load all packages
sapply(packs, require, character.only = T)

# Read data
trans = read.csv("transaction_data_8451.csv") 

# Change all column names to lower case
names(trans) %<>% tolower()

# Descriptive statistics
summary(trans)

# Function for EDA plots
trans_hist = function(column, data = "trans"){
  
  ggplot(eval(parse(text = data)), aes_string(x = column)) + 
    geom_histogram(bins = 100, aes(y = ..density..), col = "white", fill = "peachpuff") + 
    geom_line(stat = "density", col = "saddlebrown") + theme_bw() + 
    labs(title = paste(column, "Histogram"), x = column) +
    theme(plot.title = element_text(size = rel(.95)), 
          axis.title = element_text(size = rel(.85)))
}

trans_box = function(x, y, data = "trans"){
  
  ggplot(eval(parse(text = data)), aes_string(x = x, y = y)) + 
    geom_boxplot() + theme_bw() + labs(x = x, y = y, title = paste(y, "vs", x, "Boxplot")) +
    theme(plot.title = element_text(size = rel(.95)), 
          axis.title = element_text(size = rel(.85)))
}

trans_scatter = function(x, y, data = "trans"){
  
  ggplot(eval(parse(text = data)), aes_string(x = x, y = y)) + 
    geom_point(pch = 20) + theme_bw() + 
    labs(x = x, y = y, title = paste(y, "vs.", x, "Scatter Plot")) + 
    theme(plot.title = element_text(size = rel(.95)), 
          axis.title = element_text(size = rel(.85)))
  
}

trans_hist("base_spend_amt")
trans_hist("net_spend_amt")
trans_hist("quantity")
trans_hist("day")

trans_box("marital_status_code", "net_spend_amt")

trans_scatter("quantity", "base_spend_amt")

# Split data into training and testing sets
# Randomly sample 80% of data as Training set
set.seed(111)

Train_size = floor(0.8*nrow(trans))
train_id = sample(seq_len(nrow(trans)), size = Train_size, replace = FALSE)

training = trans[train_id, ]

# testing set
testing = trans[-train_id, ]

# Check if the rows of both sets add up to original
nrow(training) + nrow(testing)

### Function for separating last week from rest of data
data_split = function(dataset){
  # Separate the last week of data from the rest of training
  dataset_col <- dataset[, c("household_key", "basket_id", "day", "quantity", 
                               "net_spend_amt", "department","income_desc",
                               "hh_comp_desc", "commodity_desc")]
  
  dataset_cl = subset(dataset_col, day < 698)
  dataset_dv = subset(dataset_col, day >= 698)
  
  # data cleaning
  # rm quantity = 0, spent = 0
  dataset_cl <- dataset_cl[dataset_cl$quantity != 0, ]
  dataset_cl <- dataset_cl[dataset_cl$net_spend_amt != 0, ]
  
  return(list(dataset_cl, dataset_dv))
}

trainingSplit = data_split(training)
testingSplit = data_split(testing)

##### Aggregate data Function
dataAggregate = function(data, data_as_string = "data_cl"){
  
  ## Recency
  recent = aggregate(day ~ household_key, data = data[[1]], max)
  recent$visited_in <- 697 - recent$day 
  
  data_cl = data[[1]]
  
  ## Frequency
  # num of visits per household in the past 7-14 days
  freq_14 <- sqldf(paste("select household_key, count(distinct(basket_id))
                   from", data_as_string,
                   "where day between 691 and 697
                   group by household_key"))
  names(freq_14) <- c("household_key", "freq_14")
  # # of visits per household in the past 14-21 days - ONLY use this to fit DV of day 698-704
  freq_21 <- sqldf(paste("select household_key, count(distinct(basket_id))
                   from", data_as_string,
                   "where day between 684 and 690
                   group by household_key"))
  names(freq_21) <- c("household_key", "freq_21")
  # frequency from the beginning of time
  freq_total <- sqldf(paste("select household_key, count(distinct(basket_id))
                      from", data_as_string,
                      "group by household_key"))
  names(freq_total) <- c("household_key", "freq_total")
  
  ## Monetary value
  money = aggregate(cbind(quantity, net_spend_amt) ~ household_key, data = data[[1]], sum)
  money$price_per_amt = with(money, net_spend_amt/quantity)
  
  # #visits at Grocery Department per household in past 7-14 days - ONLY use this to fit DV of day 698-704
  Grocery_past_14 <- sqldf(paste("select household_key, count(*)
                        from", data_as_string,
                        "where day between 691 and 697 and department = 'GROCERY'
                        group by household_key"))
  names(Grocery_past_14) <- c("household_key", "Grocery_past_14")
  
  # total visits to Grocery from the beginning of time
  Grocery_total <- sqldf(paste("select household_key, count(*)
                         from", data_as_string,
                         "group by household_key"))
  names(Grocery_total) <- c("household_key", "Grocery_total")
  
  # income per household
  income = aggregate(income_desc ~ household_key, data = data[[1]], 
                     unique, na.action = NULL)
  income$income_desc <- factor(income$income_desc, 
                               levels = c(levels(income$income_desc), "Other"))
  income$income_desc[is.na(income$income_desc)] <- "Other"
  
  # hh_comp category per household
  hh_comp = aggregate(hh_comp_desc ~ household_key, data = data[[1]], 
                      unique, na.action = NULL)
  hh_comp$hh_comp_desc[is.na(hh_comp$hh_comp_desc)] <- "Unknown"
  hh_comp$hh_comp_desc <- as.character(hh_comp$hh_comp_desc)
  hh_comp$hh_comp <- ifelse( (hh_comp$hh_comp_desc == "1 Adult Kids" | hh_comp$hh_comp_desc == "2 Adults Kids"), "With Kids",
                             ifelse( (hh_comp$hh_comp_desc == "2 Adults No Kids"),  "No Kids", 
                                     ifelse( (hh_comp$hh_comp_desc == "Single Female" | hh_comp$hh_comp_desc == "Single Male"), 'Single', "Unknown")))
  hh_comp$hh_comp <- factor(hh_comp$hh_comp, levels = c("Unknown", "Single", "No Kids", "With Kids"))
  
  # For day 698 - 704, EGGS dummy per household, using training_dv 
  dv_eggs <- data[[2]][,c("household_key", "commodity_desc")]
  dv_eggs <- sqldf("select household_key, 1
                 from dv_eggs
                 where commodity_desc = 'EGGS'
                 group by household_key")
  names(dv_eggs) <- c("household_key", "dv_eggs")
  
  # Average price paid for eggs per household
  eggs_price <- sqldf(paste("select household_key, sum(net_spend_amt), sum(quantity)
                     from", data_as_string,
                     "where commodity_desc = 'EGGS'
                     group by household_key"))
  names(eggs_price) <- c("household_key", "amt_spent_eggs", "quantity_eggs")
  eggs_price$price_paid_eggs <- eggs_price$amt_spent_eggs/eggs_price$quantity_eggs
  
  # total eggs purchased from the beginning of time
  eggs_total <- data[[1]][,c("household_key", "quantity", "commodity_desc")]
  eggs_total <- sqldf(paste("select household_key, sum(quantity)
                     from", data_as_string,
                     "where commodity_desc = 'EGGS'
                     group by household_key"))
  names(eggs_total) <- c("household_key", "eggs_total")
  
  # Merge all aggregated datasets
  training_new = Reduce(function(x, y) merge(x, y, by = "household_key", all=TRUE), 
                        list(recent, freq_14, freq_21, freq_total, 
                             money, Grocery_past_14, Grocery_total, income, hh_comp, 
                             eggs_price, eggs_total, dv_eggs))
  training_new[is.na(training_new)] = 0
  
  training_new = subset(training_new, select = -c(day, quantity, net_spend_amt))
  
  return(training_new)
}

training_hh = dataAggregate(trainingSplit)
testing_hh = dataAggregate(testingSplit)

# Remove Unnecessary columns
training_hh = subset(training_hh, select = -c(amt_spent_eggs, quantity_eggs))
testing_hh = subset(testing_hh, select = -c(amt_spent_eggs, quantity_eggs))

#### Modeling
## Logistic regression
covariates = names(training_hh)[2:13]
 
logitReg = vector("list", 12)
for(ii in 1:12){
  logitReg[[ii]] = glm(as.formula(paste("dv_eggs ~ ", paste(covariates[1:(13-ii)], collapse = "+"))), 
                  data = training_hh, family = binomial)
}

stargazer(logitReg, type = "text", digits = 3, dep.var.labels = c("dv\\_eggs"),
          title = "Stepwise Logistic Regression")

## Make predictions
PredLogit = vector("list", 12)
for(jj in 1:12){
  PredLogit[[jj]] = predict(logitReg[[jj]], newdata = testing_hh, type = "response")
}

Pred.df = PredLogit %>% data.frame()
names(Pred.df) = sapply(1:12, function(x) paste0("Prob", x))

Pred.df = cbind(testing_hh, Pred.df)

## Fit linear regression of test outcomes on predicted probability 
with(Pred.df, {
     plot(Prob1, dv_eggs)
     abline(lm(dv_eggs ~ Prob1))
     abline(lm(dv_eggs ~ Prob2))
     abline(lm(dv_eggs ~ Prob3))
     abline(lm(dv_eggs ~ Prob4))
     abline(lm(dv_eggs ~ Prob5))
     abline(lm(dv_eggs ~ Prob6))
     abline(lm(dv_eggs ~ Prob7))
     abline(lm(dv_eggs ~ Prob8))
     abline(lm(dv_eggs ~ Prob9))
     abline(lm(dv_eggs ~ Prob10))
     abline(lm(dv_eggs ~ Prob11))
     abline(lm(dv_eggs ~ Prob12))})

##### Aggregate data Function
dataAggregate2 = function(data, data_as_string = "trans"){
  
  ## Recency
  recent = aggregate(day ~ household_key, data = data, max)
  recent$visited_in <- 704 - recent$day 
  
  data_cl = data
  
  ## Frequency
  # num of visits per household in the past 7-14 days
  freq_14 <- sqldf(paste("select household_key, count(distinct(basket_id))
                         from", data_as_string,
                         "where day between 698 and 704
                         group by household_key"))
  names(freq_14) <- c("household_key", "freq_14")
  # # of visits per household in the past 14-21 days - ONLY use this to fit DV of day 698-704
  freq_21 <- sqldf(paste("select household_key, count(distinct(basket_id))
                         from", data_as_string,
                         "where day between 691 and 697
                         group by household_key"))
  names(freq_21) <- c("household_key", "freq_21")
  # frequency from the beginning of time
  freq_total <- sqldf(paste("select household_key, count(distinct(basket_id))
                            from", data_as_string,
                            "group by household_key"))
  names(freq_total) <- c("household_key", "freq_total")
  
  ## Monetary value
  money = aggregate(cbind(quantity, net_spend_amt) ~ household_key, data = data, sum)
  money$price_per_amt = with(money, net_spend_amt/quantity)
  
  # #visits at Grocery Department per household in past 7-14 days - ONLY use this to fit DV of day 698-704
  Grocery_past_14 <- sqldf(paste("select household_key, count(*)
                                 from", data_as_string,
                                 "where day between 698 and 704 and department = 'GROCERY'
                                 group by household_key"))
  names(Grocery_past_14) <- c("household_key", "Grocery_past_14")
  
  # total visits to Grocery from the beginning of time
  Grocery_total <- sqldf(paste("select household_key, count(*)
                               from", data_as_string,
                               "group by household_key"))
  names(Grocery_total) <- c("household_key", "Grocery_total")
  
  # income per household
  income = aggregate(income_desc ~ household_key, data = data, 
                     unique, na.action = NULL)
  income$income_desc <- factor(income$income_desc, 
                               levels = c(levels(income$income_desc), "Other"))
  income$income_desc[is.na(income$income_desc)] <- "Other"
  
  # hh_comp category per household
  hh_comp = aggregate(hh_comp_desc ~ household_key, data = data, 
                      unique, na.action = NULL)
  hh_comp$hh_comp_desc[is.na(hh_comp$hh_comp_desc)] <- "Unknown"
  hh_comp$hh_comp_desc <- as.character(hh_comp$hh_comp_desc)
  hh_comp$hh_comp <- ifelse( (hh_comp$hh_comp_desc == "1 Adult Kids" | hh_comp$hh_comp_desc == "2 Adults Kids"), "With Kids",
                             ifelse( (hh_comp$hh_comp_desc == "2 Adults No Kids"),  "No Kids", 
                                     ifelse( (hh_comp$hh_comp_desc == "Single Female" | hh_comp$hh_comp_desc == "Single Male"), 'Single', "Unknown")))
  hh_comp$hh_comp <- factor(hh_comp$hh_comp, levels = c("Unknown", "Single", "No Kids", "With Kids"))
  
  # Average price paid for eggs per household
  eggs_price <- sqldf(paste("select household_key, sum(net_spend_amt), sum(quantity)
                     from", data_as_string,
                            "where commodity_desc = 'EGGS'
                     group by household_key"))
  names(eggs_price) <- c("household_key", "amt_spent_eggs", "quantity_eggs")
  eggs_price$price_paid_eggs <- eggs_price$amt_spent_eggs/eggs_price$quantity_eggs
  
  # total eggs purchased from the beginning of time
  eggs_total <- data[,c("household_key", "quantity", "commodity_desc")]
  eggs_total <- sqldf(paste("select household_key, sum(quantity)
                     from", data_as_string,
                            "where commodity_desc = 'EGGS'
                     group by household_key"))
  names(eggs_total) <- c("household_key", "eggs_total")
  
  # Merge all aggregated datasets
  training_new = Reduce(function(x, y) merge(x, y, by = "household_key", all=TRUE), 
                        list(recent, freq_14, freq_21, freq_total, 
                             money, Grocery_past_14, Grocery_total, income, hh_comp, 
                             eggs_price, eggs_total))
  training_new[is.na(training_new)] = 0
  
  training_new = subset(training_new, select = -c(day, quantity, net_spend_amt))
  
  return(training_new)
}

## Final model of choice
FinalModel =  glm(dv_eggs ~  visited_in + freq_14 + freq_21 + freq_total + 
                   price_paid_eggs + Grocery_total + hh_comp + eggs_total, 
                 data = training_hh, family = "binomial")

## Fit Selected model on Full dataset
FullAggregate = dataAggregate2(trans)
FinalProb = predict(FinalModel, newdata = FullAggregate, type = "response")

ProbTable = data.frame(Household_id = FullAggregate$household_key, Probabilities = FinalProb)

write.csv(ProbTable, "ProbTable.csv")
