library(haven)
library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(stringr)

accept <- read_sas("C:/Users/Jerry/Documents/MSA18/Financial_Analytics/HW/accepted_customers.sas7bdat")
accept <- data.frame(accept)

# combine very low freq levels
table(accept$CARDS)
accept[accept$CARDS %in% c("American Express", "VISA mybank", "VISA Others"), "CARDS"] <- "other"

# clean the string variables so they can be binned
accept$PRODUCT <- gsub("[,.]", "", accept$PRODUCT)
accept$PROF <- as.factor(gsub("[,.-]", "", accept$PROF))
accept[accept$PROF == "", "PROF"] <- "Others"
accept[accept$TEL == 0, "TEL"] <- 2
# the cutoff IV of significant variables
sig_cutoff <- 0.1

num_var <- c()
fac_var <- c()
for(j in 1:ncol(accept)) {
  level <- length(unique(accept[, j]))
  if(colnames(accept)[j] == "X_freq_"){
    num_var <- c(num_var, colnames(accept)[j])
    next
  } else if(level >= 5 & class(accept[, j]) == "numeric"){
    num_var <- c(num_var, colnames(accept)[j]) 
  } else if(level < 5 | class(accept[, j]) == "character"){
    fac_var <- c(fac_var, colnames(accept)[j]) 
    accept[, j] <- as.factor(accept[, j])
  } 
}

accept$GB <- as.numeric(accept$GB) - 1
accept$good <- 1 - accept$GB
# separate the trainning and test data
set.seed(789)
train_id <- sample(seq_len(nrow(accept)), size = floor(0.7*nrow(accept)))
train <- accept[train_id, ]
test <- accept[-train_id, ]
unique(train$PROF)
#iv_summary <- smbinning.sumiv(df = train, y = "good")
#iv_summary

# bin the variables
bin_result <- list()
for(j in 1:ncol(train)) {
  if(class(train[, j]) == "numeric") {
    bin_result[[colnames(train)[j]]] <- smbinning(df = train, y = "good", x = colnames(train)[j])
  } else {
    bin_result[[colnames(train)[j]]] <- smbinning.factor(df = train, y = "good", x = colnames(train)[j])
  }
}

# select the significant variables (iv > sig_cutoff)
sig_var <- list()
for(i in 1:length(bin_result)){
  if(class(bin_result[[i]]) == "list" & unlist(bin_result[[i]][2]) >= sig_cutoff) {
    sig_var[[colnames(train)[i]]] <- bin_result[[colnames(train)[i]]]
  }
}

# assign the bins and WOE into the data
for (i in 1:length(sig_var)){
  if(sig_var[[i]]$x %in% num_var){
    train <- smbinning.gen(df = train, ivout = sig_var[[i]], chrname = paste(sig_var[[i]]$x, "_bin", sep = ""))
  } else {
    train <- smbinning.factor.gen(df = train, ivout = sig_var[[i]], chrname = paste(sig_var[[i]]$x, "_bin", sep = ""))
  }
}

woe_vec <- c()
for (j in 1:length(sig_var)) {
  for (i in 1:nrow(train)) {
    bin_name <- paste(sig_var[[j]]$x, "_bin", sep = "")
    bin <- substr(train[[bin_name]][i], 2, 2)
    
    woe_name <- paste(sig_var[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(sig_var[[j]]$ivtable)[1] - 1
      train[[woe_name]][i] <- sig_var[[j]]$ivtable[bin, "WoE"]
    } else {
      train[[woe_name]][i] <- sig_var[[j]]$ivtable[bin, "WoE"]
    }
  }
  woe_vec <- c(woe_vec, woe_name)
}

woe_vec <- woe_vec[c(1,3,4,5,7)]

f <- paste("GB", "~", paste(woe_vec, collapse = "+"))

# build the logistic regression model
initial_score <- glm(data = train, formula(f),
                     weights = train$X_freq_, family = "binomial")

summary(initial_score)


# Evaluate the Initial Model - Training Data #
train$pred <- initial_score$fitted.values

smbinning.metrics(dataset = train, prediction = "pred", actualclass = "GB", report = 1)
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "GB", plot = "ks")
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "GB", plot = "auc")

#Evaluate the Initial Model - Testing Data #
for (i in 1:length(sig_var)){
  if (sig_var[[i]]$x %in% num_var) {
    test <- smbinning.gen(df = test, ivout = sig_var[[i]], chrname = paste(sig_var[[i]]$x, "_bin", sep = ""))
  } else {
    test <- smbinning.factor.gen(df = test, ivout = sig_var[[i]], chrname = paste(sig_var[[i]]$x, "_bin", sep = ""))
  }
}

for (j in 1:length(sig_var)) {
  print(j)
  for (i in 1:nrow(test)) {
    bin_name <- paste(sig_var[[j]]$x, "_bin", sep = "")
    bin <- substr(test[[bin_name]][i], 2, 2)
    
    woe_name <- paste(sig_var[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(sig_var[[j]]$ivtable)[1] - 1
      test[[woe_name]][i] <- sig_var[[j]]$ivtable[bin, "WoE"]
    } else {
      test[[woe_name]][i] <- sig_var[[j]]$ivtable[bin, "WoE"]
    }
  }
}
test$pred <- predict(initial_score, newdata=test, type='response')

smbinning.metrics(dataset = test, prediction = "pred", actualclass = "GB", report = 1)
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "GB", plot = "ks")
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "GB", plot = "auc")

#accept$pred <- predict(initial_score, newdata=accept, type='response')
#smbinning.metrics(dataset = test, prediction = "pred", actualclass = "bad", report = 1)

# Add Scores to Initial Model #
pdo <- 50
score <- 500
odds <- 20
factor <- pdo/log(2)
offset <- score - factor*log(odds)
var_names <- names(initial_score$coefficients[-1])

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- train[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  train[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*factor + offset/nvar
}

colini <- (ncol(train)-nvar + 1)
colend <- ncol(train)
train$Score <- rowSums(train[, colini:colend])

hist(train$Score, breaks = 50, main = "Distribution of Scores", xlab = "Score")

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- test[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  test[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*factor + offset/nvar
}

colini <- (ncol(test)-nvar + 1)
colend <- ncol(test)
test$Score <- rowSums(test[, colini:colend])

accepts_scored <- rbind(train, test)
hist(accepts_scored$Score, breaks = 50, main = "Distribution of Scores", xlab = "Score")

# Reject Inference
reject <- read_sas("C:/Users/Jerry/Documents/MSA18/Financial_Analytics/HW/rejected_customers.sas7bdat")
reject <- data.frame(reject)
reject[reject$CARDS %in% c("American Express", "VISA mybank", "VISA Others", "VISA Citibank"), "CARDS"] <- "other"
reject$INC <- NULL
reject$INC1 <- NULL
reject$PRODUCT <- gsub("[,.]", "", reject$PRODUCT)
reject$PROF <- as.factor(gsub("[,.-]", "", reject$PROF))
reject[reject$TEL == 0, "TEL"] <- 2

for(j in 1:ncol(reject)) {
  if(colnames(reject)[j] %in% fac_var){
    reject[, j]<- as.factor(reject[, j])
  }
}

# adjust the bin range
rejects_scored <- reject
for(i in 1:length(sig_var)) {
  if(sig_var[[i]]$x %in% num_var) {
    sig_var[[i]]$bands[1] <- min(rejects_scored[[sig_var[[i]]$x]], na.rm = TRUE)
    sig_var[[i]]$bands[length(sig_var[[i]]$bands)] <- max(rejects_scored[[sig_var[[i]]$x]], na.rm = TRUE)
  }
}
# assign bin
for(i in 1:length(sig_var)) {
  if (sig_var[[i]]$x %in% num_var) {
    rejects_scored <- smbinning.gen(df = rejects_scored, ivout = sig_var[[i]], chrname = paste(sig_var[[i]]$x, "_bin", sep = ""))
  } else {
    rejects_scored <- smbinning.factor.gen(df = rejects_scored, ivout = sig_var[[i]],
                                           chrname = paste(sig_var[[i]]$x, "_bin", sep = ""))
  }
}


# assign WOE for each bin
for (j in 1:length(sig_var)) {
  print(j)
  for (i in 1:nrow(rejects_scored)) {
    bin_name <- paste(sig_var[[j]]$x, "_bin", sep = "")
    bin <- substr(rejects_scored[[bin_name]][i], 2, 2)
    
    woe_name <- paste(sig_var[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(sig_var[[j]]$ivtable)[1] - 1
      rejects_scored[[woe_name]][i] <- sig_var[[j]]$ivtable[bin, "WoE"]
    } else {
      rejects_scored[[woe_name]][i] <- sig_var[[j]]$ivtable[bin, "WoE"]
    }
  }
}

pdo <- 50
score <- 500
odds <- 20
factor <- pdo/log(2)
offset <- score - factor*log(odds)
var_names <- names(initial_score$coefficients[-1])

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- rejects_scored[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  rejects_scored[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*factor + offset/nvar
}

colini <- (ncol(rejects_scored)-nvar + 1)
colend <- ncol(rejects_scored)
rejects_scored$Score <- rowSums(rejects_scored[, colini:colend])

reject_rate <- nrow(reject) / (nrow(accept) + nrow(reject))
good_weight <- max(accept$X_freq_)

# Reject Inference - Hard Cut-off #
rejects_scored$pred <- predict(initial_score, newdata=rejects_scored, type='response')

reject$GB <- as.numeric(rejects_scored$pred > 0.035)
reject$X_freq_ <- ifelse(reject$GB != 1, good_weight*reject_rate, reject_rate)
reject$good <- abs(reject$GB - 1)

comb_hard <- rbind(accept, reject) # New Combined Data Set #

# Reject Inference - Parcelling #
accepts_scored <- rbind(train, test)
hist(accepts_scored$Score, breaks = 50, main = "Distribution of Scores", xlab = "Score")

parc <- seq(400, 720, 40)
accepts_scored$Score_parc <- cut(accepts_scored$Score, breaks = parc)
rejects_scored$Score_parc <- cut(rejects_scored$Score, breaks = parc)

table(accepts_scored$Score_parc, accepts_scored$GB)

parc_perc <- table(accepts_scored$Score_parc, accepts_scored$GB)[,2]/rowSums(table(accepts_scored$Score_parc, accepts_scored$GB))

reject$GB <- 0

rej_bump <- 1.2

for(i in 1:(length(parc)-1)) {
  for(j in 1:length(rejects_scored$Score)) {
    if((rejects_scored$Score[j] > parc[i]) & 
       (rejects_scored$Score[j] <= parc[i+1]) & 
       (runif(n = 1, min = 0, max = 1) < (rej_bump*parc_perc[i]))) {
      reject$GB[j] <- 1
    }
  }
}

table(rejects_scored$Score_parc, reject$GB)
  
reject$X_freq_ <- ifelse(reject$GB != 1, good_weight*reject_rate, reject_rate)
reject$good <- abs(reject$GB - 1)

comb_parc <- rbind(accept, reject) # New Combined Data Set #

# Reject Inference - Fuzzy Augmentation #
rejects_scored$pred <- predict(initial_score, newdata=rejects_scored, type='response')

rejects_g <- reject
rejects_b <- reject

rejects_g$GB <- 0
rejects_g$X_freq_ <- (1-rejects_scored$pred)*good_weight*reject_rate
rejects_g$good <- 1

rejects_b$GB <- 1
rejects_b$X_freq_ <- (rejects_scored$pred)*reject_rate
rejects_b$good <- 0

comb_fuzz <- rbind(accept, rejects_g, rejects_b)
View(rejects_b)

# Build Final Scorecard Model #
comb <- comb_parc # Select which data set you want to use from above techniques #
comb <- comb_hard
comb <- comb_fuzz
set.seed(222)
train_id <- sample(seq_len(nrow(comb)), size = floor(0.7*nrow(comb)))

train_comb <- comb[train_id, ]
test_comb <- comb[-train_id, ]

#iv_summary <- smbinning.sumiv(df = train_comb, y = "good")

#smbinning.sumiv.plot(iv_summary)
#iv_summary

# bin the variables
bin_result <- list()
for(j in 1:ncol(train_comb)) {
  if(class(train_comb[, j]) == "numeric") {
    bin_result[[colnames(train_comb)[j]]] <- smbinning(df = train_comb, y = "good", x = colnames(train_comb)[j])
  } else {
    bin_result[[colnames(train_comb)[j]]] <- smbinning.factor(df = train_comb, y = "good", x = colnames(train_comb)[j])
  }
}

# select the significant variables (iv > sig_cutoff)
sig_var <- list()
for(i in 1:length(bin_result)){
  if(class(bin_result[[i]]) == "list" & unlist(bin_result[[i]][2]) >= sig_cutoff) {
    sig_var[[colnames(train_comb)[i]]] <- bin_result[[colnames(train_comb)[i]]]
  }
}

# assign the bins and WOE into the data
for (i in 1:length(sig_var)){
  if(sig_var[[i]]$x %in% num_var){
    train_comb <- smbinning.gen(df = train_comb, ivout = sig_var[[i]], chrname = paste(sig_var[[i]]$x, "_bin", sep = ""))
  } else {
    train_comb <- smbinning.factor.gen(df = train_comb, ivout = sig_var[[i]], chrname = paste(sig_var[[i]]$x, "_bin", sep = ""))
  }
}

woe_vec <- c()
for (j in 1:length(sig_var)) {
  for (i in 1:nrow(train_comb)) {
    bin_name <- paste(sig_var[[j]]$x, "_bin", sep = "")
    bin <- substr(train_comb[[bin_name]][i], 2, 2)
    
    woe_name <- paste(sig_var[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(sig_var[[j]]$ivtable)[1] - 1
      train_comb[[woe_name]][i] <- sig_var[[j]]$ivtable[bin, "WoE"]
    } else {
      train_comb[[woe_name]][i] <- sig_var[[j]]$ivtable[bin, "WoE"]
    }
  }
  woe_vec <- c(woe_vec, woe_name)
}
woe_vec
woe_vec <- woe_vec[c(1,2,4,5,7)]
# formula
f <- paste("GB", "~", paste(woe_vec, collapse = "+"))

# final model
final_score <- glm(data = train_comb, as.formula(f),
                     weights = train_comb$X_freq_, family = "binomial")

summary(final_score)

# Evaluate the Initial Model - Training Data #
train_comb$pred <- final_score$fitted.values

smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "GB", report = 1)
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "GB", plot = "ks")
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "GB", plot = "auc")

#Evaluate the Initial Model - test_combing Data #
for (i in 1:length(sig_var)){
  if (sig_var[[i]]$x %in% num_var) {
    test_comb <- smbinning.gen(df = test_comb, ivout = sig_var[[i]], chrname = paste(sig_var[[i]]$x, "_bin", sep = ""))
  } else {
    test_comb <- smbinning.factor.gen(df = test_comb, ivout = sig_var[[i]], chrname = paste(sig_var[[i]]$x, "_bin", sep = ""))
  }
}

for (j in 1:length(sig_var)) {
  for (i in 1:nrow(test_comb)) {
    bin_name <- paste(sig_var[[j]]$x, "_bin", sep = "")
    bin <- substr(test_comb[[bin_name]][i], 2, 2)
    
    woe_name <- paste(sig_var[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(sig_var[[j]]$ivtable)[1] - 1
      test_comb[[woe_name]][i] <- sig_var[[j]]$ivtable[bin, "WoE"]
    } else {
      test_comb[[woe_name]][i] <- sig_var[[j]]$ivtable[bin, "WoE"]
    }
  }
}
test_comb$pred <- predict(final_score, newdata=test_comb, type='response')

smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "GB", report = 1)
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "GB", plot = "ks")
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "GB", plot = "auc")


# Add Scores to Final Model #
pdo <- 50
score <- 500
odds <- 20
factor <- pdo/log(2)
offset <- score - factor*log(odds)
var_names <- names(final_score$coefficients[-1])

for(i in var_names) {
  beta <- final_score$coefficients[i]
  beta0 <- final_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- train_comb[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  train_comb[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*factor + offset/nvar
}

colini <- (ncol(train_comb)-nvar + 1)
colend <- ncol(train_comb)
train_comb$Score <- rowSums(train_comb[, colini:colend])

hist(train_comb$Score, breaks = 50, main = "Distribution of Scores", xlab = "Score")

for(i in var_names) {
  beta <- final_score$coefficients[i]
  beta0 <- final_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- test_comb[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  test_comb[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*factor + offset/nvar
}

colini <- (ncol(test_comb)-nvar + 1)
colend <- ncol(test_comb)
test_comb$Score <- rowSums(test_comb[, colini:colend])
hist(test_comb$Score, breaks = 50, main = "Distribution of Scores", xlab = "Score")

all_scored <- rbind(train_comb, test_comb)
hist(all_scored$Score, breaks = 50, main = "Distribution of Scores", xlab = "Score")

# AUC: 74.23%, sig_cutoff = 0.1, parcel
# AUC: 79.33%, sig_cutoff = 0.1, hard = .0603
# AUC: 79.9%, sig_cutoff = 0.1, hard = .05
# AUC: 80.78%, sig_cutoff = 0.1, hard = .03
# AUC: 80.78%, sig_cutoff = 0.1, hard = .035