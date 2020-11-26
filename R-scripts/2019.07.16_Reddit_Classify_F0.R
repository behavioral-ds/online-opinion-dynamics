source('./library_loader.R')
source("./utils.R")
library(data.table)

nb_big_nohash <<- readRDS("Data/correct_model_no_hashtags.rds")

# t_all <- initializeData(N = -1, splitt = F)
# tm_ <- unique(as.matrix(getTermMatrixWithTM(t = t_all[1:100000,], time_frame = -1, sparsity = 0.9999999999, tfidf = weightTfIdf)))
# mostFrequentWords = as.character(getOverallMostFrequentWords(tm_)$word)
# save(mostFrequentWords, file = "Data/feature-sets/F0_most_frequent_words.RData")

## if they exist, load the list of the most frequent words. This will take a while. If they don't exist, you'll need to re-run the above.
load(file = "Data/feature-sets/F0_most_frequent_words.RData")

t <- initializeData()

# train = data.frame()

# for (p in 1 : (length(t) - 1)) {
train <- rbindlist(mclapply(X = 2:length(t)-1, FUN = function(p){
  print(paste("Period", p))
  t[[p]]$Content = str_replace_all(str_replace_all(t[[p]]$Content, "&gt;.*\n", ""), "\n", "")
  t[[p+1]]$Content = str_replace_all(str_replace_all(t[[p+1]]$Content, "&gt;.*\n", ""), "\n", "")
  
  T_ = t[[p]]
  colnames(T_)[3] = "text"
  
  T_rez1 = getAggregatePredictResults(t[[p]])
  T_rez2 = getAggregatePredictResults(t[[p+1]])
  
  common_authors = intersect(T_rez1$Author, T_rez2$Author)
  
  T1_common_authors = T_rez1[T_rez1$Author %in% common_authors,]
  T2_common_authors = T_rez2[T_rez2$Author %in% common_authors,]
  
  T1_common_authors$prediction = ifelse(T1_common_authors$leave_probability >= 0.75, 1, ifelse(T1_common_authors$leave_probability >= 0.25, 2, 0))
  T2_common_authors$prediction = ifelse(T2_common_authors$leave_probability >= 0.75, 1, ifelse(T2_common_authors$leave_probability >= 0.25, 2, 0))
  
  tm_ = as.matrix(getTermMatrixWithTM(t = t, time_frame = p, sparsity = 0.99999999999999, tfidf = weightTfIdf))
  tm_ = setDT(as.data.frame(tm_[common_authors,mostFrequentWords[1:100]]), keep.rownames = "Author")[]
  
  T1_features = merge(tm_, T1_common_authors[,c(1, 5)])
  colnames(T1_features)[dim(T1_features)[2]] = c("current_stance")
  
  T1_features = merge(T1_features, T2_common_authors[,c(1, 5)])
  colnames(T1_features)[dim(T1_features)[2]] = c("next_stance")
  
  T1_features$Period <- p
  
  return(T1_features)
  # train = rbind(train, T1_features)
}, mc.preschedule = T, mc.cores = min(length(t)-1, detectCores())))

columns <- colnames(train)
colnames(train) <- c("Author", paste0("W", 1 : (dim(train)[2] - 4)), "CurrentLabel", "NextLabel", "Period")

dim(unique(train))
saveRDS(train, file = "Data/feature-sets-NEW/F0_improved_data.rds", compress = "xz")

#################################################
## this has the problem so far that there are multiple rows in the training set with the same description.
## next, remove these duplicates and assign them the majority class
train <- readRDS(file = "Data/feature-sets-NEW/F0_improved_data.rds")
train$Author <- NULL
train$Period <- NULL

## there are identical representations of with different NextLabel
T_agg = train %>%
  group_by_at(setdiff(names(train), "NextLabel")) %>%
  summarise(nr_of_posts = n(), labels = paste0(NextLabel, collapse = " "))

## this is dodgy. Why remove the "2", "2 2" and "2 2 2" ?
# T_agg_1 = T_agg[T_agg$CurrentLabel != 2 | (T_agg$labels != "2" & T_agg$labels != "2 2"& T_agg$labels != "2 2 2"),]
T_agg_1 <- T_agg

## compute the majority label from the duplicated data
for(i in 1 : dim(T_agg_1)[1]){
  test = as.numeric(unlist(regmatches(T_agg_1[i,]$labels, gregexpr("[[:digit:]]+", T_agg_1[i,]$labels))))
  T_agg_1[i,"NextLabel"] =  as.numeric(names(which.max(table(test))))
}

## remove intermediary fields
T_agg_1$nr_of_posts = NULL
T_agg_1$labels = NULL

## the distributions don't seem to match...
hist(T_agg_1$NextLabel)
hist(train$NextLabel)

## extract the training part (no NextLabel column)
## MAR: I'm not convinced, as the Python code requires the current feature.
# unq_train <- T_agg_1 %>%
# select(-NextLabel)
unq_train <- T_agg_1

## write down training dataset
write.csv(unq_train, "Data/feature-sets-NEW/F0_improved_data.csv")
write.csv(unq_train, "Python/RunClassifiers/feature-sets/F0_improved_data.csv")


