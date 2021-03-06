source('scripts/library_loader.R')
source("scripts/utils.R")

nb_big_nohash <<- readRDS("Data/correct_model_no_hashtags.rds")

## load the list of the most frequent words, if the pre-computed file exists.
## If it doesn't exist, we'll need to recompute it. This will take a while.
if (!file.exists("Data/feature-sets/F0_most_frequent_words.rds")){
  t_all <- initializeData(N = -1, splitt = F)
  tm_ <- unique(as.matrix(getTermMatrixWithTM(t = t_all[1:100000,], time_frame = -1, sparsity = 0.9999999999, tfidf = weightTfIdf)))
  mostFrequentWords = as.character(getOverallMostFrequentWords(tm_)$word)
  write_rds(x = mostFrequentWords, file = "Data/feature-sets/F0_most_frequent_words.rds", compress = "xz")
} else {
  mostFrequentWords <- read_rds(file = "Data/feature-sets/F0_most_frequent_words.rds") 
}

t <- initializeData()

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

dir.create(path = "Data/feature-sets", showWarnings = F)
saveRDS(train, file = "Data/feature-sets/F0_improved_data.rds", compress = "xz")

#################################################
## this has the problem so far that there are multiple rows in the training set with the same description.
## next, remove these duplicates and assign them the majority class
train <- readRDS(file = "Data/feature-sets/F0_improved_data.rds")
train$Author <- NULL
train$Period <- NULL

## there are identical representations of with different NextLabel
T_agg <- train %>%
  group_by_at(setdiff(names(train), "NextLabel")) %>%
  summarise(nr_of_posts = n(), labels = paste0(NextLabel, collapse = " "), .groups = "drop")

## compute the majority label from the duplicated data
for(i in 1 : dim(T_agg)[1]){
  test = as.numeric(unlist(regmatches(T_agg[i,]$labels, gregexpr("[[:digit:]]+", T_agg[i,]$labels))))
  T_agg[i,"NextLabel"] =  as.numeric(names(which.max(table(test))))
}

## remove intermediary fields
T_agg$nr_of_posts = NULL
T_agg$labels = NULL

## write down training dataset
write.csv(T_agg, "Data/feature-sets/F0_improved_data.csv")
