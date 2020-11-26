source('./library_loader.R')
source("./utils.R")


## load the stance predictor trained on Twitter -- note that you need quanteda v 1.4.3
## load the user classifier, it will be used in "utils.R::getPredictions"
# nb_big_nohash <<- readRDS("Data/correct_model_no_hashtags.rds")
# nb_big_nohash <- readRDS("Data/nb_with_hashtags_july.rds")

## read the data.frame containing all the data
reddit <- readRedditData()

## add the number of ALL comments (direct and indirect) to the current submission
res <- mclapply(X = unique(reddit$Submission.ID), FUN = function(entry){
  allComms <- rbind(reddit %>% filter(Entry.ID == entry),
                    getAllComments(entry = entry, reddit = reddit)
  )
  allComms %>%
    group_by(Stance) %>%
    tally()
}, mc.preschedule = T, mc.cores = detectCores())
result <- rbindlist(l = res, idcol = "Submission.ID")
submissions <- data.frame(Submission.ID = unique(reddit$Submission.ID))
submissions$All.Comments.A <- lapply(X = res, function(r) {
  ifelse( test = 0 %in% r$Stance, yes = r$n[r$Stance == 0], no = 0)
}) %>% as.numeric()
submissions$All.Comments.B <- lapply(X = res, function(r) {
  ifelse( test = 1 %in% r$Stance, yes = r$n[r$Stance == 1], no = 0)
}) %>% as.numeric()
submissions$All.Comments.N <- lapply(X = res, function(r) {
  ifelse( test = 2 %in% r$Stance, yes = r$n[r$Stance == 2], no = 0)
}) %>% as.numeric()
submissions$TC <- submissions$All.Comments.A + submissions$All.Comments.B + submissions$All.Comments.N
submissions$pA <- submissions$All.Comments.A / submissions$TC
submissions$pB <- submissions$All.Comments.B / submissions$TC
submissions$pN <- submissions$All.Comments.N / submissions$TC

colnames(train) = c("A1", "A2", "A3", "A4", "A5","B1", "B2", "B3", "B4", "B5", "N1", "N2", "N3", "N4", "N5", "CurrentLabel", "NextLabel")

train <- rbindlist(mclapply(X = 2:max(reddit$period)-1, FUN = function(p){
  print(paste("Period", p))
  
  ## get stance prediciton at this period and the next
  T_next <- reddit %>%
    filter(period == p+1)
  
  ## find authors that were active in both this period and the next
  common_authors <- reddit %>%
    filter(period == p, Author %in% T_next$Author) %>%
    group_by(Author) %>%
    summarise(Period = p,
              CurrentLabel = unique(Stance))
  
  res <- rbindlist(lapply(X = common_authors$Author, FUN = function(auth){
    df <- reddit %>%
      filter(Author == auth)
    apply(X = submissions[submissions$Submission.ID %in% unique(df$Submission.ID), c("pA", "pB", "pN")], MARGIN = 2, FUN = quantile) %>%
      as.data.frame() %>%
      stack() %>%
      mutate(nm = paste(ind, c("0%", "25%", "50%", "75%", "100%"), sep = "-")) %>%
      column_to_rownames(var = "nm") %>%
      select(values) %>%
      t() %>%
      as_tibble() %>%
      mutate(Author = auth) %>%
      select(Author, everything())
  }))
  next_authors <- T_next %>%
    group_by(Author) %>%
    summarize(NextLabel = unique(Stance))
  common_authors <- inner_join(x = common_authors, y = next_authors, by = "Author")
  common_authors <- inner_join(x = common_authors, y = res, by = "Author") %>%
    select(-CurrentLabel, -NextLabel, everything() )
  
}, mc.preschedule = T, mc.cores = min(max(reddit$period)-1, detectCores())))

saveRDS(train, file = "Data/feature-sets-NEW/F3_improved_data.rds", compress = "xz")

###########################################
## this has the problem so far that there are multiple rows in the training set with the same description.
## next, remove these duplicates and assign them the majority class
train <- readRDS(file = "Data/feature-sets-NEW/F3_improved_data.rds")
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
write.csv(unq_train, "Data/feature-sets-NEW/F3_improved_data.csv")
write.csv(unq_train, "Python/RunClassifiers/feature-sets/F3_improved_data.csv")


# for (p in 1:14) {
#   print(paste("Period", p))
#   t[[p]]$Content = str_replace_all(str_replace_all(t[[p]]$Content, "&gt;.*\n", ""), "\n", "")
#   t[[p+1]]$Content = str_replace_all(str_replace_all(t[[p+1]]$Content, "&gt;.*\n", ""), "\n", "")
#   
#   T_ = t[[p]]
#   colnames(T_)[3] = "text"
#   
#   T_rez1 = getAggregatePredictResults(t[[p]])
#   T_rez2 = getAggregatePredictResults(t[[p+1]])
#   
#   common_authors = intersect(T_rez1$Author, T_rez2$Author)
#   
#   T1_common_authors = T_rez1[T_rez1$Author %in% common_authors,]
#   T2_common_authors = T_rez2[T_rez2$Author %in% common_authors,]
#   
#   T1_common_authors$prediction = ifelse(T1_common_authors$leave_probability >= 0.75, 1, ifelse(T1_common_authors$leave_probability >= 0.25, 2, 0))
#   T2_common_authors$prediction = ifelse(T2_common_authors$leave_probability >= 0.75, 1, ifelse(T2_common_authors$leave_probability >= 0.25, 2, 0))
#   
#   r = getPredictions(T_)
#   T_$leave_prob = r$leave_prob
#   T_$prediction = ifelse(T_$leave_prob >= 0.75, 1, ifelse(T_$leave_prob >= 0.25, 2, 0))
#   
#   for (u in 1 : length(common_authors)) {
#     # View(T_[T_$Author == common_authors[u],])
#     ## all the posts in which my user contributed
#     submissions = unique(T_[T_$Author == common_authors[u],]$Submission.ID)
#     A = c()
#     B = c()
#     N = c()
#     
#     for (i in 1 : length(submissions)) {
#       submission = subset(T_, T_$Submission.ID == submissions[i])
#       
#       n = dim(submission)[1]
#       pA = sum(submission$prediction == 0) / n
#       pB = sum(submission$prediction == 1) / n
#       pN = sum(submission$prediction == 2) / n
#       
#       # if(pA != 0 && pB != 0) {
#       #   print(submission$Submission.ID)
#       # }
#       
#       A = c(A,pA)
#       B = c(B,pB)
#       N = c(N,pN)
#     }
#     qA = quantile(A)
#     qB = quantile(B)
#     qN = quantile(N)
#     train = rbind(train, c(qA, qB, qN, T1_common_authors[u,]$prediction, T2_common_authors[u,]$prediction))
#   }
# }
# train = as.data.frame(train)
# save(train, file = "Data/feature-sets/F3_improved_data.RData")
# 
# #################################################
# load("Data/feature-sets/F3_improved_data.RData")
# 
# T_agg = train %>%
#   group_by(A1, A2, A3, A4, A5,B1, B2, B3, B4, B5, N1, N2, N3, N4, N5, CurrentLabel) %>%
#   summarise(nr_of_posts = n(), labels = paste0(NextLabel, collapse = " "))
# 
# T_agg_1 = T_agg[T_agg$CurrentLabel != 2 | (T_agg$labels != "2" & T_agg$labels != "2 2" & T_agg$labels != "2 2 2"),]
# 
# T_agg_1 = T_agg_1[T_agg_1$CurrentLabel != 1 | (T_agg_1$labels != "2"),]
# 
# T_agg_1 = T_agg_1[T_agg_1$CurrentLabel != 0 | (T_agg_1$labels != "2"),]
# 
# 
# for(i in 1 : dim(T_agg_1)[1]){
#   test = as.numeric(unlist(regmatches(T_agg_1[i,]$labels, gregexpr("[[:digit:]]+", T_agg_1[i,]$labels))))
#   T_agg_1[i,"NextLabel"] =  as.numeric(names(which.max(table(test))))
# }
# 
# T_agg_1$nr_of_posts = NULL
# T_agg_1$labels = NULL
# 
# hist(T_agg_1$NextLabel)
# 
# unq_train = T_agg_1[rownames(unique(subset(T_agg_1, select = -c(17)))),]
# 
# write.csv(unq_train, file = "F3_improved_data.csv")
#   