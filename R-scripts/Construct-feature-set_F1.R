source('R-scripts/library_loader.R')
source("R-scripts/utils.R")

## load the stance predictor trained on Twitter -- note that you need quanteda v 1.4.3
## load the user classifier, it will be used in "utils.R::getPredictions"
# nb_big_nohash <<- readRDS("Data/correct_model_no_hashtags.rds")

## read the data.frame containing all the data
reddit <- readRedditData()

## add the number of direct comments which were not written by my author
res <- mclapply(X = reddit$Entry.ID, FUN = function(entry){
  myauthor <- reddit$Author[reddit$Entry.ID == entry]
  reddit %>%
    filter(Parent.ID == entry, Author != myauthor) %>%
    nrow()
}, mc.preschedule = T, mc.cores = detectCores()) %>% as.numeric()
reddit$Direct.Comments <- res

## add the number of ALL comments (direct and indirect) to the current post
## except direct comments authored by my own user
res <- mclapply(X = reddit$Entry.ID, FUN = function(entry){
  myauthor <- reddit$Author[reddit$Entry.ID == entry]
  allComms <- getAllComments(entry = entry, reddit = reddit)
  allComms %>%
    tally(Parent.ID != entry | Author != myauthor) %>% as.numeric
}, mc.preschedule = T, mc.cores = detectCores()) %>% as.numeric()
reddit$All.Comments <- res

train <- rbindlist(lapply(X = 2:max(reddit$period)-1, FUN = function(p){
  print(paste("Period", p))
  
  ## get stance prediciton at this period and the next
  # T_ = t[[p]]
  T_next <- reddit %>%
    filter(period == p+1)
  
  ## find authors that were active in both this period and the next
  common_authors <- reddit %>%
    filter(period == p, Author %in% T_next$Author) %>%
    group_by(Author) %>%
    summarise(Period = p,
              Total.Entries = n(),
              CS = sum(!is.na(Comment.ID)),
              ID = sum(is.na(Comment.ID)),
              RD1 = quantile(Direct.Comments)[1],
              RD2 = quantile(Direct.Comments)[2],
              RD3 = quantile(Direct.Comments)[3],
              RD4 = quantile(Direct.Comments)[4],
              RD5 = quantile(Direct.Comments)[5],
              R1 = quantile(All.Comments)[1],
              R2 = quantile(All.Comments)[2],
              R3 = quantile(All.Comments)[3],
              R4 = quantile(All.Comments)[4],
              R5 = quantile(All.Comments)[5],
              RD_sum = sum(Direct.Comments),
              R_sum = sum(All.Comments),
              CurrentLabel = unique(Stance))
  next_authors <- T_next %>%
    group_by(Author) %>%
    summarize(NextLabel = unique(Stance))
  common_authors <- inner_join(x = common_authors, y = next_authors, by = "Author")
  
}))

saveRDS(train, file = "Data/feature-sets/F1_improved_data.rds", compress = "xz")
#####################################

## this has the problem so far that there are multiple rows in the training set with the same description.
## next, remove these duplicates and assign them the majority class
train <- readRDS(file = "Data/feature-sets/F1_improved_data.rds")
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

## write down training dataset
write.csv(T_agg_1, "Data/feature-sets/F1_improved_data.csv")
# write.csv(T_agg_1, "Python/RunClassifiers/feature-sets/F1_improved_data.csv")

