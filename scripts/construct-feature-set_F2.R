source('scripts/library_loader.R')
source("scripts/utils.R")

## read the data.frame containing all the data
reddit <- readRedditData()

## add the number of direct comments which were not written by my author
res <- mclapply(X = reddit$Entry.ID, FUN = function(entry){
  myauthor <- reddit$Author[reddit$Entry.ID == entry]
  reddit %>%
    filter(Parent.ID == entry, Author != myauthor) %>%
    group_by(Stance) %>%
    tally()
}, mc.preschedule = T, mc.cores = detectCores()) 
reddit$Direct.Comments.A <- lapply(X = res, function(r) {
  ifelse( test = 0 %in% r$Stance, yes = r$n[r$Stance == 0], no = 0)
}) %>% as.numeric()
reddit$Direct.Comments.B <- lapply(X = res, function(r) {
  ifelse( test = 1 %in% r$Stance, yes = r$n[r$Stance == 1], no = 0)
}) %>% as.numeric()
reddit$Direct.Comments.N <- lapply(X = res, function(r) {
  ifelse( test = 2 %in% r$Stance, yes = r$n[r$Stance == 2], no = 0)
}) %>% as.numeric()

## add the number of ALL comments (direct and indirect) to the current post
## except direct comments authored by my own user
res <- mclapply(X = reddit$Entry.ID, FUN = function(entry){
  myauthor <- reddit$Author[reddit$Entry.ID == entry]
  allComms <- getAllComments(entry = entry, reddit = reddit)
  allComms %>%
    group_by(Stance) %>%
    tally(Parent.ID != entry | Author != myauthor)
}, mc.preschedule = T, mc.cores = detectCores())
reddit$All.Comments.A <- lapply(X = res, function(r) {
  ifelse( test = 0 %in% r$Stance, yes = r$n[r$Stance == 0], no = 0)
}) %>% as.numeric()
reddit$All.Comments.B <- lapply(X = res, function(r) {
  ifelse( test = 1 %in% r$Stance, yes = r$n[r$Stance == 1], no = 0)
}) %>% as.numeric()
reddit$All.Comments.N <- lapply(X = res, function(r) {
  ifelse( test = 2 %in% r$Stance, yes = r$n[r$Stance == 2], no = 0)
}) %>% as.numeric()

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
              CSA = sum(unlist(lapply(X = Parent.ID, FUN = function(comm) { reddit$Stance[reddit$Entry.ID == comm] == 0})), na.rm = T),
              CSB = sum(unlist(lapply(X = Parent.ID, FUN = function(comm) { reddit$Stance[reddit$Entry.ID == comm] == 1})), na.rm = T),
              CSN = sum(unlist(lapply(X = Parent.ID, FUN = function(comm) { reddit$Stance[reddit$Entry.ID == comm] == 2})), na.rm = T),
              # CS = sum(!is.na(Comment.ID)),
              # ID = sum(is.na(Comment.ID)),
              RDA1 = quantile(Direct.Comments.A)[1],
              RDA2 = quantile(Direct.Comments.A)[2],
              RDA3 = quantile(Direct.Comments.A)[3],
              RDA4 = quantile(Direct.Comments.A)[4],
              RDA5 = quantile(Direct.Comments.A)[5],
              RDA_sum = sum(Direct.Comments.A),
              RDB1 = quantile(Direct.Comments.B)[1],
              RDB2 = quantile(Direct.Comments.B)[2],
              RDB3 = quantile(Direct.Comments.B)[3],
              RDB4 = quantile(Direct.Comments.B)[4],
              RDB5 = quantile(Direct.Comments.B)[5],
              RDB_sum = sum(Direct.Comments.B),
              RDN1 = quantile(Direct.Comments.N)[1],
              RDN2 = quantile(Direct.Comments.N)[2],
              RDN3 = quantile(Direct.Comments.N)[3],
              RDN4 = quantile(Direct.Comments.N)[4],
              RDN5 = quantile(Direct.Comments.N)[5],
              RDN_sum = sum(Direct.Comments.N),
              RA1 = quantile(All.Comments.A)[1],
              RA2 = quantile(All.Comments.A)[2],
              RA3 = quantile(All.Comments.A)[3],
              RA4 = quantile(All.Comments.A)[4],
              RA5 = quantile(All.Comments.A)[5],
              RA_sum = sum(All.Comments.A),
              RB1 = quantile(All.Comments.B)[1],
              RB2 = quantile(All.Comments.B)[2],
              RB3 = quantile(All.Comments.B)[3],
              RB4 = quantile(All.Comments.B)[4],
              RB5 = quantile(All.Comments.B)[5],
              RB_sum = sum(All.Comments.B),
              RN1 = quantile(All.Comments.N)[1],
              RN2 = quantile(All.Comments.N)[2],
              RN3 = quantile(All.Comments.N)[3],
              RN4 = quantile(All.Comments.N)[4],
              RN5 = quantile(All.Comments.N)[5],
              RN_sum = sum(All.Comments.N),
              CurrentLabel = unique(Stance)
              )
  next_authors <- T_next %>%
    group_by(Author) %>%
    summarize(NextLabel = unique(Stance))
  common_authors <- inner_join(x = common_authors, y = next_authors, by = "Author")
}, mc.preschedule = T, mc.cores = min(max(reddit$period)-1, detectCores())))

dir.create(path = "Data/feature-sets", showWarnings = F)
saveRDS(train, file = "Data/feature-sets/F2_improved_data.rds", compress = "xz")

###########################################
## this has the problem so far that there are multiple rows in the training set with the same description.
## next, remove these duplicates and assign them the majority class
train <- readRDS(file = "Data/feature-sets/F2_improved_data.rds")
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
write.csv(T_agg, "Data/feature-sets/F2_improved_data.csv")