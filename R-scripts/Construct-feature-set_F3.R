source('R-scripts/library_loader.R')
source("R-scripts/utils.R")

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

train <- rbindlist(mclapply(X = 2:max(reddit$period)-1, FUN = function(p){
  print(paste("Period", p))
  
  ## get stance prediction at this period and the next
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

# colnames(train) = c("A1", "A2", "A3", "A4", "A5","B1", "B2", "B3", "B4", "B5", "N1", "N2", "N3", "N4", "N5", "CurrentLabel", "NextLabel")

saveRDS(train, file = "Data/feature-sets/F3_improved_data.rds", compress = "xz")

###########################################
## this has the problem so far that there are multiple rows in the training set with the same description.
## next, remove these duplicates and assign them the majority class
train <- readRDS(file = "Data/feature-sets/F3_improved_data.rds")
train$Author <- NULL
train$Period <- NULL

## there are identical representations of with different NextLabel
T_agg = train %>%
  group_by_at(setdiff(names(train), "NextLabel")) %>%
  summarise(nr_of_posts = n(), labels = paste0(NextLabel, collapse = " "))

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

## write down training dataset
write.csv(T_agg_1, "Data/feature-sets/F3_improved_data.csv")
# write.csv(T_agg_1, "Python/RunClassifiers/feature-sets/F3_improved_data.csv")
