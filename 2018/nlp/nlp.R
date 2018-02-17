if(.Platform$OS.type=="unix"){
  RAWmisc::UseRClone()
  RAWmisc::AllowFileManipulationFromInitialiseProject()
  
  if(dir.exists("/dropbox")){
    SHARED <- "/dropbox/analyses/results_shared/code_major/2018/nlp"
    RCLONE_SHARED <- NULL
  } else {
    SHARED <- "/tmp/results_shared/code_major/2018/nlp/"
    RCLONE_SHARED <- "data:/analyses/results_shared/code_major/2018/nlp/"
  }
  
  RAWmisc::InitialiseProject(
    HOME = "/git/code_major/2018/nlp/",
    RAW = "/tmp/data_raw/code_major/2018/nlp/",
    CLEAN = "/tmp/data_clean/code_major/2018/nlp",
    BAKED = "/tmp/results_baked/code_major/2018/nlp/",
    FINAL = "/tmp/results_final/code_major/2018/nlp/",
    SHARED = SHARED,
    RCLONE_RAW = "crypt:/data_raw/code_major/2018/nlp/",
    RCLONE_SHARED = RCLONE_SHARED
  )
}

library(data.table)
library(magrittr)
library(text2vec)
library(ggplot2)


RAWmisc::SaveProject()

data <- readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"vocab.xlsx"))
saveRDS(data,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"tedst.rds"))
setDT(data)
data[,id:=1:.N]
setkey(data,id)
set.seed(2017L)
all_ids = data$id
train_ids = sample(all_ids, round(nrow(data)*0.75))
test_ids = setdiff(all_ids, train_ids)
train = data[J(train_ids)]
test = data[J(test_ids)]

prep_fun = tolower
tok_fun = word_tokenizer

# training
it_train = itoken(train$freetext, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = train$id, 
                  progressbar = FALSE)
vocab = create_vocabulary(it_train, ngram = c(1L, 2L))
vocab = prune_vocabulary(vocab, term_count_min = 2, 
                         doc_proportion_max = 0.5)
bigram_vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, bigram_vectorizer)

# testing
it_test = test$freetext %>% 
  prep_fun %>% tok_fun %>% 
  itoken(ids = test$id, progressbar = FALSE)
dtm_test = create_dtm(it_test, bigram_vectorizer)

# model fitting
library(glmnet)
NFOLDS = 4
t1 = Sys.time()
glmnet_classifier = cv.glmnet(x = dtm_train, y = train[['isPositive']], 
                              family = 'binomial', 
                              # L1 penalty
                              alpha = 1,
                              # interested in the area under ROC curve
                              type.measure = "auc",
                              # 5-fold cross-validation
                              nfolds = NFOLDS,
                              # high value is less accurate, but has faster training
                              thresh = 1e-3,
                              # again lower number of iterations for faster training
                              maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'sec'))
plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))


preds = predict(glmnet_classifier, dtm_test, type = 'response')[,1]
cutoffs <- seq(0.1,0.9,0.1)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  prediction <- ifelse(preds >= cutoffs[i], 1, 0) #Predicting for cut-off
  accuracy <- c(accuracy,length(which(test$isPositive ==prediction))/length(prediction)*100)
}
res <- data.table(cutoffs,accuracy)

q <- ggplot(res,aes(x=cutoffs,y=accuracy))
q <- q + geom_line()
q

coef(glmnet_classifier)
plot(glmnet_classifier)
