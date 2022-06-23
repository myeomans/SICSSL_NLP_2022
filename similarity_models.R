################################################
#
#       SICSS-London 2022 NLP Workshop
#
#  More complex models that group similar words
#
#
################################################

# Later we will need a file of the pre-trained word embeddings
# The real word vector files are ~ 6GB - too big! This is a smaller version,
# containing only the 50,000 most common words
vecSmall<-readRDS("vecSmall.RDS")


# If you use the full version.. Big files must be loaded using data.table - it's faster
# library(data.table)
# vecFile<-data.table::fread("crawl-300d-2M.vec",
#                            quote="",header=F,col.names = c("word",paste0("vec",1:300)))
#rm(vecFile)

# Word frequency file - to reweight common words later on
load("wfFile.RData")


######################################################################
# Load job descriptions data
######################################################################

jobdesc<-readRDS("jobdescriptions.RDS")

# Explore main meta-data
jobdesc %>%
  with(sort(table(Category)))

######################################################################
# A topic model example
######################################################################

# shrink the focus on a single category for topic modeling
jd_small<- jobdesc %>%
  filter(Category=="Engineering Jobs") %>%
  mutate(desc_wdct=str_count(FullDescription,"[[:alpha:]]+")) %>%
  rename(salary="SalaryNormalized")

# note - this makes the "random" split identical for all of us, so we get the same results
set.seed(2022)

train_split=sample(1:nrow(jd_small),3500)

jd_small_train<-jd_small[train_split,]
jd_small_test<-jd_small[-train_split,]

# First we need a dfm object (ngram matrix in a quanteda file format)
# Topic models are usually estimated with only unigrams, and without stopwords
jd_small_dfm_train<-SICSSL_dfm(jd_small_train$FullDescription,ngrams=1)

jd_small_dfm_test<-SICSSL_dfm(jd_small_test$FullDescription,ngrams=1) %>%
  dfm_match(colnames(jd_small_dfm_train))


# Train a 20-topic model
#jd_topicMod20<-stm(jd_small_dfm_train,K=20)

# Note - you can save topic models as RDS files, too!

#saveRDS(jd_topicMod20,file="jd_topicMod20.RDS")

# It takes a long time to train, so you can just load my pre-trained one
jd_topicMod20<-readRDS("jd_topicMod20.RDS")

# There are metrics you can use to choose the topic number.
# These are controversial... you are better off adjusting to taste
# This is how you would run that, though....
# Fist convert to stm format, then put the documents and vocab into searchK()

# jd_stm_format<-jd_small_dfm_train %>%
#   convert(to="stm")
# sk<-searchK(jd_stm_format$documents,
#             jd_stm_format$vocab,
#             K=c(10,20,30,40))
# plot(sk)

topicNum=jd_topicMod20$settings$dim$K

# LDA will not name your topics for you! It's good to come up with some names on your own
topicNames<-paste0("Topic",1:topicNum)

# Most common topics, and most common words from each topic
plot(jd_topicMod20,type="summary",n = 7,xlim=c(0,.3),labeltype = "frex",
     topic.names = topicNames) 

# You can add names to the vector one at a time

# I came up with these based on reading the example texts and common words
# do not just apply these to new data! Read and come up with your own.

topicNames[1]="Food Manufacturing"
topicNames[2]="Design"
topicNames[3]="CNC"
topicNames[5]="Software"
topicNames[9]="Technical Sales"
topicNames[11]="Electrical"
topicNames[13]="Quality Assurance"
topicNames[16]="Gas Appliances"
topicNames[20]="Systems"

# Re-do the last plot with topic names
plot(jd_topicMod20,type="summary",n = 7,xlim=c(0,.3),labeltype = "frex",
     topic.names = topicNames) 


# We can also grab more words per topic
labelTopics(jd_topicMod20)

findThoughts(model=jd_topicMod20,
             texts=jd_small_train$FullDescription,
             topics=5,n = 1)

# We can even put them in a word cloud! If you fancy it

cloud(jd_topicMod20,11)

cloud(jd_topicMod20,9)

# Which topics correlate with one another?
plot(topicCorr(jd_topicMod20),
     vlabels=topicNames,
     vertex.size=20)

stmEffects<-estimateEffect(1:topicNum~salary,
                           jd_topicMod20,
                           meta= jd_small_train %>%
                             select(salary))


# The default plotting function is bad... Here's another version
bind_rows(lapply(summary(stmEffects)$tables,function(x) x[2,1:2])) %>%
  mutate(topic=factor(topicNames,ordered=T,
                      levels=topicNames),
         se_u=Estimate+`Std. Error`,
         se_l=Estimate-`Std. Error`) %>%
  ggplot(aes(x=topic,y=Estimate,ymin=se_l,ymax=se_u)) +
  geom_point() +
  geom_errorbar() +
  coord_flip() +
  geom_hline(yintercept = 0)+
  theme_bw() +
  labs(y="Correlation with Salary",x="Topic") +
  theme(panel.grid=element_blank(),
        axis.text=element_text(size=20))


# This contains the topic proportions for each document..
topic_prop_train<-jd_topicMod20$theta
dim(topic_prop_train)
colnames(topic_prop_train)<-topicNames

# We can use these topic proportions just like any other feature
jd_model_stm<-glmnet::cv.glmnet(x=topic_prop_train,
                                y=jd_small_train$salary)

# Note that we didn't give enough features... there is no U shape
plot(jd_model_stm)

topic_prop_test<-fitNewDocuments(jd_topicMod20,
                                 jd_small_dfm_test %>%
                                   convert(to="stm") %>%
                                   `$`(documents))

test_stm_predict<-predict(jd_model_stm,
                          newx = topic_prop_test$theta)[,1]

# Note the drop in performance, compared to the ngrams
acc_stm<-kendall_acc(jd_small_test$salary,test_stm_predict)

acc_stm

jd_model_dfm<-glmnet::cv.glmnet(x=jd_small_dfm_train,
                                y=jd_small_train$salary)

plot(jd_model_dfm)

test_dfm_predict<-predict(jd_model_dfm,
                          newx = jd_small_dfm_test)[,1]

acc_dfm<-kendall_acc(jd_small_test$salary,test_dfm_predict)

acc_dfm

# And a wordcount benchmark for good measure

acc_wdct<-kendall_acc(jd_small_test$salary,jd_small_test$desc_wdct)

acc_wdct

###################################################
# Word Embeddings example
###################################################

# the vector file has one column with words, 
# and 300 with vector projections (uninterpretable!)
head(vecSmall[,1:20])

head(wfFile)

# Calculating similarity using bag of words doesn't know the difference between sad and happy!
bowSimCalc(x=c("I am very sad","I am very happy"),
           y="I am thrilled")

# However, processing the text as dense vectors allows the meaning to emerge. 
vecSimCalc(x=c("I am very sad","I am very happy"),
           y="I am thrilled",
           vecfile=vecSmall)


#############################################
# project data to embedding space
vdat_train<-vecCheck(jd_small_train$FullDescription,
                     vecSmall,
                     wfFile)

vdat_test<-vecCheck(jd_small_test$FullDescription,
                    vecSmall,wfFile)

#############################################
# Train a vector classifier

lasso_vec<-glmnet::cv.glmnet(x=vdat_train,
                             y=jd_small_train$salary)

# looks good
plot(lasso_vec)

test_vec_predict<-predict(lasso_vec,newx = vdat_test,
                          s="lambda.min")

kendall_acc(test_vec_predict,jd_small_test$salary)

#############################################
# vector embeddings + ngrams
lasso_all<-glmnet::cv.glmnet(x=cbind(vdat_train,jd_small_dfm_train),
                             y=jd_small_train$salary)

plot(lasso_all)

test_alls_predict<-predict(lasso_all,
                          newx = cbind(vdat_test,jd_small_dfm_test),
                          s="lambda.min")

kendall_acc(test_all_predict,jd_small_test$salary)

########################################
# similarity calculation
########################################

table(jd_small_train$Company)

jd_small_train %>%
  filter(grepl("Technique",Company)) %>%
  with(table(Company))

jd_small_train %>%
  filter(Company=="Technique Recruitment Solutions") %>%
  pull(FullDescription)

which(jd_small_train$Company=="Technique Recruitment Solutions")

target<-jd_small_train %>%
  filter(Company=="Technique Recruitment Solutions") %>%
  pull(FullDescription)


sims<-vecSimCalc(x=jd_small_train$FullDescription,
                 y=target,
                 vecfile=vecSmall,
                 wffile = wfFile,
                 PCAtrim=1)

hist(sims)
which.max(sims)

# least similar
jd_small_train %>%
  arrange(sims) %>%
  slice(1:2) %>%
  pull(FullDescription)

# most similar
jd_small_train %>%
  arrange(-sims) %>%
  slice(1:2) %>%
  pull(FullDescription)

# save to dataframe
jd_small_train$sims<-sims

######################################################################
# Distributed Dictionary
######################################################################

# these are the different dictionaries contained in the dataset
textdata::lexicon_loughran() %>%
  with(table(sentiment))

# extract dictionary as document
lm_uncertainty<-textdata::lexicon_loughran() %>%
  filter(sentiment=="uncertainty") %>%
  pull(word) %>%
  paste(collapse=" ")

# calculate similarities to dictionary "document"
lm_sim<-vecSimCalc(x=jd_small_train$FullDescription,
                   y=lm_uncertainty,
                   vecfile=vecSmall,
                   wffile = wfFile,
                   PCAtrim=1)

# add the similarity scores to the data.frame
jd_small_train$lm_sim<-lm_sim


#############################################
# extract dictionary the normal way
#############################################

# extract word list as dictionary
uncertain_dict<-dictionary(list(
  lm_uncertainty=textdata::lexicon_loughran() %>%
    filter(sentiment=="uncertainty") %>%
    pull(word)))

# Traditional dictionary approach using dfm_lookup()
jd_small_train_dicts<-jd_small_train %>%
  pull(FullDescription) %>%
  tokens() %>%
  dfm() %>%
  dfm_lookup(uncertain_dict) %>%
  convert(to="data.frame")

# Accuracy score using traditional dictionary
kendall_acc(jd_small_train_dicts$lm_uncertainty,
            jd_small_train$salary)


# Accuracy score using distributed dictionary
kendall_acc(jd_small_train$lm_sim,
            jd_small_train$salary)


# ALWAYS ALWAYS ALWAYS
# Clear big files out of the workspace when you're done
# Otherwise Rstudio will be very slow to load the next time
rm(vecSmall,wfFile)
