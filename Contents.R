################################################
#
#     Natural Langauge Processing Tutorial
#
#                Prof Yeomans
#
#             SICSS London 2022
#
################################################

# Un-comment and run these once, if you haven't installed them before
# install.packages("tidyverse") # useful everything
# install.packages("ggrepel") # nice labels for plots
# install.packages("glmnet") # supervised machine learning
# install.packages("quanteda") # very useful text stuff
# install.packages("textclean") # useful text stuff
# install.packages("stm") # topic models
# install.packages("textdata") # dictionaries
# install.packages("syuzhet") # sentiment
# install.packages("doc2concrete") # ngram model
# install.packages("politeness") # grammar parsing & dialogue acts

# Run these every time
library(tidyverse)
library(ggrepel)
library(glmnet)
library(quanteda)
library(textclean)
library(stm) 
library(textdata)
library(syuzhet)
library(doc2concrete)
library(politeness)

# Here I am loading separate R scripts that contain functions we will use
# make sure these are all in your Rstudio project
# All in the main folder, along with this script!

source("SICSSL_dfm.R") # our dfm function as a separate script
source("kendall_acc.R") # our accuracy function
source("vectorFunctions.R") # for word embeddings

############################################################
# Code tutorials - open the files and run line by line
############################################################

# useful functions - how to combine, trim, find/replace, etc.
source("text_basics.R") 

# Restaurant review data
review_dat<-readRDS("review_dat.RDS")

# a simple workflow for building an NLP prediction model
# covers pre-processing, accuracy evaluation, interpretability
source("ngram_model.R") 

# Job description data
jobdesc<-readRDS("jobdescriptions.RDS")

# more advanced models that leverage word similarity
# coverse topic models, word embeddings, similarity scores
source("similarity_models.R") 


