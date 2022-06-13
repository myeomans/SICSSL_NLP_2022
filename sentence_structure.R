library(politeness)

review_dat<-readRDS("review_dat.RDS")

review_polite<-politeness(review_dat$text,parser="spacy")

saveRDS(review_polite,file="review_polite.RDS")

review_polite<-readRDS(file="review_polite.RDS")

politenessPlot(review_polite,
               review_dat$male,
               split_levels=c("Female","Male"),
               split_name="Reviewer Gender",
               middle_out=.1)

# Use politeness to find the most masculine texts
findPoliteTexts(review_polite,
                review_dat$male)


################################################
# Some other uses of spacy
################################################

library(spacyr) # a new one!


spacyr::spacy_initialize()

rev_small=review_dat %>%
  slice(1:1000)

rev_small_sp<-spacy_parse(rev_small$text,
                     nounphrase = T,
                     lemma = T,
                     dependency = T,
                     pos = T,
                     tag=T)

saveRDS(rev_small_sp,file="rev_small_sp.RDS")


rev_small_sp<-readRDS(file="rev_small_sp.RDS")

head(rev_small_sp,20)

##################################################
# Use lemmas instead of stems!
##################################################

rev_small_lemma_docs<-rev_small_sp %>%
  group_by(doc_id) %>%
  summarize(text=paste(lemma, collapse=" ")) %>%
  mutate(doc_id=as.numeric(str_replace_all(doc_id,"text",""))) %>%
  arrange(doc_id)


lemma_data<-rev_small_lemma_docs$text %>%
  replace_contraction() %>%
  tokens(remove_numbers=TRUE,
         remove_punct = TRUE) %>%
  tokens_ngrams(1) %>%
  dfm() %>%
  dfm_trim(min_docfreq = .01,docfreq_type="prop")

colnames(lemma_data)

# Using POS tags to disambiguate words
##################################################

# words with two senses
two_senses<-rev_small_sp %>%
  group_by(token,pos) %>%
  summarize(pos_ct=n()) %>%
  left_join(rev_small_sp %>%
              group_by(token) %>%
              summarize(all_ct=n())) %>%
  mutate(pos_ratio=pos_ct/all_ct) %>%
  filter(all_ct>80) %>%
  filter(pos_ratio>.2 & pos_ratio<.8) %>%
  as.data.frame()

#  words with multiple POS
two_senses 

rev_small_sp_tagged <- rev_small_sp %>%
  left_join(two_senses %>%
              mutate(token_tag=paste0(token,"_",pos)) %>%
              select(token,pos,token_tag)) %>%
  mutate(tagged_tokens=ifelse(is.na(token_tag),token,token_tag))

# create a dfm from this
rev_small_tagged_docs<-rev_small_sp_tagged %>%
  group_by(doc_id) %>%
  summarize(text=paste(tagged_tokens, collapse=" ")) %>%
  mutate(doc_id=as.numeric(str_replace_all(doc_id,"text",""))) %>%
  arrange(doc_id)

SICSSL_dfm(rev_small_tagged_docs$text,stop.words = F) %>%
  colnames() %>%
  sort()

##################################################
# named entity recognition
##################################################
rev_small_ner<-spacy_extract_entity(rev_small$text)

rev_small_ner %>%
  filter(ent_type=="DATE") %>%
  with(rev(sort(table(text)))[1:10])

rev_small_ner %>%
  filter(ent_type=="LOC") %>%
  with(rev(sort(table(text)))[1:10])

rev_small_ner <- rev_small_ner %>%
  uncount(length) %>%
  group_by(doc_id,start_id) %>%
  mutate(doc_token_id=start_id+0:(n()-1),
         first=1*(start_id==doc_token_id)) %>%
  ungroup() %>%
  mutate(text=str_replace_all(text," ","_")) %>%
  select(doc_id,ner_text="text",first,doc_token_id) 

# combine noun phrases into single tokens
rev_small_sp_ner <- rev_small_sp %>%
  group_by(doc_id) %>%
  # annoying that the nounphrase counts doc tokens, not sentence tokens
  # but we do what we must
  mutate(doc_token_id=1:n()) %>%
  ungroup()%>%
  left_join(rev_small_ner) %>%
  filter(is.na(ner_text)|first==1) %>%
  mutate(ner_token=ifelse(is.na(ner_text),token,ner_text)) %>%
  select(-pos,-tag,-head_token_id,-first,-dep_rel,-nounphrase,-ner_text)

# generate a dfm from this

rev_small_ner_docs<-rev_small_sp_ner %>%
  group_by(doc_id) %>%
  summarize(text=paste(ner_token, collapse=" ")) %>%
  mutate(doc_id=as.numeric(str_replace_all(doc_id,"text",""))) %>%
  arrange(doc_id)


SICSSL_dfm(rev_small_ner_docs$text) %>%
  colnames()
