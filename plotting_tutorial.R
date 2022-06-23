
library(tidyverse)
library(ggrepel)
library(ggridges)
##############################################
# Ethics ratings - one at a time and together
##############################################

# Load the data
effix<-read_csv("SICSS_ethics.csv") %>%
  slice((-2):(-1)) %>%
  select(contains("_"),ResponseId)

# Concern Responses
effixE<-effix %>%
  select(contains("_1"),ResponseId) %>%
  # wide to long
  pivot_longer(-ResponseId,
               names_to="question",
               values_to="concern") %>%
  mutate(question=gsub("_1","",question))

# Value responses
effixV<-effix %>%
  select(contains("_2"),ResponseId) %>%
  pivot_longer(-ResponseId,
               names_to="question",
               values_to="value") %>%
  mutate(question=gsub("_2","",question))

# merge the data
effixAll<-left_join(effixE,effixV) %>%
  mutate_at(c("concern","value"), ~as.numeric(.)) %>%
  # create summary stats
  group_by(question) %>%
  summarize(concern_m=mean(concern,na.rm=T),
            concern_se=sd(concern,na.rm=T)/sqrt(n()),
            value_m=mean(value,na.rm=T),
            value_se=sd(value,na.rm=T)/sqrt(n())) 

head(effixAll)

rm(effix,effixE,effixV)
####################################

# Concern Plot
effixAll %>%
  ggplot(aes(x=question,
             y=concern_m,
             color=question,
             ymin=concern_m-concern_se,
             ymax=concern_m+concern_se))+
  geom_point() +
  geom_errorbar()

# problems:
# x axis is bunched
# legend is redundant
# gray background with grid is basic
# axis labels are not english
# all the text is too small

effixAll %>%
  ggplot(aes(x=question,
             y=concern_m,
             color=question,
             ymin=concern_m-concern_se,
             ymax=concern_m+concern_se))+
  geom_point() +
  geom_errorbar() +
  coord_flip() +
  theme_bw() +
  labs(x="Project",y="Concern Rating") +
  theme(panel.grid=element_blank(),
        legend.position="none",
        axis.title = element_text(size=24),
        axis.text = element_text(size=20))

#save the plot
ggsave("concern.png")

# Let's plot concern and value both at once
effixAll %>%
  ggplot(aes(y=value_m,
             ymin=value_m-value_se,
             ymax=value_m+value_se,
             color=question,
             label=question,
             x=concern_m,
             xmin=concern_m-concern_se,
             xmax=concern_m+concern_se))+
  geom_abline(slope=1,intercept=0)+
  geom_point() +
  geom_label_repel(force = 3) +
  theme_bw() +
  xlim(25,100) +
  ylim(25,100) +
  labs(y="Value Rating",x="Concern Rating") +
  theme(panel.grid=element_blank(),
        legend.position="none",
        axis.title = element_text(size=24),
        axis.text = element_text(size=20)) +
  geom_text(x=40, y=90, 
            label="You can just write stuff",
            color="forestgreen")

ggsave("diagonal.png")


########################################
# Let's get wild
########################################

topix<-readRDS("topix.RDS")

# stacked...annoying
topix %>%
  ggplot(aes(x=stay,group=topicID,
             fill=topicID)) +
  geom_histogram()

# overlapping... also annoying
topix %>%
  ggplot(aes(x=stay,group=topicID,
             fill=topicID)) +
  geom_density(alpha=.2)

# better
topix %>%
  ggplot(aes(x=stay,y=topicID)) +
  geom_density_ridges()

#needs colors, axis titles, etc.
topix %>%
  ggplot(aes(x=stay,fill=topicID,
             y=topicID)) +
  geom_density_ridges() +
  theme_bw() +
  labs(x="Stay on Topic",
       y="Topic ID") +
  theme(legend.position="none",
        panel.grid=element_blank(),
        axis.text = element_text(size=20),
        axis.title = element_text(size=24))


##### Let's put them in order

# order them from low to high
tOrder_dat=topix %>%
  group_by(topicID) %>%
  summarize(tOrder=mean(stay))

head(tOrder_dat)

topix %>%
  # merge order data
  left_join(tOrder_dat) %>%
  # use the order column to re-order topicID as a factor
  mutate(topicID=fct_reorder(topicID,tOrder)) %>%
  ggplot(aes(x=stay,fill=topicID,
             y=topicID)) +
  geom_density_ridges() +
  theme_bw() +
  theme(legend.position="none")

# do the same with word count
topix %>%
  ggplot(aes(x=wdct,fill=topicID,
             y=topicID)) +
  geom_density_ridges() +
  theme_bw() +
  theme(legend.position="none")

# long tail is annoying! let's trim the x limit
# and add a line at the mean
topix %>%
  ggplot(aes(x=wdct,fill=topicID,
             y=topicID)) +
  geom_density_ridges() +
  theme_bw() +
  xlim(0,100) +
  geom_vline(xintercept=mean(topix$wdct))+
  theme(legend.position="none")

##### now look at the correlation between the two

# this is messy
topix %>%
  ggplot(aes(x=stay,y=wdct)) +
  geom_point() 

# this is better  
topix %>%
  ggplot(aes(x=stay,y=wdct)) +
  geom_smooth()

# default is loess, can also do linear
topix %>%
  ggplot(aes(x=stay,y=wdct)) +
  geom_smooth(method="lm")

# you can do points and a line
# but the y axis spread is not useful
topix %>%
  ggplot(aes(x=stay,y=wdct)) +
  geom_point() +
  geom_smooth(fill="blue",lwd=1)

# note that the order matters!
topix %>%
  ggplot(aes(x=stay,y=wdct)) +
  geom_smooth(fill="blue",lwd=3) +
  geom_point() +
  coord_cartesian(ylim=c(40, 60))

topix %>%
  ggplot(aes(x=stay,y=wdct)) +
  geom_point() +
  geom_smooth(fill="blue",lwd=3) +
  coord_cartesian(ylim=c(40, 60))

# can also bin the x axis and present means/SEs
topix %>%
  # generate bins
  mutate(stay=round(stay)) %>%
  group_by(stay) %>%
  # calculate binned averages
  summarize(m=mean(wdct),
            se=sd(wdct)/sqrt(n())) %>%
  ggplot(aes(x=stay,y=m,
             ymin=m-se,ymax=m+se)) +
  geom_point() +
  geom_errorbar()

# we want lines of best fit for all 12 topics, though
# solution - facets!!
topix %>%
  ggplot(aes(x=stay,y=wdct)) +
  facet_wrap(~ topicID) +
  geom_smooth(color="blue4",fill="blue1") +
  xlab("Stay/Switch Rating") +
  ylab("Word Count") +
  theme_bw() +
  theme(strip.background = element_rect(fill="white", size=.5, linetype="solid",colour="black"),
        strip.text.x = element_text(size=18, face="bold"),
        legend.position="none",
        axis.text = element_text(size=12,family = "Times"),
        axis.title = element_text(size=20, face="bold",family = "Times"))

#ggsave("sentiment_facet.png")

# you can also do groups

topix %>%
  ggplot(aes(x=stay,y=wdct,
             color=gender,
             group=gender)) +
  geom_smooth()


topix %>%
  mutate(gender=ifelse(gender==1,"Male","Female")) %>%
  ggplot(aes(x=stay,y=wdct,
             color=gender,
             group=gender)) +
  geom_smooth() +
  theme_bw() +
  geom_vline(xintercept=0) +
  labs(x="Stay/Switch Rating",
       y="Word Count") +
  theme(legend.position=c(.2,.8),
        panel.grid = element_blank(),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15))

########################################
# Legends, colors, etc.
########################################

library(politeness)

cpe_dat<-read.csv("complete_bcpe_study4.csv") %>%
  filter(!is.na(condition)) 

# extract politeness features
cpe_polite<-politeness(cpe_dat$message,parser="spacy")

# these are ggplots!
politenessPlot(cpe_polite,
               cpe_dat$condition,
               middle_out=.05)

# so you can add as needed
politenessPlot(cpe_polite,
               cpe_dat$condition,
               middle_out=.05) +
  geom_text(x=4,y=2,color="forestgreen",
            label="Writing on the plot")

# Calculate some NLP measures
cpe_dat<-cpe_dat %>%
  bind_cols(cpe_polite) %>%
  mutate(recept_NLP=receptiveness(message),
         txt_wdct=str_count(message,"[[:alpha:]]+"),
         sentiment=Positive.Emotion-Negative.Emotion)


effectSet<-cpe_dat %>%
  # choose my variables, rename some
  select(ID,cond,sentiment,
         Receptiveness="recept_NLP",
         Word_Count="txt_wdct",
         Agreement,Acknowledgement,
         affect,empathy,persuasion,
         outgroup_affect="outgroup_aff",
         conciliatory_tone,argument_count,
         strength_position) %>%
  # wide to long
  pivot_longer(-c(ID,cond),
               names_to="name",
               values_to="value") %>%
  # summary statistics
  group_by(name) %>%
  summarize(d=summary(lm(scale(value)~cond))$coef[2,1],
            se=summary(lm(scale(value)~cond))$coef[2,2],
            l=d-se,u=d+se) %>%
  mutate(vartype=case_when(
    name%in%c("Word_Count","sentiment","Receptiveness",
              "Agreement","Acknowledgement") ~ "Text",
    name%in%c("persuasion","outgroup_affect","empathy","affect") ~ "Participants",
    name%in%c("conciliatory_tone","argument_count","strength_position") ~ "Annotations",
    T ~ "none"))


effectSet %>%
  mutate(name=fct_reorder(name,d)) %>%
  ggplot(aes(x=name,
             # color is determined by my vartype variable
             color=vartype,
             y=d,ymin=l,ymax=u)) +
  geom_point() +
  geom_errorbar() +
  coord_flip() + 
  theme_bw() +
  ylim(-.3,1) +
  labs(color="Variable Type",
       y="Cohen's D",
       x="")+
  geom_hline(yintercept=0)+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=24),
        # note how I move the legend around
        legend.position=c(.73,.15),
        legend.text = element_text(size=15),
        legend.background = element_rect(color="black"),
        panel.grid = element_blank())
