library(tidyverse)
library(ggrepel)


effix<-read_csv("SICSS_ethics.csv") %>%
  slice((-2):(-1)) %>%
  select(contains("_"),ResponseId)

# Concern Responses
effixE<-effix %>%
  select(contains("_1"),ResponseId) %>%
  pivot_longer(-ResponseId,
               names_to="question",
               values_to="rating") %>%
  mutate(rating=as.numeric(rating)) %>%
  mutate(question=gsub("_1","",question)) %>%
  group_by(question) %>%
  summarize(concern_m=mean(rating,na.rm=T),
            concern_se=sd(rating,na.rm=T)/sqrt(n())) 

# Value responses
effixV<-effix %>%
  select(contains("_2"),ResponseId) %>%
  pivot_longer(-ResponseId,
               names_to="question",
               values_to="rating") %>%
  mutate(rating=as.numeric(rating)) %>%
  mutate(question=gsub("_2","",question)) %>%
  group_by(question) %>%
  summarize(value_m=mean(rating,na.rm=T),
            value_se=sd(rating,na.rm=T)/sqrt(n())) 

effixAll<-left_join(effixE,effixV)

rm(effix,effixE,effixV)
####################################


# Value Plot

effixAll %>%
  arrange(value_m) %>%
  mutate(question=as.factor(question)) %>%
  ggplot(aes(x=question,
             y=value_m,
             color=question,
             ymin=value_m-value_se,
             ymax=value_m+value_se))+
  geom_point() +
  geom_errorbar() +
  coord_flip() +
  theme_bw() +
  labs(x="Project",y="Value Rating") +
  theme(panel.grid=element_blank(),
        legend.position="none",
        axis.title = element_text(size=24),
        axis.text = element_text(size=20))

# Concern Plot
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

ggsave("concern.png")

# Both at once
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
        axis.text = element_text(size=20))

ggsave("diagonal.png")