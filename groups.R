library(tidyverse)
skills<-read.csv("SICSS_welcome.csv") 

dist(skills)

hc_groups<-hclust(dist(skills))

plot(hc_groups,labels=skills$name)

hc_order=skills$name[hc_groups$order]

# close
data.frame(name=hc_order,
           ordering=sort(rep(1:4,6))[1:21]) %>%
  arrange(ordering)


# dispersed
data.frame(name=hc_order,
           ordering=rep(1:5,6)[1:21]) %>%
  arrange(ordering)

#############################################

interests<-read.csv("SICSSgroups_two.csv") %>%
  select(-Prashant)

iRatings<-interests %>%
  select(-Project.name,-Population,
         -Quantities,-Challenges) %>%
  t()

colnames(iRatings)<-interests$Project.name

iRatings<-data.frame(iRatings)

hc_groups<-hclust(dist(iRatings))

plot(hc_groups)

hc_order=rownames(iRatings)[hc_groups$order]

# close
data.frame(name=hc_order,
           ordering=sort(rep(1:4,6))[1:20]) %>%
  arrange(ordering)


# dispersed
data.frame(name=hc_order,
           ordering=rep(1:4,6)[1:20]) %>%
  arrange(ordering)

