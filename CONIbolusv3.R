rm(list=ls())

setwd("~/Documents/UoA/Projects/Food Note/Analysis")

library(stringi)
library(dplyr)
library(ggplot2)
library(sciplot)
library(reshape2)
library(readxl)
library(tidyr)
library(lme4)
library(nlme)
library(gridExtra)
library(tidyverse)

my.theme <- theme_classic() +
  theme(text=element_text(size=16, family="Arial"),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1))

#1. Load data and clean----

Sample <- as.numeric(c(1,2,3,4,5,6))
Sex <- c("M", "M", "M", "M", "M", "F")
Year <- c("2015", "2015", "2016", "2016", "2016", "2016")
IDs <- as.data.frame(cbind(Sample, Sex))

fd.1 <- read_excel("CONI bolus all.xlsx", col_names = T) %>% 
  filter(Sample != 1 & Sample != 2)
fd.1$Order <- gsub("unk", "Unknown", fd.1$Order)
fd.1$Family <- gsub("unk", "Unknown", fd.1$Family)
fd.1$Sex <- ifelse(fd.1$Sample==6, "F", "M")
str(fd.1)
fd.1$W <- as.numeric(fd.1$W)
fd.1$L <- as.numeric(fd.1$L)
fd.1$W[is.na(fd.1$W)] <- 0.0001
fd.1$Sample <- as.factor(fd.1$Sample)

fd.2 <- fd.1 %>% 
  group_by(Sample) %>% 
  dplyr::summarize(sample.mass = sum(W), sample.count = n())

fd.3 <- fd.1 %>% 
  left_join(fd.2)

fd.fam.1 <- fd.3 %>% 
  group_by(Sample, Sex, Order, Family, sample.mass, sample.count) %>% 
  dplyr::summarize(mass = sum(W), count = n()) %>% 
  mutate(prop.mass = mass/sample.mass*100, prop.count = count/sample.count*100)

fd.ord.1 <- fd.3 %>% 
  group_by(Sample, Sex, Order, sample.mass, sample.count) %>% 
  dplyr::summarize(mass = sum(W), count = n()) %>% 
  mutate(prop.mass = mass/sample.mass*100, prop.count = count/sample.count*100)


#2. Summary Stats----

#weight per sample
View(fd.2)

#Number of orders and families
unique(fd.1$Order)
unique(fd.1$Family)

#Mean mass and number of prey items per sample
mean(fd.2$sample.mass)
sd(fd.2$sample.mass)
mean(fd.2$sample.count)
sd(fd.2$sample.count)

#Mean % mass and prey items for coleoptera
fd.ord.b <- subset(fd.ord.1, Order=="Coleoptera")
mean(fd.ord.b$prop.mass)
sd(fd.ord.b$prop.mass)
mean(fd.ord.b$prop.count)
sd(fd.ord.b$prop.count)

#Quantify prop of insect parts
parts <- fd.1 %>% 
  group_by(Type) %>% 
  summarize(mass = sum(as.numeric(W)))

0.4660/(0.4660+2.6928)

#Quantify unknowns
fd.1.whole <- fd.1 %>% 
  filter(Type=="Whole insect")
summary(as.factor(fd.1.whole$Family))
summary(as.factor(fd.1.whole$Order))

#3. Beetle family composition----

fd.fam.2 <- fd.fam.1 %>%
  filter(Order=="Coleoptera") %>% 
  gather(key=calc, value=prop, prop.mass:prop.count) %>% 
  group_by(Order, Family, calc) %>% 
  summarize(mean=mean(prop), sd=sd(prop))

fd.fam.2[is.na(fd.fam.2)] <- 0

#4. Differences between sexes----

#By order
fd.ord.m.1 <- subset(fd.ord.1, Sex=="M")
fd.ord.f.1 <- subset(fd.ord.1, Sex=="F")

fd.ord.m.2 <- fd.ord.m.1 %>%
  gather(key=calc, value=prop, prop.mass:prop.count) %>% 
  group_by(Order, calc) %>% 
  dplyr::summarize(mean.m=mean(prop), sd.m=sd(prop))

fd.ord.f.2 <- fd.ord.f.1 %>%
  gather(key=calc, value=prop, prop.mass:prop.count) %>% 
  group_by(Order, calc) %>% 
  dplyr::summarize(mean.f=mean(prop), sd.f=sd(prop))

fd.ord.3 <- full_join(fd.ord.f.2, fd.ord.m.2, by =c("Order", "calc"))
fd.ord.3[is.na(fd.ord.3)] <- 0

#By beetle family

fd.fam.3 <- fd.fam.1 %>%
  gather(key=calc, value=prop, prop.mass:prop.count) %>% 
  group_by(Order, Family, calc, Sex) %>% 
  summarize(mean=mean(prop), sd=sd(prop))

#5. Analyze differences between origins----

fd.orig.1 <- fd.3 %>% 
  group_by(Sample, Origin, sample.mass, sample.count) %>% 
  dplyr::summarize(mass = sum(W), count = n()) %>% 
  mutate(prop.mass = mass/sample.mass*100, prop.count = count/sample.count*100)

fd.orig.aq <- subset(fd.orig.1, Origin=="Aquatic")
fd.orig.terr <- subset(fd.orig.1, Origin=="Terrestrial")

mean(fd.orig.aq$prop.mass)
sd(fd.orig.aq$prop.mass)
mean(fd.orig.aq$prop.count)
sd(fd.orig.aq$prop.count)

mean(fd.orig.terr$prop.mass)
sd(fd.orig.terr$prop.mass)
mean(fd.orig.terr$prop.count)
sd(fd.orig.terr$prop.count)

#6. Fig 1----

fd.ord.mass.2 <- fd.ord.1 %>%
  ungroup %>% 
  select(Sample, Order, prop.mass) %>% 
  dcast(Sample ~ Order)

fd.ord.mass.2[is.na(fd.ord.mass.2)] <- 0

fd.ord.3 <- fd.ord.mass.2 %>% 
  gather(key=Order, value=prop.mass, Coleoptera:Unknown) %>% 
  left_join(fd.ord.1, by=c("Sample", "Order", "prop.mass")) %>% 
  mutate(prop.count=ifelse(is.na(prop.count), 0, prop.count)) %>% 
  left_join(IDs, by="Sample") %>% 
  group_by(Order, Sex.y) %>% 
  summarize(mass.mn=mean(prop.mass), mass.sd=sd(prop.mass),
            count.mn=mean(prop.count), count.sd=sd(prop.count)) %>% 
  rename(Sex = Sex.y)

fd.ord.3[is.na(fd.ord.3)] <- 0

gg6 <- ggplot(fd.ord.3) +
  geom_bar(aes(x=Order, y=mass.mn, fill=Sex), stat="identity", position="dodge", color="black") +
  geom_errorbar(aes(x=Order, ymin=mass.mn-mass.sd, ymax=mass.mn+mass.sd, group=Sex), position=position_dodge(width=0.9), color="black", size=0.5, width=0.3) +
  labs(x="", y="% sample mass") +
  scale_fill_grey(start=0.4, end = 0.9, labels=c("Female", "Male")) +
  theme_classic() +
  theme(text=element_text(size=16, family="Arial"),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1), 
        axis.line.y=element_line(linetype=1), 
        axis.ticks.x=element_blank(),
        legend.title=element_blank(),
        legend.position = c(0.8, 0.85))

gg7 <- ggplot(fd.ord.3) +
  geom_bar(aes(x=Order, y=count.mn, fill=Sex), stat="identity", position="dodge", color="black") +
  geom_errorbar(aes(x=Order, ymin=count.mn-count.sd, ymax=count.mn+count.sd, group=Sex), position=position_dodge(width=0.9), color="black", size=0.5, width=0.3) +
  labs(x="Insect order", y="% prey items in sample") +
  scale_fill_grey(start=0.4, end = 0.9, labels=c("Female", "Male")) +
  theme_classic() +
  theme(text=element_text(size=16, family="Arial"),
        axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1), 
        legend.title=element_blank(),
        legend.position = "none")

grid.arrange(gg6, gg7, nrow=2, heights=c(4,5))


#6b. Test for beetle difference between male and female----

#Proportion

fd.fam.2 <- fd.3 %>% 
  filter(Order=="Coleoptera", Sex=="M") %>% 
  group_by(Sample, Order, sample.mass, sample.count) %>% 
  dplyr::summarize(mass = sum(W), count = n()) %>% 
  mutate(prop.mass = mass/sample.mass*100, prop.count = count/sample.count*100)

hist(fd.fam.2$prop.count)

t.test(fd.fam.2$prop.count, alternative="two.sided", mu=23.59)
t.test(fd.fam.2$prop.mass, alternative="two.sided", mu=77.91)

#Raw values

t.test(fd.fam.2$count, alternative="two.sided", mu=4)
t.test(fd.fam.2$prop.mass, alternative="two.sided", mu=0.3050)

wilcox.test(fd.fam.2$count, alternative="two.sided", mu=4)
wilcox.test(fd.fam.2$prop.mass, alternative="two.sided", mu=0.3050)

#7. Make Figure by Origin----

fd.orig.2 <- fd.orig.1 %>%
  gather(key=calc, value=prop, prop.mass:prop.count) %>% 
  group_by(Origin, calc) %>% 
  dplyr::summarize(mean=mean(prop), sd=sd(prop)) %>% 
  ungroup() %>% 
  mutate(Origin1= factor(Origin, levels=c("Terrestrial", "Aquatic", "Unknown")))

gg8 <- ggplot(fd.orig.2) +
  geom_bar(aes(x=Origin1, y=mean, fill=calc), stat="identity", position="dodge", color="black") +
  geom_errorbar(aes(x=Origin1, ymin=mean-sd, ymax=mean+sd, group=calc), position=position_dodge(width=0.9), color="black", size=0.5, width=0.3) +
  labs(x="Origin", y="Percent of sample") +
  scale_fill_grey(start=0.4, end = 0.9, labels=c("Number of prey items", "Dry mass")) +
  theme_classic() +
  theme(text=element_text(size=16, family="Arial"),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1), 
        legend.title=element_blank(),
        legend.position = c(0.68, 0.85))
gg8


#8. Make Figure by Family for Beetles----

fd.fam.2 <- fd.fam.1 %>%
  filter(Order=="Coleoptera") %>% 
  gather(key=calc, value=prop, prop.mass:prop.count) %>% 
  group_by(Order, Family, calc) %>% 
  summarize(mean=mean(prop), sd=sd(prop))

fd.fam.2[is.na(fd.fam.2)] <- 0

gg7 <- ggplot(fd.fam.2) +
  geom_bar(aes(x=Family, y=mean, fill=calc), stat="identity", position="dodge", color="black") +
  geom_errorbar(aes(x=Family, ymin=mean-sd, ymax=mean+sd, group=calc), position=position_dodge(width=0.9), color="black", size=0.5, width=0.3) +
  labs(x="Coleoptera Family", y="Percent of Sample") +
  scale_fill_grey(start=0.4, end = 0.9, labels=c("Number of Prey Items", "Dry Mass")) +
  theme_classic() +
  theme(text=element_text(size=16, family="Arial"),
        axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.line.x=element_line(linetype=1),
        axis.line.y=element_line(linetype=1), 
        legend.title=element_blank(),
        legend.position = c(0.8, 0.85))
gg7

#9. Exploring insect length----

fd.3$L <- as.numeric(fd.3$L)
fd.len <- subset(fd.3, L > -1)

ggplot(fd.len) +
  geom_boxplot(aes(x=Family, y=L, fill=Order)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))

ggplot(fd.len) +
  geom_boxplot(aes(x=Order, y=L)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))

ggplot(fd.len) +
  geom_boxplot(aes(x=Order, y=W)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0))

#10. Revisions----

