## analysis for Identifying cost-effective recovery actions for a critically endangered species ##
## last updated 9/07/2021 by Ella Kelly 


## load library
library(readr)
library(tidyverse)
library(ggplot2)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(reshape2)
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

### analysis of expert elicitation data ####

#load the EE data & fix location names if needed
#load the Expert data
data<-read.csv("alldataOBP_paper.csv",stringsAsFactors = F, fileEncoding="UTF-8-BOM")

data$Location<-gsub(" ","",gsub("-","",data$Location))

data$Estimate<-as.numeric(data$Estimate)


# # Calculate the mean benefit scores for each action
# meanbens = data %>% group_by(Action, Location, Bird.number) %>% 
#   dplyr::summarise(Ben=mean(Ben,na.rm=TRUE), Low=mean(BenLow,na.rm=TRUE), Upp=mean(BenUp,na.rm=TRUE), NoAction_Estimate = mean(NoAction_Estimate, na.rm=TRUE))

## assessing variation in BenFun_sp using a triangular distribution
## with Bird.number = 100
data<-data %>% filter(Location=="Inthewild") #select scenarios that are location specific and just one scenario 
data<-data[complete.cases(data), ]
data$Scenario<-paste(data$Action, " ", data$Location, " ",data$Expert)

#remove instances where upper/lower is identical to best guess to avoid errors in rtri
for(i in 1:nrow(data)){
  if(data$Estimate[i]==data$Lower[i]){data$Lower[i]<-data$Lower[i]-0.001}
  if(data$Estimate[i]==data$Upper[i]){data$Upper[i]<-data$Upper[i]+0.001}
}

# draw triangular distrubtion from each expert's minimum, maximum and best estimates
library(EnvStats)
# create output matrix for loop
vec_out<-matrix(nrow=length(unique(data$Scenario)), ncol=10000)
rownames(vec_out)<-unique(data$Scenario)

#create 1000 vectors of estimates based on triangular distribution
vec_out<-matrix(nrow=nrow(data), ncol=1000)
for (i in 1:nrow(data)){
  dat<-data[i,]
  Ests<-rtri(1000, min=dat$Lower, max=dat$Upper, mode= dat$Estimate)
  vec_out[i,]<-Ests
}


tridata<-cbind(data[,c("Expert","Scenario","Action", "Location", "MS", "Bird.number","Fire", "PBFV", "Cost")], Estimate=rowMeans(vec_out)) # data with individual expert Ben estimates from trianglar distrabution


# Grab the scenario estimates that were for the no-action scenario (Scenario 1) and merge the data frames back together
noaction = tridata %>% filter(Action=="No action + no captive population") %>% dplyr::select(Expert, Location, Estimate) %>% dplyr::rename(NoAction_Estimate=Estimate)

tridata = merge(tridata, noaction, by=c("Expert", "Location"))

# Calculate the benefit scores for each action and for each species and each expert
tridata$Ben = tridata$Estimate-tridata$NoAction_Estimate

tridata$CE <- tridata$Ben/tridata$Cost
### lmer analysis ###

#general benefit
tridata$MS<-as.factor(tridata$MS)

alldata.releases<-  tridata %>%
  filter(MS %in% c(1:7)) %>%
  filter(!Ben<0)

library(lme4)
## benefit
model <- lmer(Ben ~ Bird.number+ MS + (1|Expert), data = alldata.releases)
summary(model)  

model.bird<-lmer(Ben ~ MS + (1|Expert), data = alldata.releases)
anova(model, model.bird)

model.action<-lmer(Ben ~ Bird.number + (1|Expert), data = alldata.releases)
anova(model, model.action)


#cost-effectiveness
model <- lmer(CE ~ Bird.number+ MS + (1|Expert), alldata.releases, REML=FALSE)
summary(model) 

model.bird<-lmer(CE ~ MS + (1|Expert), alldata.releases, REML=FALSE)
anova(model, model.bird)

model.action<-lmer(CE ~ Bird.number + (1|Expert), alldata.releases, REML=FALSE)
anova(model, model.action)


# #fire and PBFV
alldata.extras<-filter(tridata, MS %in% c(1,2,8,9,10))

scen <- ifelse(alldata.extras$MS==1, 1,
               ifelse(alldata.extras$MS==8, 1,
                      ifelse(alldata.extras$MS==9, 1,
                             ifelse(!alldata.extras$MS==1, 2,1))))

alldata.extras<- cbind(alldata.extras, scen)

model2<- lmer(Ben ~ scen +Fire + (1|Expert), alldata.extras, REML=FALSE)
summary(model2)

model3<- lmer(Ben ~ scen +PBFV + (1|Expert), alldata.extras, REML=FALSE)
summary(model3)

model4<- lmer(Ben ~ scen + (1|Expert), alldata.extras, REML=FALSE)

anova(model2, model4)
anova(model3, model4)
# 

### plots for the paper

raincloud_theme <- theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 16),
  legend.position = "right",
  plot.title = element_text(lineheight = .8, face = "bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
  axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"))


data.wild<- tridata %>% 
  filter(Location == "Inthewild") %>% 
  filter(!Bird.number==0) %>% 
  filter(!MS %in% c(0,8,9,10)) %>%
  filter(!Ben<0)

data.wild$Bird.number<- as.factor(data.wild$Bird.number)
data.wild$MS<-droplevels(data.wild$MS)

g1 <- ggplot(data = data.wild, 
             aes(y = Ben, x = MS, fill = Bird.number)) +
  #geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .2) +
  geom_point(aes(y = Ben, color = Bird.number), 
             position = position_jitter(width = .15), size = 2, alpha = 1) +
  geom_boxplot(width = .35, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_viridis(discrete = TRUE, option = "D", begin=0.2, end=0.8)+
  scale_fill_viridis(discrete = TRUE,begin=0.2, end=0.8) +
  coord_flip() + # flip or not
  theme_bw()+
  scale_x_discrete(limits = rev(levels(data.wild$MS)))+
  raincloud_theme+
  labs(y="\nBenefit", x="Management Scenario\n")+ 
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.001))

g2 <- ggplot(data = data.wild, 
             aes(y = CE, x = MS, fill = Bird.number)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .2) +
  geom_point(aes(y = CE, color = Bird.number), 
             position = position_jitter(width = .15), size = 2, alpha = 1) +
  geom_boxplot(width = .35, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_viridis(discrete = TRUE, option = "D", begin=0.2, end=0.8)+
  scale_fill_viridis(discrete = TRUE,begin=0.2, end=0.8) + 
  coord_flip() + # flip or not
  theme_bw()+
  scale_x_discrete(limits = rev(levels(data.wild$MS)))+
  raincloud_theme+
  labs(y="\nCost-effectiveness", x="")



gridExtra::grid.arrange(g1,g2, nrow=1)

## uncirtanty upper/lower plot

dataplot<- data %>% 
  filter(Location == "Inthewild") %>% 
  filter(!Bird.number==0) %>% 
  filter(!MS %in% c(0,8,9,10)) 

Mean <- dataplot %>%
  group_by(Bird.number) %>%
  summarise(MN = mean(Estimate, na.rm=TRUE))

ggplot(dataplot, aes(x=Expert, y=Estimate))+
  geom_point(size=3)+
  geom_hline(data = Mean, aes(yintercept = MN), col="red", linetype="dashed") +
  facet_grid(rows=vars(MS), cols=vars(Bird.number), space = "free") +
  theme(strip.text.y = element_text(angle = 90))+
  geom_errorbar(aes(ymin=Lower, ymax=Upper), size=1)+
  theme_bw()


