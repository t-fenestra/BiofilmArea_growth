library(tidyverse)
library(dplyr)
library(ggridges)
library(gridExtra)
library(stringr)
library(modelr)
library(broom)
options(na.action = na.warn)

FolderToSavePlots='./output/figures/'
AreaY<-read_delim(file='./data/ResultTable_SliceY.txt',col_names = TRUE,delim=',')

#========================================================================#
# Introduce new colums with strain type, frame number
#========================================================================#
AreaY<-AreaY %>% 
  # new colums with strain
  separate(Experiment,c('Strain'),sep='_',remove=FALSE,extra="drop") %>%
  mutate(Strain=as.factor(Strain) )%>%
  # extract data frame
  mutate(frame=as.numeric(str_split(datafile,'_',simplify = TRUE)[,5]),data_stamp=as.numeric(str_split(datafile,'_',simplify = TRUE)[,3]))

# Select only first 15 frames
AreaY <- AreaY %>% filter(frame %in% seq(from=1,to=15,by=1)) # frame 1-15

#========================================================================#
# Growth per layer
#========================================================================#  
ggplot(AreaY,aes(x=frame,y=SumSliceY,color=Strain))+
  geom_line(aes(group=Experiment),size=1)+facet_grid(Layer~.)+
  theme_bw(base_size = 12)+
  labs(title="Area per layer counted from ALI")+
  ylab("Area per Layer")

#========================================================================#
# Percent per Layer
#========================================================================#
AreaTotal<-AreaY %>% group_by(Experiment,frame,Strain) %>%
    summarise(AreaTotal=sum(SumSliceY))
    
# Ratio per Layer
AreaYSumSliceY<-AreaY %>% select(Experiment,Strain,Layer,frame,SumSliceY,
                                 SumSliceYgain,SumSliceYlost,SumSliceYsame) %>%
  inner_join(AreaTotal,by=c("Experiment","frame","Strain")) %>% 
  filter(AreaTotal!=0) %>% 
  mutate(Ratio=SumSliceY/AreaTotal)  
  

AreaYSumSliceY %>% filter(Strain=='dwss') %>%
  ggplot(aes(x=Layer,y=Ratio),color=as.numeric(Layer))+geom_bar(size=1,stat="identity",fill="darkblue")+
  scale_x_reverse()+
  coord_flip()+
  facet_grid(frame~Experiment)+
  
AreaYSumSliceY %>% filter(Strain=='SM') %>%
  ggplot(aes(x=Layer,y=Ratio),color=as.numeric(Layer))+geom_bar(size=1,stat="identity",fill="darkblue")+
  scale_x_reverse()+
  coord_flip()+
  facet_grid(frame~Experiment)
    

#========================================================================#
# Gain-lost per layer
#========================================================================# 

AreaY %>% filter(Strain=='dwss') %>%
  mutate(SumSliceYlost=-SumSliceYlost)%>%
  gather(key=Area_type,value='Area',SumSliceYgain,SumSliceYlost,factor_key = TRUE) %>% 
  ggplot(aes(x=Layer,y=Area,group=Area_type))+
    geom_line(aes(color=Area_type),size=2)+
    geom_hline(yintercept=0)+
    facet_grid(frame~data_stamp)

AreaY %>% filter(Strain=='SM') %>%
  mutate(SumSliceYlost=-SumSliceYlost)%>%
  gather(key=Area_type,value='Area',SumSliceYgain,SumSliceYlost,factor_key = TRUE) %>% 
  ggplot(aes(x=Layer,y=Area,group=Area_type))+
  geom_line(aes(color=Area_type),size=2)+
  geom_hline(yintercept=0)+
  facet_grid(frame~data_stamp)






AreaY %>% filter(SumSliceY!=0 & Strain=='SM') %>%
  mutate(GainTotal=SumSliceYgain/SumSliceY,LostTotal=-SumSliceYlost/SumSliceY,SameTotal=SumSliceYsame/SumSliceY) %>%
  select(-SumSliceYgain,-SumSliceYlost,-SumSliceYsame,-SameTotal) %>%
  gather(key=Area_type,value='Area',GainTotal,LostTotal,factor_key = TRUE) %>% 
  filter(Area>-20) %>% 
  ggplot(aes(x=Layer,y=Area,group=Area_type))+
  geom_line(aes(color=Area_type),size=2)+
  ylim(-4,4)+
  geom_hline(yintercept=0)+
  facet_grid(frame~data_stamp)




