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
# Integral of Areas across of all layer
# gain-lost-same per experiment
#========================================================================#
IntegralArea<-AreaY %>%
  group_by(Strain,Experiment,data_stamp,frame) %>%
  summarise(IntAreaTotal=sum(SumSliceY),IntAreaGain=sum(SumSliceYgain),IntAreaLost=sum(SumSliceYlost),IntAreaSame=sum(SumSliceYsame)) %>%
  mutate(GainTotal=IntAreaGain/IntAreaTotal,LostTotal=IntAreaLost/IntAreaTotal,SameTotal=IntAreaSame/IntAreaTotal) %>%
  select(-IntAreaGain,-IntAreaLost,-IntAreaSame) %>%
  gather(key=Area_type,value='Area',IntAreaTotal,GainTotal,LostTotal,SameTotal,factor_key = TRUE) 
  
# IntAreaTotal all in one
IntegralArea %>% filter(Area_type=='IntAreaTotal') %>% 
  ggplot(aes(x=frame,y=Area,color=Strain))+
  geom_line(aes(group=data_stamp),size=2)+
  ggtitle('Sum of areas across all Y-layers ') + 
  xlab("time frame in minutes") + ylab("Area")+
  theme_bw(base_size = 12)

# dwss gain-lost-same per experiment
IntegralArea %>% filter(Strain=='dwss') %>%
  ggplot(aes(x=frame,y=Area,color=Area_type))+
  geom_line(size=1)+
  facet_grid(Area_type~data_stamp,scales="free_y")+
  ggtitle('dwss Sum of areas across all Y-layers ') + 
  xlab("time frame in minutes") + ylab("log Area")+
  theme_bw(base_size = 12)

# SM gain-lost-same per experiment
IntegralArea %>% filter(Strain=='SM') %>%
  ggplot(aes(x=frame,y=Area,color=Area_type))+
  geom_line(aes(group=data_stamp),size=1)+
  facet_grid(Area_type~data_stamp,scales="free_y")+
  ggtitle('SM Sum of areas across all Y-layers ') + 
  xlab("time frame in minutes") + ylab("log Area")+
  theme_bw(base_size = 12)


#========================================================================#
# Fit linear regression
#========================================================================#
# model LogIntArea ~ Intersept + frame
fit_lm<-function(df){
  lm(log(Area)~frame,data=df)
}


## nest data by experiments
by_experiment<-IntegralArea %>%
  filter(Area_type=="IntAreaTotal" & Area>0) %>%
  ungroup(data_stamp) %>%
  select(-data_stamp,-Area_type) %>%
  group_by(Experiment) %>%
  nest()




# calculate approximation coefficients
by_experiment <- by_experiment %>% 
  mutate(model = map(data, fit_lm)) %>%
  mutate(resids = map2(data,model, add_residuals)) %>%
  mutate(predict = map2(data,model, add_predictions))
  
  
DataPredict<-by_experiment %>% 
  unnest(predict) %>%
  mutate(pred=exp(pred)) %>%
  mutate(res=pred-Area) %>%
  gather(key="Area_type",value="Area",Area,pred,factor_key = TRUE) 
  
  
ggplot(DataPredict,aes(x=frame,y=Area,color=Area_type,group=Experiment))+
    geom_line(aes(group=Area_type),size=2)+
    facet_wrap(.~Experiment)

ggplot(DataPredict,aes(x=frame,y=res,group=Experiment))+
  geom_line(size=2)+
  facet_wrap(.~Experiment)

# coefficients
coeff<-by_experiment %>%
  mutate(tidy_model=map(model,broom::tidy)) %>%
  unnest(tidy_model)
  

# per model statistics
glance<-by_experiment %>%
  mutate(glance=map(model,broom::glance)) %>%
  unnest(glance,.drop=TRUE) %>%
  arrange(r.squared)

glance %>% 
  ggplot(aes(Experiment, r.squared)) + 
  geom_jitter(width = 0.5)+coord_flip()  






