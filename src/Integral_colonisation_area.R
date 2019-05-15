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

IntegralArea_GALI<-AreaY %>%
  filter(Layer>200 & frame>5) %>%
  group_by(Strain,Experiment,data_stamp,frame) %>%
  summarise(IntAreaTotal=sum(SumSliceY)) 
#========================================================================#
# Fit linear regression
#========================================================================#
# model LogIntArea ~ Intersept + frame
# model LogIntArea ~ Intersept + frame
fit_lm<-function(df){
  lm(log(IntAreaTotal)~frame,data=df)
}
#======================================
df<-IntegralArea_GALI
## 1.nest data by experiments
by_experiment<-df %>%
  #exclude points that produced Nans
  filter(IntAreaTotal>0) %>%
  #converted frames to the minutes
  mutate(frame=60*frame) %>%
  group_by(Experiment) %>%
  nest()

#======================================
# 2. apply model to the data
by_experiment <- by_experiment %>%
  mutate(model = map(data, fit_lm)) %>%
  mutate(resids = map2(data,model, add_residuals)) %>%
  mutate(predict = map2(data,model, add_predictions))

#======================================
# 3. extract coefficients Intersept,frame
FitCoeff<-by_experiment %>%
  mutate(tidy_model=map(model,broom::tidy)) %>%
  unnest(tidy_model) %>%
  separate(Experiment,c('Strain'),sep='_',remove=FALSE,extra="drop") %>%
  mutate(Strain=as.factor(Strain))
  

#======================================
# 4. per model statistics
glance<-by_experiment %>%
  mutate(glance=map(model,broom::glance)) %>%
  unnest(glance,.drop=TRUE) %>%
  select(Experiment,r.squared)

#======================================
# 5. plot fit to the data and residuals
DataPredict<-by_experiment %>%
  unnest(predict) %>%
  mutate(pred=exp(pred)) %>%
  mutate(res=pred-IntAreaTotal) %>%
  gather(key=Area_type,value=Area,IntAreaTotal,pred)

#plot fit
ggplot(DataPredict,aes(x=frame,y=Area,color=Area_type,group=Experiment))+
  geom_line(aes(group=Area_type),size=1)+
  facet_wrap(.~Experiment,scales = "free")+
  labs(title="Fit to the data")

#plot residual
ggplot(DataPredict,aes(x=frame,y=res,group=Experiment))+
  geom_line(size=1)+
  facet_wrap(.~Experiment,scales = "free")+
  labs(title="Residuals")

#plot coefficients
# frame
FitCoeff %>% filter(term=="frame") %>%
 ggplot(aes(x=Experiment,y=estimate,fill=Strain,label = Experiment))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=.2,
                position=position_dodge(.9))+
  ylab("growth rate 1/min")+
  xlab("Experiment")+
  labs(title="colonization rate estimate")+
  coord_flip()+
  theme_grey(base_size = 12)


# Intersept
FitCoeff %>% filter(term=="(Intercept)") %>%
  ggplot(aes(x=Experiment,y=estimate,fill=Strain,label = Experiment))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=.2,
                position=position_dodge(.9))+
  ylab("Intersept")+
  xlab("Experiment")+
  labs(title="colonization Intersept estimate")+
  coord_flip()+
  theme_grey(base_size = 12)

# model quality extimate
glance %>% separate(Experiment,c('Strain'),sep='_',remove=FALSE,extra="drop") %>%
  mutate(Strain=as.factor(Strain)) %>%
  ggplot(aes(y=r.squared,x=Experiment, fill=Strain)) +
  geom_col()+
  xlab("Experiment")+
  coord_flip()+
  labs(title="Quality of model fit R.squared")






