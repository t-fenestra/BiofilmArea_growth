library(tidyverse)
library(dplyr)
library(ggridges)
library(gridExtra)
library(stringr)
library(modelr)
library(broom)
library(ggpubr)
library(minpack.lm)
source("./src/LinearRegresionFit.R")

options(na.action = na.warn)
FolderToSavePlots='./output/figures/'

#========================================================================#
# Introduce new colums with strain type, frame number
#========================================================================#
# read file 
AreaY<-read_delim(file='data/ResultTable_SliceY.txt',col_names = TRUE,delim=',')

# extract frame and strain type from the name 
AreaY<-AreaY %>% 
  # new colums with strain
  separate(Experiment,c('Strain'),sep='_',remove=FALSE,extra="drop") %>%
  mutate(Strain=as.factor(Strain) )%>%
  # extract data frame
  mutate(frame=as.numeric(str_split(datafile,'_',simplify = TRUE)[,5]),data_stamp=as.numeric(str_split(datafile,'_',simplify = TRUE)[,3]))

# take 15 hours of experiment
AreaY<-AreaY %>% filter(frame %in% seq(from=1,to=15,by=1)) # frame 1-15

#========================================================================#
# Make ALI and GLI 
#========================================================================#

IntegralArea_ALI<-AreaY %>%
  filter(Layer<400 & frame>5) %>%
  group_by(Strain,Experiment,data_stamp,frame) %>%
  summarise(IntAreaTotal=sum(SumSliceY)) 

PlotALI<-IntegralArea_ALI %>% ggplot(aes(x=frame,y=IntAreaTotal,color=Strain))+
  geom_line(aes(group=data_stamp),size=2)+
  ggtitle('Integral area ALI') + 
  xlab("time frame in hours") + ylab("Area")+
  theme_bw(base_size = 12)

IntegralArea_GLI<-AreaY %>%
  filter(Layer>200 & frame>5) %>%
  group_by(Strain,Experiment,data_stamp,frame) %>%
  summarise(IntAreaTotal=sum(SumSliceY)) 

PlotGLI<-IntegralArea_GLI %>% ggplot(aes(x=frame,y=IntAreaTotal,color=Strain))+
  geom_line(aes(group=data_stamp),size=2)+
  ggtitle('Integral area GLI') + 
  xlab("time frame in minutes") + ylab("Area")+
  theme_bw(base_size = 12)

ggarrange(PlotALI,PlotGLI,ncol=2, nrow =1,common.legend = TRUE, legend="bottom")


#========================================================================#
# Fit model to the experiment one data point
#========================================================================#
IntegralArea_ALI_sample<-IntegralArea_ALI %>% 
  filter(Experiment=="SM_1_04232019_FR") %>%
  mutate(IntAreaTotal=IntAreaTotal+0.5) %>%
  mutate(frame=frame*60)

# estimate initial parameters
fit_lm<-function(df) lm(log(IntAreaTotal)~frame,data=df)
lm_result=fit_lm(IntegralArea_ALI_sample)
parameter=coef(lm_result)


#================ Test fit ================# 
Deff<-0.1*(pi/2)^2 #mkm2/s
Wa_test=parameter[2]/100
Ws_test=0.23
M_test=exp(parameter[1])

TestFit<-function(frame){
  Area<-M_test/(Wa_test-Ws_test+Deff)*(exp(Wa_test*frame)-exp(Ws_test*frame-Deff*frame))
  return(Area)
}
AreTestFit<-IntegralArea_ALI_sample %>% mutate(AreaTestFit=TestFit(frame))
AreTestFit %>% ggplot(aes(x=frame,y=IntAreaTotal))+geom_line()+
               geom_line(aes(x=frame,y=AreaTestFit),color="red")

#================================# 
#ModelFormula<-IntAreaTotal~ (M/(Wa-Ws+Deff)*(exp(Wa*frame)-exp(Ws*frame-Deff*frame)))
#nfit<-nlsLM(ModelFormula,IntegralArea_ALI_sample,start=list(M=M_test,Wa=Wa_test, Ws=Ws_test))
#cor(IntegralArea_ALI_sample$IntAreaTotal,predict(nfit))
#coefficients(nfit)
#summary(nfit)
#AreTestFit<-IntegralArea_ALI_sample %>% mutate(res=residuals(nfit),fit=fitted(nfit))

#AreTestFit %>% ggplot(aes(x=frame,y=IntAreaTotal))+
#  geom_line()+
#  geom_line(aes(x=frame,y=fit),color="red")+
#  geom_line(aes(x=frame,y=res),color="blue")

#fit_nls(IntegralArea_ALI_sample)
#========================================================================#
# Fit model to the experiment 
#========================================================================#
Deff<-0.1*(pi/2)^2 #mkm2/s
ModelFormula<- IntAreaTotal ~ M/(Wa-Ws+Deff)(exp(Wa*frame)-exp((Ws-Deff)*frame))

TestFitM<-function(FrameList,Wa,Ws,M){
  Area<-M/(Wa-Ws+Deff)*(exp(Wa*FrameList)-exp(Ws*FrameList-Deff*FrameList))
  return(Area)
}

fit_nls<-function(df) {
  lm_result<-lm(log(IntAreaTotal)~frame,data=df)
  parameter=coef(lm_result)
  #intersept
  Ws_start=0.25
  Wa_start=0.001
  M_start=exp(parameter[1])*((Wa_start-parameter[2]+Deff))
  #print(M_start)
  
  #Predict<-TestFitM(df$frame,Wa_start,Ws_start,M_start)
  #Coef_mean<-mean(df$IntAreaTotal/Predict)
  #print(Coef_mean)
  #M_start<-M_start*Coef_mean
  #print(M_start)
  result_fit<-nlsLM(ModelFormula,df,start=list(M=M_start,Wa=Wa_start, Ws=Ws_start))
  return(result_fit)
  }

#======================================
## 1.nest data by experiments
by_experiment<-IntegralArea_ALI %>%
  #replace points that produces nans after log
  mutate(IntAreaTotal=IntAreaTotal+0.5) %>%
  #converted frames to the minutes
  mutate(frame=60*frame) %>%
  group_by(Experiment) %>%
  nest() %>%
  mutate(Intersept=0,Wa=0,Ws=0,LogLik=0,AIC=0,BIC=0)

#======================================
# 2. apply model to the data
for (i in 1:nrow(by_experiment)){
  print(by_experiment$Experiment[i])
  result<-fit_nls(by_experiment$data[[i]])
  parameters<-coef(result)
  print(summary(result))
  print(parameters)
  
  by_experiment$Intersept[i]=parameters[1]
  by_experiment$Wa[i]=parameters[2]
  by_experiment$Ws[i]=parameters[3]
  by_experiment$LogLik[i]=logLik(result)
  by_experiment$AIC[i]=AIC(result)
  by_experiment$BIC[i]=BIC(result)
  
}
by_experiment <- by_experiment %>%
  mutate(model = map(data, fit_nls)) %>%
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
  unnest(glance,.drop=TRUE) 
  
