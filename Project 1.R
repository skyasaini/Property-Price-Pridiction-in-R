
library(dplyr)
library(ggplot2)
library(car)
library(Metrics)
library(gvlma)
library(tidyverse)

test_df<-read.csv('D:\\Data science\\Project 1\\Property_Price_Test.csv')
head(test_df)

test_df$is.train<-ifelse(test_df$Id==-1,1,0)## adding a col. is.train with value of 0

train_df<-read.csv('D:\\Data science\\Project 1\\Property_Price_Train.csv')
head(train_df)

train_df$is.train<-ifelse(train_df$Id==-1,0,1)## adding a coloum is.train with value of 1
  
test_df$Sale_Price<-ifelse(train_df$Sale_Price==1,1,0)## adding a coloum Sale_price with value of 0

head(test_df)

head(train_df)

rbind(train_df,test_df)->house

head(house)
## EDA start
myfun=function(x)
{
    plot(x,house$Sale_Price,col='red')
    return(summary(x))
}
##coloum 2
myfun(house$Building_Class)

##coloum 3## NA
myfun(house$Zoning_Class)
house$Zoning_Class[is.na(house$Zoning_Class)]<-'RLD'
myfun(house$Zoning_Class)

house$Zoning_Class<-ifelse(house$Zoning_Class=="RHD"|house$Zoning_Class=="RLD","RHD-RLD",
                           ifelse(house$Zoning_Class!="RHD"|house$Zoning_Class!="RLD","Commer-FVR-RMD",
                                  as.character(house$Zoning_Class)))
house$Zoning_Class<-as.factor(house$Zoning_Class)
myfun(house$Zoning_Class)

##coloum 4 # NA
myfun(house$Lot_Extent)

ind<-which(is.na(house$Lot_Extent))
mean(house[ind,"Sale_Price"])
summary(house$Lot_Extent[is.na(house$Lot_Extent)]<-mean(house$Lot_Extent,na.rm=TRUE))

##coloum 5
myfun(house$Lot_Size)
summary(house$Lot_Size<-ifelse(house$Lot_Size<0,sqrt(house$Lot_Size^2),house$Lot_Size))## converting the -ve value inti +ve

ggplot(house,aes(x=Lot_Size,y=Sale_Price))+
  geom_point(shape=18,color="blue")+
geom_smooth(method = lm,linetype="dashed",color="blue")
myfun(house$Lot_Size)

## coloum 6
myfun(house$Road_Type)

##coloum 7
summary(house$Lane_Type)
plot(house$Lane_Type,house$Sale_Price)
tapply(house$Sale_Price,house$Road_Type,mean)
ind<-which(is.na(house$Lane_Type))
mean(house[ind,"Sale_Price"])

house%>%filter(Lane_Type=="Paved")%>%select(Sale_Price)->Amt_M
p<-ggplot(house[ind,],aes(x=Sale_Price))
q<-ggplot(Amt_M,aes(x=Sale_Price))

p+geom_histogram()
q+geom_histogram()

house$Lane_Type<-ifelse(is.na(house$Lane_Type),'NO',as.factor(house$Lane_Type))
house$Lane_Type<-as.factor(house$Lane_Type)
summary(house$Lane_Type)
house$Lane_Type<-factor(house$Lane_Type,labels = c("Grvl","Paved","NO"))
summary(house$Lane_Type)

## coloum 8
summary(house$Property_Shape)
plot(house$Property_Shape,house$Sale_Price,col='red')
house$Property_Shape<-ifelse(house$Property_Shape=='IR2'|house$Property_Shape=='IR3','IR2-IR3',as.character(house$Property_Shape))
house$Property_Shape<-as.factor(house$Property_Shape)
summary(house$Property_Shape)
plot(house$Property_Shape,house$Sale_Price,col='red')

##coloum 9
summary(house$Land_Outline)
plot(house$Land_Outline,house$Sale_Price,col='red')
house$Land_Outline<-ifelse(house$Land_Outline=="HLS"|house$Land_Outline=="Lvl","HLS-Lvl",as.character(house$Land_Outline))
house$Land_Outline<-as.factor(house$Land_Outline)

##coloum 10
summary(house$Utility_Type)
plot(house$Utility_Type,house$Sale_Price,col='red')
tapply(house$Sale_Price,house$Utility_Type,mean)
## needed to remove coloum 10

##coloum 11
summary(house$Lot_Configuration)
plot(house$Lot_Configuration,house$Sale_Price)
house$Lot_Configuration<-ifelse(house$Lot_Configuration=="FR3P"|house$Lot_Configuration=="I","FR3P-I",as.character(house$Lot_Configuration))
house$Lot_Configuration<-as.factor(house$Lot_Configuration)
plot(house$Lot_Configuration,house$Sale_Price,col='red')
house$Lot_Configuration<-ifelse(house$Lot_Configuration=='CulDSac'|house$Lot_Configuration=='FR2P','CulDSac-FR2P',as.character(house$Lot_Configuration))
house$Lot_Configuration<-as.factor(house$Lot_Configuration)
plot(house$Lot_Configuration,house$Sale_Price,col='red')

## coloum 12
summary(house$Property_Slope)
plot(house$Property_Slope,house$Sale_Price,col='red')

##coloum 13
summary(house$Neighborhood)
plot(house$Neighborhood,house$Sale_Price,col='red')
house$Neighborhood<-ifelse(house$Neighborhood=='Crawfor'|house$Neighborhood=='Gilbert'|
                             house$Neighborhood=='IDOTRR'|house$Neighborhood=='MeadowV'|
                             house$Neighborhood=='Mitchel'|house$Neighborhood=='NPkVill'|
                             house$Neighborhood=='NridgHt'|house$Neighborhood=='OldTown'|
                             house$Neighborhood=='Sawyer'|house$Neighborhood=='SawyerW'|
                             house$Neighborhood=='Somerst'|house$Neighborhood=='StoneBr'|
                             house$Neighborhood=='Veenker','C+G+IM+Mit+NPK+Nri+O+S+SaW+Som+Sto+Veen',
                           as.character(house$Neighborhood))
house$Neighborhood<-as.factor(house$Neighborhood)
summary(house$Neighborhood)
plot(house$Neighborhood,house$Sale_Price,col='red')
house$Neighborhood<-ifelse(house$Neighborhood=='BrkSide'|house$Neighborhood=='SWISU','BrkS+SWI',
                             as.character(house$Neighborhood))
house$Neighborhood<-as.factor(house$Neighborhood)
plot(house$Neighborhood,house$Sale_Price,col='red')
house$Neighborhood<-ifelse(house$Neighborhood=='Blmngtn'|house$Neighborhood=='Timber'
                           |house$Neighborhood=='ClearCr','Blmn+Timb+Clear',
                           as.character(house$Neighborhood))
house$Neighborhood<-as.factor(house$Neighborhood)
plot(house$Neighborhood,house$Sale_Price,col='red')
house$Neighborhood<-ifelse(house$Neighborhood=='BrDale'|house$Neighborhood=='NAmes','BrDa+NAmes',
                           as.character(house$Neighborhood))
house$Neighborhood<-as.factor(house$Neighborhood)
plot(house$Neighborhood,house$Sale_Price,col='red')


## coloum 14
summary(house$Condition1)
plot(house$Condition1,house$Sale_Price,col='red')
house$Condition1<-ifelse(house$Condition1=="Norm"|house$Condition1=="RRAn"|house$Condition1=='RRNn',"Norm+RRAn+RRNn",as.character(house$Condition1))
house$Condition1<-as.factor(house$Condition1)
summary(house$Condition1)
plot(house$Condition1,house$Sale_Price,col='red')
house$Condition1<-ifelse(house$Condition1=='Feedr'|house$Condition1=='PosA'|house$Condition1=='PosN'|house$Condition1=='RRAe'|house$Condition1=='RRNe','Feedr+PosA+PosN+RRAe+RRNe',as.character(house$Condition1))
house$Condition1<-as.factor(house$Condition1)
plot(house$Condition1,house$Sale_Price,col='red')


## coloum 15
summary(house$Condition2)
plot(house$Condition2,house$Sale_Price,col='red')
house$Condition2<-ifelse(house$Condition2=="Norm"|house$Condition2=="PosN","Norm+PosN",as.character(house$Condition2))
house$Condition2<-as.factor(house$Condition2)
summary(house$Condition2)
plot(house$Condition2,house$Sale_Price,col='red')
house$Condition2<-ifelse(house$Condition2=="Artery"|house$Condition2=="Feedr"|house$Condition2=='PosA',"Artery+Feedr+PosA",as.character(house$Condition2))
house$Condition2<-as.factor(house$Condition2)
summary(house$Condition2)
plot(house$Condition2,house$Sale_Price,col='red')


## coloum 16
summary(house$House_Type)
plot(house$House_Type,house$Sale_Price,col='red')
house$House_Type<-ifelse(house$House_Type=="Duplex"|house$House_Type=="Twnhs","Duplex+Twnhs",as.character(house$House_Type))
house$House_Type<-as.factor(house$House_Type)
plot(house$House_Type,house$Sale_Price,col='red')

## coloum 17
summary(house$House_Design)
plot(house$House_Design,house$Sale_Price,col='red')
house$House_Design<-ifelse(house$House_Design=='2.5Unf'|house$House_Design=='SFoyer','2.5Unf+SFoyer',as.character(house$House_Design))
house$House_Design<-as.factor(house$House_Design)
plot(house$House_Design,house$Sale_Price,col='red')
house$House_Design<-ifelse(house$House_Design=='1.5Fin'|house$House_Design=='1Story','1.5Fin+1Story',as.character(house$House_Design))
house$House_Design<-as.factor(house$House_Design)
plot(house$House_Design,house$Sale_Price,col='red')
house$House_Design<-ifelse(house$House_Design=='2Story'|house$House_Design=='SLvl','2Story+SLvl',as.character(house$House_Design))
house$House_Design<-as.factor(house$House_Design)
plot(house$House_Design,house$Sale_Price,col='red')

## coloum 18
summary(house$Overall_Material)
plot(house$Overall_Material,house$Sale_Price,col='red')
ggplot(house,aes(x=Overall_Material,y=Sale_Price))+
  geom_point(shape=18,color="blue")+
  geom_smooth(method = lm,linetype="dashed",color="blue")
tapply(house$Sale_Price,house$Overall_Material,mean)

## coloum 19
summary(house$House_Condition)
plot(house$House_Condition,house$Sale_Price,col='red')
tapply(house$Sale_Price,house$House_Condition,mean)

## coloum 20
summary(house$Construction_Year)
plot(house$Construction_Year,house$Sale_Price,col='red')
ggplot(house,aes(x=Construction_Year,y=Sale_Price))+
  geom_point(shape=18,color="blue")+
  geom_smooth(method = lm,linetype="dashed",color="blue")

## coloum 21
summary(house$Remodel_Year)
plot(house$Remodel_Year,house$Sale_Price,col='red')
ggplot(house,aes(x=Remodel_Year,y=Sale_Price))+
  geom_point(shape=18,color="blue")+
  geom_smooth(method = lm,linetype="dashed",color="blue")


## coloum 22
summary(house$Roof_Design)
plot(house$Roof_Design,house$Sale_Price,col='red')
house$Roof_Design<-ifelse(house$Roof_Design=='Gable'|house$Roof_Design=='Shed','Gable+Shed',as.character(house$Roof_Design))
house$Roof_Design<-as.factor(house$Roof_Design)
plot(house$Roof_Design,house$Sale_Price,col='red')


## coloum 23
summary(house$Roof_Quality)
plot(house$Roof_Quality,house$Sale_Price,col='red')
house$Roof_Quality<-ifelse(house$Roof_Quality=='SS'|house$Roof_Quality=='TG','SS+TG',as.character(house$Roof_Quality))
house$Roof_Quality<-as.factor(house$Roof_Quality)
plot(house$Roof_Quality,house$Sale_Price,col='red')


## coloum 24
summary(house$Exterior1st)## How to remove NA
house$Exterior1st[is.na(house$Exterior1st)]<-c('VinylSd')
summary(house$Exterior1st)
plot(house$Exterior1st,house$Sale_Price,col='red')
tapply(house$Sale_Price,house$Exterior1st,mean)
house$Exterior1st<-ifelse(house$Exterior1st=='MetalSd'|house$Exterior1st=='Plywood'|house$Exterior1st=='WdShing'|house$Exterior1st=='CemntBd',"MetalSd+Plywood+WdShing+CemntBd",as.character(house$Exterior1st))
house$Exterior1st<-as.factor(house$Exterior1st)
plot(house$Exterior1st,house$Sale_Price,col='red')
summary(house$Exterior1st)
house$Exterior1st<-ifelse(house$Exterior1st=='AsbShng'|
                            house$Exterior1st=='BrkComm',"AsbShng+BrkComm",as.character(house$Exterior1st))
house$Exterior1st<-as.factor(house$Exterior1st)
plot(house$Exterior1st,house$Sale_Price,col='red')
summary(house$Exterior1st)
house$Exterior1st<-ifelse(house$Exterior1st=='HdBoard'|
                            house$Exterior1st=='Stucco',"HdBoard+Stucco",as.character(house$Exterior1st))
house$Exterior1st<-as.factor(house$Exterior1st)
plot(house$Exterior1st,house$Sale_Price,col='red')
summary(house$Exterior1st)
house$Exterior1st<-ifelse(house$Exterior1st=='AsphShn'|
                            house$Exterior1st=='VinylSd',"AsphShn+VinylSd",as.character(house$Exterior1st))
house$Exterior1st<-as.factor(house$Exterior1st)
plot(house$Exterior1st,house$Sale_Price,col='red')
summary(house$Exterior1st)


## coloum 25
summary(house$Exterior2nd)##Needed to remove NA
house$Exterior2nd[is.na(house$Exterior2nd)]<-c('VinylSd')
summary(house$Exterior1st)
plot(house$Exterior2nd,house$Sale_Price,col='red')
tapply(house$Sale_Price,house$Exterior2nd,mean)
house$Exterior2nd<-ifelse(house$Exterior2nd=='Brk Cmn'|house$Exterior2nd=='CBlock'|
                            house$Exterior2nd=='CmentBd'|house$Exterior2nd=='MetalSd'|house$Exterior2nd=='VinylSd'|
                            house$Exterior2nd=='Wd Shng',
                          'Brk Cmn+CBlock+CmentBd+MetalSd+VinylSd+Wd Shng',as.character(house$Exterior2nd))
house$Exterior2nd<-as.factor(house$Exterior2nd)
plot(house$Exterior2nd,house$Sale_Price,col='red')
summary(house$Exterior2nd)
house$Exterior2nd<-ifelse(house$Exterior2nd=='BrkFace'|house$Exterior2nd=='HdBoard'|
                            house$Exterior2nd=='Stucco',
                          'BrkFace+HdBoard+Stucco',as.character(house$Exterior2nd))
house$Exterior2nd<-as.factor(house$Exterior2nd)
plot(house$Exterior2nd,house$Sale_Price,col='red')
summary(house$Exterior2nd)
house%>%filter(Exterior2nd=='AsphShn'|Exterior2nd=='ImStucc')%>%select(Sale_Price,Exterior2nd)->a
plot(a$Exterior2nd,a$Sale_Price)
a
## column 26##Needed to remove NA  ??
summary(house$Brick_Veneer_Type)
tapply(house$Sale_Price,house$Brick_Veneer_Type,mean)
house$Brick_Veneer_Type[is.na(house$Brick_Veneer_Type)]<-c('None')
summary(house$Brick_Veneer_Type)
plot(house$Property_Slope,house$Sale_Price,col='red')


## column 27## Need to remove NA  
summary(house$Brick_Veneer_Area)
plot(house$Brick_Veneer_Area,house$Sale_Price,col='red')
house$Brick_Veneer_Area[is.na(house$Brick_Veneer_Area)]<-0
summary(house$Brick_Veneer_Area)

## column 28
summary(house$Exterior_Material)
plot(house$Exterior_Material,house$Sale_Price,col='red')
house$Exterior_Material<-ifelse(house$Exterior_Material=='Ex'|
                                  house$Exterior_Material=='Fa'|
                                  house$Exterior_Material=='Gd','Ex+Fa+Gd',as.character(house$Exterior_Material))
house$Exterior_Material<-as.factor(house$Exterior_Material)
summary(house$Exterior_Material)
plot(house$Exterior_Material,house$Sale_Price,col='red')

## column 29
summary(house$Exterior_Condition)
plot(house$Exterior_Condition,house$Sale_Price,col='red')
house$Exterior_Condition<-ifelse(house$Exterior_Condition=='Ex'|
                                  house$Exterior_Condition=='Fa'|
                                  house$Exterior_Condition=='Gd'|house$Exterior_Condition=='Po',
                                 'Ex+Fa+Gd+Po',as.character(house$Exterior_Condition))
house$Exterior_Condition<-as.factor(house$Exterior_Condition)
summary(house$Exterior_Condition)
plot(house$Exterior_Condition,house$Sale_Price,col='red')


## column 30
summary(house$Foundation_Type)
plot(house$Foundation_Type,house$Sale_Price,col='red')
house$Foundation_Type<-ifelse(house$Foundation_Type=='BT'|
                                house$Foundation_Type=='PC'|
                                house$Foundation_Type=='SL','BT+PC+SL',as.character(house$Foundation_Type))
house$Foundation_Type<-as.factor(house$Foundation_Type)
summary(house$Foundation_Type)
plot(house$Foundation_Type,house$Sale_Price,col='red')


## column 31##  Need to remove NA 
summary(house$Basement_Height)
plot(house$Basement_Height,house$Sale_Price,col='red')
house$Basement_Height<-as.character(house$Basement_Height)
house$Basement_Height[which(is.na(house$Basement_Height))]<-"NO"
house$Basement_Height<-ifelse(house$Basement_Height=='Ex'|
                                house$Basement_Height=='Fa','Ex+Fa',as.character(house$Basement_Height))
house$Basement_Height<-as.factor(house$Basement_Height)
summary(house$Basement_Height)
plot(house$Basement_Height,house$Sale_Price,col='red')

## column 32## need to remove NA 
summary(house$Basement_Condition)
plot(house$Basement_Condition,house$Sale_Price,col='red')
house$Basement_Condition<-as.character(house$Basement_Condition)
house$Basement_Condition[is.na(house$Basement_Condition)]<-'NO'
house$Basement_Condition<-ifelse(house$Basement_Condition=='Fa'|
                                house$Basement_Condition=='Po','Fa+Po',as.character(house$Basement_Condition))
house$Basement_Condition<-as.factor(house$Basement_Condition)
summary(house$Basement_Condition)
plot(house$Basement_Condition,house$Sale_Price,col='red')


## column 33## Need to remove NA 
summary(house$Exposure_Level)
plot(house$Exposure_Level,house$Sale_Price,col='red')
house$Exposure_Level<-as.character(house$Exposure_Level)
house$Exposure_Level[is.na(house$Exposure_Level)]<-'NO'
house$Exposure_Level<-ifelse(house$Exposure_Level=='Gd'|
                                   house$Exposure_Level=='Mn','Gd+Mn',as.character(house$Exposure_Level))
house$Exposure_Level<-as.factor(house$Exposure_Level)
summary(house$Exposure_Level)
plot(house$Exposure_Level,house$Sale_Price,col='red')


## column 34
summary(house$BsmtFinType1)## Need to remove NA
plot(house$BsmtFinType1,house$Sale_Price,col='red')
house$BsmtFinType1<-as.character(house$BsmtFinType1)
house$BsmtFinType1[is.na(house$BsmtFinType1)]<-'NO'
house$BsmtFinType1<-ifelse(house$BsmtFinType1=='GLQ'|
                               house$BsmtFinType1=='LwQ'|house$BsmtFinType1=='Rec',
                           'GLQ+LeQ+Rec',as.character(house$BsmtFinType1))
house$BsmtFinType1<-as.factor(house$BsmtFinType1)
summary(house$BsmtFinType1)
plot(house$BsmtFinType1,house$Sale_Price,col='red')

## column 35
summary(house$BsmtFinSF1)
plot(house$BsmtFinSF1,house$Sale_Price,col='red')
house$BsmtFinSF1[is.na(house$BsmtFinSF1)]<-mean(house$BsmtFinSF1)

## column 36## need to remove NA
summary(house$BsmtFinType2)
plot(house$BsmtFinType2,house$Sale_Price,col='red')
house$BsmtFinType2<-as.character(house$BsmtFinType2)
house$BsmtFinType2[is.na(house$BsmtFinType2)]<-'NO'
house$BsmtFinType2<-ifelse(house$BsmtFinType2=='ALQ'|
                             house$BsmtFinType2=='BLQ'|house$BsmtFinType2=='GLQ',
                           'ALQ+BLQ+GLQ',as.character(house$BsmtFinType2))
house$BsmtFinType2<-as.factor(house$BsmtFinType2)
summary(house$BsmtFinType2)
plot(house$BsmtFinType2,house$Sale_Price,col='red')


## column 37## NA
summary(house$BsmtFinSF2)
plot(house$BsmtFinSF2,house$Sale_Price,col='red')
house$BsmtFinSF2[is.na(house$BsmtFinSF2)]<-0
summary(house$BsmtFinSF2)


## column 38
summary(house$BsmtUnfSF)##NA
plot(house$BsmtUnfSF,house$Sale_Price,col='red')
house$BsmtUnfSF[is.na(house$BsmtUnfSF)]<-467
summary(house$BsmtUnfSF)

## column 39## NA
summary(house$Total_Basement_Area)
plot(house$Total_Basement_Area,house$Sale_Price,col='red')
house$Total_Basement_Area[is.na(house$Total_Basement_Area)]<-989
summary(house$Total_Basement_Area)


## column 40
summary(house$Heating_Type)
plot(house$Heating_Type,house$Sale_Price,col='red')


## column 41
summary(house$Heating_Quality)
plot(house$Heating_Quality,house$Sale_Price,col='red')
house$Heating_Quality<-ifelse(house$Heating_Quality=='Ex'|
                                house$Heating_Quality=='Po'|
                                house$Heating_Quality=='TA','Ex+Po+TA',as.character(house$Heating_Quality))
house$Heating_Quality<-as.factor(house$Heating_Quality)
summary(house$Heating_Quality)
plot(house$Heating_Quality,house$Sale_Price,col='red')

## column 42
summary(house$Air_Conditioning)
plot(house$Air_Conditioning,house$Sale_Price,col='red')

## column 43
summary(house$Electrical_System)
plot(house$Electrical_System,house$Sale_Price,col='red')
house$Electrical_System[is.na(house$Electrical_System)]<-c('SBrkr')
house$Electrical_System<-ifelse(house$Electrical_System=='FuseP'|
                                  house$Electrical_System=='SBrkr','FuseP+SBrkr',as.character(house$Electrical_System) )
house$Electrical_System<-as.factor(house$Electrical_System)
summary(house$Electrical_System)
plot(house$Electrical_System,house$Sale_Price,col='red')


## column 44
summary(house$First_Floor_Area)
plot(house$First_Floor_Area,house$Sale_Price,col='red')


## column 45
summary(house$Second_Floor_Area)
plot(house$Second_Floor_Area,house$Sale_Price,col='red')


## column 46
summary(house$LowQualFinSF)
plot(house$LowQualFinSF,house$Sale_Price,col='red')


## column 47
summary(house$Grade_Living_Area)
plot(house$Grade_Living_Area,house$Sale_Price,col='red')


## column 48 ## Need to remove NA
summary(house$Underground_Full_Bathroom)
plot(house$Underground_Full_Bathroom,house$Sale_Price,col='red')
house$Underground_Full_Bathroom[is.na(house$Underground_Full_Bathroom)]<-0
summary(house$Underground_Full_Bathroom)

## column 49 ## Need to remove NA
summary(house$Underground_Half_Bathroom)
plot(house$Underground_Half_Bathroom,house$Sale_Price,col='red')
house$Underground_Half_Bathroom[is.na(house$Underground_Half_Bathroom)]<-0

## column 50
summary(house$Full_Bathroom_Above_Grade)
plot(house$Full_Bathroom_Above_Grade,house$Sale_Price,col='red')

## column 51
summary(house$Half_Bathroom_Above_Grade)
plot(house$Half_Bathroom_Above_Grade,house$Sale_Price,col='red')

## column 52
summary(house$Bedroom_Above_Grade)
plot(house$Bedroom_Above_Grade,house$Sale_Price,col='red')

## column 53
summary(house$Kitchen_Above_Grade)
plot(house$Kitchen_Above_Grade,house$Sale_Price,col='red')

## column 54
summary(house$Rooms_Above_Grade)
plot(house$Rooms_Above_Grade,house$Sale_Price,col='red')

## column 55
summary(house$Kitchen_Quality)
plot(house$Kitchen_Quality,house$Sale_Price,col='red')
house$Kitchen_Quality[is.na(house$Kitchen_Quality)]<-'TA'
house$Kitchen_Quality<-ifelse(house$Kitchen_Quality=='Ex'|
                                house$Kitchen_Quality=='TA','Ex+TA',as.character(house$Kitchen_Quality))
house$Kitchen_Quality<-as.factor(house$Kitchen_Quality)
summary(house$Kitchen_Quality)
plot(house$Kitchen_Quality,house$Sale_Price,col='red')

## column 56
summary(house$Functional_Rate)
plot(house$Functional_Rate,house$Sale_Price,col='red')
house$Functional_Rate[is.na(house$Functional_Rate)]<-c('TF')
house$Functional_Rate<-ifelse(house$Functional_Rate=='MD1'|
                                house$Functional_Rate=='MD2','MD1+MD2',as.character(house$Functional_Rate))
house$Functional_Rate<-as.factor(house$Functional_Rate)
summary(house$Functional_Rate)
plot(house$Functional_Rate,house$Sale_Price,col='red')

## column 57
summary(house$Fireplaces)
plot(house$Fireplaces,house$Sale_Price,col='red')

## column 58
summary(house$Fireplace_Quality)
plot(house$Fireplace_Quality,house$Sale_Price,col='red')
house$Fireplace_Quality<-as.character(house$Fireplace_Quality)
house$Fireplace_Quality[is.na(house$Fireplace_Quality)]<-'NO'
house$Fireplace_Quality<-ifelse(house$Fireplace_Quality=='Fa'|house$Fireplace_Quality=='Po','Fa+Po',as.character(house$Fireplace_Quality))
house$Fireplace_Quality<-as.factor(house$Fireplace_Quality)
tapply(house$Sale_Price,house$Fireplace_Quality,mean)
summary(house$Fireplace_Quality)
plot(house$Fireplace_Quality,house$Sale_Price,col='red')

## column 59
summary(house$Garage)
plot(house$Garage,house$Sale_Price,col='red')
house$Garage<-as.character(house$Garage)
house$Garage[is.na(house$Garage)]<-'No'
house$Garage<-ifelse(house$Garage=='2TFes'|house$Garage=='BuiltIn'|house$Garage=='Detchd',
                     '2TFes+BuiltIn+Detchd',as.character(house$Garage))
house$Garage<-as.factor(house$Garage)
summary(house$Garage)
plot(house$Garage,house$Sale_Price,col='red')

## column 60 ## NA found
summary(house$Garage_Built_Year)
plot(house$Garage_Built_Year,house$Sale_Price,col='red')
house$Garage_Built_Year[is.na(house$Garage_Built_Year)]<-1979
summary(house$Garage_Built_Year)

## column 61 ## NA found
summary(house$Garage_Finish_Year)
plot(house$Garage_Finish_Year,house$Sale_Price,col='red')
house$Garage_Finish_Year<-as.character(house$Garage_Finish_Year)
house$Garage_Finish_Year[is.na(house$Garage_Finish_Year)]<-'NO'
house$Garage_Finish_Year<-ifelse(house$Garage_Finish_Year=='Fin'|house$Garage_Finish_Year=='Unf',
                                 'Fin+Unf',as.character(house$Garage_Finish_Year))
house$Garage_Finish_Year<-as.factor(house$Garage_Finish_Year)
summary(house$Garage_Finish_Year)
plot(house$Garage_Finish_Year,house$Sale_Price,col='red')

## column 62## NA Found replaced with median
summary(house$Garage_Size)
plot(house$Garage_Size,house$Sale_Price,col='red')
house$Garage_Size[is.na(house$Garage_Size)]<-2

## column 63 ## NA
summary(house$Garage_Area)
plot(house$Garage_Area,house$Sale_Price,col='red')
house$Garage_Area[is.na(house$Garage_Area)]<-477

## column 64
summary(house$Garage_Quality)
plot(house$Garage_Quality,house$Sale_Price,col='red')
house$Garage_Quality<-as.character(house$Garage_Quality)
house$Garage_Quality[is.na(house$Garage_Quality)]<-'NO'
house$Garage_Quality<-ifelse(house$Garage_Quality=='Po'|house$Garage_Quality=='TA',
                                 'Fin+Unf',as.character(house$Garage_Quality))
house$Garage_Quality<-as.factor(house$Garage_Quality)
summary(house$Garage_Quality)
plot(house$Garage_Quality,house$Sale_Price,col='red')


## column 65
summary(house$Garage_Condition)
plot(house$Garage_Condition,house$Sale_Price,col='red')
house$Garage_Condition<-as.character(house$Garage_Condition)
house$Garage_Condition[is.na(house$Garage_Condition)]<-'NO'
house$Garage_Condition<-ifelse(house$Garage_Condition=='Fa'|house$Garage_Condition=='TA',
                             'Fa+TA',as.character(house$Garage_Condition))
house$Garage_Condition<-as.factor(house$Garage_Condition)
summary(house$Garage_Condition)
plot(house$Garage_Condition,house$Sale_Price,col='red')

## column 66
summary(house$Pavedd_Drive)
plot(house$Pavedd_Drive,house$Sale_Price,col='red')
house$Pavedd_Drive<-ifelse(house$Pavedd_Drive=='N'|house$Pavedd_Drive=='P','N+P',as.character(house$Pavedd_Drive))
house$Pavedd_Drive<-as.factor(house$Pavedd_Drive)
summary(house$Pavedd_Drive)
plot(house$Pavedd_Drive,house$Sale_Price,col='red')

## column 67
summary(house$W_Deck_Area)
plot(house$W_Deck_Area,house$Sale_Price,col='red')
house$W_Deck_Area<-ifelse(house$W_Deck_Area<0,sqrt(house$W_Deck_Area^2),house$W_Deck_Area)
summary(house$W_Deck_Area)

## column 68
summary(house$Open_Lobby_Area)
plot(house$Open_Lobby_Area,house$Sale_Price,col='red')
house$Open_Lobby_Area<-ifelse(house$Open_Lobby_Area<0,sqrt(house$Open_Lobby_Area^2),house$Open_Lobby_Area)
summary(house$Open_Lobby_Area)


## column 69
summary(house$Enclosed_Lobby_Area)
plot(house$Enclosed_Lobby_Area,house$Sale_Price,col='red')
house$Enclosed_Lobby_Area<-ifelse(house$Enclosed_Lobby_Area<0,sqrt(house$Enclosed_Lobby_Area^2),house$Enclosed_Lobby_Area)
summary(house$Enclosed_Lobby_Area)

## column 70
summary(house$Three_Season_Lobby_Area)
plot(house$Three_Season_Lobby_Area,house$Sale_Price,col='red')


## column 71
summary(house$Screen_Lobby_Area)
plot(house$Screen_Lobby_Area,house$Sale_Price,col='red')


## column 72
summary(house$Pool_Area)
plot(house$Pool_Area,house$Sale_Price,col='red')


## column 73
summary(house$Pool_Quality)
plot(house$Pool_Quality,house$Sale_Price,col='red')
house$Pool_Quality<-as.character(house$Pool_Quality)
house$Pool_Quality[is.na(house$Pool_Quality)]<-'NO'
house$Pool_Quality<-as.factor(house$Pool_Quality)
summary(house$Pool_Quality)
plot(house$Pool_Quality,house$Sale_Price,col='red')

## column 74
summary(house$Fence_Quality)
plot(house$Fence_Quality,house$Sale_Price,col='red')
house$Fence_Quality<-as.character(house$Fence_Quality)
house$Fence_Quality[is.na(house$Fence_Quality)]<-'NO'
house$Fence_Quality<-as.factor(house$Fence_Quality)
house$Fence_Quality<-ifelse(house$Fence_Quality=='GdWo'|house$Fence_Quality=='MnPrv','GdWo+MnPrv',
                            as.character(house$Fence_Quality))
house$Fence_Quality<-as.factor(house$Fence_Quality)
summary(house$Fence_Quality)
plot(house$Fence_Quality,house$Sale_Price,col='red')


## column 75
summary(house$Miscellaneous_Feature)
plot(house$Miscellaneous_Feature,house$Sale_Price,col='red')
house$Miscellaneous_Feature<-as.character(house$Miscellaneous_Feature)
house$Miscellaneous_Feature[is.na(house$Miscellaneous_Feature)]<-'NO'
house$Miscellaneous_Feature<-as.factor(house$Miscellaneous_Feature)
summary(house$Miscellaneous_Feature)
plot(house$Miscellaneous_Feature,house$Sale_Price,col='red')


## column 76
summary(house$Miscellaneous_Value)
plot(house$Miscellaneous_Value,house$Sale_Price,col='red')



## column 77
summary(house$Month_Sold)
plot(house$Month_Sold,house$Sale_Price,col='red')


## column 78
summary(house$Year_Sold)
plot(house$Year_Sold,house$Sale_Price,col='red')


## column 79
summary(house$Sale_Type)
plot(house$Sale_Type,house$Sale_Price,col='red')
house$Sale_Type[is.na(house$Sale_Type)]<-'New'
house$Sale_Type<-ifelse(house$Sale_Type=='COD'|house$Sale_Type=='Con'|
                          house$Sale_Type=='ConLD'|
                          house$Sale_Type=='Oth'|house$Sale_Type=='CWD','COD+Con+ConLD+Oth+CWD',
                            as.character(house$Sale_Type))
house$Sale_Type<-as.factor(house$Sale_Type)
summary(house$Sale_Type)
plot(house$Sale_Type,house$Sale_Price,col='red')

##column 80
summary(house$Sale_Condition)
plot(house$Sale_Condition,house$Sale_Price,col='red')
house$Sale_Condition<-ifelse(house$Sale_Condition=='AdjLand'|house$Sale_Condition=='Family','AdjLamd+Family',
                          as.character(house$Sale_Condition))
house$Sale_Condition<-as.factor(house$Sale_Condition)
summary(house$Sale_Condition)
plot(house$Sale_Condition,house$Sale_Price,col='red')

head(house)
house[is.na(house)]

## deviding the house data set
train_df1<-house%>%filter(is.train==1)
head(train_df1)
test_df1<-house%>%filter(is.train==0)                          
head(test_df1)
#spiliting data sets into test and training datasets
set.seed(1)
indext<-sample(nrow(train_df1),.75*nrow(train_df1),replace = F)
x<-train_df1[indext,]
y<-train_df1[-indext,]



model_1 <- lm(Sale_Price~.,data = x)
summary(model_1) 
#stepwise regression
step(model_1,direction = "both")

model_2<-lm(formula = Sale_Price ~ Zoning_Class + Lot_Size + Road_Type + 
              Land_Outline + Property_Slope + Neighborhood + Condition1 + 
              House_Type + Overall_Material + House_Condition + Construction_Year + 
              Remodel_Year + Roof_Quality + Exterior1st + Brick_Veneer_Type + 
              Brick_Veneer_Area + Exterior_Material + Basement_Height + 
              Exposure_Level + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + Air_Conditioning + 
              First_Floor_Area + Second_Floor_Area + Underground_Full_Bathroom + 
              Bedroom_Above_Grade + Kitchen_Above_Grade + Kitchen_Quality + 
              Rooms_Above_Grade + Functional_Rate + Fireplaces + Fireplace_Quality + 
              Garage_Size + Garage_Quality + Garage_Condition + Open_Lobby_Area + 
              Screen_Lobby_Area + Pool_Area + Pool_Quality + Month_Sold + 
              Sale_Condition, data = x)

summary(model_2)

## dummy variabls 
summary(x$Lot_Size)
x$Road_Typed1<-ifelse(x$Road_Type=='Paved',1,0)
summary(x$Land_Outline)
x$Land_Outlined1<-ifelse(x$Land_Outline=="HLS-Lvl",1,0)

summary(x$Property_Slope)
x$Property_Sloped1<-ifelse(x$Property_Slope=='MS',1,0)

summary(x$Neighborhood)
x$Neighborhoodd1<-ifelse(x$Neighborhood=='BrkS+SWI',1,0)
x$Neighborhoodd2<-ifelse(x$Neighborhood=='C+G+IM+Mit+NPK+Nri+O+S+SaW+Som+Sto+Veen',1,0)
x$Neighborhoodd3<-ifelse(x$Neighborhood=='NoRidge',1,0)

summary(x$Condition1)
x$Condition1d1<-ifelse(x$Condition1=='Norm+RRAn+RRNn',1,0)

x$House_Typed1<-ifelse(x$House_Type=='Duplex+Twnhs',1,0)
x$House_Typed2<-ifelse(x$House_Type=='TwnhsE',1,0)

x$Roof_Qualityd1<-ifelse(x$Roof_Quality=='SS+TG',1,0)
x$Exterior1std1<-ifelse(x$Exterior1st=='BrkFace',1,0)
summary(x$Exterior_Material)
x$Exterior_Materiald1<-ifelse(x$Exterior_Material=='TA',1,0)

x$Basement_Heightd1<-ifelse(x$Basement_Height=='Gd',1,0)
x$Basement_Heightd2<-ifelse(x$Basement_Height=='TA',1,0)
x$Exposure_Leveld1<-ifelse(x$Exposure_Level=='Gd+Mn',1,0)
x$Functional_Rated1<-ifelse(x$Functional_Rate=='TF',1,0)
x$Functional_Rated2<-ifelse(x$Functional_Rate=='MS',1,0)

x$Kitchen_Qualityd1<-ifelse(x$Kitchen_Quality=='Gd',1,0)
x$Fireplace_Qualityd1<-ifelse(x$Fireplace_Quality=='Fa+Po',1,0)
x$Fireplace_Qualityd2<-ifelse(x$Fireplace_Quality=='Gd',1,0)
x$Fireplace_Qualityd3<-ifelse(x$Fireplace_Quality=='TA',1,0)
x$Fireplace_Qualityd4<-ifelse(x$Fireplace_Quality=='NO',1,0)

x$Garaged1<-ifelse(x$Garage=='Attchd',1,0)
summary(x$Garage_Condition)
x$Garage_Qualityd1<-ifelse(x$Garage_Quality=='Fa',1,0)
x$Garage_Qualityd2<-ifelse(x$Garage_Quality=='Fin+Unf',1,0)
x$Garage_Qualityd3<-ifelse(x$Garage_Quality=='Gd',1,0)

x$Garage_Conditiond1<-ifelse(x$Garage_Condition=='Fa+TA',1,0)
x$Garage_Conditiond2<-ifelse(x$Garage_Condition=='Gd',1,0)
x$Pool_Qualityd1<-ifelse(x$Pool_Quality=='Fa',1,0)
x$Pool_Qualityd2<-ifelse(x$Pool_Quality=='Gd',1,0)
x$Pool_Qualityd3<-ifelse(x$Pool_Quality=='NO',1,0)

x$Sale_Conditiond1<-ifelse(x$Sale_Condition=='Normal',1,0)
x$Sale_Conditiond2<-ifelse(x$Sale_Condition=='Partial',1,0)
summary(x$Sale_Type)

## dummy variable in y
## dummy variabls 
summary(y$Lot_Size)
y$Road_Typed1<-ifelse(y$Road_Type=='Paved',1,0)
summary(y$Land_Outline)
y$Land_Outlined1<-ifelse(y$Land_Outline=="HLS-Lvl",1,0)

summary(y$Property_Slope)
y$Property_Sloped1<-ifelse(y$Property_Slope=='MS',1,0)

summary(y$Neighborhood)
y$Neighborhoodd1<-ifelse(y$Neighborhood=='BrkS+SWI',1,0)
y$Neighborhoodd2<-ifelse(y$Neighborhood=='C+G+IM+Mit+NPK+Nri+O+S+SaW+Som+Sto+Veen',1,0)
y$Neighborhoodd3<-ifelse(y$Neighborhood=='NoRidge',1,0)

summary(y$Condition1)
y$Condition1d1<-ifelse(y$Condition1=='Norm+RRAn+RRNn',1,0)

y$House_Typed1<-ifelse(y$House_Type=='Duplex+Twnhs',1,0)
y$House_Typed2<-ifelse(y$House_Type=='TwnhsE',1,0)

y$Roof_Qualityd1<-ifelse(y$Roof_Quality=='SS+TG',1,0)
y$Exterior1std1<-ifelse(y$Exterior1st=='BrkFace',1,0)
summary(y$Exterior_Material)
y$Exterior_Materiald1<-ifelse(y$Exterior_Material=='TA',1,0)

y$Basement_Heightd1<-ifelse(y$Basement_Height=='Gd',1,0)
y$Basement_Heightd2<-ifelse(y$Basement_Height=='TA',1,0)
y$Exposure_Leveld1<-ifelse(y$Exposure_Level=='Gd+Mn',1,0)
y$Functional_Rated1<-ifelse(y$Functional_Rate=='TF',1,0)
y$Functional_Rated2<-ifelse(y$Functional_Rate=='MS',1,0)

y$Kitchen_Qualityd1<-ifelse(y$Kitchen_Quality=='Gd',1,0)
y$Fireplace_Qualityd1<-ifelse(y$Fireplace_Quality=='Fa+Po',1,0)
y$Fireplace_Qualityd2<-ifelse(y$Fireplace_Quality=='Gd',1,0)
y$Fireplace_Qualityd3<-ifelse(y$Fireplace_Quality=='TA',1,0)
y$Fireplace_Qualityd4<-ifelse(y$Fireplace_Quality=='NO',1,0)

y$Garaged1<-ifelse(y$Garage=='Attchd',1,0)
summary(y$Garage_Condition)
y$Garage_Qualityd1<-ifelse(y$Garage_Quality=='Fa',1,0)
y$Garage_Qualityd2<-ifelse(y$Garage_Quality=='Fin+Unf',1,0)
y$Garage_Qualityd3<-ifelse(y$Garage_Quality=='Gd',1,0)

x$Garage_Conditiond2<-ifelse(x$Garage_Condition=='Gd',1,0)

y$Garage_Conditiond1<-ifelse(y$Garage_Condition=='Fa+TA',1,0)
y$Pool_Qualityd1<-ifelse(y$Pool_Quality=='Fa',1,0)
y$Pool_Qualityd2<-ifelse(y$Pool_Quality=='Gd',1,0)
y$Pool_Qualityd3<-ifelse(y$Pool_Quality=='NO',1,0)

y$Sale_Conditiond1<-ifelse(y$Sale_Condition=='Normal',1,0)
y$Sale_Conditiond2<-ifelse(y$Sale_Condition=='Partial',1,0)
summary(y$Sale_Type)


model_3<-lm(formula = Sale_Price ~ Zoning_Class + Lot_Size + Road_Type + 
               
             Overall_Material + House_Condition + Construction_Year + 
              Remodel_Year  + Brick_Veneer_Type + 
              Brick_Veneer_Area  + 
               BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + Air_Conditioning + 
              First_Floor_Area + Second_Floor_Area + Underground_Full_Bathroom + 
              Bedroom_Above_Grade + Kitchen_Above_Grade  + 
              Rooms_Above_Grade  + Fireplaces + 
              Garage_Size + Garage_Quality  + Open_Lobby_Area + 
              Screen_Lobby_Area + Pool_Area  + Month_Sold +
           
               Land_Outlined1+Property_Sloped1+Neighborhoodd1+Neighborhoodd2+Neighborhoodd3+Condition1d1+House_Typed1+
              House_Typed2+Roof_Qualityd1+Exterior1std1+Exterior_Materiald1+Basement_Heightd1+Basement_Heightd2+
              Exposure_Leveld1+Functional_Rated1+Functional_Rated2+Kitchen_Qualityd1+Fireplace_Qualityd1+Fireplace_Qualityd2+Fireplace_Qualityd3+Fireplace_Qualityd4+Garaged1+Garage_Qualityd1+Garage_Qualityd2+Garage_Qualityd3+
              Garage_Conditiond1+Garage_Conditiond2+Pool_Qualityd1+Pool_Qualityd2+Pool_Qualityd3+Sale_Conditiond1+
              Sale_Conditiond2,data = x)

summary(model_3)

model_4<-lm(formula = Sale_Price ~ Zoning_Class + Lot_Size + Road_Type + 
              
              Overall_Material + House_Condition + Construction_Year +
              Brick_Veneer_Area  + 
              BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + 
              First_Floor_Area + Second_Floor_Area +
              Bedroom_Above_Grade + Kitchen_Above_Grade  + 
              Rooms_Above_Grade  + Fireplaces + 
              Garage_Size   +
              Screen_Lobby_Area + Pool_Area  + Month_Sold +
              
              Land_Outlined1+Property_Sloped1+Neighborhoodd1+Neighborhoodd2+Neighborhoodd3+Condition1d1+House_Typed1+
              House_Typed2+Roof_Qualityd1+Exterior1std1+Exterior_Materiald1+Basement_Heightd1+Basement_Heightd2+
              Exposure_Leveld1+Functional_Rated1+Functional_Rated2+
              Pool_Qualityd1+Pool_Qualityd2+Pool_Qualityd3+Sale_Conditiond1+
              Sale_Conditiond2,data = x)

summary(model_4)
model_5<-lm(formula = Sale_Price ~ Zoning_Class + Lot_Size + Road_Type + 
              
              Overall_Material + House_Condition + Construction_Year +
              Brick_Veneer_Area  + 
              BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + 
              First_Floor_Area + Second_Floor_Area +
              Bedroom_Above_Grade + Kitchen_Above_Grade  + 
              Rooms_Above_Grade  + Fireplaces + 
              Garage_Size   +
              Screen_Lobby_Area + Pool_Area +
              
              Land_Outlined1+Property_Sloped1+Neighborhoodd1+Neighborhoodd2+Neighborhoodd3+Condition1d1+House_Typed1+
              House_Typed2+Roof_Qualityd1+Exterior1std1+Exterior_Materiald1+Basement_Heightd1+Basement_Heightd2+
              Exposure_Leveld1+Functional_Rated1+Functional_Rated2+
              Pool_Qualityd1+Pool_Qualityd2+Pool_Qualityd3+Sale_Conditiond1+
              Sale_Conditiond2,data = x)

summary(model_5)
hist(model_5$residuals)
qqPlot(model_5$residuals)
vif(model_5)

model_6<-lm(formula = log(Sale_Price) ~ Zoning_Class + Lot_Size + Road_Type + 
              
              Overall_Material + House_Condition + Construction_Year +
              Brick_Veneer_Area  + 
              BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + 
              First_Floor_Area + Second_Floor_Area +
              Bedroom_Above_Grade + Kitchen_Above_Grade  + 
              Rooms_Above_Grade  + Fireplaces + 
              Garage_Size   +
              Screen_Lobby_Area + Pool_Area +
              
              Land_Outlined1+Property_Sloped1+Neighborhoodd1+Neighborhoodd2+Neighborhoodd3+Condition1d1+House_Typed1+
              House_Typed2+Roof_Qualityd1+Exterior1std1+Exterior_Materiald1+Basement_Heightd1+Basement_Heightd2+
              Exposure_Leveld1+Functional_Rated1+Functional_Rated2+
              Pool_Qualityd1+Pool_Qualityd2+Pool_Qualityd3+Sale_Conditiond1+
              Sale_Conditiond2,data = x)

summary(model_6)
hist(model_6$residuals)
qqPlot(model_6$residuals)
vif(model_6)
model_7<-lm(formula = sqrt(Sale_Price) ~ Zoning_Class + Lot_Size + Road_Type + 
              
              Overall_Material + House_Condition + Construction_Year +
              Brick_Veneer_Area  + 
              BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + 
              First_Floor_Area + Second_Floor_Area +
              Bedroom_Above_Grade + Kitchen_Above_Grade  + 
              Rooms_Above_Grade  + Fireplaces + 
              Garage_Size   +
              Screen_Lobby_Area + Pool_Area +
              
              Land_Outlined1+Property_Sloped1+Neighborhoodd1+Neighborhoodd2+Neighborhoodd3+Condition1d1+House_Typed1+
              House_Typed2+Roof_Qualityd1+Exterior1std1+Exterior_Materiald1+Basement_Heightd1+Basement_Heightd2+
              Exposure_Leveld1+Functional_Rated1+Functional_Rated2+
              Pool_Qualityd1+Pool_Qualityd2+Pool_Qualityd3+Sale_Conditiond1+
              Sale_Conditiond2,data = x)

summary(model_7)
hist(model_7$residuals)
qqPlot(model_7$residuals)
vif(model_7)
plot(model_7$fitted.values,model_7$residuals)
## multicilinearity operation + sqrt of lot size+ removing brick
model_8<-lm(formula = sqrt(Sale_Price) ~ Zoning_Class + Lot_Size + Road_Type + 
              
              Overall_Material + House_Condition + Construction_Year+
              BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + 
              First_Floor_Area + Second_Floor_Area +
              Bedroom_Above_Grade + Kitchen_Above_Grade  + 
              Rooms_Above_Grade  + Fireplaces + 
              Garage_Size   +
              Screen_Lobby_Area + Pool_Area +
              
              Land_Outlined1+Property_Sloped1+Neighborhoodd1+Neighborhoodd2+Neighborhoodd3+Condition1d1+House_Typed1+
              House_Typed2+Roof_Qualityd1+Exterior1std1+Exterior_Materiald1+Basement_Heightd1+Basement_Heightd2+
              Exposure_Leveld1+Functional_Rated1+Functional_Rated2+
              Pool_Qualityd1+Sale_Conditiond1+
              Sale_Conditiond2,data = x)

summary(model_8)
hist(model_8$residuals)
qqPlot(model_8$residuals)
vif(model_8)
plot(model_8$fitted.values,model_8$residuals)

predicted<-model_8$fitted.values
actual<-sqrt(x$Sale_Price)

dat<-data.frame(predicted,actual)

p<-ggplot(dat,aes(x=row(dat)[,2],y=predicted))
p+geom_line(colour="blue")+geom_line(data=dat,aes(y=actual),colour="red")
library(Metrics)
mape(predicted,actual)

predictions<-predict(model_8,y)
predictions
rmse(y$Sale_Price,predictions)

