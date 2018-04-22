#install.packages("ggplot2")
#library(ggplot2)
library(ggplot2)
library(gridExtra)
library(dplyr)

############## Preprocessing ####################################

#Loading dataset
obama <- x[,c("commonweight_vv_post","tookpost","gender","educ","race","pid7","CC16_326","CC16_410a","CC16_331_1","CC16_331_2","CC16_331_3","CC16_331_7")]
obama

#Choosing Obama and those who voted for him

obama <- obama[which(obama$CC16_326 == "Barack Obama" & obama$tookpost == "Yes"),]
obama

#Binry variable for trump votes
Trump = ifelse(obama$CC16_410a == "Donald Trump (Republican)","Y","N")

obama$Trump <- Trump 
obama



#Replacing with other for least significant races
obama$race[obama$race == 'Asian' | obama$race == 'Native American' | obama$race == 'Mixed' | obama$race == 'Middle Eastern'] <- 'Other'
obama

#Counting number of positive responses of immigrants

CC163311 = ifelse(obama$CC16_331_1 == "Yes",1,0)
CC163312 = ifelse(obama$CC16_331_2 == "Yes",1,0)
CC163313 = ifelse(obama$CC16_331_3== "Yes",1,0)
CC163317 = ifelse(obama$CC16_331_7 == "Yes",1,0)

obama$CC163311 <- CC163311
obama$CC163312 <- CC163312
obama$CC163313 <- CC163313
obama$CC163317 <- CC163317


obama$Count = obama$CC163311 + obama$CC163312 + obama$CC163313 + obama$CC163317
obama

#Numeric version of ordered categorical variables
obama$party <- as.numeric(factor(obama$pid7 , levels=c("Strong Democrat","Not very strong Democrat","Lean Democrat","Independent","Lean Republican","Not very strong Republican","Strong Republican")))

obama$education <- as.numeric(factor(obama$educ , levels=c("No HS","High school graduate","Some college","2-year","4-year","Post-grad")))

obama

# Rearranging everything obama to "O"bama

Obama <- obama[,c("commonweight_vv_post","tookpost","gender","educ", "education", "race","pid7","party","CC16_326","CC16_410a","Trump","CC16_331_1","CC16_331_2","CC16_331_3","CC16_331_7","CC163311","CC163312","CC163313","CC163317","Count")]

#Omiting NA values
Obama[complete.cases(Obama),]

install.packages("dplyr")

Obama$race<-recode(Obama$race, White = "White", Black="Black",Hispanic="Hispanic", .default = "Other")

############## Question 1 ####################################

# weighted proportion of Obama voters in each demographic group that switched to trump
#install.packages("readstata13")
par(xpd=TRUE)
# For variable gender (Female)
Obama_new <- Obama[which(Obama$Trump=='Y'),]
a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$gender=='Female')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$gender=='Female')]))
prop_female = b/a

# For variable gender (Male)
a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$gender=='Male')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$gender=='Male')]))
prop_male = b/a

#For variable educ

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$educ=='No HS')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$educ=='No HS')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$educ=='High school graduate')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$educ=='High school graduate')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$educ=='Some college')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$educ=='Some college')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$educ=='2-year')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$educ=='2-year')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$educ=='4-year')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$educ=='4-year')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$educ=='Post-grad')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$educ=='Post-grad')]))
prop = b/a



# for variable race

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$race=='White')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$race=='White')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$race=='Black')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$race=='Black')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$race=='Hispanic')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$race=='Hispanic')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$race=='Other')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$race=='Other')]))
prop = b/a


# for variable pid7

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$pid7=='Strong Democrat')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$pid7=='Strong Democrat')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$pid7=='Not very strong Democrat')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$pid7=='Not very strong Democrat')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$pid7=='Lean Democrat')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$pid7=='Lean Democrat')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$pid7=='Independent')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$pid7=='Independent')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$pid7=='Lean Republican')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$pid7=='Lean Republican')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$pid7=='Not very strong Republican')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$pid7=='Not very strong Republican')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$pid7=='Strong Republican')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$pid7=='Strong Republican')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(Obama$pid7=='Not sure')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$pid7=='Not sure')]))
prop = b/a


# weighted proportion of Obama voters in each Immigration attribute variable that switched to trump

Obama_new <- Obama[which(obama$Trump=='Y'),]

a <- sum(as.numeric(obama$commonweight_vv_post[which(obama$Count==0)]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$Count==0)]))
prop = b/a

a <- sum(as.numeric(obama$commonweight_vv_post[which(obama$Count=='1')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$Count=='1')]))
prop = b/a

a <- sum(as.numeric(obama$commonweight_vv_post[which(obama$Count=='2')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$Count=='2')]))
prop = b/a

a <- sum(as.numeric(obama$commonweight_vv_post[which(obama$Count=='3')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$Count=='3')]))
prop = b/a

a <- sum(as.numeric(Obama$commonweight_vv_post[which(obama$Count=='4')]))
b <- sum(as.numeric(Obama_new$commonweight_vv_post[which(Obama_new$Count=='4')]))
prop = b/a

data <- data.frame(x = c(0,1,2,3,4), y = c(0.07286873,0.1081605,0.1034236,0.1163758,0.1751409))
#k = plot(x = c(0,1,2,3,4), y= c(0.07286873,0.1081605,0.1034236,0.1163758,0.1751409),xlab = "Immigration attitude variable",ylab = "Weighted proportion of Obama Voters shifted to Trump", main="weighted proportion of Trump voters Vs Immigration attitude")
p <- ggplot(data=data, aes(x=x, y= y)) +
  geom_line() +
  geom_point() +
  xlab("Immigration attitude variable") + ylab("Weighted proportion of Obama Voters shifted to Trump") + # Set axis labels
  ggtitle("weighted proportion of Trump voters Vs Immigration attitude")

p

############## Question 2 ####################################

Trump = ifelse(Obama$CC16_410a == "Donald Trump (Republican)",1,0)
Count <- Obama$Count

Obama$Trump <- Trump 

library(magrittr)
Obama$race_num<-unclass(Obama$race) %>% as.numeric
Obama$educ_num<-unclass(Obama$educ) %>% as.numeric 
Obama$gender_num<-unclass(Obama$gender) %>% as.numeric 
Obama$pid7_num<-unclass(Obama$pid7) %>% as.numeric
#Obama$Trump_num<- unclass(Obama$Trump) %>% as.numeric

#Obama$Count<-Obama$Count

race_model_i<-glm(Trump~race_num*Count,family="binomial",weights = commonweight_vv_post,data = Obama)
educ_model_i<-glm(Trump~educ_num*Count,family="binomial",weights = commonweight_vv_post,data = Obama)
gender_model_i<-glm(Trump~gender_num*Count,family="binomial",weights = commonweight_vv_post,data = Obama)
pid7_model_i<-glm(Trump~pid7_num*Count,family="binomial",weights = commonweight_vv_post,data = Obama)

race_model<-glm(Trump~race_num+Count,family="binomial",weights = commonweight_vv_post,data = Obama)
educ_model<-glm(Trump~educ_num+Count,family="binomial",weights = commonweight_vv_post,data = Obama)
gender_model<-glm(Trump~gender_num+Count,family="binomial",weights = commonweight_vv_post,data = Obama)
pid7_model<-glm(Trump~pid7_num+Count,family="binomial",weights = commonweight_vv_post,data = Obama)



glm.grid<-expand.grid(race_num=c(1,2,3,4),Count=sort(unique(Obama$Count)))
trump.predict_i<-predict(race_model_i,type="response",newdata = glm.grid)
trump.df_i<-data.frame(glm.grid,fit=as.vector(trump.predict_i))
trump.predict<-predict(race_model,type="response",newdata = glm.grid)
trump.df<-data.frame(glm.grid,fit=as.vector(trump.predict))
ggplot()+geom_line(data = trump.df,aes(x=Count,y=fit,color="Blue"))+geom_line(data = trump.df_i,aes(x=Count,y=fit,color="Red"))+facet_wrap(~race_num)+ggtitle("Interaction of Immigration Attitude with Race")
#p2<-ggplot()+geom_line(data = trump.df,aes(x=Count,y=fit,group=race_num,color=race_num))+ggtitle("Race Without Interaction")
#p1<-ggplot()+geom_line(data = trump.df_i,aes(x=Count,y=fit,group=race_num,color=race_num))+ggtitle("Race With Interaction(Immigration Attitude)")
#grid.arrange(p1, p2, nrow = 1)
ggsave("race_model.png", width = 17.78, units = "cm")


glm.grid<-expand.grid(educ_num=sort(unique(Obama$educ_num)),Count=sort(unique(Obama$Count)))
trump.predict_i<-predict(educ_model_i,type="response",newdata = glm.grid)
trump.df_i<-data.frame(glm.grid,fit=as.vector(trump.predict_i))
trump.predict<-predict(educ_model,type="response",newdata = glm.grid)
trump.df<-data.frame(glm.grid,fit=as.vector(trump.predict))
ggplot()+geom_line(data = trump.df,aes(x=Count,y=fit,color="Blue"))+geom_line(data = trump.df_i,aes(x=Count,y=fit,color="Red"))+facet_wrap(~educ_num)+ggtitle("Interaction of Immigration Attitude with Education")
# p2<-ggplot()+geom_line(data = trump.df,aes(x=Count,y=fit,group=educ_num,color=educ_num))+ggtitle("Education Without Interaction")
# p1<-ggplot()+geom_line(data = trump.df_i,aes(x=Count,y=fit,group=educ_num,color=educ_num))+ggtitle("Education With Interaction(Immigration Attitude)")
# grid.arrange(p1, p2, nrow = 1)
ggsave("educ_model.png", width = 17.78, units = "cm")


glm.grid<-expand.grid(gender_num=sort(unique(Obama$gender_num)),Count=sort(unique(Obama$Count)))
trump.predict_i<-predict(gender_model_i,type="response",newdata = glm.grid)
trump.df_i<-data.frame(glm.grid,fit=as.vector(trump.predict_i))
trump.predict<-predict(gender_model,type="response",newdata = glm.grid)
trump.df<-data.frame(glm.grid,fit=as.vector(trump.predict))
ggplot()+geom_line(data = trump.df,aes(x=Count,y=fit,color="Blue"))+geom_line(data = trump.df_i,aes(x=Count,y=fit,color="Red"))+facet_wrap(~gender_num)+ggtitle("Interaction of Immigration Attitude with Gender")
# p2<-ggplot()+geom_line(data = trump.df,aes(x=Count,y=fit,group=gender_num,color=gender_num))+ggtitle("Gender Without Interaction")
# p1<-ggplot()+geom_line(data = trump.df_i,aes(x=Count,y=fit,group=gender_num,color=gender_num))+ggtitle("Gender With Interaction(Immigration Attitude)")
# grid.arrange(p1, p2, nrow = 1)
ggsave("gender_model.png", width = 17.78, units = "cm")


glm.grid<-expand.grid(pid7_num=sort(unique(Obama$pid7_num)),Count=sort(unique(Obama$Count)))
trump.predict_i<-predict(pid7_model_i,type="response",newdata = glm.grid)
trump.df_i<-data.frame(glm.grid,fit=as.vector(trump.predict_i))
trump.predict<-predict(pid7_model,type="response",newdata = glm.grid)
trump.df<-data.frame(glm.grid,fit=as.vector(trump.predict))
ggplot()+geom_line(data = trump.df,aes(x=Count,y=fit,color="Blue"))+geom_line(data = trump.df_i,aes(x=Count,y=fit,color="Red"))+facet_wrap(~pid7_num)+ggtitle("Interaction of Immigration Attitude with Party")
# p2<-ggplot()+geom_line(data = trump.df,aes(x=Count,y=fit,group=pid7_num,color=pid7_num))+ggtitle("Party Without Interaction")
# p1<-ggplot()+geom_line(data = trump.df_i,aes(x=Count,y=fit,group=pid7_num,color=pid7_num))+ggtitle("Party With Interaction(Immigration Attitude)")
# grid.arrange(p1, p2, nrow = 1)
ggsave("pid7_model.png", width = 17.78, units = "cm")



