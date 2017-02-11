directory<-"/Users/tali/Documents/mortality_visualisation/submission/"

library(RCurl)
myfile <- getURL('https://data.code4sa.org/api/views/di7x-4ek4/rows.csv?accessType=DOWNLOAD', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
mydat <- read.csv(textConnection(myfile), header=T)


## data cleaning
mydata$maincause<-ifelse(mydata$Underlying_Broad_Grp=="A15-A19", "Tuberculosis", "other")
mdat<-matrix(c("Tuberculosis", "A15-A19", "HIV", "B20-B24", "Diabetes", "E10-E14", "Transport accidents", "V01-V99", "Other viral diseases", "B25-B34", "Intestinal infect. disease", "A00-A09", "Other accidents", "W00-X59","Assault", "X85-Y09","Unnatural, unknown intent", "Y10-Y34") , nrow=2)
mdat[2,2]
for (x in 1:9) {
  mydata$maincause<-ifelse(mydata$Underlying_Broad_Grp==mdat[2,x], mdat[1,x], mydata$maincause)
} 



# sex
mydata$sex<-ifelse(mydata$Sex==1, "male", ifelse(mydata$Sex==2,"female","unspecified"))
table(mydata$sex)
mydata$maincause<-ifelse(mydata$Underlying_Main_Grp=="R00-R99","Undiagnosed/unknown" , mydata$maincause)
mydata$maincause<-ifelse(mydata$Underlying_Main_Grp=="C00-D48","Cancer" , mydata$maincause)
mydata$maincause<-ifelse(mydata$Underlying_Main_Grp=="I00-I99","Vascular disease" , mydata$maincause)
mydata$maincause<-ifelse(mydata$Underlying_Main_Grp=="J00-J99","Respiratory disease" , mydata$maincause)
mydata$natural<-ifelse(mydata$NaturalUnnatural=="1","Natural Causes" , ifelse(mydata$NaturalUnnatural=="2","unnatural causes","stillbirth"))
## province

mydata$province[mydata$DeathProv==1]="Western Cape"
mydata$province[mydata$DeathProv==2]="Eastern Cape"
mydata$province[mydata$DeathProv==3]="Northern Cape"
mydata$province[mydata$DeathProv==4]="Free State"
mydata$province[mydata$DeathProv==5]="KwaZulu-Natal"
mydata$province[mydata$DeathProv==6]="North West"
mydata$province[mydata$DeathProv==7]="Gauteng"
mydata$province[mydata$DeathProv==8]="Mpumalanga"
mydata$province[mydata$DeathProv==9]="Limpopo"
mydata$province[mydata$DeathProv>96]="Unknown"


attach(mydata)
####DATES
#library(eeptools)
mydata$dob<-as.numeric(as.Date(ISOdate(BirthYear, BirthMonth, BirthDay)))
mydata$dod<-as.numeric(as.Date(ISOdate(DeathYear, DeathMonth, DeathDay)))
mydata$age<-as.numeric((mydata$dod-mydata$dob)/365.25)

##
mydata$all<-"any"


####smoker

mydata$smoke<-ifelse(mydata$Smoker==1,"yes",ifelse(mydata$Smoker==2,"no","unknown"))

myvars <- c("maincause", "natural", "age", "sex", "DeathInst", "province", "DeathProv", "smoke", "all")
mydata1<-na.omit(mydata[myvars])

## ommit stillbirths
mydata1<-(subset(mydata1,mydata1$natural!="stillbirth"))
summary(mydata1$maincause)

saveRDS(mydata1,paste0(directory,"data.rds"))  
