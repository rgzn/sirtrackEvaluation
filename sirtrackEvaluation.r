library(tidyverse)
library(adehabitatHR)
library(sp)




################## Sirtrack Data #################
pth<-"M:/COLLAR_ALL/Sirtrack/SirtrackCollarTestData"
list.sirtrack.files<-list.files(path = pth ,pattern="*.csv")

colnames.sirtrack<-c("GMT.Time", "Latitude", "Longitude", "Altitude", "Duration", 
                "Temperature", "DOP", "Satellites", "Cause.of.Fix")
sirtrack.data<-NULL
for (i in 1:length(list.sirtrack.files)){ # i=1
  next.file<-paste(pth,list.sirtrack.files[i],sep="/")
  next.collar <- read.csv(file=next.file,skip=4)
  sn<-substring(list.sirtrack.files[i],10,14)
  next.collar$SN<-rep(sn,nrow(next.collar))
  if(i==1){sirtrack.data<-next.collar}else{sirtrack.data<-rbind(sirtrack.data,next.collar)}
} 
sirtrack.data<-sirtrack.data[sirtrack.data$Duration<181,]
sirtrack.data<-sirtrack.data[!is.na(sirtrack.data$SN),]
sirtrack.data<-sirtrack.data[!is.na(sirtrack.data$Altitude),]

################## Collar Data #################
# Get information on the test collars from the collar summary spreadsheet

collar_file = paste(pth, "collar_summary.xlsx", sep = "/")
collars = readxl::read_excel(collar_file)
collars = collars %>% mutate(SN = as.character(collar))

test_data = left_join(sirtrack.data, collars, by = "SN" )

# histograms of fix time

ggplot(test_data, aes(Duration)) +
  geom_histogram(aes(fill = `fix type`)) +
  xlab("duration (s)")

ggplot(test_data, aes(Satellites)) +
  geom_histogram(aes(fill = `fix type`)) +
  xlab("# visible sats")

ggplot(test_data, aes(DOP)) +
  geom_histogram(aes(fill = `fix type`)) +
  xlab("DOP")

ggplot(test_data, aes(Altitude)) +
  geom_histogram(aes(fill = `fix type`)) +
  xlab("DOP")


(dur<-ggplot(data=sirtrack.data, aes(sirtrack.data$Duration)) + 
  geom_histogram() +  facet_wrap(facets="SN",ncol=2)) + xlab("Duration(secs)")
(sat<-ggplot(data=sirtrack.data, aes(sirtrack.data$Satellites)) + 
  geom_histogram() +  facet_wrap(facets="SN",ncol=2))+ xlab("Satellites visible")
(DOP<-ggplot(data=sirtrack.data, aes(sirtrack.data$DOP)) + 
  geom_histogram(bins=10) +  facet_wrap(facets="SN",ncol=2))+ xlab("Degree of Precision(DOP")

filename.out<-"AllSunnySlope_Sirtrack.csv"
write.csv(x = sirtrack.data, file = filename.out, row.names=F)  # write out summary data

(summ<-as.data.frame(sirtrack.data %>% group_by(SN)%>%summarize(N=n(),
                                 fix70_good=sum(Altitude > 1000 & Duration<71),
                                 fix70_bad=sum(Altitude < 1000 & Duration<71),
                                 Time_70=sum(Duration==70),
                                 fix180_good=sum(Altitude > 1000 & Duration > 70),
                                 fix180_bad=sum(Altitude < 1000 & Duration>70 ),
                                 Time_180=sum(Duration==180))))

# look at error from median
# convert lat long to utm  head(lotek.data)
sirtrack.data$DateTime<-strptime(as.character(sirtrack.data$GMT.Time),"%m/%d/%Y%I:%M:%S %p",tz="UTC")
str(sirtrack.data)
sirtrack.data1<- sirtrack.data[!is.na(sirtrack.data$Latitude) & sirtrack.data$Latitude !=0,]
utm.add<-sirtrack.data1[,c("Longitude", "Latitude")]
coordinates(utm.add)<-~Longitude+Latitude
proj4string(utm.add)<- CRS("+proj=longlat +datum=WGS84")  ## identify as lat long coord sys
utm.add <- spTransform(utm.add,CRS("+proj=utm  +zone=11 +north +ellps=WGS84 +datum=WGS84") ) # change to utm
utm.add.df<-format(as.data.frame(utm.add), scientific=FALSE)	# make df
names(utm.add.df)<-c("UTM_E","UTM_N")
utm.add.df$UTM_E<-as.numeric(utm.add.df$UTM_E);utm.add.df$UTM_N<-as.numeric(utm.add.df$UTM_N)
sirtrack.data.utm<-cbind(sirtrack.data1,utm.add.df)
sirtrack.data.utm<-sirtrack.data.utm[,c("SN","DateTime","GMT.Time","UTM_E","UTM_N", "Latitude", "Longitude", "Altitude", "Duration", 
                                "Temperature", "DOP", "Satellites", "Cause.of.Fix")]

filename.out<-"AllSunnySlope_Sirtrack_UTM.csv"
write.csv(x = sirtrack.data.utm, file = filename.out, row.names=F)  # write out summary data

