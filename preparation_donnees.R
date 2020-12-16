sensor_1_P1 <- read.csv("~/Cours/Supagro/Projet_Pneu/1. 1er passage-20201214/sensor_1.csv", header=FALSE)
sensor_1_P2 <- read.csv("~/Cours/Supagro/Projet_Pneu/1. 1er passage-20201214/sensor_2.csv", header=FALSE)
sensor_2 <- read.csv("~/Cours/Supagro/Projet_Pneu/2. 2eme passage-20201214/sensor_1.csv", header=FALSE)
simple_log_1 <- read.csv("~/Cours/Supagro/Projet_Pneu/1. 1er passage-20201214/simple-log_1.txt", header=FALSE)
simple_log_2 <- read.csv("~/Cours/Supagro/Projet_Pneu/2. 2eme passage-20201214/simple-log.txt", header=FALSE)

#Tous ont apparemment les millisecondes. Importer avec fread

#sensor_1_P1$sensor_id <- substr(sensor_1_P1$sensor_id, 14, 14)

sensor_1 <- rbind(sensor_1_P1, sensor_1_P2)
#sensor <- rbind(sensor_1, sensor_2_1)


sensor_clean_2=data.frame(matrix(1:length(sensor_2[,1]),nrow=length(sensor_2[,1])))
names(sensor_clean_2)[1] <- "ID_mesure"


sensor_clean_2$sensor_ID= gsub(".*:", "", sensor_2$V1)
sensor_2$V2= gsub("time:", "", sensor_2$V2)

temp1=gsub("T.*", "", sensor_2$V2)
temp1=gsub(".*_", "", temp1)

temp2=gsub(".*T", "", sensor_2$V2)
temp2=gsub("Z", "", temp2)

sensor_clean_2$time=paste(temp1, temp2, sep=" ")
sensor_clean_2$value= gsub("}", "", gsub(".*:", "", sensor_2$V3))


##

#simple_log <- rbind(simple.log_1, simple.log_2)
names(simple_log_2)[1] <- "time"
names(simple_log_2)[2] <- "date_GNSS"
names(simple_log_2)[3] <- "heure_GNSS"
names(simple_log_2)[4] <- "y"
names(simple_log_2)[5] <- "x"
names(simple_log_2)[6] <- "vitesse"


#sensor_clean_2$time
#as.POSIXct(sensor_clean_2$time, format = "%Y-%m-%d %H:%M:%OS3")

#d <- as.POSIXct(sensor_clean_2$time)
#format(d,"%Y-%m-%d %H:%M:%OS3")s

simple_log_2$time=substr(simple_log_2$time, 1, 19)
sensor_clean_2$time=substr(sensor_clean_2$time, 1, 19)

##
#data= merge(simple_log, sensor_clean, by.x = "time", by.y = "time")
##

#simple_log_2=simple_log_2[-which(is.na(simple_log_2$time)),]

#

simple_log_2$ID_simple=1:length(simple_log_2$time)



#simple_log_2=simple_log_2[-which(is.na(simple_log_2$x)),]

library(tidyverse)

sensor_clean_2$value=as.numeric(sensor_clean_2$value)

sensor_clean_clean_2= sensor_clean_2 %>% group_by(sensor_ID, time) %>% summarise(median = median(value))

sensor_clean_clean_2=as.data.frame(sensor_clean_clean_2)

require(lubridate)

sensor_clean_clean_2$time=ymd_hms(sensor_clean_clean_2$time)
sensor_clean_2$time=ymd_hms(sensor_clean_2$time)
simple_log_2$time=ymd_hms(simple_log_2$time)





# sensor_clean_clean_2$time=ymd_hms(sensor_clean_clean_2$time)
# #sensor_clean_clean_2$time=sensor_clean_clean_2$time-hours(1)
# simple_log_2$time=ymd_hms(simple_log_2$time)



#M=matrix(nrow=length(sensor_clean_clean_2[,1]))
#Boucle stupide et très lente
# j=1
# for (i in 1:length(sensor_clean_clean_2[,1])){
#   while (sensor_clean_clean_2$time[i]>simple_log_2$time[j]) {
#     j=j+1
#   }
#   sensor_clean_clean_2$V2[i]=simple_log_2$V2[j]
#   sensor_clean_clean_2$V3[i]=simple_log_2$V3[j]
#   sensor_clean_clean_2$y[i]=simple_log_2$y[j]
#   sensor_clean_clean_2$x[i]=simple_log_2$x[j]
#   sensor_clean_clean_2$V6[i]=simple_log_2$V6[j]
# }


j=1
for (i in 1:length(sensor_clean_2[,1])){
  while (sensor_clean_2$time[i]>simple_log_2$time[j]) {
    j=j+1
  }
  sensor_clean_2$date_GNSS[i]=simple_log_2$date_GNSS[j]
  sensor_clean_2$heure_GNSS[i]=simple_log_2$heure_GNSS[j]
  sensor_clean_2$y[i]=simple_log_2$y[j]
  sensor_clean_2$x[i]=simple_log_2$x[j]
  sensor_clean_2$vitesse[i]=simple_log_2$vitesse[j]
}







fig <- economics
fig <- fig %>% tidyr::gather(variable, value, -date)
fig <- fig %>% transform(id = as.integer(factor(variable)))
fig <- fig %>% plot_ly(x = ~date, y = ~value, color = ~variable, colors = "Dark2",
                       yaxis = ~paste0("y", id))
fig <- fig %>% add_lines()
fig <- fig %>% subplot(nrows = 5, shareX = TRUE)

fig



















