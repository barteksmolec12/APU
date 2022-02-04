# 7a
a <- 20/log(2.78)
b <- 2*a
min(a,b)

#7b
help(abs)

#7c

a <- seq(8,75)
 sqrt(mean(a^2))

#7d
??plot
 
#7e
setwd("C:/Users/Dell/Desktop/STUDIA/studia magisterskie/1 sem/Analiza uczenia maszynowego/lab1")
getwd()
cat(a,file="lab1test.txt")
remove(a)
a
data <- read.table(file = "lab1test.txt", header = TRUE)

#7f
install.packages("gridExtra")
setwd("C:/Users/Dell/Desktop/STUDIA/studia magisterskie/1 sem/Analiza uczenia maszynowego/lab1")
getwd()
a=read.csv("volcano.csv",na.strings = "")
b=a[seq(1,10),]
grid.table(b)

#7g
a=seq(1000,200,-8)
a

#7h
a=seq(50,30)
b=seq(4,50)
d=c(b,a)
d

#7i
tablet = c("iPad 1 iOS 9 2Ghz", "iPad 2s iOS 9 2Ghz", "iPad 3S iOS 9 2Ghz", "iPad mini iOS 9 2Ghz", "iPad pro iOS 9 2Ghz", "iPad XS iOS 9 2Ghz", "iPad Plus iOS 9 2Ghz", "iPad Ultra iOS 9 2Ghz", "iPad Eco iOS 9 2Ghz", "iPad Note iOS 9 2Ghz")
wyswietlacz = c("10''", "10''", "9''", "10''", "9,5''", "10''", "10''", "10''", "10''", "10''")
modem=c("LTE", "3G", "2G", "4G", "LTE", "LTE", "LTE", "LTE", "LTE", "LTE")
pamiec_ram = c("2 GB", "6 GB", "6 GB", "3 GB", "6,5 GB", "8 GB", "2 GB", "6 GB", "8 GB", "4 GB")
pamiec_wbudowana = c("64 GB", "64 GB", "128 GB", "128 GB", "128 GB", "128 GB", "128 GB", "128 GB", "256 GB", "128 GB")
cena = c(1700, 4500, 2000, 1700, 2099, 2000, 2500, 1700, 2400, 3000)
liczba_opinii = c("19", "44", "30", "100", "123", "34", "39", "29", "20", "11")
ramka=data.frame(tablet,modem,wyswietlacz,pamiec_ram,pamiec_wbudowana,cena,liczba_opinii)
sr_cena=mean(ramka[,"cena"])
sr_cena

#7j
nowy_tablet=data.frame(tablet="iPad NEW iOS 9 2Ghz",modem="LTE",wyswietlacz="10'",pamiec_ram="6 GB",pamiec_wbudowana="512 GB",cena=7000,liczba_opinii="29")
nowy_tablet
ramka=rbind(ramka,nowy_tablet)
sr_cena_2=mean(ramka[,"cena"])
sr_cena_2

#7k
ocena=c("5", "3.5", "4.5", "4.5", "2", "3", "3", "4", "3.5", "5", "5")
ocena
ramka = cbind(ramka, ocena)
ocena_5 = mean(ramka[ramka$ocena == "5", "cena"])
ocena_4_5 = mean(ramka[ramka$ocena == "4.5", "cena"])
ocena_4 = mean(ramka[ramka$ocena == "4", "cena"])
ocena_3_5 = mean(ramka[ramka$ocena == "3.5", "cena"])
ocena_3 = mean(ramka[ramka$ocena == "3", "cena"])
ocena_2_5 = mean(ramka[ramka$ocena == "2.5", "cena"])
ocena_2 = mean(ramka[ramka$ocena == "2", "cena"])
ocena_0_5 = mean(ramka[ramka$ocena == "0.5", "cena"])
ocena_0 = mean(ramka[ramka$ocena == "0", "cena"])

#7l
nowy_tablet_1=data.frame(tablet="iPad Tablet 1 iOS 9 2Ghz",modem="LTE",wyswietlacz="10'",pamiec_ram="6 GB",pamiec_wbudowana="512 GB",cena=7000,liczba_opinii="23",ocena="3.5")
nowy_tablet_2=data.frame(tablet="iPad Tablet 2 iOS 9 2Ghz",modem="LTE",wyswietlacz="10'",pamiec_ram="2 GB",pamiec_wbudowana="512 GB",cena=7000,liczba_opinii="23",ocena="4.5")
noy_tablet_3=data.frame(tablet="iPad Tablet 3 iOS 9 2Ghz",modem="LTE",wyswietlacz="10'",pamiec_ram="2 GB",pamiec_wbudowana="512 GB",cena=6000,liczba_opinii="23",ocena="3.0")
nowy_tablet_4=data.frame(tablet="iPad Tablet 4 iOS 9 2Ghz",modem="LTE",wyswietlacz="10'",pamiec_ram="2 GB",pamiec_wbudowana="512 GB",cena=3000,liczba_opinii="23",ocena="4")
nowy_tablet_3=data.frame(tablet="iPad Tablet 3 iOS 9 2Ghz",modem="LTE",wyswietlacz="10'",pamiec_ram="2 GB",pamiec_wbudowana="512 GB",cena=6000,liczba_opinii="23",ocena="3")
ramka=rbind(ramka,nowy_tablet_1)
ramka=rbind(ramka,nowy_tablet_2)
ramka=rbind(ramka,nowy_tablet_3)
ramka=rbind(ramka,nowy_tablet_4)
install.packages("plotrix")
library(plotrix)
count <- table(ramka$ocena)
barplot(count,main = "Liczba",ylim = c(0,10),xlab = "Ocena",ylab = "Ilość")


#7m
wykres_data <- table(ramka$ocena)/length(ramka$ocena)
pie(wykres_data)

#7n
status_opinii=c("nie ma","mniej 50 opinii","50-100 opinii","wicej niż 100 opinii","nie ma","mniej 50 opinii","50-100 opinii","wicej niż 100 opinii","nie ma","mniej 50 opinii","50-100 opinii","wicej niż 100 opinii","nie ma","mniej 50 opinii","50-100 opinii")
ramka=cbind(ramka,status_opinii)
wykres_data <- table(ramka$status_opinii)/length(ramka$status_opinii)
pie(wykres_data)

#7o
for(i in 1:nrow(ramka)) {
  print(paste(ramka[i,1 ]," ma ocenę klientów",ramka[i,8],", bo ma liczbę opinii",ramka[i,7]))
  
}

#7p
write.csv(ramka,"ramka_dane.csv")
ramka_read=read.csv("ramka_dane.csv")



