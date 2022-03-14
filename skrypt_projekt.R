#wczytanie bibliotek i instalacja
install.packages("knitr")
install.packages("psych")
install.packages("ggplot2")
install.packages("reshape2")
library(readxl)
library(psych)
library(ggplot2)
library(reshape2)

#wczytanie danych i przypisanie im nazwy
dane <- readxl::read_excel("dane2.xlsx")
names(dane)

#sprawdzanie klas zmiennych
class(dane$category)
class(dane$prizeAmount)
class(dane$name)
class(dane$birth_countryNow)
class(dane$affiliation_1)
class(dane$Wiek)

#przypisywanie nowych nazw zmiennym
dane$nagroda <- dane$`prizeAmount` 
dane$imie <- dane$`name` 
dane$kraj <- dane$`birth_countryNow` 
dane$uni <- dane$`affiliation_1` 
dane$wiek <- dane$`Wiek` 
dane$plec <- dane$`gender`

#średnia wieku oraz nagrody oraz informacje dotyczące zmiennej nagroda
mean(dane$wiek)
mean(dane$nagroda)


#sprawdzanie ile krajów znalazło się w naszym data frame
unique(dane$kraj)

#wyliczenie podstawowych statystyk dla wieku i nagrody
describe(dane$wiek)
describe(dane$nagroda)
zmien1 <- sd(dane$Wiek)/mean(dane$Wiek)
zmien1
zmien2 <- sd(normalized_nag)/mean(normalized_nag)
zmien2

#stworzenie funkcji, dzięki której wyrysujemy curve na histogramie
x <- seq(min(dane$wiek), max(dane$wiek), length = 40)
fun <- dnorm(x, mean = mean(dane$wiek), sd = sd(dane$wiek))

#histogram dla danych wieku + wyrysowanie curve
hist(dane$wiek,prob=TRUE, main="Wiek Noblistów",
     xlab="Wiek w latach",
     xlim=c(30,100),
     ylim=c(0,0.035),
     #ylab="Ilość osób w tym wieku",
     col="darkseagreen2")
lines(x, fun, col = "violet", lwd = 2)

#normalizacja dla danych nagrody, aby łatwiej było zauważyć różnicę w danych 
normalized_nag <- dane$nagroda/100000
normalized_nag

(summary(dane$wiek))
(summary(normalized_nag))
find_mode(dane$wiek)
#stworzenie funkcji, dzięki której wyrysujemy curve na histogramie
x1 <- seq(min(normalized_nag), max(normalized_nag), length = 40)
fun1 <- dnorm(x1, mean = mean(normalized_nag), sd = sd(normalized_nag))

#histogram dla danych nagrody + wyrysowanie curve
hist(normalized_nag,prob=TRUE, main="Nagroda pieniężna",
     xlab="Nagroda/100 000",
     #ylab="Ilość osób, które otrzymały nagrodę",
     ylim=c(0,0.06),
     col="thistle1")
lines(x1, fun1, col = "springgreen", lwd = 2)

#wyliczenie podstawowych statystyk dla znormalizowanych danych nagrody
describe(normalized_nag)

#informacje dotyczące zmiennej kraj
summary(dane$kraj)

#tworzenie contingency table pokazujacej liczebność grup
tab <- table(dane$kraj)
tab
tab2 <- table(dane$plec)

#liczenie procentów, aby pokazać je na wykresie kołowym
piepercent<- round(100*tab2/sum(tab2), 1)
pie(tab2, labels = piepercent, main = "Płeć wśród Noblistów w %", col= c("violet", "darkseagreen2"))
legend("bottomleft", legend = c("Kobieta", "Mężczyzna"), 
       fill =  c("violet", "darkseagreen2")) 

#tworzenie zmiennej zawierającej tylko osoby z usa
usa <- subset(dane, kraj == 'USA')
usa

#tworzenie contingency table pokazujacej liczebność grup
tab3 <- table(usa$kraj)
tab3

#procenty oraz pie chart 
piepercent2<- round(100*tab/sum(tab), 1)
pie(tab, labels = piepercent2, main = "Kraj pochodzenia Noblistów w %")

#regresja liniowa - nie dziala
scatter.smooth(x=dane$Wiek, y=normalized_nag, main="nag-wiek")
cor(dane$Wiek, normalized_nag, method = c("pearson"))
#linearMod <- lm(normalized_nag ~ dane$Wiek)  # build linear regression model on full data
#print(linearMod)
#summary(linearMod)
#plot(dane$wiek, normalized_nag, main = "Regresja liniowa",
 #    xlab = "wiek", ylab = "nagroda/100 000",
  #   pch = 19, frame = FALSE)
#abline(lm(wiek ~ nagroda, data = dane), col = "blue")

#korealcja
plot(dane$Wiek, normalized_nag, main="Korelacja", 
     xlab = "Wiek", ylab="Nagroda/100 000", 
     xlim = c(30,100),
     pch = 19, col = "lightblue")
abline(lm(normalized_nag ~ dane$Wiek), col = "red", lwd = 3)
text(paste("Correlation:", round(cor(dane$Wiek, normalized_nag), 2)))

#dominanta z prawdopodobieńśtwem 

sort(table(dane$wiek), decreasing = T)
prop.table(table(dane$wiek))

sort(table(normalized_nag), decreasing = T)
prop.table(table(normalized_nag))


