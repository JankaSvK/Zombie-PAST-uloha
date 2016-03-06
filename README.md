## Jana Bátoryová
6.3.2016

Nastavenie seedu:
```
set.seed(22111995)
```

###1. Jedna zombie - osem dní
Vytvoríme si funkciu ```novyDen```, ktorá vracia nový počet zombie po prejdení dňa.
Pre každého aktuálne zombíka pomocou Poissonoveho rozdelenia s parametrom ```5``` vypočítame 
odhadovaný počet obetí. Počet obetí je teda súčet cez všetkých zombíkov. 
Následne využijeme binomockého rozdelenia, ktoré nám poslúži pre odhad nakazených obetí, 
takže mu nastavíme parameter ```0.5```. Dokopy spolu so zombíkmi a novými pokusanámi obeťami 
získame nový počet zombíkov. 

Ďalej využijeme funkciu ```pocetZombieNDen```, ktorá odsimulujn ```n``` dní šírenia nákazy.

```
# novyDen(#početZombieNaZačiatkuDňa)
# - vracia počet zombie na konci dňa
novyDen <- function(aktZombie) {
  pocetObeti <- sum(rpois(aktZombie, 5))
  pocetNakazenych <- rbinom(1,pocetObeti, 0.5)
  spoluZombie <- pocetNakazenych + aktZombie
  return(spoluZombie)
}

# pocetZombieNDen(#početDní, #početZačínajúcichZombie)
# - vracia počet zombíkov po n-tom dni
pocetZombieNDen <- function(n, aktZombie) {
  pocetZombie <- aktZombie
  for(i in 1:as.integer(n)) {
    pocetZombie <- novyDen(pocetZombie)
  }
  return(pocetZombie)
}
```

Výsledok šírenia nákazy jedným zombíkom zistíme takto:
```
# koľko bude zombie na ôsmy deň? (7 nocí kusania)
print(pocetZombieNDen(7,1))
```
Výsledok: 
>4772

###2. Odhad strednej hodnoty zombie na ôsmy deň
Vytvoríme si vzorku 100 miest.
```
# vzorka 100 miest
udaje <- vector()
for(i in 1:100) {
  udaje <- c(udaje, pocetZombieNDen(7,1))
}
```
#####a) Bodový a intervalový odhad
Využijeme funkciu R-ka k vypočítaniu strednej hodnoty, ktorú môžeme považovať za bodový odhad.
```
# bodový odhad
paste("Bodovy odhad na 8. den: ", mean(udaje))
```
> [1] "Bodovy odhad na 8. den:  6348.82"

R-ko má taktiež implementovanú funkciu ```t.test``` z ktorej použijeme konfidenčný interval.
```
# 95% konfidenčný interval
t.test(udaje, conf.int=TRUE)$conf.int
```
> [1] 5563.743 7133.897  
> attr(,"conf.level")  
> [1] 0.95  

#####b) Preukázateľnosť strednej hodnoty
Nie, nie je možné s istotou 95% preukázať, že stredná hodnota sa líši od 6000. Bodový odhad nám spadá do 95% konfidenčného intervalu, takže tvrdenie nie je možné s 5% hladinou významovosti preukázať.

#####c) Histogram
```
# histogram pre počet zombie v mestách
hist(udaje, main="Počet zombie v mestách", ylab="Počet miest", xlab="Počet zombie")
```
![histogram](http://atrey.karlin.mff.cuni.cz/~jankasvk/myself/hist1.png)


shapiro.test(udaje)

hist(udaje, prob=TRUE, breaks = 20, main = "Počet zombie v jednotlivých miestach v 8. deň", xlab = "Počet miest", ylab = "Počet zombie")
curve(dnorm(x, mean=mean(udaje), sd=sd(udaje)), add=TRUE)

#Vylepšení, brániaci sa ľudia.
pocetSkutocnychObeti <- function(pocetObeti){
  
}

denZombika <- function(pocetObeti){
  pocetZombikov <- 1
  for(i in 1:pocetObeti){
    pokusany <- rbinom(1,1,0.5)
    
    if(pokusany == 1){main = "Počet zombie v jednotlivých miestach v 8. deň", xlab = "Počet miest", ylab = "Počet zombie"
      pocetZombikov <- pocetZombikov + 1
    } else {
      zabilZombie <- rbinom(1,1,0.2)
      if(zabilZombie == 1){
        pocetZombikov <- pocetZombikov - 1
        break
      }
    }
  }
  return(pocetZombikov)
}
denZombika(9)

novyDenVylepsenychLudi <- function(aktZombie) {
  pocetZombie <- 0
  for(i in 1:aktZombie){
    pocetZombie <- pocetZombie + denZombika(rpois(1,5))
  }
  return(pocetZombie)
}

pocetZombieNDenVylepsenychLudi <- function(n, aktZombie) {
  pocetZombie <- aktZombie
  for(i in 1:as.integer(n)) {
    pocetZombie <- novyDenVylepsenychLudi(pocetZombie)
  }
  return(pocetZombie)
}

pocetZombieNDenVylepsenychLudi(7,1)
udaje <- vector()
for(i in 1:100) {
  udaje <- c(udaje, pocetZombieNDenVylepsenychLudi(7,1))
}

pocet <- 0
for(i in 1:length(udaje)){
  if(udaje[i] <= 1000) pocet <- pocet + 1
}
print(pocet)

paste("Bodovy odhad na 8. den: ", mean(udaje))

hist(udaje, breaks = 20, main = "#zombie v 8. deň s bojujúcimi ľudmi", xlab = "Počet miest", ylab = "Počet zombie")

paste("Pravdepodobnost, ze pcet neprekroci 1000: ", pocet/length(udaje), "%.")

```
