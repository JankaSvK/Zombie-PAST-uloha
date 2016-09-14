Úloha k získaniu zápočtu z predmetu Pravdepodobnosť a štatistika. 
Dokumentácia obsahuje priamo všetok kód napísaný v Rku pre vyriešenie úlohy.

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

###2. Odhady zombie na ôsmy deň
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

Z grafu by mohlo ísť o normálne rozdelenie, keďže sa to podobá na Gaussovu krivku.

#####d) Shapiro-Wilkov test
Test vyvrátil normalitu rozdelenia s istotou 0.99991925%.
```
# Shapirov test o normalite rozdelenia
shapiro.test(udaje)
```
> Shapiro-Wilk normality test  
> data:  udaje  
> W = 0.93366, p-value = 8.075e-05

#####e) Histogram s Gaussovou krivkou normálneh rozdelenia
```
# Histogram s krivkou normálneho rozdelenia
hist(udaje, prob=TRUE, breaks = 20, main = "Počet zombie v jednotlivých miestach v 8. deň", xlab = "Počet miest", ylab = "Počet zombie")
curve(col="red",dnorm(x, mean=mean(udaje), sd=sd(udaje)), add=TRUE)
```
![histogram](http://atrey.karlin.mff.cuni.cz/~jankasvk/myself/hist2.png)

###3. Ľudia schopní bránenia sa
Podobne ako v prvej úlohe vytvoríme funkcie:

```
# Simulácia dňa zombíka - zahrhňuje jeho možnú smrť
# denZombika(#početVyhliadnutýchObetí)
# - vráti počet zombíkov (vrátane prvého)
denZombika <- function(pocetObeti){
  pocetZombikov <- 1
  for(i in 1:pocetObeti){
    pokusany <- rbinom(1,1,0.5)
    if(pokusany == 1){
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

# Obdobne ako funkcia novyDen
novyDenVylepsenychLudi <- function(aktZombie) {
  pocetZombie <- 0
  for(i in 1:aktZombie){
    pocetZombie <- pocetZombie + denZombika(rpois(1,5))
  }
  return(pocetZombie)
}

# Obdobne ako funkcia pocetZombieNDenVylepsenychLudi
pocetZombieNDenVylepsenychLudi <- function(n, aktZombie) {
  pocetZombie <- aktZombie
  for(i in 1:as.integer(n)) {
    pocetZombie <- novyDenVylepsenychLudi(pocetZombie)
  }
  return(pocetZombie)
}

```

Pre ďalšie výpočty si pripravíme vzorku:
```
# Uloženie vzorových údajov vylepšených ľudí
udajeNovychLudi <- vector()
for(i in 1:100) {
  udajeNovychLudi <- c(udajeNovychLudi, pocetZombieNDenVylepsenychLudi(7,1))
}
```

#####a) Stredná hodnota počtu zombie
```
# Odhad strednej hodnoty
paste("Bodovy odhad na 8. den: ", mean(udajeNovychLudi))
```
> [1] "Bodovy odhad na 8. den:  871.34"

#####b) Histogram
```
# Histogram pre lepších ľudí
hist(udajeNovychLudi, breaks = 20, main = "#zombie v 8. deň s bojujúcimi ľudmi", xlab = "Počet miest", ylab = "Počet zombie")
```
![histogram](http://atrey.karlin.mff.cuni.cz/~jankasvk/myself/hist3.png)

#####c) Pravdepodobnosť, že ich na 8. deň nebude viac než 1000
Túto pravdepodobnosť vypočítame z našej vzorky a to ako podiel takých kedy ich nebolo viac než 1000 na ôsmy deň v pomere ku všetkým.
```
# Pravdepodobnosť, že počet zombí na začiatku 8. dňa neprekročí 1000.
pocet <- 0
for(i in 1:length(udajeNovychLudi)){
  if(udajeNovychLudi[i] <= 1000) pocet <- pocet + 1
}
paste("Pravdepodobnosť, že počet neprekročí 1000 je ", pocet/length(udajeNovychLudi), "%.")
```
> [1] "Pravdepodobnosť, že počet zombie neprekročí 1000 je 0.68 %."
