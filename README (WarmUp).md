# Vitajte

Tento kurz je venovaný osvojeniu si základných praktických zručností lineárnej regresie plne mapujúci prednášky doktora Lafférsa. Ako isto viete, samotná regresia sa pokladá za základ praktickej štatistiky využívanej po celom svete. Niektoré prednášky zahraničných profesorov dokonca označujú regresiu za nástroj na "všetko" a je to svojím spôsobom pravda. Princípy sa používajú v širokej škále štatistických nástrojov ako napríklad vypočítanie bankového klasifikátora na poskytovanie pôžičky žiadateľovi ba dokonca aj základ umelej inteligencie. 

Slovo **moderná** v názve predmetu je opodstatnené vďaka [prostrediu R](https://www.r-project.org/) a [RStudiu](https://www.rstudio.com/), ktoré sa dnes pokladá za jeden z najrýchlejšie rozvýjajúcich a najkompatibilnejších programovacích prostredí na Importovanie, úpravu, transformáciu, vizualiáciu a modelovanie dát. R slúži okrem klasického počítania (lepšej kalkulačky) a už spomenutých možnosti aj na vytváranie profesionálnych reportov. Okrem iného je tu možnosť zaobaliť vaše výsledky do interaktívnej online aplikácie.



# Sylabus

Môžeme voľne rozdelovať na okruhy inšpirované (Faraway) a doplnené (Wickham, Grolemund).  

1. **Exploratory data analysis** - v skratke EDA po slovensky Obhliadka dát pred analýzou, ktorá zahŕňa hlavne vizualizáciu.    
2. **Parameter estimation** - odhadnutie parametrov modelu nie je vždy jednoznačné. 
3. **Inference** - testy a konfidenčné intervaly.   
4. **Prediction** - ako model dokáže predikovať.    
5. **Diagnostics** - či model spĺňa predpoklady.    
6. **Predictors** - vlastnosti odhadnutých predikotorov.    
7. **Robust** - robustný model pomáha pri nezvyčajných pozorovaniach.    
8. **Transformation** - niekedy model vyžaduje transformáciu regresorov.    
9. **Model selection** - voľba modelu s množstva možností.    
10. **Shrinkage** - ak má model "veľa" nepotrebných parametrov.    
11. **Complete Example** - celé si to zhrnieme.    
    
    
**BONUS** na koniec kurzu si môžeme ukázať ako robiť [R Markdown](http://rmarkdown.rstudio.com) [^1] report alebo [Shiny](https://shiny.rstudio.com/) [^2] interaktívnu aplikáciu vaších analýz (mimo hodnotenia).    

[^1]: Táto stránka bola vytvorená v R Markdown 
[^2]: Príklad na Shiny aplikáciu <https://samuell.shinyapps.io/NRSR2016/>

*** 

# Čriepky modelovania

Na krátku ukážku si vezmeme *Fuel economy data from 1999 and 2008 for 38 popular models of car* z knižnice `ggplot2`. Nás bude zaujímať ako vplýva **vzdvihový objem motora** (v litroch) na **dojazd** (v mílach) populárnych modelov áut na jeden natankovaný galón paliva. V štatistickej hantírke sa pýtame aký má vplyv **Predictor** (vzdvihový objem motora) na **Response** (dojazd).

#### Obhliadka dát   

```{r sums, echo = F, cached = T}
library(ggplot2)
set.seed(315)
ii=sample(1:234,10)
knitr::kable(mpg[ii,c(2,3,9,11)])
```
...    

***
#### Vizualizácia

```{r plot,echo=F,cached = T}
ggplot(mpg,aes(displ,hwy)) +
  geom_point(aes(color=class)) +theme_minimal() +
  labs(x="Engine displacement (L)",y="Highway fuel economy (mpg)",colour="Car type")
```

***
#### Jednoduchý linarny regresný model


```{r model,echo=F,cached = T}
ggplot(mpg,aes(displ,hwy)) +
  geom_point(alpha=0.7) +
  geom_smooth(method="lm",se=F) + theme_minimal() +
  labs(x="Engine displacement (L)",y="Highway fuel economy (mpg)")
```

#### Ako na to a čo s tým ďalej, už bližšie na kurze... :)

***

# Podmienky a pravidlá kurzu

Kurz je zostavený z jedenástich cvičení obsahujúci vyššie spomínané okruhy, teda každý s dĺžkou 120 minút. Na cvičenia sa **odporúča** chodiť pravidelne, prípadné nejasnosti sa potom dajú riešiť okamžite. Cvičenia a aj úlohy vždy najdete na <https://www.lukaslaffers.com/mar>.  

#### Domáce úlohy
Po každom cvičení dostanete **povinnú domácú úlohu** obsahujúcu úlohy naväzujúce alebo priamo doplňujúce obsah cvičenia. Termíny odovzdania sú **pevne** stanovené na deň pred nasledujúcimi cvičeniami. Posunutie termínu odovzdania môže jednotlivec požiadať najneskôr 3 dni pred jeho vypršaním a to len vtedy,  ak má dobrý dôvod (je na cvičiacom či vyhovie, všetko záleží na okolnostiach). Úlohu je možné konzultovať pred finálnym odovzdaním s cvičiacim, samozrejme je **neprístupné** odpisovať od spolužiaka. Neokomentovaný kód a používanie knižníc, ktorým nerozumiete sa **nehodnotí**.


#### Body a kredity
Z každej domácej úlohy môžete získať **100b**, plus na prípadné prilepšenie ešte bonusových 10b za extra časť. Teda to znamená, že za celý kurz môžete získať 11x100 bodov, ak teda nepočítame bonusové úlohy. Tieto body budú predstavovať *riadny termín skúšky*. Čo sa môže stať na konci kurzu?

- Máte dostatok bodov a obdržíte známku.
- Máte dostatok bodov, ale nie ste spokojný so známkou a myslíte si, že máte na viac (čo ma bude veľmi tešiť) v tom prípade dostanete ešte jednu úlohu na doma za 100b.
- Nemáte dostatok kreditov, teda bojujete o to, aby ste získali aspoň hodnotenie E. V tomto prípade dostanete vypracovať úlohy v R na 120min, ktoré budú obsahovo zastrešovať celý kurz (vedené ako *opravný termín* a konať sa bude v škole).    

Stupnica hodnotenia študenta je stanovená v [Štúdijný poriadok FPV UMB](http://www.fpv.umb.sk/app/cmsSiteAttachment.php?ID=3541) podľa **Čl. 16 Hodnotenie študijných výsledkov §2**

***

# Použitá Literatúra

J.J. Faraway, *Linear models with R*, Second Edition 2015    
H. Wickham, *ggplot2, Elegant Graphics for Data Analysis* Second Edition 2016    
H. Wickham, G. Grolemund, *R for data science* 2016    

Ďalšie užitočné odkazy na články a videa najdete na <https://www.lukaslaffers.com/mar>


