
# Cvicenia 11
# COMPLETE EXAMPLE
# Samuel Hudec


## uzitocne kniznice ####
library(faraway)
require(ggplot2)
library(GGally)


## INSURANCE REDLINING ####
# cvicenie bolo prelozene z kapitolou 12 J.J. Faraway, Linear Models 

# Data pochadazju zo 1970-tych rokov zo studie vztahov Insurance redlining
# v meste Chicago, rasovim zlozenim, poziarmi, poctami kradezi, 
# vekom domov a prijmami v 47 oblastiach mesta.

# nase regresory

# race - rasove zlozenie v percentach mensiny
# fire - pocet poziarov na 100 domov
# theft - kradeze na 1000 obyvatelov
# age - percento domov postavenich pred 1939
# involact - nova FAIR plan politika a zasahy na 100 domov
# income - median rodinneho primu v stovkach dolarov
# side - severna alebo juzna cast mesta Chicago

# nasledujuca studia bude zamerana na vysvetlenie vztahov


## dataset eco and chredlin ####


## Pred tym nez sa pustime do analyzy Insurance redlining 

# my nevieme nic o tom, ktore rasy odmieta poistovna, teda mame informaciu 
# len o percente mensin v oblasti. Potrebujeme sa dozvediet nieco o 
# aj o pristahovalcoch zastupujuci znacnu cast mensin.

data(eco, package="faraway")

plot(income ~ usborn, data=eco, xlab="Proportion US born", ylab="Mean Annual Income")
lmod <- lm(income ~ usborn, eco)
sumary(lmod) # jasne vidiet, ze rocne prijmy a percento populacie narodenej v 
# danej krajine je linearne zavisle.
plot(income ~ usborn, data=eco, xlab="Proportion US born", ylab="Mean Annual Income",xlim=c(0,1),ylim=c(15000,70000),xaxs="i")
abline(coef(lmod))

# krivka nam ukazuje, ze v priemere v krajine narodeny obcan zaraba o dva krat 
# menej ako pristahovany obcan. Kde je chyba ? 

## DISKUSIA ####

#
#
#
#
#

# toto ponaucenie si nesieme dalej 
# Teraz pokracujeme v analyze nasho datasetu


## Pociatocna obhliadka dat ####

data(chredlin, package="faraway")

# pozrieme sa na cisla
head(chredlin)

# Hruba chyba dat ? 
summary(chredlin)
# Vysvetlite summary regresora race. Ake pozitiva v tom vidite ? 
# pokracujte v ostatnych

# parovy graf 

ggred=data.frame(chredlin[,1:4],income=chredlin[,6],involact=chredlin[,5],side=chredlin[,7])
ggpairs(ggred[,-7])

# ofarbene podla casti mesta
ggpairs(ggred, aes(color = side, alpha = 0.5))


# obzrieme si data, ale teraz nie cez pairs() ale trochu sofistikovanejsie
ggplot(chredlin,aes(race,involact)) + geom_point() +stat_smooth(method="lm")
ggplot(chredlin,aes(fire,involact)) + geom_point() +stat_smooth(method="lm")
ggplot(chredlin,aes(theft,involact)) + geom_point() +stat_smooth(method="lm")
ggplot(chredlin,aes(age,involact)) + geom_point() +stat_smooth(method="lm")
ggplot(chredlin,aes(income,involact)) + geom_point() +stat_smooth(method="lm")
ggplot(chredlin,aes(side,involact)) + geom_point(position = position_jitter(width = .2,height=0))
# vysvetlite co vam hovoria jednotlive ploty, to znamena:
# vieme datalinearne odmodelovat ?
# su zrejeme outliere ? 
# hovoria obrazky to, co by sme intuitivne (za pomoci bacoveho rozumu) cakali?

# tu vidime, ze nie je potrebna ziadna transformacia.

# podme rozobrat otazku rasy 
sumary(lm(involact ~ race,chredlin))
# Co nam hovori summary ? 

# Mozu poistovne pouzivat tuto vedomost o oblastiach ? 
# Na druhu stranu mozu to obist, ale do akej miery ? 
ggplot(chredlin,aes(race,fire)) + geom_point() +stat_smooth(method="lm")
ggplot(chredlin,aes(race,theft)) + geom_point() +stat_smooth(method="lm")
# Co nam hovoria obrazky ? 
# Z toho co sme sa dozvedeli ako by ste stanovili model ?

## DISKUSIA ####

#
#
#
#
#

## Full model a diagnostika ####

# ako sme uz mali zaciname s full modelom 
lmod <- lm(involact ~ race + fire + theft + age + log(income),  chredlin)
sumary(lmod)
plot(lmod,1:2)
# Rychla diagnostika co hovorite na model, ktory sme si stanovili ? 
# Kde vznikol ten diagonalny prepad na plote fittet vs. resid ?
# Mozu byt resid normalne rozdelene ? 

# jeden z dalsich nastrojov ako detekovat potrebu transformovania je 
termplot(lmod, partial.resid=TRUE, terms=1:2)
# v skratke ukazuju, ze nie je potrebna transformacia
# Mozeme sa kuknut aj na ine spôsoby ake navrhujete ? 

# problem, ktory mame je vysoky pocet nul v odozve.

## DISKUSIA ####

#
#
#
#
#

## Analyza sensitivity ####

# rasa je velmi citliva otazka. Pouzijeme trik zvaný senzitivita prediktora.
# postavime vsetky submodely, ktore sa daju nakombinovat s prediktorom rasa
# a pozrieme sa na odhady parametrov a ich p hodnoty
listcombo <- unlist(sapply(0:4,function(x) combn(4, x, simplify=FALSE)),recursive=FALSE)
predterms <- lapply(listcombo, function(x) paste(c("race",c("fire","theft","age","log(income)")[x]),collapse="+"))
coefm <- matrix(NA,16,2)
for(i in 1:16){
  lmi <- lm(as.formula(paste("involact ~ ",predterms[[i]])), data=chredlin)
  coefm[i,] <- summary(lmi)$coef[2,c(1,4)]
}
rownames(coefm) <- predterms
colnames(coefm) <- c("beta","pvalue")
round(coefm,4)
# v prvom stlpci su odhady koeficienta race a v druhom ich p-hodnoty. 
# Co vidite v tejto tabulke ? 
# Je koeficient senzitivny ? 


diags <- data.frame(lm.influence(lmod)$coef)
ggplot(diags,aes(row.names(diags),race)) +geom_linerange(aes(ymax=0,ymin=race)) +theme(axis.text.x=element_text(angle=90)) + xlab("ZIP") +geom_hline(yintercept=0)
# Pouzili sme influence hodnoty co nam hovori podla vas plot ?
# tu sa snazime demonstrovat ako zlozita je v takomto pripade kauzalita

# dalsie 
ggplot(diags,aes(row.names(diags),theft)) +geom_linerange(aes(ymax=0,ymin=theft)) +theme(axis.text.x=element_text(angle=90)) + xlab("ZIP") +geom_hline(yintercept=0)
ggplot(diags,aes(row.names(diags),fire)) +geom_linerange(aes(ymax=0,ymin=fire)) +theme(axis.text.x=element_text(angle=90)) + xlab("ZIP") +geom_hline(yintercept=0)



# pozieme sa blizsie aj na ostatne prediktory ako su od seba zavisle a kde 
# sa mozu skrivat cudaci.
ggplot(diags,aes(x=fire,y=theft))+geom_text(label=row.names(diags))
plot(lmod,5)
# par vplyvnich pozorovani sme nasli, podme sa na nich blizsie pozriet
# vieme, ze taketo pozorovania nesu vela informacie
chredlin[c("60607","60610"),]
match(c("60607", "60610"),row.names(chredlin)) # jedna z moznosti ako ich najst
# ked neviete kolke v poradi je dane cislo

# vyhodime a kukneme na model
lmode <- lm(involact ~ race + fire + theft + age + log(income),  chredlin,subset=-c(6,24))
sumary(lmode)
# Su prediktory theft a age stale signifikantne ?
# Zase zlepsili sme si R2
# Co si myslite teraz oplatilo sa nam vyhodit týchto cudakov ? 
# Nazorna ukazka toho, aky je model senzitivny na zmeny struktury a 
# Vyhadzovania pozorovani.

# pokracujme pre zaujimavost s vyhodenim nesignifikantnych koefientov
modalt <- lm(involact ~ race+fire+log(income),chredlin,  subset=-c(6,24))
sumary(modalt)
# Co si myslite je tento model spravny ?
# Ake zmeny ste zaznamenali ? 
# Ktory z modelov by ste zvolili vy ?

## DISKUSIA ####

#
#
#
#
#

## Na zaver este nieco ####

# tymto sme si ukazali ze nie je priamociare postavit jednoznacny model
# ktory by popisoval data, hlavne ked musime popri tom riesit aj politicke
# otazky. Ine to je ak mame nezavislu studiu od ktorej nie je ocakavany 
# vysledok. O tomto sa da vela hovorit ak by vas to zaujimalo mam par 
# clankov ako dalej a co sa da este robit.



# posledna otazka ostala nezodpovedana, ako je to v roznych castiach mesta ? 

lmod <- lm(involact ~ race+fire+theft+age, subset=(side == "s"), chredlin)
sumary(lmod)
lmod <- lm(involact ~ race+fire+theft+age, subset=(side == "n"), chredlin)
sumary(lmod)
# nastala niejaka zmena ? 

## POSLEDNA DISKUSIA ####

#
#
#
#
#






