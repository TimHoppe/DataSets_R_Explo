#Herzlich Willkommen zur Übung Explorative Datenanalyse

#Zuerst ein paar Infos zum Program.

#Wo stehen die Ergebnisse? Wie werden Packages geladen? Wo werden zum Beispiel Html Ausgaben gezeigt. 
#Wo werden Grafiken angezeigt? Wo finde ich Hilfe? 







#Daten Einlesen aus einer CSV-Datei z.B.
Bewerber2015<-read.csv2("C:/Users/tim/Desktop/BBA/explorative_datenanalyse/Daten_Bewerber_2015.csv")

#Datensatz löschen
Bewerber2015<- NULL

#Daten Einlesen nach Copy-Befehl z.B. aus Excel
Bewerber2015<-read.table("clipboard", header=TRUE,sep="\t")

# Etwas für zwischendurch Wo erscheinen die Daten? Kann ich mir die Daten anschauen?

#Aufzeichnung der Befehle (wie Log-File)
savehistory(file="C:/Users/tim/Desktop/BBA/explorative_datenanalyse/meinLog_Mittwoch.Rhistory")

#Aufzeichnen des Outputs 
sink("C:/Users/tim/Desktop/BBA/explorative_datenanalyse/myfile2.pdf", append=TRUE, split=TRUE)






# Erstellen einer Variable, die die Gesamtsumme der Testergebnisse angibt.
Test_Gesamt <- Bewerber2015$Test1+Bewerber2015$Test2+Bewerber2015$Test3


# Hinzufügen der Variable an den Datensatz ich möchte nochwas dazu schreiben 
Bewerber2015 <- cbind(Bewerber2015, "Test_Summe"=Test_Gesamt)

# Aktivierung der Pakete sjmisc und sjplot
library(sjmisc)
library(sjPlot)


# Data Frame - Oder in welcher Form liegen unsere Daten vor?
view_df(Bewerber2015)
view_df(Bewerber2015, show.frq = TRUE)
view_df(Bewerber2015, show.frq = TRUE, show.prc = TRUE)



# Eine Untermenge erstellen des Datensatzes erstellen
Bewerber2015_weiblich <- subset(Bewerber2015, Bewerber2015$Geschlecht == 0)
Bewerber2015_männlich <- subset(Bewerber2015, Bewerber2015$Geschlecht == 1)

#Variablenwerte neu labeln 
Bewerber2015$Geschlecht <- factor(Bewerber2015$Geschlecht, levels = c (0,1), labels = c("female", "male"))
Bewerber2015$Nationalitaet <- ordered(Bewerber2015$Nationalitaet, levels = c(0,1,2), labels = c("German", "Swiss", "Austrian"))

#Untergruppe durch Spalte erzeugen
Alter2 <- ifelse(Bewerber2015$Alter<40, 1, 0)
Bewerber2015 <- cbind(Bewerber2015, "Alter_40"=Alter2)

#Häufigkeiten

#Häufigkeiten erstellen absolut
table(Bewerber2015$Alter)
table(Bewerber2015$Nationalitaet)

#Häufigkeiten erstellen mit dem prettyR Packet
library(prettyR)
freq(Bewerber2015$Alter)
freq(Bewerber2015$Nationalitaet)

#Häufigkeiten erstellen mit dem sj Packeten
library(sjPlot)
library(sjmisc)
sjmisc::frq(Bewerber2015$Alter, out = c("viewer") )
sjmisc::frq(Bewerber2015$Nationalitaet, out = c("viewer") )
sjmisc::frq(Bewerber2015$Berufserfahrung_in_Jahren, out = c("viewer") )

#Häufigkeitstabelle mit Gruppen 
sjmisc::frq(Bewerber2015$Alter, auto.grp = 3, out = c("viewer") )




#Kreuztabelle erstellen absolut als auch relativ
KTabelle <- table(Bewerber2015$Geschlecht,Bewerber2015$Nationalitaet)
table(KTabelle)
prop.table(KTabelle)


#oder
library(gmodels)
CrossTable(Bewerber2015$Geschlecht,Bewerber2015$Nationalitaet)
CrossTable(Bewerber2015$Geschlecht, digits=2)


#oder mit dem sj Paketen
sjt.xtab(Bewerber2015$Geschlecht, Bewerber2015$Nationalitaet)
sjt.xtab(Bewerber2015$Geschlecht, Bewerber2015$Nationalitaet, show.cell.prc = TRUE, show.row.prc = TRUE, show.col.prc = TRUE, show.exp = TRUE, show.summary = TRUE )

 
#Wert korrigieren in Berufserfahrung
Bewerber2015$Berufserfahrung_in_Jahren <- replace(Bewerber2015$Berufserfahrung_in_Jahren, Bewerber2015$Berufserfahrung_in_Jahren==111, 11)

#Histogramm erstellen
hist(Bewerber2015$Alter, main="Histogramm Alter", xlab="Alter")

#oder
#Darstellung des Histogramms inklusive des Mittelwert und der Standardabweichung
sjp.frq(Bewerber2015$Alter, 
        type = "hist", 
        geom.size = 1,
        show.mean = TRUE)


#Dichtefunktion erstellen
d <- density(Bewerber2015$Alter) 
plot(d, main="Dichtefunktion Alter", xlab="Alter" )

#oder
#Einfügen einer Dichtefunktion und gleichzeitiger Vergleich zu einer Normalverteilung
sjp.frq(Bewerber2015$Alter, 
        type = "bar", 
        normal.curve = TRUE, 
        normal.curve.alpha = .3)

#Deskriptive Statistik
library(pastecs)
stat.desc(Bewerber2015$Alter)

library(psych)
describe(Bewerber2015$Alter)

#Achtung auf das demaskieren achten!
library(Hmisc)
describe(Bewerber2015$Alter)

#Deskriptive Kennzahlen ausgeben in einer Tabelle für alle Variablen
sjmisc::descr(Bewerber2015, out = c("viewer"))


#Boxplot erstellen
boxplot(Bewerber2015$Alter)

#Boxplot für mehrere Variablen
boxplot(Bewerber2015$Gehaltsvorstellung~Bewerber2015$Geschlecht, data=Bewerber2015, main="Boxplot Gehaltsvorstellung",
        xlab="Geschlecht", ylab="Euro")

#Boxplot mit sj Paketen

sjp.frq(Bewerber2015$Alter, 
        type = "box", 
        geom.size = .3, 
        inner.box.width = 5)

library(quantreg)

sjp.grpfrq(Bewerber2015$Test1, Bewerber2015$Geschlecht, type = "box", show.summary = TRUE)
sjp.grpfrq(Bewerber2015$Test1, Bewerber2015$Nationalitaet, type = "box", show.summary = TRUE)
sjp.grpfrq(Bewerber2015$Test1, Bewerber2015$Nationalitaet, intr.var = Bewerber2015$Geschlecht , type = "box", show.summary = TRUE)






#Hypothesentests

#Formulierung der allgemeinen H_0-Hypothese: Es gibt keine Unterschiede zwischen den Beobachtungsgruppen

#Chi² Test

#H_1 Es gibt einen Unterschied hinsichtlich der Nationalität bei den Geschlechtern
#Chi² Test für Kreuztabelle
#Kreuztabelle erstellen absolut als auch relativ
KTabelle <- table(Bewerber2015$Geschlecht,Bewerber2015$Nationalitaet)
table(KTabelle)
chisq.test(KTabelle)

#Chi² Test mit Crosstable-Befehl
CrossTable(Bewerber2015$Geschlecht,Bewerber2015$Nationalitaet, digits=2, expected=TRUE, dnn=c("Geschlecht", "Nationalität"))
# Der p-Wert ist größer als alpha 0,05 und somit gibt es keinen signifikanten Unterschied
# Die Alternativhypothese wird abgelehnt.
#Der p-Wert ist für beide Wege größer als alpha und somit ist die Alternativhypothese abzulehnen.

#Fisher Test
#H_1 Es gibt einen Unterschied hinsichtlich der Nationalität bei den Geschlechtern
fisher.test(KTabelle)
#Der p-Wert ist größer als alpha und somit ist die Alternativhypothese abzulehnen.

# One Sample t-test
#Achtung hier Ausnahme der H_0 und H_1 Hypothese
#H_0 Der Mittelwert des Alters der Bewerber ist 40 Jahre
#H_1 Der Mittelwert des Alters der Bewerber ist ungleich 40 Jahre

t.test(Bewerber2015$Alter,mu=40) # Ho: mu=40
#Der p-Wert ist kleiner als alpha= 0,05, somit wird die Nullhypothese dass das Alter der Bewerber 40 Jahre ist abgelehnt


#Independent 2-group t-test 

#H_1 Es gibt einen Unterschied in der Gehaltsvorstellung zwischen Männern und Frauen
#wobei die erste Variable numerisch und die zweite binär ist
t.test(Bewerber2015$Gehaltsvorstellung~Bewerber2015$Geschlecht) 
#Der p-Wert ist kleiner als alpha= 0,05, somit wird die Nullhypothese abgelehnt

#H_1 Es gibt einen Unterschied in der Gehaltsvorstellung an der Altersgrenze 40
#oder mit zwei numerischen Variablen
t.test(Bewerber2015$Gehaltsvorstellung,Bewerber2015$Alter_40)
#Der p-Wert ist kleiner als alpha= 0,05, somit wird die Nullhypothese abgelehnt


# Independent 2-group Mann-Whitney U Test 

#H_1 Es gibt einen Unterschied in der Gehaltsvorstellung zwischen Männern und Frauen
#wobei die erste Variable numerisch und die zweite binär ist
t.test(Bewerber2015$Gehaltsvorstellung~Bewerber2015$Geschlecht) 
#Der p-Wert ist kleiner als alpha= 0,05, somit wird die Nullhypothese abgelehnt

#H_1 Es gibt einen Unterschied in der Gehaltsvorstellung an der Altersgrenze 40
#oder mit zwei numerischen Variablen
wilcox.test(Bewerber2015$Gehaltsvorstellung,Bewerber2015$Alter_40)
#Der p-Wert ist kleiner als alpha= 0,05, somit wird die Nullhypothese abgelehnt


#Paired T-Test

#Wobei beide Variablen numerische Variablen sind
#H_1 Es gibt einen signfikanten Unterschied zwischen der Gehakltsvorstellung und dem letzten Brutto
t.test(Bewerber2015$Gehaltsvorstellung,Bewerber2015$Letztes_Brutto, paired=TRUE) 
#Der p-Wert ist kleiner als alpha= 0,05, somit wird die Nullhypothese abgelehnt

#Dependent 2-group Wilcoxon Signed Rank Test 

#Wobei beide Variablen numerische Variablen sind
#H_1 Es gibt einen signfikanten Unterschied zwischen der Gehakltsvorstellung und dem letzten Brutto
wilcox.test(Bewerber2015$Gehaltsvorstellung,Bewerber2015$Letztes_Brutto, paired=TRUE) 
#Der p-Wert ist kleiner als alpha= 0,05, somit wird die Nullhypothese abgelehnt

#Korrelationen
#Korrelation für zwei Variablen wobei die method für pearson, spearman or kendall steht und use für Options are all.obs (assumes no missing data - missing data will produce an error), complete.obs (listwise deletion), and pairwise.complete.obs (pairwise deletion)   
cor.test(Bewerber2015$Letztes_Brutto, Bewerber2015$Gehaltsvorstellung, use="complete.obs", method="pearson" )

#Scatterplot
plot(Bewerber2015$Letztes_Brutto, Bewerber2015$Gehaltsvorstellung, main="Scatterplot", xlab="Letztes Brutto", ylab="Gehaltsvorstellung", pch=19)


#Regression
#Regressionen


# Multiple Linear Regression Example 
fit2 <- lm(Bewerber2015$Gehaltsvorstellung ~ Bewerber2015$Letztes_Brutto+Bewerber2015$Alter+Bewerber2015$Geschlecht+Bewerber2015$Test_Summe+Bewerber2015$Fremdsprachen+Bewerber2015$Personalverantwortung)
summary(fit2) # show results


