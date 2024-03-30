{#Stringe usate dopo
.S_zero=paste("\nHello World!, Questo è il proggetto in R del Gruppo 3.\nFare attenzione che la directory di lavoro sia  impostata correttamente,\naltrimenti vi invitiamo ad impostarla tramite il comando: setwd(-direcory-)\nEcco la directory impostata attualmente:",getwd());
.S_uno="\nRiassunto DataSet:\nVariabile Dipendente:\n • y_ImageQuality -> Qualità percepita dell'immagine\n\nVariabili Indipendenti (paramentri della fotocamera e di scatto):\n • x1_ISO -> ISO (sensibilità del sensore)\n • x2_FRatio -> Rapporto Focale\n • x3_Time -> Tempo di Esposizione\n • x4_MP -> Megapixel del sensore\n • x5_CROP -> Fattore di Crop\n • x6_FOCAL -> Focale\n • x7_PixDensity -> Densità di pixel\n\n";
.S_due="\nCommenti sulla variabile dipendente:\nTale indice è frutto di una opportuna trasformazione di un punteggio assegnato a un campione di immagini da volontari che compilano un questionario.\nÈ quindi evidente che sarebbe opportuno utilizzare un appropriato trattamento per la raccalta di tali dati\nAssumiamo che ogni candidato sia equivalente nel dare un giudizio all'immagine\ne che si sia adottato un piano completamente casualizzato";
.S_tre="Visto che abbiamo una sufficiente quantità di valori possiamo permetterci di eliminare eventuali osservazioni con dati mancanti\nNumero osservazioni eliminate:";
.S_quattro="Per poter studiare al meglio le variabili indipendenti risulta utile conoscere la loro distribuzione\nProveremo quindi a vedere se possiamo considerare i dati in nostro possesso generati da una distribuzione gaussiana\nSe così non fosse non potremmo svolgere con facilità stime intervallari \n\nCiò detto ovviamente è sempre possibile stimare i parametri da noi riportati sia prima che dopo\n\nNel test useremo come livello di rischio a=0.5"
.S_cinque="come abbiamo visto dagli scatterplot possiamo già intuire delle relazioni lineari tra la variabilie dipendente e quasti tutte le variabili indipendenti (fatta eccezione per la x3 e x7)\nRisulta inoltre evidente una forte correlazione tra x4 ed x7, il che è da tenere presente per la selezione del modello di regressione";
.S_sei="È evidente che le variabili del dataset siano quantitative(valori numerici) continue, quindi risulta necessario per i nostri scopi suddividere i valori in classi\nPer il numero di classi usiamo la relazione empirica K_classi=1+33log(N_elementi):";
.S_sette="\n\nNon dobbiamo preoccuparci dei valori negativi del dataset visto che le variabili indipendenti rappresentano indici standardizzati cioè svincolati dall'unità di misura, \nè quindi ovvio che ci siano valori negativi e che la loro media sia zero (verificheremo per sicurezza)";
.S_otto="Numero di unità sperimentali con almeno una variabile osservata negativa: "
.S_nove="Nota divisione in classi:\nveidamo che più o meno tutte le variabili indipendenti possono essere divise circa con gli stessi intervalli \nfatta eccezione per la variabile x7 che ha ben... "
.S_dieci=" valori maggiori del massimo di tutte le altre variabili \nmotivo per il quale decidiamo di utilizzare una classe in più per la variabile X7\ncosì che gli istogrammi siano più giustamente paragonabili"
}
tema_colori="Greens"
tema_boxplot="#41ab5d";tema_boxplot2="#5E1113"

library(sjPlot)
library(ggplot2)
library(corrplot)
library(gridExtra)




##INIZIO-------------------
{
cat(.S_zero);rm(.S_zero)
myds = read.csv("Dataset_AH_gruppo3.csv", header=TRUE) #load the dataset
colnames(myds) = c('y','x1','x2','x3','x4','x5','x6','x7')#assegnamo nomi più rapidi alle collone del dataset
attach(myds) #Associamo il dataset allo spazio di lavoro così da non dover richiamare sempre "myds$..."
# myds = myds[order(y),] #solo per comodità ordiniamo per i valori di y

{v1=nrow(myds)
  v2=nrow(na.omit(myds))
  myds=na.omit(myds) #ridefinisco il dataset eliminando tutte le unità con omissione di una variabile
  cat(.S_tre,(v1-v2));rm(v1,v2)} #Verifichiamo non ci sia alcun elemento con dati mancanti

View(myds)
cat(.S_uno);summary(myds);cat(.S_due);rm(.S_uno,.S_due) #Diamo qualche informazione riassuntiva sul dataset
}




##Valori negativi?(piccola nota)####

has_negative <- function(x) {
  any(x < 0)
}
num_rows_with_negative = apply(myds, 1, has_negative);num_rows_with_negative =sum(num_rows_with_negative)
cat(.S_otto,num_rows_with_negative,.S_sette);rm(num_rows_with_negative,has_negative,.S_sette,.S_otto)







##Cominciamo la statistica descrittiva#####################---------------
cat(.S_sei);rm(.S_sei)
K_classi=round(1+3.3*log10(nrow(myds)));K_classi

##Creazione dataset dedicati per ogni variabilie####

  
  min_x1 <- min(x1);max_x1 <- max(x1)
  min_x2 <- min(x2);max_x2 <- max(x2)
  min_x3 <- min(x3);max_x3 <- max(x3)
  min_x4 <- min(x4);max_x4 <- max(x4)
  min_x5 <- min(x5);max_x5 <- max(x5)
  min_x6 <- min(x6);max_x6 <- max(x6)
  min_x7 <- min(x7);max_x7 <- max(x7)
  min_y <- min(y);max_y <- max(y)
  
  
 min_1_6=min(min_x1,min_x2,min_x3,min_x4,min_x5,min_x6)
 max_1_6=max(max_x1,max_x2,max_x3,max_x4,max_x5,max_x6)
 
 cat(.S_nove,sum(x7>max_1_6) , .S_dieci);rm(.S_nove,.S_dieci)
  
 
  
  intervalli_x1<-cut(x1, breaks=seq(-1.85, round(max_1_6), length.out = K_classi+1), include.lowest = TRUE ) 
  intervalli_x2<-cut(x2, breaks=seq(-1.85, round(max_1_6), length.out = K_classi+1), include.lowest = TRUE ) 
  intervalli_x3<-cut(x3, breaks=seq(-1.85, round(max_1_6), length.out = K_classi+1), include.lowest = TRUE ) 
  intervalli_x4<-cut(x4, breaks=seq(-1.85, round(max_1_6), length.out = K_classi+1), include.lowest = TRUE ) 
  intervalli_x5<-cut(x5, breaks=seq(-1.85, round(max_1_6), length.out = K_classi+1), include.lowest = TRUE ) 
  intervalli_x6<-cut(x6, breaks=seq(-1.85, round(max_1_6), length.out = K_classi+1), include.lowest = TRUE )
  intervalli_x7<-cut(x7, breaks=seq(-1.85, 2.5, length.out = K_classi+2), include.lowest = TRUE )
  intervalli_y<-cut(y, breaks=K_classi, include.lowest = TRUE )
  
  
  
  
  centro_di_classe_x1 <- tapply(x1, intervalli_x1, mean)
  centro_di_classe_x2 <- tapply(x2, intervalli_x2, mean)
  centro_di_classe_x3 <- tapply(x3, intervalli_x3, mean)
  centro_di_classe_x4 <- tapply(x4, intervalli_x4, mean)
  centro_di_classe_x5 <- tapply(x5, intervalli_x5, mean)
  centro_di_classe_x6 <- tapply(x6, intervalli_x6, mean)
  centro_di_classe_x7 <- tapply(x7, intervalli_x7, mean)
  centro_di_classe_y <- tapply(y, intervalli_y, mean)
  
  freq_assolute_x1<-table(intervalli_x1)
  freq_assolute_x2<-table(intervalli_x2)
  freq_assolute_x3<-table(intervalli_x3)
  freq_assolute_x4<-table(intervalli_x4)
  freq_assolute_x5<-table(intervalli_x5)
  freq_assolute_x6<-table(intervalli_x6)
  freq_assolute_x7<-table(intervalli_x7)
  freq_assolute_y<-table(intervalli_y)
  
  
  
  freq_relative_x1<-freq_assolute_x1/sum(freq_assolute_x1)
  freq_relative_x2<-freq_assolute_x2/sum(freq_assolute_x2)
  freq_relative_x3<-freq_assolute_x3/sum(freq_assolute_x3)
  freq_relative_x4<-freq_assolute_x4/sum(freq_assolute_x4)
  freq_relative_x5<-freq_assolute_x5/sum(freq_assolute_x5)
  freq_relative_x6<-freq_assolute_x6/sum(freq_assolute_x6)
  freq_relative_x7<-freq_assolute_x7/sum(freq_assolute_x7)
  freq_relative_y<-freq_assolute_y/sum(freq_assolute_y)
  
  
  freq_cumulate_x1<-cumsum(freq_assolute_x1)
  freq_cumulate_x2<-cumsum(freq_assolute_x2)
  freq_cumulate_x3<-cumsum(freq_assolute_x3)
  freq_cumulate_x4<-cumsum(freq_assolute_x4)
  freq_cumulate_x5<-cumsum(freq_assolute_x5)
  freq_cumulate_x6<-cumsum(freq_assolute_x6)
  freq_cumulate_x7<-cumsum(freq_assolute_x7)
  freq_cumulate_y<-cumsum(freq_assolute_y)
  
  
  
  freq_rel_cum_x1<-cumsum(freq_relative_x1)
  freq_rel_cum_x2<-cumsum(freq_relative_x2)
  freq_rel_cum_x3<-cumsum(freq_relative_x3)
  freq_rel_cum_x4<-cumsum(freq_relative_x4)
  freq_rel_cum_x5<-cumsum(freq_relative_x5)
  freq_rel_cum_x6<-cumsum(freq_relative_x6)
  freq_rel_cum_x7<-cumsum(freq_relative_x7)
  freq_rel_cum_y<-cumsum(freq_relative_y)
  
  
  
  df_x1=data.frame(row.names = c("Intervallo 1", "Intervallo 2", "Intervallo 3","Intervallo 4", "Intervalli_ 5", "Intervallo 6", "Intervallo 7", "Intervallo 8"),
                    Intervallo = levels(intervalli_x1),
                    centri_di_classe = round(centro_di_classe_x1,2),
                    Frequenze_assolute = as.numeric(freq_assolute_x1),
                    Frequenze_relative = as.numeric(freq_relative_x1),
                    Frequenze_cumulate = as.numeric(freq_cumulate_x1),
                    Frequenze_cumulate_relative = as.numeric(freq_rel_cum_x1) )
  
  df_x2=data.frame(row.names = c("Intervallo 1", "Intervallo 2", "Intervallo 3","Intervallo 4", "Intervalli_ 5", "Intervallo 6", "Intervallo 7", "Intervallo 8"),
                   Intervallo = levels(intervalli_x2),
                   centri_di_classe = round(centro_di_classe_x2,2),
                   Frequenze_assolute = as.numeric(freq_assolute_x2),
                   Frequenze_relative = as.numeric(freq_relative_x2),
                   Frequenze_cumulate = as.numeric(freq_cumulate_x2),
                   Frequenze_cumulate_relative = as.numeric(freq_rel_cum_x2) )
  
  df_x3=data.frame(row.names = c("Intervallo 1", "Intervallo 2", "Intervallo 3","Intervallo 4", "Intervalli_ 5", "Intervallo 6", "Intervallo 7", "Intervallo 8"),
                   Intervallo = levels(intervalli_x3),
                   centri_di_classe = round(centro_di_classe_x3,2),
                   Frequenze_assolute = as.numeric(freq_assolute_x3),
                   Frequenze_relative = as.numeric(freq_relative_x3),
                   Frequenze_cumulate = as.numeric(freq_cumulate_x3),
                   Frequenze_cumulate_relative = as.numeric(freq_rel_cum_x3) )
  
  df_x4=data.frame(row.names = c("Intervallo 1", "Intervallo 2", "Intervallo 3","Intervallo 4", "Intervalli_ 5", "Intervallo 6", "Intervallo 7", "Intervallo 8"),
                   Intervallo = levels(intervalli_x4),
                   centri_di_classe = round(centro_di_classe_x4,2),
                   Frequenze_assolute = as.numeric(freq_assolute_x4),
                   Frequenze_relative = as.numeric(freq_relative_x4),
                   Frequenze_cumulate = as.numeric(freq_cumulate_x4),
                   Frequenze_cumulate_relative = as.numeric(freq_rel_cum_x4) )
  
  df_x5=data.frame(row.names = c("Intervallo 1", "Intervallo 2", "Intervallo 3","Intervallo 4", "Intervalli_ 5", "Intervallo 6", "Intervallo 7", "Intervallo 8"),
                   Intervallo = levels(intervalli_x5),
                   centri_di_classe = round(centro_di_classe_x5,2),
                   Frequenze_assolute = as.numeric(freq_assolute_x5),
                   Frequenze_relative = as.numeric(freq_relative_x5),
                   Frequenze_cumulate = as.numeric(freq_cumulate_x5),
                   Frequenze_cumulate_relative = as.numeric(freq_rel_cum_x5) )
  
  df_x6=data.frame(row.names = c("Intervallo 1", "Intervallo 2", "Intervallo 3","Intervallo 4", "Intervalli_ 5", "Intervallo 6", "Intervallo 7", "Intervallo 8"),
                   Intervallo = levels(intervalli_x6),
                   centri_di_classe = round(centro_di_classe_x6,2),
                   Frequenze_assolute = as.numeric(freq_assolute_x6),
                   Frequenze_relative = as.numeric(freq_relative_x6),
                   Frequenze_cumulate = as.numeric(freq_cumulate_x6),
                   Frequenze_cumulate_relative = as.numeric(freq_rel_cum_x6) )
  
  df_x7=data.frame(row.names = c("Intervallo 1", "Intervallo 2", "Intervallo 3","Intervallo 4", "Intervalli_ 5", "Intervallo 6", "Intervallo 7", "Intervallo 8","Intervallo 9"),
                   Intervallo = levels(intervalli_x7),
                   centri_di_classe = round(centro_di_classe_x7,2),
                   Frequenze_assolute = as.numeric(freq_assolute_x7),
                   Frequenze_relative = as.numeric(freq_relative_x7),
                   Frequenze_cumulate = as.numeric(freq_cumulate_x7),
                   Frequenze_cumulate_relative = as.numeric(freq_rel_cum_x7) )
  
  
  df_y=data.frame(row.names = c("Intervallo 1", "Intervallo 2", "Intervallo 3","Intervallo 4", "Intervalli_ 5", "Intervallo 6", "Intervallo 7", "Intervallo 8"),
                   Intervallo = levels(intervalli_y),
                   centri_di_classe = round(centro_di_classe_y,2),
                   Frequenze_assolute = as.numeric(freq_assolute_y),
                   Frequenze_relative = as.numeric(freq_relative_y),
                   Frequenze_cumulate = as.numeric(freq_cumulate_y),
                   Frequenze_cumulate_relative = as.numeric(freq_rel_cum_y) )
  

 
 
#cancellazione 
{  rm(centro_di_classe_x1, centro_di_classe_x2, centro_di_classe_x3, centro_di_classe_x4, centro_di_classe_x5, centro_di_classe_x6, centro_di_classe_x7, centro_di_classe_y)
  rm(freq_assolute_x1, freq_assolute_x2, freq_assolute_x3, freq_assolute_x4, freq_assolute_x5, freq_assolute_x6, freq_assolute_x7, freq_assolute_y)
  rm(freq_relative_x1, freq_relative_x2, freq_relative_x3, freq_relative_x4, freq_relative_x5, freq_relative_x6, freq_relative_x7, freq_relative_y)
  rm(freq_cumulate_x1, freq_cumulate_x2, freq_cumulate_x3, freq_cumulate_x4, freq_cumulate_x5, freq_cumulate_x6, freq_cumulate_x7, freq_cumulate_y)
  rm(freq_rel_cum_x1, freq_rel_cum_x2, freq_rel_cum_x3, freq_rel_cum_x4, freq_rel_cum_x5, freq_rel_cum_x6, freq_rel_cum_x7, freq_rel_cum_y)
  rm(min_x1, min_x2, min_x3, min_x4, min_x5, min_x6, min_x7, min_y)
  rm(max_x1, max_x2, max_x3, max_x4, max_x5, max_x6, max_x7, max_y)
  
  rm(K_classi)
  rm(max_1_6,min_1_6)
} 

#visualizzazione
View(df_x1)
View(df_x2)
View(df_x3)
View(df_x4)
View(df_x5)
View(df_x6)
View(df_x7)
View(df_y)


##Istogramma delle classi####
library(ggplot2)


g1=ggplot(data=myds, aes(x=intervalli_x1, fill = intervalli_x1) ) + #parametri base
  geom_bar()+ #tipo di grafico 
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8"))+ #nome intervalli
  scale_y_continuous(breaks = seq(from=0, to=30, by=2), limits = c(0, 20))+
  scale_fill_brewer(palette=tema_colori) + #colori
  labs(x = "Intervalli",y="Frequenza assoluta",title="Istogramma frequenza assoluta di x1")+ #testo scritte
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));g1 #parametri scritte



g2=ggplot(data=myds, aes(x=intervalli_x2, fill = intervalli_x2)) + 
  geom_bar()+ 
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8"))+
  scale_y_continuous(breaks = seq(from=0, to=30, by=2), limits = c(0, 20))+
  scale_fill_brewer(palette=tema_colori) + 
  labs(x = "Intervalli",y="Frequenza assoluta",title="Istogramma frequenza assoluta di x2")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));g2 


g3=ggplot(data=myds, aes(x=intervalli_x3, fill = intervalli_x3)) + 
  geom_bar()+
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8"))+
  scale_y_continuous(breaks = seq(from=0, to=30, by=2), limits = c(0, 20))+
  scale_fill_brewer(palette=tema_colori) + 
  labs(x = "Intervalli",y="Frequenza assoluta",title="Istogramma frequenza assoluta di x3")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));g3 


g4=ggplot(data=myds, aes(x=intervalli_x4, fill = intervalli_x4)) + 
  geom_bar()+ 
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8"))+
  scale_y_continuous(breaks = seq(from=0, to=30, by=2), limits = c(0, 20))+
  scale_fill_brewer(palette=tema_colori) + 
  labs(x = "Intervalli",y="Frequenza assoluta",title="Istogramma frequenza assoluta di x4")+
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));g4 


g5=ggplot(data=myds, aes(x=intervalli_x5, fill = intervalli_x5)) + 
  geom_bar()+ 
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8"))+
  scale_y_continuous(breaks = seq(from=0, to=30, by=2), limits = c(0, 20))+
  scale_fill_brewer(palette=tema_colori) + 
  labs(x = "Intervalli",y="Frequenza assoluta",title="Istogramma frequenza assoluta di x5")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));g5 


g6=ggplot(data=myds, aes(x=intervalli_x6, fill = intervalli_x6)) + 
  geom_bar()+
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8"))+
  scale_y_continuous(breaks = seq(from=0, to=30, by=2), limits = c(0, 20))+
  scale_fill_brewer(palette=tema_colori) + 
  labs(x = "Intervalli",y="Frequenza assoluta",title="Istogramma frequenza assoluta di x6")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));g6 


g7=ggplot(data=myds, aes(x=intervalli_x7, fill = intervalli_x7)) +
  geom_bar()+ 
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8", expression(italic("Int_9")) ))+
  scale_y_continuous(breaks = seq(from=0, to=30, by=2), limits = c(0, 23))+
  scale_fill_brewer(palette=tema_colori) +
  labs(x = "Intervalli",y="Frequenza assoluta",title="Istogramma frequenza assoluta di x7")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));g7 


gy=ggplot(data=myds, aes(x=intervalli_y, fill = intervalli_y)) +
  geom_bar()+ 
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8"))+
  scale_y_continuous(breaks = seq(from=0, to=30, by=2), limits = c(0, 28))+
  scale_fill_brewer(palette=tema_colori) +
  labs(x = "Intervalli",y="Frequenza assoluta",title="Istogramma frequenza assoluta di y")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));gy 





dev.new(width=1000, height=1000,title="Grafici frequenza assoluta");cat("si consigila una finestra molto ampia") #apro una nuova finestra per i grafici


g=grid.arrange(g1, g2, g3, g4, g5, g6, g7,gy,nrow = 4) #mostro grafici tuttiinsieme



rm(intervalli_x1,intervalli_x2,intervalli_x3,intervalli_x4,intervalli_x5,intervalli_x6,intervalli_x7)






##frequenza comulata####


intervalli__x1=reorder(df_x1$Intervallo,df_x1$Frequenze_cumulate)

gg1=ggplot(data=df_x1, aes(x = intervalli__x1, y=Frequenze_cumulate, fill = intervalli__x1)) +
  geom_bar(stat = "identity")+ 
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8"))+
  scale_y_continuous(breaks = seq(from=0, to=100, by=10), limits = c(0, 100))+
  scale_fill_brewer(palette=tema_colori) + 
  labs(x = "Intervalli",y="Frequenza cumulata",title="Istogramma frequenza cumulata di x1")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));gg1 



intervalli__x2=reorder(df_x2$Intervallo,df_x2$Frequenze_cumulate)

gg2=ggplot(data=df_x2, aes(x = intervalli__x2, y=Frequenze_cumulate, fill = intervalli__x2)) +
  geom_bar(stat = "identity")+ 
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8"))+
  scale_y_continuous(breaks = seq(from=0, to=100, by=10), limits = c(0, 100))+
  scale_fill_brewer(palette=tema_colori) + 
  labs(x = "Intervalli ",y="Frequenza cumulata",title="Istogramma frequenza cumulata di x2")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));gg2 




intervalli__x3=reorder(df_x3$Intervallo,df_x3$Frequenze_cumulate)

gg3=ggplot(data=df_x3, aes(x = intervalli__x3, y=Frequenze_cumulate, fill = intervalli__x3)) +
  geom_bar(stat = "identity")+ 
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8"))+
  scale_y_continuous(breaks = seq(from=0, to=100, by=10), limits = c(0, 100))+
  scale_fill_brewer(palette=tema_colori) + 
  labs(x = "Intervalli",y="Frequenza cumulata",title="Istogramma frequenza cumulata di x3")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));gg3 



intervalli__x4=reorder(df_x4$Intervallo,df_x4$Frequenze_cumulate)

gg4=ggplot(data=df_x4, aes(x = intervalli__x4, y=Frequenze_cumulate, fill = intervalli__x4)) +
  geom_bar(stat = "identity")+ 
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8"))+
  scale_y_continuous(breaks = seq(from=0, to=100, by=10), limits = c(0, 100))+
  scale_fill_brewer(palette=tema_colori) + 
  labs(x = "Intervalli ",y="Frequenza cumulata",title="Istogramma frequenza cumulata di x4")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));gg4 




intervalli__x5=reorder(df_x5$Intervallo,df_x5$Frequenze_cumulate)

gg5=ggplot(data=df_x5, aes(x = intervalli__x5, y=Frequenze_cumulate, fill = intervalli__x5)) +
  geom_bar(stat = "identity")+ 
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8"))+
  scale_y_continuous(breaks = seq(from=0, to=100, by=10), limits = c(0, 100))+
  scale_fill_brewer(palette=tema_colori) + 
  labs(x = "Intervalli",y="Frequenza cumulata",title="Istogramma frequenza cumulata di x5")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));gg5 




intervalli__x6=reorder(df_x6$Intervallo,df_x6$Frequenze_cumulate)

gg6=ggplot(data=df_x6, aes(x = intervalli__x6, y=Frequenze_cumulate, fill = intervalli__x6)) +
  geom_bar(stat = "identity")+ 
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8"))+
  scale_y_continuous(breaks = seq(from=0, to=100, by=10), limits = c(0, 100))+
  scale_fill_brewer(palette=tema_colori) + 
  labs(x = "Intervalli ",y="Frequenza cumulata",title="Istogramma frequenza cumulata di x6")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));gg6 





intervalli__x7=reorder(df_x7$Intervallo,df_x7$Frequenze_cumulate)

gg7=ggplot(data=df_x7, aes(x = intervalli__x7, y=Frequenze_cumulate, fill = intervalli__x7)) +
  geom_bar(stat = "identity")+ 
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8", expression(italic("Int_9"))))+
  scale_y_continuous(breaks = seq(from=0, to=100, by=10), limits = c(0, 100))+
  scale_fill_brewer(palette=tema_colori) + 
  labs(x = "Intervalli",y="Frequenza cumulata",title="Istogramma frequenza cumulata di x7")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));gg7 




intervalli__y=reorder(df_y$Intervallo,df_y$Frequenze_cumulate)

ggy=ggplot(data=df_y, aes(x = intervalli__y, y=Frequenze_cumulate, fill = intervalli__y)) +
  geom_bar(stat = "identity")+ 
  scale_x_discrete(labels = c("Int_1", "Int_2","Int_3","Int_4","Int_5","Int_6","Int_7","Int_8", expression(italic("Int_9"))))+
  scale_y_continuous(breaks = seq(from=0, to=100, by=10), limits = c(0, 100))+
  scale_fill_brewer(palette=tema_colori) + 
  labs(x = "Intervalli",y="Frequenza cumulata",title="Istogramma frequenza cumulata di y")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));ggy 


dev.new(width=1000, height=1000,title="Grafici frequenza assoluta");cat("si consigila una finestra molto ampia")

gg=grid.arrange(gg1, gg2, gg3, gg4, gg5, gg6, gg7,ggy,nrow = 4) #mostro grafici tuttiinsieme













##Commenti e calcoli su parametri del BoxPlot####




#tabella 
  tabella_boxplot=data.frame( min=round (apply( myds,2, min ),2 ),
                              Ist_Qu=round (apply(myds,2,quantile,probs=0.25) ,2),
                              median=round ( apply(myds,2,median),2 ),
                              mean=round( colMeans(myds),2 ),
                              IIIst_Qu=round ( apply(myds,2,quantile,probs=0.75),2 ),
                              max=round (apply( myds,2, max ),2  )
                                );tabella_boxplot=t(tabella_boxplot);View(tabella_boxplot)



##BoxPlot#####

#troviamo la distanza interuantile oltre la quale considerare valori outlier
IQ<-function (x) {
 unname( quantile(x, probs =0.75 ) -quantile(x, probs =0.25 ) )
}

{ 
P_IQR_x1=unname (quantile (x1, probs =0.25) ) -(1.5*IQR(x1)); T_IQR_x1=unname (quantile (x1, probs =0.75) ) +(1.5*IQR(x1))
P_IQR_x2=unname (quantile (x2, probs =0.25) ) -(1.5*IQR(x2)); T_IQR_x2=unname (quantile (x2, probs =0.75) ) +(1.5*IQR(x2))
P_IQR_x3=unname (quantile (x3, probs =0.25) ) -(1.5*IQR(x3)); T_IQR_x3=unname (quantile (x3, probs =0.75) ) +(1.5*IQR(x3))
P_IQR_x4=unname (quantile (x4, probs =0.25) ) -(1.5*IQR(x4)); T_IQR_x4=unname (quantile (x4, probs =0.75) ) +(1.5*IQR(x4))
P_IQR_x5=unname (quantile (x5, probs =0.25) ) -(1.5*IQR(x5)); T_IQR_x5=unname (quantile (x5, probs =0.75) ) +(1.5*IQR(x5))
P_IQR_x6=unname (quantile (x6, probs =0.25) ) -(1.5*IQR(x6)); T_IQR_x6=unname (quantile (x6, probs =0.75) ) +(1.5*IQR(x6))
P_IQR_x7=unname (quantile (x7, probs =0.25) ) -(1.5*IQR(x7)); T_IQR_x7=unname (quantile (x7, probs =0.75) ) +(1.5*IQR(x7))
p_IQR_y=unname (quantile (y, probs =0.25) ) -(1.5*IQR(y)); T_IQR_y=unname (quantile (y, probs =0.75) ) +(1.5*IQR(y))
}



#grafici

ggg1=ggplot(data=myds, aes(y=x1))+
  geom_boxplot(notch = TRUE, linetype="solid", fill = tema_boxplot,color=tema_boxplot2)+
  scale_y_continuous( breaks =round ( c( unname (quantile (x1, probs =c(0.25,0.5,0.75) ) ), max(x1), min(x1), P_IQR_x1, T_IQR_x1 ), 2 ) , limits = c(-3.4,3.4))+
  scale_x_discrete(labels = "" )+
  labs(y="Valori x1",title="BoxPlot x1")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15) );ggg1

 ggg2=ggplot(data=myds, aes(y=x2))+
  geom_boxplot(notch = TRUE, linetype="solid", fill = tema_boxplot,color=tema_boxplot2)+
  scale_y_continuous( breaks =round ( c( unname (quantile (x2, probs =c(0.25,0.5,0.75) ) ), max(x2), min(x2), P_IQR_x2, T_IQR_x2 ), 2 ) , limits = c(-3.4,3.4))+ 
  scale_x_discrete(labels = "" )+
  labs(y="Valori x2",title="BoxPlot x2")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15));ggg2


ggg3=ggplot(data=myds, aes(y=x3))+
  geom_boxplot(notch = TRUE, linetype="solid", fill = tema_boxplot,color=tema_boxplot2)+
  scale_y_continuous( breaks =round ( c( unname (quantile (x3, probs =c(0.25,0.5,0.75) ) ), max(x3), min(x3), P_IQR_x3, T_IQR_x3 ), 2 ) , limits = c(-3.4,3.4))+
  scale_x_discrete(labels = "" )+
  labs(y="Valori x3",title="BoxPlot x3")+ 
  theme(plot.title = element_text(hjust = 0.5));ggg3


ggg4=ggplot(data=myds, aes(y=x4))+
  geom_boxplot(notch = TRUE, linetype="solid", fill = tema_boxplot,color=tema_boxplot2)+
  scale_y_continuous( breaks =round ( c( unname (quantile (x4, probs =c(0.25,0.5,0.75) ) ), max(x4), min(x4), P_IQR_x4, T_IQR_x4 ), 2 ) , limits = c(-3.4,3.4))+
  scale_x_discrete(labels = "" )+
  labs(y="Valori x4",title="BoxPlot x4")+ 
  theme(plot.title = element_text(hjust = 0.5));ggg4


ggg5=ggplot(data=myds, aes(y=x5))+
  geom_boxplot(notch = TRUE, linetype="solid", fill = tema_boxplot,color=tema_boxplot2)+
  scale_y_continuous( breaks =round ( c( unname (quantile (x5, probs =c(0.25,0.5,0.75) ) ), max(x5), min(x5), P_IQR_x5, T_IQR_x5 ), 2 ) , limits = c(-3.4,3.4))+
  scale_x_discrete(labels = "" )+
  labs(y="Valori x5",title="BoxPlot x5")+ 
  theme(plot.title = element_text(hjust = 0.5));ggg5


ggg6=ggplot(data=myds, aes(y=x6))+
  geom_boxplot(notch = TRUE, linetype="solid", fill = tema_boxplot,color=tema_boxplot2)+
  scale_y_continuous( breaks =round ( c( unname (quantile (x6, probs =c(0.25,0.5,0.75) ) ), max(x6), min(x6), P_IQR_x6, T_IQR_x6 ), 2 ) , limits = c(-3.4,3.4))+
  scale_x_discrete(labels = "" )+
  labs(y="Valori x6",title="BoxPlot x6")+ 
  theme(plot.title = element_text(hjust = 0.5));ggg6


ggg7=ggplot(data=myds, aes(y=x7))+
  geom_boxplot(notch = TRUE, linetype="solid", fill = tema_boxplot,color=tema_boxplot2)+
  scale_y_continuous( breaks =round ( c( unname (quantile (x7, probs =c(0.25,0.5,0.75) ) ), max(x7), min(x7), P_IQR_x7, T_IQR_x7 ), 2 ) , limits = c(-3.4,3.4))+
  scale_x_discrete(labels = "" )+
  labs(y="Valori x7",title="BoxPlot x7")+ 
  theme(plot.title = element_text(hjust = 0.5));ggg7


gggy=ggplot(data=myds, aes(y=y))+
  geom_boxplot(notch = TRUE, linetype="solid", fill = tema_boxplot,color=tema_boxplot2)+
  scale_y_continuous( breaks =round ( c( unname (quantile (y, probs =c(0.25,0.5,0.75) ) ), max(y), min(y), p_IQR_y, T_IQR_y, 10.7 ), 2 ) )+
  scale_x_discrete(labels = "" )+
  labs(y="Valori y",title="BoxPlot y")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15) );gggy








dev.new(width=2000, height=500,title="Grafici frequenza assoluta");cat("si consigila una finestra molto ampia") 

ggg=grid.arrange(ggg1, ggg2, ggg3, ggg4, ggg5, ggg6, ggg7,gggy,nrow = 1)


rm(P_IQR_x1, P_IQR_x2, P_IQR_x3, P_IQR_x4, P_IQR_x5, P_IQR_x5, P_IQR_x6, P_IQR_x7,T_IQR_x1, T_IQR_x2, T_IQR_x3, T_IQR_x4, T_IQR_x5, T_IQR_x6, T_IQR_x7)










##Indice di dispersione########

tabella_conr=data.frame( var=round (apply( myds, 2, var ),2 ),
                         dev_stand= round (apply( myds,2, sd ),2 ),         #Sarebbe la radie della varianza
                         Coeff_Var= round((apply(myds, 2, sd)) / abs( ( apply(myds, 2, mean)) ) , 2)
                        );tabella_conr=t(tabella_conr);

View(tabella_conr)





##Commenti sulla distribuzione####
cat(.S_quattro);rm(.S_quattro)

test_s=function(x){
  if(shapiro.test(x)$p.value<0.05)
    cat(round(shapiro.test(x)$p.value,3) ,"False" )
  else
    cat(round(shapiro.test(x)$p.value,3),"True")
}## L'ipotesi nulla è che la distribuzione sia normale

test_s(x1)
test_s(x2)
test_s(x3)
test_s(x4)
test_s(x5)
test_s(x6)
test_s(x7)
test_s(y)
cat("nessuna variabile indipendente è considerabile gaussiana, quella dipendente si");rm(test_s)


#Grafici

dis_x1=ggplot(myds, aes(x1 , fill="" )) +
  geom_density(adjust = 1,alpha=.5 )+
  scale_fill_manual(values = tema_boxplot)+
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15))+
  labs(y="",x="",title="Distribuzione x1");dis_x1

dis_x2=ggplot(myds, aes(x2 , fill="" )) +
  geom_density(adjust = 1,alpha=.5 )+
  scale_fill_manual(values = tema_boxplot)+
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15))+
  labs(y="",x="",title="Distribuzione x2");dis_x2

dis_x3=ggplot(myds, aes(x3 , fill="" )) +
  geom_density(adjust = 1,alpha=.5 )+
  scale_fill_manual(values = tema_boxplot)+
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15))+
  labs(y="",x="",title="Distribuzione x3");dis_x3

dis_x4=ggplot(myds, aes(x4 , fill="" )) +
  geom_density(adjust = 1,alpha=.5 )+
  scale_fill_manual(values = tema_boxplot)+
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15))+
  labs(y="",x="",title="Distribuzione x4");dis_x4

dis_x5=ggplot(myds, aes(x5, fill="" )) +
  geom_density(adjust = 1,alpha=.5 )+
  scale_fill_manual(values = tema_boxplot)+
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15))+
  labs(y="",x="",title="Distribuzione x5");dis_x5

dis_x6=ggplot(myds, aes(x6 , fill="" )) +
  geom_density(adjust = 1,alpha=.5 )+
  scale_fill_manual(values = tema_boxplot)+
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15))+
  labs(y="",x="",title="Distribuzione x6");dis_x6

dis_x7=ggplot(myds, aes(x7 , fill="" )) +
  geom_density(adjust = 1,alpha=.5 )+
  scale_fill_manual(values = tema_boxplot)+
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15))+
  labs(y="",x="",title="Distribuzione x7");dis_x7




normale_y=ggplot(myds, aes(y , fill="" )) +
  geom_density(adjust = 1,alpha=.5 )+
  scale_fill_manual(values = tema_boxplot)+
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15))+
  labs(y="",x="",title="Distribuzione Y");normale_y



 
ggg=grid.arrange(ggg1, ggg2, ggg3, ggg4, ggg5, ggg6, ggg7,gggy,nrow = 1)














#dataset senza outlier(in più)#####
myds2 <- subset(myds, myds[,1] != min(y))



#tabella riassuntiva senza outlier
tabella_tot=data.frame( min=round (apply( myds2,2, min ),2 ),
                            Ist_Qu=round (apply(myds2,2,quantile,probs=0.25) ,2),
                            median=round ( apply(myds2,2,median),2 ),
                            mean=round( colMeans(myds2),2 ),
                            IIIst_Qu=round ( apply(myds2,2,quantile,probs=0.75),2 ),
                            max=round (apply( myds2,2, max ),2  ),
                            var=round (apply( myds2, 2, var ),2 ),
                            dev_stand= round (apply( myds2,2, sd ),2 ),        
                            Coeff_Var= round((apply(myds2, 2, sd)) / abs( ( apply(myds2, 2, mean)) ) , 2)
                            );tabella_tot=t(tabella_tot);View(tabella_tot)





##Bloxplot senza outlier
gggy_2=ggplot(data=myds2, aes(y=y))+
  geom_boxplot(notch = TRUE, linetype="solid", fill = tema_boxplot,color=tema_boxplot2)+
  scale_fill_brewer(palette=tema_colori) + 
  scale_y_continuous( breaks =round ( c( unname (quantile (y, probs =c(0.25,0.5,0.75) ) ), max(y), min(y), p_IQR_y, T_IQR_y, 10.7 ), 2 ) )+
  scale_x_discrete(labels = "" )+
  labs(y="Valori y",title="BoxPlot y")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15) );gggy_2









##Correalzione/Scatter Plot ######################


plot(myds)



pairs(myds,
      col = tema_boxplot, 
      panel=panel.smooth,# colore
      pch = 20,                   # forma punti
      labels = c("Y","x1","x2","x3","x4","x5","x6","x7"), # Change labels
      main = "Scatter Plot 1" # Titolo
)



cor1 = round(cor(myds),  2)


library(corrplot)
corrplot.mixed(cor(myds),number.cex=0.8,tl.cex=0.8, main= "Cor Plot") 














##"Inizio Regressione"#####################


##REGRESSIONE SEMPLICE####
#x1 molto x1+x1^2

  mod1 = lm(y~x1, data=myds); summary(mod1)
  mod2 = lm(y~x1+I(x1^2), data=myds); summary(mod2)
  mod3 = lm(y~x1+I(x1^2)+I(x1^3), data=myds); summary(mod3)
  mod4= lm(y~I(x1^2), data=myds); summary(mod4)
  mod5 = lm(y~I(x1^3), data=myds); summary(mod5)
  
  
  
  plot(x1,y,main="Modello esperimento", col = tema_boxplot,pch = 20)
  
  x_range <- seq(min(x1), max(x1), length.out = 100)
  
  y_pred1 <- predict(mod1, data.frame(x1 = x_range))
  lines(x_range, y_pred1, col = "red",lwd = 2)
  legend("bottomleft", legend=c("x1"),col=c("red"), lty=1:2, cex=0.6)
  
  y_pred2 <- predict(mod2, data.frame(x1 = x_range))
  lines(x_range, y_pred2, col = "blue",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x1","x1+x1^2"),col=c("red","blue"), lty=1:2, cex=0.6)
  
  y_pred3 <- predict(mod3, data.frame(x1 = x_range))
  lines(x_range, y_pred3, col = "brown",lwd = 2)
  legend("bottomleft", legend=c("x1","x1+x1^2","x1+x1^2+x1^3"),col=c("red","blue","brown"), lty=1:2, cex=0.6)
  
  y_pred4 <- predict(mod4, data.frame(x1 = x_range))
  lines(x_range, y_pred4, col = "purple",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x1","x1+x1^2","x1+x1^2+x1^3","x1^2"),col=c("red","blue","brown","purple"), lty=1:2, cex=0.6)
  
  y_pred5 <- predict(mod5, data.frame(x1 = x_range))
  lines(x_range, y_pred5, col = "black",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x1","x1+x1^2","x1+x1^2+x1^3","x1^2","x1^3"),col=c("red","blue","brown","purple","black"), lty=1:2, cex=0.6)
  



g_relazione_x1=ggplot(myds, aes(x1, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),linewidth = 1, level = 0.95, color="green")+
  labs(y="y",x="",title="Regressione x1");g_relazione_x1

#x2 molto

  mod1 = lm(y~x2, data=myds); summary(mod1)
  mod2 = lm(y~x2+I(x2^2), data=myds); summary(mod2)
  mod3 = lm(y~x2+I(x2^2)+I(x2^3), data=myds); summary(mod3)
  mod4=  lm(y~x2+I(x2^2)+I(x2^3), data=myds); summary(mod4)
  mod5 = lm(y~I(x2^3), data=myds); summary(mod5)
  
  
  
  plot(x2,y,main="Modello esperimento", col = tema_boxplot,pch = 20)
  
  x_range <- seq(min(x2), max(x2), length.out = 100)
  
  y_pred1 <- predict(mod1, data.frame(x2 = x_range))
  lines(x_range, y_pred1, col = "red",lwd = 2)
  legend("bottomleft", legend=c("x2"),col=c("red"), lty=1:2, cex=0.6)
  
  y_pred2 <- predict(mod2, data.frame(x2 = x_range))
  lines(x_range, y_pred2, col = "blue",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x2","x2+x2^2"),col=c("red","blue"), lty=1:2, cex=0.6)
  
  y_pred3 <- predict(mod3, data.frame(x2 = x_range))
  lines(x_range, y_pred3, col = "brown",lwd = 2)
  legend("bottomleft", legend=c("x2","x2+x2^2","x2+x2^2+x^3"),col=c("red","blue","brown"), lty=1:2, cex=0.6)
  
  y_pred4 <- predict(mod4, data.frame(x2 = x_range))
  lines(x_range, y_pred4, col = "purple",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x2","x2+x2^2","x2+x2^2+x2^3","x2^2"),col=c("red","blue","brown","purple"), lty=1:2, cex=0.6)
  
  y_pred5 <- predict(mod5, data.frame(x2 = x_range))
  lines(x_range, y_pred5, col = "black",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x2","x2+x2^2","x2+x2^2+x2^3","x2^2","x2^3"),col=c("red","blue","brown","purple","black"), lty=1:2, cex=0.6)
  




g_relazione_x2=ggplot(myds, aes(x2, y)) +
  geom_point() +
  stat_smooth(method='lm', formula = y ~ x, linewidth = 1, level = 0.95, color="blue")+
  labs(y="y",x="",title="Regressione x2");g_relazione_x2



#x3 null

  mod1 = lm(y~x3, data=myds); summary(mod1)
  mod2 = lm(y~x3+I(x3^2), data=myds); summary(mod2)
  mod3 = lm(y~x3+I(x3^2)+I(x3^3), data=myds); summary(mod3)
  mod4 = lm(y~I(x3^2), data=myds); summary(mod4)
  mod5 = lm(y~I(x3^3), data=myds); summary(mod5)
  
  
  
  plot(x3,y,main="Modello esperimento", col = tema_boxplot,pch = 20)
  
  x_range <- seq(min(x3), max(x3), length.out = 100)
  
  y_pred1 <- predict(mod1, data.frame(x3 = x_range))
  lines(x_range, y_pred1, col = "red",lwd = 2)
  legend("bottomleft", legend=c("x3"),col=c("red"), lty=1:2, cex=0.6)
  
  y_pred2 <- predict(mod2, data.frame(x3 = x_range))
  lines(x_range, y_pred2, col = "blue",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x3","x3+x3^2"),col=c("red","blue"), lty=1:2, cex=0.6)
  
  y_pred3 <- predict(mod3, data.frame(x3 = x_range))
  lines(x_range, y_pred3, col = "brown",lwd = 2)
  legend("bottomleft", legend=c("x3","x3+x3^2","x3+x3^2+x^3"),col=c("red","blue","brown"), lty=1:2, cex=0.6)
  
  y_pred4 <- predict(mod4, data.frame(x3 = x_range))
  lines(x_range, y_pred4, col = "purple",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x3","x3+x3^2","x3+x3^2+x3^3","x3^2"),col=c("red","blue","brown","purple"), lty=1:2, cex=0.6)
  
  y_pred5 <- predict(mod5, data.frame(x3 = x_range))
  lines(x_range, y_pred5, col = "black",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x3","x3+x3^2","x3+x3^2+3^3","x3^2","x3^3"),col=c("red","blue","brown","purple","black"), lty=1:2, cex=0.6)
  
  
  
  



g_relazione_x3=ggplot(myds, aes(x3, y)) +
  geom_point()+
  labs(y="y",x="",title="Regressione assente x3");g_relazione_x3





#x4 poco ^3

  mod1 = lm(y~x4, data=myds); summary(mod1)
  mod2 = lm(y~x4+I(x4^2), data=myds); summary(mod2)
  mod3 = lm(y~x4+I(x4^2)+I(x4^3), data=myds); summary(mod3)
  mod4 = lm(y~I(x4^2), data=myds); summary(mod4)
  mod5 = lm(y~I(x4^3), data=myds); summary(mod5)
  
  
  
  plot(x4,y,main="Modello esperimento", col = tema_boxplot,pch = 20)
  
  x_range <- seq(min(x4), max(x4), length.out = 100)
  
  y_pred1 <- predict(mod1, data.frame(x4 = x_range))
  lines(x_range, y_pred1, col = "red",lwd = 2)
  legend("bottomleft", legend=c("x4"),col=c("red"), lty=1:2, cex=0.6)
  
  y_pred2 <- predict(mod2, data.frame(x4 = x_range))
  lines(x_range, y_pred2, col = "blue",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x4","x4+x4^2"),col=c("red","blue"), lty=1:2, cex=0.6)
  
  y_pred3 <- predict(mod3, data.frame(x4 = x_range))
  lines(x_range, y_pred3, col = "brown",lwd = 2)
  legend("bottomleft", legend=c("x4","x4+x4^2","x4+x4^2+x^3"),col=c("red","blue","brown"), lty=1:2, cex=0.6)
  
  y_pred4 <- predict(mod4, data.frame(x4 = x_range))
  lines(x_range, y_pred4, col = "purple",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x4","x4+x4^2","x4+x4^2+x4^3","x4^2"),col=c("red","blue","brown","purple"), lty=1:2, cex=0.6)
  
  y_pred5 <- predict(mod5, data.frame(x4 = x_range))
  lines(x_range, y_pred5, col = "black",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x4","x4+x4^2","x4+x4^2+x4^3","x4^2","x4^3"),col=c("red","blue","brown","purple","black"), lty=1:2, cex=0.6)
  
  
  


g_relazione_x4=ggplot(myds, aes(x4, y)) +
  geom_point() +
  stat_smooth(method='lm', formula = y ~ I(x^3), linewidth = 1, level = 0.95, color="purple")+
  labs(y="y",x="",title="Regressione x4");g_relazione_x4 


#5 molto

  mod1 = lm(y~x5, data=myds2); summary(mod1)
  mod2 = lm(y~x5+I(x5^2), data=myds2); summary(mod2)
  mod3 = lm(y~x5+I(x5^2)+I(x5^3), data=myds2); summary(mod3)
  mod4 = lm(y~x5+I(x5^2), data=myds2); summary(mod4)
  mod5 = lm(y~x5+I(x5^3), data=myds2); summary(mod5)
  
  
  
  plot(x5,y,main="Modello esperimento", col = tema_boxplot,pch = 20)
  
  x_range <- seq(min(x5), max(x5), length.out = 100)
  
  y_pred1 <- predict(mod1, data.frame(x5 = x_range))
  lines(x_range, y_pred1, col = "red",lwd = 2)
  legend("bottomleft", legend=c("x5"),col=c("red"), lty=1:2, cex=0.6)
  
  y_pred2 <- predict(mod2, data.frame(x5 = x_range))
  lines(x_range, y_pred2, col = "blue",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x5","x5+x5^2"),col=c("red","blue"), lty=1:2, cex=0.6)
  
  y_pred3 <- predict(mod3, data.frame(x5 = x_range))
  lines(x_range, y_pred3, col = "brown",lwd = 2)
  legend("bottomleft", legend=c("x5","x5+x5^2","x5+x5^2+x^3"),col=c("red","blue","brown"), lty=1:2, cex=0.6)
  
  y_pred4 <- predict(mod4, data.frame(x5 = x_range))
  lines(x_range, y_pred4, col = "purple",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x5","x5+x5^2","x5+x5^2+x5^3","x5^2"),col=c("red","blue","brown","purple"), lty=1:2, cex=0.6)
  
  y_pred5 <- predict(mod5, data.frame(x5 = x_range))
  lines(x_range, y_pred5, col = "black",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x5","x5+x5^2","x5+x5^2+x5^3","x5^2","x5^3"),col=c("red","blue","brown","purple","black"), lty=1:2, cex=0.6)
  
  



g_relazione_x5=ggplot(myds, aes(x5, y)) +
  geom_point() +
  stat_smooth(method='lm', formula = y ~ x, linewidth = 1, level = 0.95,  color="orange")+
  labs(y="y",x="",title="Regressione x5");g_relazione_x5

#6 poco

  mod1 = lm(y~x6, data=myds); summary(mod1)
  mod2 = lm(y~x6+I(x6^2), data=myds); summary(mod2)
  mod3 = lm(y~x6+I(x6^2)+I(x6^3), data=myds); summary(mod3)
  mod4 = lm(y~x6+I(x6^2), data=myds); summary(mod4)
  mod5 = lm(y~x6+I(x6^3), data=myds); summary(mod5)
  
  
  
  plot(x6,y,main="Modello esperimento", col = tema_boxplot,pch = 20)
  
  x_range <- seq(min(x6), max(x6), length.out = 100)
  
  y_pred1 <- predict(mod1, data.frame(x6 = x_range))
  lines(x_range, y_pred1, col = "red",lwd = 2)
  legend("bottomleft", legend=c("x6"),col=c("red"), lty=1:2, cex=0.6)
  
  y_pred2 <- predict(mod2, data.frame(x6 = x_range))
  lines(x_range, y_pred2, col = "blue",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x6","x6+x6^2"),col=c("red","blue"), lty=1:2, cex=0.6)
  
  y_pred3 <- predict(mod3, data.frame(x6 = x_range))
  lines(x_range, y_pred3, col = "brown",lwd = 2)
  legend("bottomleft", legend=c("x6","x6+x6^2","x6+x6^2+x^3"),col=c("red","blue","brown"), lty=1:2, cex=0.6)
  
  y_pred4 <- predict(mod4, data.frame(x6 = x_range))
  lines(x_range, y_pred4, col = "purple",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x6","x6+x6^2","x6+x6^2+x6^3","x6^2"),col=c("red","blue","brown","purple"), lty=1:2, cex=0.6)
  
  y_pred5 <- predict(mod5, data.frame(x6 = x_range))
  lines(x_range, y_pred5, col = "black",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x6","x6+x6^2","x6+x6^2+x6^3","x6^2","x6^3"),col=c("red","blue","brown","purple","black"), lty=1:2, cex=0.6)
  


g_relazione_x6=ggplot(myds2, aes(x6, y)) +
  geom_point() +
  stat_smooth(method='lm', formula = y ~ x, linewidth = 1, level = 0.95, color="brown")+
  labs(y="y",x="",title="Regressione x6");g_relazione_x6


#7 no

  mod1 = lm(y~x7, data=myds); summary(mod1)
  mod2 = lm(y~x7+I(x7^2), data=myds); summary(mod2)
  mod3 = lm(y~x7+I(x7^2)+I(x7^3), data=myds); summary(mod3)
  mod4 = lm(y~x7+I(x7^2), data=myds); summary(mod4)
  mod5 = lm(y~x7+I(x7^3), data=myds); summary(mod5)
  
  
  
  plot(x7,y,main="Modello esperimento", col = tema_boxplot,pch = 20)
  
  x_range <- seq(min(x7), max(x7), length.out = 100)
  
  y_pred1 <- predict(mod1, data.frame(x7 = x_range))
  lines(x_range, y_pred1, col = "red",lwd = 2)
  legend("bottomleft", legend=c("x7"),col=c("red"), lty=1:2, cex=0.6)
  
  y_pred2 <- predict(mod2, data.frame(x7 = x_range))
  lines(x_range, y_pred2, col = "blue",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x7","x7+x7^2"),col=c("red","blue"), lty=1:2, cex=0.6)
  
  y_pred3 <- predict(mod3, data.frame(x7 = x_range))
  lines(x_range, y_pred3, col = "brown",lwd = 2)
  legend("bottomleft", legend=c("x7","x7+x7^2","x7+x7^2+x^3"),col=c("red","blue","brown"), lty=1:2, cex=0.6)
  
  y_pred4 <- predict(mod4, data.frame(x7 = x_range))
  lines(x_range, y_pred4, col = "purple",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x7","x7+x7^2","x7+x7^2+x7^3","x7^2"),col=c("red","blue","brown","purple"), lty=1:2, cex=0.6)
  
  y_pred5 <- predict(mod5, data.frame(x7 = x_range))
  lines(x_range, y_pred5, col = "black",lwd = 3,lty = 2)
  legend("bottomleft", legend=c("x7","x7+x7^2","x7+x7^2+x7^3","x7^2","x7^3"),col=c("red","blue","brown","purple","black"), lty=1:2, cex=0.6)
  


g_relazione_x7=ggplot(myds2, aes(x7, y)) +
  geom_point()+
  labs(y="y",x="",title="Regressione assente x7");g_relazione_x7




g_relazione_tot=grid.arrange(g_relazione_x1, g_relazione_x2, g_relazione_x3, g_relazione_x4, g_relazione_x5, g_relazione_x6, g_relazione_x7,nrow = 2);g_relazione_tot






###Vero inizio regressione######
cat(.S_cinque);rm(.S_cinque)



modello_1 = lm(y~x1, data=myds); summary(modello_1)
F_statistic_1=summary(modello_1)$fstatistic[1]
F_p_value_1=pf(summary(modello_1)$fstatistic[1],summary(modello_1)$fstatistic[2],summary(modello_1)$fstatistic[3],lower.tail=FALSE)
R_squared_1=summary(modello_1)$r.squared
RSS_1=sum(residuals(modello_1)^2)   #(sarebbe l'sqe)

modello_2 = lm(y~x1+x2, data=myds); summary(modello_2)
F_statistic_2=summary(modello_2)$fstatistic[1]
F_p_value_2=pf(summary(modello_2)$fstatistic[1],summary(modello_2)$fstatistic[2],summary(modello_2)$fstatistic[3],lower.tail=FALSE)
R_squared_2=summary(modello_2)$r.squared
RSS_2=sum(residuals(modello_2)^2)


modello_3 = lm(y~x1+x2+x3, data=myds); summary(modello_3)
F_statistic_3=summary(modello_3)$fstatistic[1]
F_p_value_3=pf(summary(modello_3)$fstatistic[1],summary(modello_3)$fstatistic[2],summary(modello_3)$fstatistic[3],lower.tail=FALSE)
R_squared_3=summary(modello_3)$r.squared
RSS_3=sum(residuals(modello_3)^2)



modello_4 = lm(y~x1+x2+x3+x4, data=myds); summary(modello_4)
F_statistic_4=summary(modello_4)$fstatistic[1]
F_p_value_4=pf(summary(modello_4)$fstatistic[1],summary(modello_4)$fstatistic[2],summary(modello_4)$fstatistic[3],lower.tail=FALSE)
R_squared_4=summary(modello_4)$r.squared
RSS_4=sum(residuals(modello_4)^2)



modello_5 = lm(y~x1+x2+x3+x4+x5, data=myds); summary(modello_5)
F_statistic_5=summary(modello_5)$fstatistic[1]
F_p_value_5=pf(summary(modello_5)$fstatistic[1],summary(modello_5)$fstatistic[2],summary(modello_5)$fstatistic[3],lower.tail=FALSE)
R_squared_5=summary(modello_5)$r.squared
RSS_5=sum(residuals(modello_5)^2)



modello_6 = lm(y~x1+x2+x3+x4+x5+x6, data=myds); summary(modello_6)
F_statistic_6=summary(modello_6)$fstatistic[1]
F_p_value_6=pf(summary(modello_6)$fstatistic[1],summary(modello_6)$fstatistic[2],summary(modello_6)$fstatistic[3],lower.tail=FALSE)
R_squared_6=summary(modello_6)$r.squared
RSS_6=sum(residuals(modello_6)^2)



modello_7 = lm(y~x1+x2+x3+x4+x5+x7, data=myds); summary(modello_7)
F_statistic_7=summary(modello_7)$fstatistic[1]
F_p_value_7=pf(summary(modello_7)$fstatistic[1],summary(modello_7)$fstatistic[2],summary(modello_7)$fstatistic[3],lower.tail=FALSE)
R_squared_7=summary(modello_7)$r.squared
RSS_7=sum(residuals(modello_7)^2)



modello_8 = lm(y~x1+x2+x5, data=myds); summary(modello_8)
F_statistic_8=summary(modello_8)$fstatistic[1]
F_p_value_8=pf(summary(modello_8)$fstatistic[1],summary(modello_8)$fstatistic[2],summary(modello_8)$fstatistic[3],lower.tail=FALSE)
R_squared_8=summary(modello_8)$r.squared
RSS_8=sum(residuals(modello_8)^2)



#anche se inizialmente avevamo scartato x4 ora lo stiamo rivalutando, x6 invece no
modello_9 = lm(y~x1+x2+x5+x7, data=myds); summary(modello_9)
F_statistic_9=summary(modello_9)$fstatistic[1]
F_p_value_9=pf(summary(modello_9)$fstatistic[1],summary(modello_9)$fstatistic[2],summary(modello_9)$fstatistic[3],lower.tail=FALSE)
R_squared_9=summary(modello_9)$r.squared
RSS_9=sum(residuals(modello_9)^2)


modello_10 = lm(y~x1+I(x1^2)+x2+x5, data=myds); summary(modello_10)
F_statistic_10=summary(modello_10)$fstatistic[1]
F_p_value_10=pf(summary(modello_10)$fstatistic[1],summary(modello_10)$fstatistic[2],summary(modello_10)$fstatistic[3],lower.tail=FALSE)
R_squared_10=summary(modello_10)$r.squared
RSS_10=sum(residuals(modello_10)^2)


modello_11 = lm(y~x1+I(x1^2)+x2+I(x4^3)+x5, data=myds); summary(modello_11)
F_statistic_11=summary(modello_11)$fstatistic[1]
F_p_value_11=pf(summary(modello_11)$fstatistic[1],summary(modello_11)$fstatistic[2],summary(modello_11)$fstatistic[3],lower.tail=FALSE)
R_squared_11=summary(modello_11)$r.squared
RSS_11=sum(residuals(modello_11)^2)





##INTERAZIONI MODELLO 11########################################################
cat("cerchiamo delle correlazioni nel modello che più ci convince")

library(sjPlot)
library(ggplot2)
#x1 nessuna interazione

modello_ = lm(y~x1+I(x1^2)+x2+I(x4^3)+x5 +x1:x2, data=myds); summary(modello_)
plot_model(modello_,type="int",title="")


modello_ = lm(y~x1+I(x1^2)+x2+I(x4^3)+x5 + x1:x3, data=myds); summary(modello_)
plot_model(modello_,type="int")


modello_ = lm(y~x1+I(x1^2)+x2+I(x4^3)+x5+ x1:x4, data=myds); summary(modello_)
plot_model(modello_,type="int")



modello_ = lm(y~x1+I(x1^2)+x2+I(x4^3)+x5+ x1:x5, data=myds); summary(modello_)
plot_model(modello_,type="int")


#x2 nessuna interazione

modello_ = lm(y~x1+I(x1^2)+x2+I(x4^3)+x5+ x2:x4, data=myds); summary(modello_)
plot_model(modello_,type="int")


modello_ = lm(y~x1+I(x1^2)+x2+I(x4^3)+x5+x2:x5, data=myds); summary(modello_)
plot_model(modello_,type="int")



#x4 nessuna interazione

modello_ = lm(y~x1+I(x1^2)+x2+I(x4^3)+x5+x4:x5, data=myds); summary(modello_)
plot_model(modello_,type="int")


#nessuna interazione













##tabelle#####

tabella_modelli=data.frame( row.names = c("modello_1", "modello_2", "modello_3", "modello_4", "modello_5", "modello_6", "modello_7", "modello_8", "modello_9", "modello_10","modello_11"),
                            R_squared=   round (c( R_squared_1, R_squared_2, R_squared_3, R_squared_4, R_squared_5, R_squared_6, R_squared_7,  R_squared_8, R_squared_9, R_squared_10, R_squared_11 ), 3),
                            RSS=c( RSS_1, RSS_2, RSS_3, RSS_4, RSS_5, RSS_6, RSS_7, RSS_8, RSS_9, RSS_10,  RSS_11 )
                            )

View(tabella_modelli)




#grafico RSS
      linechart_RSS=ggplot( tabella_modelli , aes(x=seq(from=1, to=11,by=1), y=RSS)) +
                    geom_line(color="green",linewidth=1.3) +
                    geom_point(size=2.5)+
                    scale_x_continuous(breaks=seq(from=1, to=11,by=1))+
                    scale_y_continuous( limits = c(10000, 45000))+
                    labs(x = "Modelli",y="RSS",title="RSS")+ 
                    theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));linechart_RSS 
  
  
#grafico R_squared  
linechart_R_squared=ggplot( tabella_modelli , aes(x=seq(from=1, to=11,by=1), y=R_squared)) +
                    geom_line(color="green",linewidth=1.3) +
                    geom_point(size=2.5)+
                    scale_y_continuous(breaks =seq(from=0,to=1,by=0.2), limits = c(0, 1))+
                    scale_x_continuous(breaks=seq(from=1, to=11,by=1))+
                    labs(x = "Modelli",y="R^2",title="R_squared")+ 
                    theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 10));linechart_R_squared 

        
        
        
        
        
# quindi paragoniamo il modello 10 ed il modello 11#####

n=100        
dataset_parametrimodelli=data.frame(row.names = c( "modello_10","modello_11"),
                                    R_squared = round (c( R_squared_10, R_squared_11),2),
                                    F_statistic= round (c( F_statistic_10, F_statistic_11),2),
                                    Media_redisui=c( mean(residuals(modello_10)), mean(residuals(modello_11)) ),
                                    RSE=c(sqrt(RSS_8/(n-length(modello_8$coefficients))),sqrt(RSS_8/(n-length(modello_8$coefficients))) ),
                                    RSS=c(RSS_10, RSS_11),
                                    sqe=c( sum((residuals(modello_10))^2),  sum((residuals(modello_11))^2)),           #nominatore variabilità
                                    msqe=c(  RSS_10/(n-length(modello_10$coefficients)),  RSS_11/(n-length(modello_11$coefficients))) #SE_8/4  #ha 3 regressori (k+1) #SE_10/5 #ha 5 regressori (k+1)       
                                    )

attach(dataset_parametrimodelli)     
View(dataset_parametrimodelli)


##Metodo minimi quadrati a mano#######################

{
Y_vetttore=c(y)
X_matrice=cbind(rep(1, 100),x1,I(x1^2),x2,I(x4^3),x5)
solve(t(X_matrice) %*% X_matrice) %*% t(X_matrice) %*% Y_vetttore
modello_11$coefficients


rm(Y_vetttore,X_matrice)
}



##Intervalli di confidenza######################
##A MANO
{
b0=modello_11$coefficients[1]  
b1=modello_11$coefficients[2]
b2=modello_11$coefficients[3]
b3=modello_11$coefficients[4]
b4=modello_11$coefficients[5]
b5=modello_11$coefficients[6]


X=cbind(rep(1, 100),modello_11$model[,-1])
X_tr=t(X);X=t(X_tr)   

inverted=solve(X_tr%*%X)

S=sqrt( RSS_11/(n-length(modello_11$coefficients)) )

C0=inverted[1,1]
C1=inverted[2,2]
C2=inverted[3,3]
C3=inverted[4,4]
C4=inverted[5,5]
C5=inverted[6,6]

S.B0=S*sqrt(C0)
S.B1=S*sqrt(C1)
S.B2=S*sqrt(C2)
S.B3=S*sqrt(C3)
S.B4=S*sqrt(C4)
S.B5=S*sqrt(C5)

#alpha = 0.05
t.val=qt(1-0.025,92) #a=0.05

b0-t.val*S.B0;b0+t.val*S.B0
b1-t.val*S.B1;b1+t.val*S.B1
b3-t.val*S.B2;b3+t.val*S.B2
b4-t.val*S.B3;b4+t.val*S.B3
b5-t.val*S.B4;b5+t.val*S.B4

confint(modello_11)


rm(b0,b1,b2,b3,b4,b5,X_tr,X,t.val,S.B0,S.B1,S.B2,S.B3,S.B4,S.B5,C0,C1,C2,C3,C4,C5)
}



##Intervalli AUTOMATICI

#11
intervalli_b_11=data.frame(row.names = c("b0", "b1","b2", "b3","b4","b5"),
                           coefficienti=round(modello_11$coefficients,2),
                           confint(modello_11)
                             
)
  
View(intervalli_b_11)  

  
  
#10
intervalli_b_10=data.frame(row.names = c("b0", "b1","b2", "b3","b4"),
                           coefficienti=round(modello_10$coefficients,2),
                           confint(modello_10)
  
                          )

View(intervalli_b_10)









##Grafici paragona modello#### 
#ci serve sopratutto per i grafici di ggplot
dataset_perilgrafico=data.frame(y_= myds$y,
                                y_cappello_11=modello_11$fitted.values,
                                y_cappello_10=modello_10$fitted.values,
                                residui_11=residuals(modello_11),
                                residui_10=residuals(modello_10),
                                RSS_11 = RSS_11,
                                RSS_10 = RSS_10
                                )    
attach(dataset_perilgrafico) 




##Grafico y-ycappello


#10        
paragona_10=ggplot(dataset_perilgrafico, aes( x=y_,y= y_cappello_10)) +
            geom_point()+
            geom_abline(intercept = 0, slope = 1,linewidth=2, color="blue")+
            scale_x_continuous(limits = c(-10,120))+
            scale_y_continuous(breaks =seq(from=0,to=120,by=10))+
            labs(y="y_cappello",x="y",title="Valutazione modello 10");paragona_10;


#11        
paragona_11=ggplot(dataset_perilgrafico, aes(x=y_,y= y_cappello_11)) +
  geom_point()+
  geom_abline(intercept = 0, slope = 1,linewidth=2, color="green")+
  scale_x_continuous(limits = c(-10,120))+
  scale_y_continuous(breaks =seq(from=0,to=120,by=10))+
  labs(x="y",y="y_cappello",title="Valutazione modello 11");paragona_11;







##boxplot residui


#10
ggresidui_10=ggplot(data=dataset_perilgrafico, aes(y=residui_10))+
  geom_boxplot( linetype="solid", fill ="blue",color=tema_boxplot)+
  scale_x_discrete(labels = "" )+
  labs(y="",title="Residui 10")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15) );ggresidui_10


#11
ggresidui_11=ggplot(data=dataset_perilgrafico, aes(y=residui_11))+
  geom_boxplot( linetype="solid", fill ="green",color=tema_boxplot)+
  scale_x_discrete(labels = "" )+
  labs(y="",title="Residui 11")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15) );ggresidui_11




{
dataset_perilgrafico[which.min(dataset_perilgrafico[, "residui_10"]),1]
dataset_perilgrafico[which.min(dataset_perilgrafico[, "residui_11"]),1]
cat("confermiamo che y=-3 non dovrebbe essere un errore di trascrizione perchè non causa alcun outlier anzi y=14 è il valore più problematico")
}  



######################################################VERIFICA Proprietà

par(mfrow=c(2,2))
plot(modello_10, col="blue",main ="modello 10",  pch = 20) 

par(mfrow=c(2,2))
plot(modello_11, col="green",main ="modello 11",  pch = 18)        
        
test_s=function(x){
  if(shapiro.test(x)$p.value<0.05)
    cat(round(shapiro.test(x)$p.value,3) ,"False" )
  else
    cat(round(shapiro.test(x)$p.value,3),"True")
}

test_s(residuals(modello_10))
test_s(residuals(modello_11))


####################################################STEPWISE

lm.null<- lm(y~1,data=myds)    
fitAICfwd <- step(lm.null, scope= ~ (x1+x2+x3+x4+x5+x6+x7+I(x1^2)+I(x1^3)+I(x4^2)+I(x4^3))^2, trace =1) #+x1:x2+x1:x4+x1:x5+x2:x4+x2:x5+x4:x5
summary(fitAICfwd)

fitAICfwd_2= update(fitAICfwd, ~ . -x1-I(x1^3):I(x4^3)-I(x1^3):I(x4^2)-x5:x1 )
summary(fitAICfwd_2)

modello_12= update(fitAICfwd_2, ~ . -I(x1^2) -I(x4^2) -x5:I(x1^3) -x2:I(x4^3) )
summary(modello_12)



par(mfrow=c(2,2))
plot(fitAICfwd, col="red",main ="modello stepwise",  pch = 18)    


par(mfrow=c(2,2))
plot(modello_12, col="orange",main ="modello 12",  pch = 18)   
   
###########################################################ANOVA
anova(modello_12,modello_11,modello_10) ###anche se l'ipotesi di normalia 
#non è verificata (ma è quasi verificata) usiamo il test del anova per farci un test di paragone



#######################################################MODELLO FINALE
cat("miglioriamo il nostro modello con le considerazione stepwise")

modello_finale=lm(y~I(x1^2)+I(x1^3)+x2+x5+I(x2^2):I(x4^2), data=myds); summary(modello_finale)


#grafico proprietà
par(mfrow=c(2,2))
plot(modello_finale, col="green",main ="modello finale",  pch = 10) 



#boxplot
dataset_perilgrafico$residui_fianle=residuals(modello_finale)

ggresidui_finale=ggplot(data=dataset_perilgrafico, aes(y=residui_fianle))+
  geom_boxplot( linetype="solid", fill ="green",color=tema_boxplot)+
  scale_x_discrete(labels = "" )+
  labs(y="",title="Residui finale")+ 
  theme(plot.title = element_text(hjust = 0.5),text=element_text(size = 15) );ggresidui_finale


#outliner
dataset_perilgrafico[which.min(dataset_perilgrafico[, "residui_fianle"]),1]


#grafico y -y_cappello
dataset_perilgrafico$y_cappello_finale =modello_finale$fitted.values

ggresidui_finale=ggplot(dataset_perilgrafico, aes( x=y_, y= y_cappello_10)) +
  geom_point()+
  geom_abline(intercept = 0, slope = 1,linewidth=2, color="green")+
  scale_x_continuous(limits = c(-10,120))+
  scale_y_continuous(breaks =seq(from=0,to=120,by=10))+
  labs(y="y_cappello",x="y",title="Valutazione modello finale");ggresidui_finale;

#residui normali
test_s(residuals(modello_finale))

###anova
anova(modello_12,modello_11,modello_10,modello_finale)

########################################################MODELLO SENZA RESIDUO OUTLIER  di y=14  
cat("valutiamo il modello se togliessimo y=14")
myds_clean <-myds[!(myds[,1] > 14 & myds[,1] < 15),]
summary(lm(formula =y ~ x1 + I(x1^2) + x2 + x5, data = myds_clean)) #1

summary(lm(y~I(x1^2)+I(x1^3)+x2+x5+I(x2^2):I(x4^2), data = myds_clean))








