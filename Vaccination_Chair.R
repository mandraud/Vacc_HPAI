lots=NULL

initLots<-function(n=1000,id1=0,Sale,minV1=10,maxV1=21,minDelayV2=18,maxDelayV2=23){
  values <- minV1:maxV1
  # Définir les probabilités
  # Probabilité plus forte sur 8:15
  prob <- c(rep(0.1, length(minV1:15)), rep(0.05,length(16:maxV1)))
    # Normaliser pour s'assurer que la somme des probabilités est égale à 1
  prob <- prob / sum(prob)
    # Vérification des probabilités
  # print(data.frame(values, prob))
    # Générer des échantillons à partir de cette distribution
  # Pour la reproductibilité

  
  # Visualiser la distribution
  # table(sample_values)
  # barplot(table(sample_values), col = "skyblue", main = "Distribution générée", xlab = "Valeurs", ylab = "Fréquence")
  lots=data.frame('id' = id1+(1:n),
                "Age" = rep(1,n),
                "statut" = "NV",
                "AgeV1" =  sample(values, size = n, replace = TRUE, prob = prob)
                # "AgeV2"=sample(30:40,n,replace = T), 
                )
  tmp=runif(n)
  lots$AgeCull=Sale[sapply(tmp,function(x) which.min(x>Sale$proba)),1]
  lots$AgeV2=lots$AgeV1 + sample(minDelayV2:maxDelayV2,n,replace=TRUE)
  lots$AgeCrit= lots$AgeV2 + 7 + 42
  lots$statut=factor(lots$statut,levels=c("NV","NP","PP","FP"))
lots
  }


Vaccination_Modele_Chair<- function(nweeks=26,ninit=21,Sale,MEP){
  
  
lots=initLots(n=ninit,Sale=Sale)


res=c("week"=0,table(lots$statut))
res2=data.frame("week"=0,id=lots$id,statut=lots$statut)
for (t in 2:(nweeks*7+1)){
  lots$Age=lots$Age+1
  if (any(lots$Age==lots$AgeV1)) lots$statut[which(lots$Age==lots$AgeV1)]="PP"
  if (any((lots$statut=="PP" & lots$Age==lots$AgeV2 + 7)->index)) lots$statut[index]="FP"
  if (any((lots$statut=="FP" & lots$Age==lots$AgeCrit)->index)) lots$statut[index]="NP"
  if (any(lots$Age==lots$AgeCull)) {
    
    lots=lots[-which(lots$Age==lots$AgeCull),]
  }
  if (any(MEP[,1]==t)){
  intro=initLots(MEP[which(MEP[,1]==t),2],id1=max(lots$id),Sale=Sale)}
  intro$Age=1
  lots=rbind(lots,intro)
  if (t%%7==0){ tmp=c("week"=t/7,table(lots$statut)) 
                res=rbind(res,tmp)
                res2=rbind(res2,
                            data.frame("week"=t/7,
                                  id=lots$id,
                                  statut=lots$statut))
                }
}

res2$week=factor(res2$week,levels=unique(res2$week))
levels(res2$statut)=c("Too young to be protected","End Of Protection",
                      "Primary protection","Secondary protection")
Plot1<-ggplot(res2, aes(x=week,fill=statut))+geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)+ 
  scale_fill_manual(values=c("orange3", "seashell4", "seagreen2", "seagreen4"))+
  xlab("Week")+ylab('Percentage of flocks')+ 
  theme(legend.position = "bottom",legend.box = "horizontal") +
  guides( fill = guide_legend(title = NULL,ncol = 2, byrow=TRUE))

return(list(res2,Plot1))
# ggplot(res2, aes(x=week,fill=statut))+geom_bar() 
}

