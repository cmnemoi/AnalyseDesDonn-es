#Installation des libraries
install.packages("Factoshiny")
install.packages("FactoMineR")
install.packages("plot3D")
install.packages("plotly")

library(FactoMineR)
library(Factoshiny)
library(plotly)


#Hobbies

#Chargement des données
data(hobbies)

#ACM
res.hobbies.MCA = MCA(hobbies,quali.sup = 19:22, quanti.sup = 23, graph=FALSE)

#Graphique de la représentation des variables
plot.MCA(res.hobbies.MCA,choix="var")

#Représentation des hobbies dans le plan factoriel
activities = c(seq(2,36,by=2),37:39)
plot.MCA(res.hobbies.MCA,choix = "ind",invisible = c("ind","quali.sup"),selectMod = activities)

#Représentation des modalités de la variable age dans le plan factoriel
age = 3:10
plot.MCA(res.hobbies.MCA,choix = "ind",invisible = c("ind","var"),selectMod = age)


#German credit

#Chargement des données
load("C:/Users/madic/Desktop/Charles/Ecole/Analyse de données/germanCredit.RData")

### Donnees quantitatives et qualitatives
sub_quanti = germanCredit.data[,which(names(germanCredit.data) %in% liste_var_quanti)]
sub_quanti.scale=data.frame(scale(sub_quanti, scale=TRUE, center=TRUE))
sub_quali = germanCredit.data[,which(names(germanCredit.data) %in% liste_var_quali)]

#PCA
res.credit.pca=PCA(sub_quanti, scale=TRUE, graph=FALSE)
#Représentation des individus dans le plan factoriel
plot.PCA(res.credit.pca,choix="ind")

#CAH et dendrogramme
res.credit.hclust=hclust(dist(sub_quanti.scale), method="ward.D")
plot(res.credit.hclust, sub="", xlab="")

#HCPC
res.credit.pca=PCA(sub_quanti, scale=TRUE, graph=FALSE)
res.credit.hcpc=HCPC(res.credit.pca, graph=FALSE, nb.clust=3)

#Représentation des clusters dans le premier plan factoriel
res.credit.pca=PCA(sub_quanti, scale=TRUE, graph=FALSE)
res.credit.hcpc=HCPC(res.credit.pca, graph=FALSE, nb.clust=3)
individus_dans_l_hyperplan_factoriel = res.credit.pca$ind$coord
individus_dans_l_hyperplan_factoriel = data.frame(individus_dans_l_hyperplan_factoriel)
clusters = res.credit.hcpc$data.clust$clust


fig = plot_ly(individus_dans_l_hyperplan_factoriel,x=~Dim.1,y=~Dim.2,color=clusters, mode="markers", type='scatter') %>%
  layout(title = "Représentation des individus dans le premier plan factoriel",legend=list(title=list(text="Cluster trouvé par l'ACH")))

fig

#Représentation des clusters dans un sous-espace 3D
res.credit.hcpc = HCPC(sub_quanti, graph=FALSE, nb.clust=4)
individus = res.credit.hcpc$data.clust

plot_ly(individus, x=~AGE, y=~AMOUNT, z=~DURATION,color=~clust, mode="markers", type="scatter3d")
