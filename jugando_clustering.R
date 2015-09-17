###################################
##### hierarchical clustering
###################################

#Create data

ncontract_id<-c("123", "124", "126", "144", "150", "200")
units<-c("1", "4", "3", "1", "2", "2")
dif_tlds<-c("1", "4", "3", "1", "2", "2")
dif_slds<-c("1","4", "1", "1", "2", "1")


DF<-data.frame(ncontract_id, units, dif_tlds, dif_slds)

#function for later

myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
        ## modifiction of plclust for plotting hclust objects *in colour*!
        ## Copyright Eva KF Chan 2009
        ## Arguments:
        ##    hclust:    hclust object
        ##    lab:        a character vector of labels of the leaves of the tree
        ##    lab.col:    colour for the labels; NA=default device foreground colour
        ##    hang:     as in hclust & plclust
        ## Side effect:
        ##    A display of hierarchical cluster with coloured leaf labels.
        y <- rep(hclust$height,2)
        x <- as.numeric(hclust$merge)
        y <- y[which(x<0)]
        x <- x[which(x<0)]
        x <- abs(x)
        y <- y[order(x)]
        x <- x[order(x)]
        plot( hclust, labels=FALSE, hang=hang, ... )
        text( x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order], col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA, ... )
}

## Here we prepare the data and plot

distxy<-dist(DF)
hClustering<-hclust(distxy)

myplclust(hClustering, lab=rep(1:3, each=3), lab.col=rep(1:3, each=3))


## Here a new option of ploting with heatmap()

dataMatrix<-data.matrix(DF)[,2:4]

heatmap(dataMatrix)



