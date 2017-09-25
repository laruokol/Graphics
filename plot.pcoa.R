# plot.pcoa
#
# Function for plotting a PCoA analysis.
#
# INPUT    ord     ordination result object. Can be either a monoMDS-object,
#                  a list-object (a cmdscale object with eigenvalues), 
#                  or a matrix.
#          group   an optional grouping factor.
#          choises if more than 2, which axes should be plotted?
#          cols    optional set of colors to be used for plotting 
#                  (argument 'col' can be used to set different color for
#                   point border).
#          legend  should a legend be drawn? (default = TRUE)
#          ellipse should a concnetration ellipse be drawn? (default = TRUE)
#          ci      if ellipse is drawn, what is the concentration treshold?
#                  (default = 0.75, i.e. 75%)
#          spider  should a spider be drawn to connect points to group 
#                  centroids? (default = TRUE)
#          cluster an optional clustering result. 
#          type    plotting type (default = 'p'). Setting type = 'n' 
#                  omits data points.
#          labels  should group labels be added? (default = TRUE)
#          lab.type  type of group labels. Either 'ellipse' or 'box'.
#          lab.cex  character expansion for group labels.
#          ...     additional arguments passed to plot(). 
#
# (c) Lasse Ruokolainen, 2016
############################################################################

plot.pcoa = function(ord,group=NULL,choises=c(1,2),ci=0.75,cols=NULL,legend=TRUE,
				    ellipse=TRUE,spider=TRUE,cluster=NULL,pch=21,
				    col=NULL,lwd.ellipse=NULL,type='p',
				    labels=TRUE,lab.type='ellipse',lab.cex=NULL,...){
	
	# Test ordination method:
	if(class(ord) == 'monoMDS'){
		Y = ord$points[,choises]
	}else{
		if(class(ord) == 'list'){
			cont = signif(ord$eig[ord$eig>0][choises]/
						  sum(ord$eig[ord$eig>0])*100,2)
			tmp = ord$points[,choises]			  
			colnames(tmp) = c(paste('PCoA-',choises[1],' (',as.character(cont[1]),'%)',sep=''),
									 paste('PCoA-',choises[2],' (',as.character(cont[2]),'%)',sep=''))
			Y = tmp					 
		}else{
			Y = ord[,choises]
		}
	}
  
	if(is.null(cols)){
		defc = c('lightblue','red3','gold','olivedrab','purple',
				 'royalblue','firebrick','tan','springgreen')
		}else{
			defc = cols
	}
	bg.col = as.character(as.numeric(group));
	k=0; for(ii in sort(unique(bg.col))){k=k+1; bg.col[bg.col==ii] = defc[k]}
	if(is.null(col)) col = rep(1,nlevels(group))
	ed.col = as.character(as.numeric(group))
	k=0; for(ii in sort(unique(ed.col))){k=k+1; ed.col[ed.col==ii] = col[k]}
	
	plot(Y,bg=bg.col,col=ed.col,pch=pch,type='n',...)
	if(!is.null(group)){
	    for(ii in levels(group)){
	    	tmp = bg.col[group==ii]
	    	if(spider==T){
				ordispider(Y[group==ii,],
						   rep(1,sum(group==ii)),col=tmp,lwd=.5)
			}
	    	if(ellipse==T){			
				ordiellipse(Y[group==ii,],
							rep(1,sum(group==ii)),col=tmp,
							draw='polygon',alpha=25,conf=ci,border=tmp,lwd=lwd.ellipse)
			}		
    		}
	}
	if(!is.null(cluster)){
		ordicluster(Y,cluster,col='gray80')
	}		
	if(type=='n'){
		cntr = cbind(tapply(Y[,1],group,'mean'),tapply(Y[,2],group,'mean'))
		points(cntr,col=defc,pch=16,...)		
	}else{
		points(Y,bg=bg.col,col=ed.col,pch=pch,...)
	}
	
	# Add group identification:
	if(labels==TRUE){
		library(plotrix)
		library(dplyr)
		tmp = Y; colnames(tmp) = c('X1','X2')
		s = as.tbl(data.frame(tmp,group)) %>% group_by(group) %>% summarize(mu1=mean(X1),mu2=mean(X2))
		s = as.matrix(s[,2:3])
		nam = levels(group)
		if(is.null(lab.cex)) lab.cex = 1.5
		if(lab.type=='box'){
			op=par(lwd=2)
			boxed.labels(s[,1],s[,2],nam,col=defc[1:nlevels(group)],
				  border=defc[1:nlevels(group)],cex=lab.cex,
				  ypad=1.8,xpad=1.2)
			par(op)
		}else{
			if(lab.type=='ellipse'){
				for(ii in 1:nrow(s)){					
					draw.ellipse(s[ii,1],s[ii,2],b=.05*max(Y[,2])*lab.cex,
					a=lab.cex*nchar(nam[ii])/max(Y[,1])/200,col=defc[ii],border=F)
					text(as.matrix(s[ii,1]),as.matrix(s[ii,2]),nam[ii],
					font=2,col='white',cex=lab.cex)	
				}				
			}
		}
	
	}
	if(legend==TRUE){
		legend('topleft',legend=levels(group),ncol=floor(nlevels(group)/2),pch=21,pt.bg=defc)
	}
}
