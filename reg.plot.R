# reg.plot
#
# Function for plotting regression results
#
# INPUT    x       the independent variable
#          y       the dependent variable
#          group   optional grouping factor
#          SE      should confidence intervals be plotted? 
#                  (default = TRUE)
#          col     colors to be used for plotting (optional)
#          leg.pos legend position (default = top-left)
#          ...     any graphical parameters that can be sent 
#                  to plot(...)
#
# (c) Lasse Ruokolainen, 2017
#         last modified -- January 2018
#############################################################

reg.plot = function(x,y,group=NULL,SE=T,col=NULL,pch=21,leg.pos='topleft',
                    leg.title=NULL,...){

	if(is.null(group)) group = factor(rep('1',length(x)))
	if(is.null(col)){
		col = c("#56B4E9","#E69F00","#009E73",
				"#0072B2", "#D55E00","#CC79A7")[1:nlevels(group)]
	}	
	gcol = group; levels(gcol) = col; gcol = as.character(gcol)
	
	# regression:
	if(nlevels(group)>1){
		m = lm(y~x*group)
	}else{
		m = lm(y~x)
	}	

	# generate prediction data:
	newx = numeric(0)
	for(ii in levels(group)){
		w = which(group==ii)
		newx = c(newx, seq(min(x[w]), max(x[w]), length.out=20))
	}
	newD = data.frame(x=newx,group=rep(levels(group),each=20))

	# predicts + interval:
	preds = predict(m, newdata = newD, interval = 'confidence')
	
	# plot points:
	plot(x,y,pch=pch,bg=gcol,...)
	
	# plot regression lines + CI:
	k=0
	for(ii in levels(group)){
		k=k+1
		w = which(newD$group==ii)
		if(SE==T){
			tmp = col2rgb(col[k]); tmp = tmp/max(tmp)
			tmp = rgb(tmp[1],tmp[2],tmp[3],.2)
			polygon(c(rev(newD$x[w]), newD$x[w]), 
			c(rev(preds[w,3]), preds[w,2]), 
			col = tmp, border = NA)	
		}
		
		lines(newD$x[w],preds[w,'fit'],col=col[k],lwd=2)
	}
	
	legend(leg.pos,legend=levels(group),text.font=3,bty='n',
		   pch=pch,pt.bg=col,col=col,lwd=2,x.intersp=.8,title=leg.title)
}
