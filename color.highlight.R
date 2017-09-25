# color.highlight
#
# Function for highlighting a given set of observations
# in a scatter plot.
#
# INPUT:  group      a grouping factor                    
#         selection  a logical vector indicating which    
#                    observations should be highlighted   
#         colors     an optional vector of desired colors 
#         alpha      alpha parameter for non-highlighted  
#                    data points
#                                                         
# (c) Lasse Ruokolainen 2017                              
##########################################################

color.highlight = function(group, selection, colors=NULL, alpha=0.5){
	if(is.null(colors)){
		cc = c('dodgerblue','red3','olivedrab','orange2','slateblue')		
	}else{
		cc = colors
	}
	
	cols = group
	levels(cols) = cc[nlevels(group)]
	cols = as.character(cols)

	for(ii in 1:length(cols)){
		tmp = col2rgb(cols[ii],alpha=T)/255
		alp = ifelse(w[ii],1,alpha)
		cols[ii] = rgb(tmp[1],tmp[2],tmp[3],alpha=tmp[4]/alp)
	}

	return(cols)
}