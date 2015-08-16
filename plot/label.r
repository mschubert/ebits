filter = function(df, label="label", filter="pvalue", top=20) {
#    distx = df$effect - min(df$effect)
#    distx = distx / max(distx) - 0.5
#    disty = -log10(df$pvalue)
#    disty = disty - min(disty)
#    disty = disty / max(disty)
#    distsq = distx*distx + 2*disty*disty
#    ord = order(distsq, decreasing=T)
    ord = order(df[[filter]], decreasing=FALSE)

    df[['label']][ord[top+1:length(ord)]] = ""
    df
}

filter_p = function(pvalue, label=names(pvalue),
                    cutoff=0.05, showMin=3, sign=rep(T,length(pvalue))) {
    if (is.matrix(pvalue))
        label = array_elementnames(pvalue)

    if (sum(pvalue<cutoff) > showMin) {
        label[pvalue>0.05] = ""
        return(label)
    }
    else {        
#        n = length(pvalue)
#        top = pvalue<=sort(pvalue,partial=n-(showMin-1))[n-(showMin-1)]
#        bottom = pvalue<=sort(pvalue,decreasing=T,partial=n-(showMin-1))[n-(showMin-1)]
#        label[pvalue>0.05 & !top] = ""
        return(label)
    }
}
