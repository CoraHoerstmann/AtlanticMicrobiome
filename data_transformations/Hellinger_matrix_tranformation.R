#transform into matrix

list_ASVCounts$ASVCount_16S.tax.auto.hellinger <-as.matrix(sapply(list_ASVCounts$ASVCount_16S.tax.auto.hellinger, as.numeric))
list_ASVCounts$ASVCount_16S.tax.hellinger <-as.matrix(sapply(list_ASVCounts$ASVCount_16S.tax.hellinger, as.numeric))
list_ASVCounts$ASVCount_16S.tax.het.hellinger <-as.matrix(sapply(list_ASVCounts$ASVCount_16S.tax.het.hellinger, as.numeric))
list_ASVCounts$ASVCount_18S.tax.auto.hellinger <-as.matrix(sapply(list_ASVCounts$ASVCount_18S.tax.auto.hellinger, as.numeric))
list_ASVCounts$ASVCount_18S.tax.hellinger <-as.matrix(sapply(list_ASVCounts$ASVCount_16S.tax.hellinger, as.numeric))
list_ASVCounts$ASVCount_mixo.hellinger <-as.matrix(sapply(list_ASVCounts$ASVCount_mixo.hellinger, as.numeric))
list_ASVCounts$ASVCount_18S.tax.het.hellinger <-as.matrix(sapply(list_ASVCounts$ASVCount_18S.tax.het.hellinger, as.numeric))
#PP
list_ASVCounts$ASVCount_auto_euk.PP.hellinger <-as.matrix(sapply(list_ASVCounts$ASVCount_auto_euk.PP.hellinger, as.numeric))
list_ASVCounts$ASVCount_auto_cyano.PP.hellinger <-as.matrix(sapply(list_ASVCounts$ASVCount_auto_cyano.PP.hellinger, as.numeric))
list_ASVCounts$ASVCount_mixo.PP.hellinger <-as.matrix(sapply(list_ASVCounts$ASVCount_mixo.PP.hellinger, as.numeric)) 
list_ASVCounts$ASVCount_het_euk.PP.hellinger <-as.matrix(sapply(list_ASVCounts$ASVCount_het_euk.PP.hellinger, as.numeric))
list_ASVCounts$ASVCount_het_prok.PP.hellinger <-as.matrix(sapply(list_ASVCounts$ASVCount_het_prok.PP.hellinger, as.numeric))