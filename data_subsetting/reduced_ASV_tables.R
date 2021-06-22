#reduce the ASV tables to the matching sites

ASV_autoeuk_clr.red <- list_ASVCounts$ASVCount_18S.tax.auto.clr%>%dplyr::select(list_meta$meta_functions.z$Site)
ASV_autoeuk_ait.red <- list_ASVCounts$ASVCount_18S.tax.auto_ait%>%as.matrix()%>%as.data.frame()%>%dplyr::select(list_meta$meta_functions.z$Site)
ASV_autoprok_clr.red <- list_ASVCounts$ASVCount_16S.tax.auto.clr%>%dplyr::select(list_meta$meta_functions.z$Site)
ASV_autoprok_ait.red <- list_ASVCounts$ASVCount_16S.tax.auto_ait%>%as.matrix()%>%as.data.frame()%>%dplyr::select(list_meta$meta_functions.z$Site)
ASV_mixo_clr.red <- list_ASVCounts$ASVCount_mixo.clr%>%dplyr::select(list_meta$meta_functions.z$Site)
ASV_mixo_ait.red <- list_ASVCounts$ASVCount_18S.tax.mixo_ait%>%as.matrix()%>%as.data.frame()%>%dplyr::select(list_meta$meta_functions.z$Site)
ASV_heteuk_clr.red <- list_ASVCounts$ASVCount_18S.tax.het.clr%>%dplyr::select(list_meta$meta_functions.z$Site)
ASV_heteuk_ait.red <- list_ASVCounts$ASVCount_18S.tax.het_ait%>%as.matrix()%>%as.data.frame()%>%dplyr::select(list_meta$meta_functions.z$Site)
ASV_hetprok_clr.red <- list_ASVCounts$ASVCount_16S.tax.het.clr%>%dplyr::select(list_meta$meta_functions.z$Site)
ASV_hetprok_ait.red <- list_ASVCounts$ASVCount_16S.tax.het_ait%>%as.matrix()%>%as.data.frame()%>%dplyr::select(list_meta$meta_functions.z$Site)



