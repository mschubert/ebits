import_package('dplyr', attach=TRUE)

#' Retrieves CIN signatures
cin = function() {
    fname = file.path(module_file("cache", mustWork=TRUE), "cin.rds")
    if (file.exists(fname)) {
        sets = readRDS(fname)
    } else {
        warning("Creating cache, this may take a while", immediate.=TRUE)

        # https://static-content.springer.com/esm/art%3A10.1038%2Fng1861/MediaObjects/41588_2006_BFng1861_MOESM8_ESM.pdf
        carter2006_CIN70 = c("TPX2", "PRC1", "FOXM1", "CDC2", "TGIF2", "MCM2",
            "H2AFZ", "TOP2A", "PCNA", "UBE2C", "MELK", "TRIP13", "CNAP1",
            "MCM7", "RNASEH2A", "RAD51AP1", "KIF20A", "CDC45L", "MAD2L1",
            "ESPL1", "CCNB2", "FEN1", "TTK", "CCT5", "RFC4", "ATAD2", "ch-TOG",
            "NUP205", "CDC20", "CKS2", "RRM2", "ELAVL1", "CCNB1", "RRM1",
            "AURKB", "MSH6", "EZH2", "CTPS", "DKC1", "OIP5", "CDCA8", "PTTG1",
            "CEP55", "H2AFX", "CMAS", "BRRN1", "MCM10", "LSM4", "MTB", "ASF1B",
            "ZWINT", "TOPK", "FLJ10036", "CDCA3", "ECT2", "CDC6", "UNG",
            "MTCH2", "RAD21", "ACTL6A", "GPIandMGC13096", "SFRS2", "HDGF",
            "NXT1", "NEK2", "DHCR7", "STK6", "NDUFAB1", "KIAA0286", "KIF4A")

        # https://cancerres.aacrjournals.org/highwire/filestream/290223/field_highwire_adjunct_files/9/tab1-19.xlsx
        sheltzer = file.path(module_file("data"), "tab1-19.xlsx")
        pcna = readxl::read_xlsx(sheltzer, sheet="Table S15")[[1]][-1]
        tri70 = readxl::read_xlsx(sheltzer, sheet="Table S16")[[1]][-1]
        het70 = readxl::read_xlsx(sheltzer, sheet="Table S17")[[1]][-1]

        # https://static-content.springer.com/esm/art%3A10.1038%2Fnature25432/MediaObjects/41586_2018_BFnature25432_MOESM3_ESM.xlsx
        bakhoum = file.path(module_file("data", "41586_2018_BFnature25432_MOESM3_ESM.xlsx"))
        bak = sapply(readxl::excel_sheets(bakhoum), readxl::read_xlsx, path=bakhoum, simplify=FALSE)

        bucc = file.path(module_file("data"), "Buccitelli_Supplemental_Data_3")

        sets = list(
#            CIN4 = ,
#            CIN25 = ,
            CIN70_Carter2006 = carter2006_CIN70,
            HET70 = het70,
            PCNA = pcna,
            TRI70_pos = tri70[1:20],
            TRI70_neg = tri70[23:71],
            Bakhoum2018_CIN = bak[["CIN-Signature"]][[1]],
            Bakhoum2018_NC_NFkB = bak[["NC-NFkB-Targets"]][[1]],
            Bakhoum2018_NC_NFkB_pos = bak[["NC-NFkB-Targets"]][[1]][1:13],
            Bakhoum2018_NC_NFkB_neg = bak[["NC-NFkB-Targets"]][[1]][14:18],
            Bakhoum2018_NC_NFkB_reg_pos = toupper(bak[["NC-NFkB-Regulators"]][[1]]),
            Bakhoum2018_NC_NFkB_reg_pos = toupper(bak[["NC-NFkB-Regulators"]][[1]][1:3]),
            Bakhoum2018_NC_NFkB_reg_neg = bak[["NC-NFkB-Regulators"]][[1]][4:7],
            Bakhoum2018_NFkB_reg = toupper(bak[["NFkB-Regulators"]][[1]]),
            Bakhoum2018_IFN_reg = bak[["Interferon-Regulators"]][[1]],
            Bakhoum2018_EMT = bak[["EMT-Genes"]][[1]],
            Bakhoum2018_Inflammation = bak[["Inflammation-Genes"]][[1]],
            Bakhoum2018_Migration = bak[["MigrationMotility-Genes"]][[1]],
            Buccitelli_up = read.table(file.path(bucc, "concensus_genes", "up_consensus.txt"))$V1,
            Buccitelli_down = read.table(file.path(bucc, "concensus_genes", "down_consensus.txt"))$V1
        )

        bt549 = file.path(module_file("data"), "mad2pb_bt549_stat1de.rds")
        bt549_sets = lapply(readRDS(bt549), function(x) head(x$gene_name, 70))
        sets = c(sets, bt549_sets)

        saveRDS(sets, file=fname)
    }

    sets
}

if (is.null(module_name())) {
    cache = cin()
}
