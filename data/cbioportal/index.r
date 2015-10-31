library(dplyr)
library(tidyr)
library(cgdsr)
.omit = import('../../base/omit')
.p = import('../path')
.mycgds = cgdsr::CGDS("http://www.cbioportal.org/public-portal/")

studies = function(tcga=NULL, exclude=NULL) {
    if (is.null(tcga))
        re = cgdsr::getCancerStudies(.mycgds)[,1]
    else
        re = grep("tcga", cgdsr::getCancerStudies(.mycgds)[,1], value=TRUE, invert=!tcga)
    re[! re %in% exclude]
}

samples = function(studies) {
    samples_by_study = function(study) {
        data_avail = cgdsr::getCaseLists(.mycgds, study) %>%
            mutate(case_ids = strsplit(case_ids, "\\s+")) %>%
            tidyr::unnest(case_ids) %>%
            transmute(id = case_ids,
                      study2 = study,
                      avail = sub(paste0("^", study, "_"), "", case_list_id)) %>%
            tidyr::spread(avail, avail) %>%
            rename(study = study2)

        for (col in 3:ncol(data_avail))
            data_avail[[col]] = !is.na(data_avail[[col]])

        data_avail
    }

    lapply(studies, samples_by_study) %>%
        bind_rows()
}

profile = function(profile, study) {
    profile_name = list(
        mutation = "Mutations",
        cna = "Putative copy-number alterations from GISTIC",
        rna_seq = "mRNA expression (RNA Seq V2 RSEM)"
    )
    profile_name = profile_name[profile]
    if (is.na(profile_name))
        stop("invalid profile")

    profile_id = cgdsr::getGeneticProfiles(.mycgds, study) %>%
        filter(genetic_profile_name == profile_name) %>%
        select(genetic_profile_id) %>%
        unlist()
    if (length(profile_id) == 0) {
        warning("No ", profile_name, " for: ", study)
        profile_id = NA
    }
    profile_id
}

variants = function(studies, genes) {
    study2variants = function(study) {
        p = profile("mutation", study)
        if (is.na(p))
            return(NULL)

        # _allele is sometimes logical, don't know why
        cgdsr::getMutationData(.mycgds,
            caseList = study,
            geneticProfile = p,
            genes = genes) %>%
            transmute(id = case_id,
                      study = study,
                      hgnc = gene_symbol,
                      validation_status = validation_status,
                      mutation_status = mutation_status,
                      mutation_type = mutation_type,
                      amino_acid_change = amino_acid_change,
                      functional_impact_score = functional_impact_score,
                      chr = chr,
                      start_position = start_position,
                      end_position = end_position,
                      reference_allele = as.character(reference_allele),
                      variant_allele = as.character(variant_allele))
    }
    
    lapply(studies, study2variants) %>% bind_rows()
}

rna = function(studies, genes, na.rm=TRUE) {
    study2rna = function(study) {
        p = profile("rna_seq", study)
        if (is.na(p))
            return(NULL)

        cgdsr::getProfileData(.mycgds,
            caseList = paste0(study, "_all"),
            geneticProfiles = p,
            genes = genes) %>%
            cbind(id = rownames(.), .) %>%
            mutate(id = gsub("\\.", "-", id)) %>% # invalid if not TCGA?
            .omit$na(omit=na.rm)
    }

    lapply(studies, study2rna) %>% bind_rows()
}

cna = function(studies, genes, na.rm=TRUE, only_altered=FALSE) {
    study2cna = function(study) {
        p = profile("cna", study)
        if (is.na(p))
            return(NULL)

        re = cgdsr::getProfileData(.mycgds,
            caseList = paste0(study, "_all"),
            geneticProfiles = p,
            genes = genes) %>%
            cbind(id = rownames(.), .) %>%
            mutate(id = gsub("\\.", "-", id)) %>% # invalid if not TCGA?
            .omit$na(omit = na.rm)

        if (only_altered)
            re = re[apply(select(re, -id), 1, function(x) any(x != 0)),]

        re
    }

    lapply(studies, study2cna) %>% bind_rows()
}

if (is.null(module_name())) {
    studies = studies(tcga=TRUE, exclude="pcpg_tcga")
    # pcpg_tcga has 2 samples, reference allele = "TRUE" (just drop it)

    variants = variants(studies, "TP53")
    cna = cna(studies, "PIK3CA")
    rnaseq = rna(studies, "KMT2D") # will replace "MLL2"

    testthat::expect_gte(nrow(variants), 4183)
    testthat::expect_equal(ncol(variants), 13)
    testthat::expect_equal(unique(variants$hgnc), "TP53")
    testthat::expect_gte(length(unique(variants$id)), 2160)

    testthat::expect_gte(nrow(cna), 15865)
    testthat::expect_equal(sapply(cna, class),
        setNames(c("character", "numeric"), c("id", "PIK3CA")))
    testthat::expect_gte(length(unique(cna$id)), 10200)
    
    testthat::expect_gte(nrow(rnaseq), 12490)
    testthat::expect_equal(sapply(rnaseq, class),
        setNames(c("character", "numeric"), c("id", "KMT2D")))
    testthat::expect_gte(length(unique(rnaseq$id)), 9076)
}
