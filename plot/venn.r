# this script is able to convert lists of elements into venn diagrams
# 
# for instance, consider
# a = c('a', 'b', 'c')
# b = c('a', 'c')
# c = c('c')
# vennDiagramFromList(pr=list(a,b,c), categories=c("c1","c2","c3"))
#
# for now, it works with a 3-5 lists

vennDiagramFromList = function(pr, categories=names(pr), proportional=F) {
    require(VennDiagram)

    stopifnot(length(pr) >= 3 & length(pr) <= 5)
    stopifnot(length(pr) == length(categories))

    if (length(pr) >= 3) {
        n1 = setdiff(pr[[1]], unlist(pr[-1]))
        n2 = setdiff(pr[[1]], unlist(pr[-2]))
        n3 = setdiff(pr[[1]], unlist(pr[-3]))
        n12 = intersect(pr[[1]], pr[[2]])
        n13 = intersect(pr[[1]], pr[[3]])
        n23 = intersect(pr[[2]], pr[[3]])
        n123 = intersect(n12, pr[[3]])
    }

    if (length(pr) >= 4) {
        n14 = intersect(pr[[1]], pr[[4]])
        n24 = intersect(pr[[2]], pr[[4]])
        n34 = intersect(pr[[3]], pr[[4]])
        n124 = intersect(n12, pr[[4]])
        n134 = intersect(n13, pr[[4]])
        n234 = intersect(n23, pr[[4]])
        n1234 = intersect(n123, pr[[4]])
    }

    if (length(pr) == 5) {
        n15 = intersect(pr[[1]], pr[[5]])
        n25 = intersect(pr[[2]], pr[[5]])
        n35 = intersect(pr[[3]], pr[[5]])
        n45 = intersect(pr[[4]], pr[[5]])
        n125 = intersect(n12, pr[[5]])
        n135 = intersect(n13, pr[[5]])
        n145 = intersect(n14, pr[[5]])
        n235 = intersect(n23, pr[[5]])
        n245 = intersect(n24, pr[[5]])
        n345 = intersect(n34, pr[[5]])
        n1235 = intersect(n123, pr[[5]])
        n1245 = intersect(n124, pr[[5]])
        n1345 = intersect(n134, pr[[5]])
        n2345 = intersect(n234, pr[[5]])
        n12345 = intersect(n1234, pr[[5]])
    }

    if (length(pr) == 3) {
        if (proportional) {
            require(Vennerable)
            return(Venn(SetNames = categories, Weight=c(0, length(n1),
                                                 length(n2),
                                                 length(n3),
                                                 length(n12),
                                                 length(n13),
                                                 length(n23),
                                                 length(n123))))
#            require(venneuler)
#            venneuler(c(
#                a = length(n1),
#                b = length(n2),
#                c = length(n3),
#                'a&b' = length(n12),
#                'a&c' = length(n13),
#                'b&c' = length(n23),
#                'a&b&c' = length(n123)
#            ))
        } else {
            draw.triple.venn(
                area1 = length(pr[[1]]),
                area2 = length(pr[[2]]),
                area3 = length(pr[[3]]),
                n12 = length(n12),
                n23 = length(n23),
                n13 = length(n13),
                n123 = length(n123),
                category = categories,
                fill = c("blue", "red", "green"),
                lty = "blank",
                cex = 2,
                cat.cex = 2,
                cat.col = c("blue", "red", "green")
            )
        }
    }

    if (length(pr) == 4) {
        return (draw.quad.venn(
            area1 = length(pr[[1]]),
            area2 = length(pr[[2]]),
            area3 = length(pr[[3]]),
            area4 = length(pr[[4]]),
            n12 = length(n12),
            n13 = length(n13),
            n14 = length(n14),
            n23 = length(n23),
            n24 = length(n24),
            n34 = length(n34),
            n123 = length(n123),
            n124 = length(n124),
            n134 = length(n134),
            n234 = length(n234),
            n1234 = length(n1234),
            category = categories,
            fill = c("orange", "red", "green", "blue"),
            lty = "dashed",
            cex = 2,
            cat.cex = 2,
            cat.col = c("orange", "red", "green", "blue")
        ))
    }

    if (length(pr) == 5) {
        return (draw.quintuple.venn(
            area1 = length(pr[[1]]),
            area2 = length(pr[[2]]),
            area3 = length(pr[[3]]), 
            area4 = length(pr[[4]]),
            area5 = length(pr[[5]]), 
            n12 = length(n12), 
            n13 = length(n13), 
            n14 = length(n14), 
            n15 = length(n15), 
            n23 = length(n23), 
            n24 = length(n24), 
            n25 = length(n25),
            n34 = length(n34), 
            n35 = length(n35), 
            n45 = length(n45), 
            n123 = length(n123), 
            n124 = length(n124), 
            n125 = length(n125), 
            n134 = length(n134), 
            n135 = length(n135), 
            n145 = length(n145), 
            n234 = length(n234), 
            n235 = length(n235), 
            n245 = length(n245), 
            n345 = length(n345), 
            n1234 = length(n1234),
            n1235 = length(n1235), 
            n1245 = length(n1245), 
            n1345 = length(n1345), 
            n2345 = length(n2345), 
            n12345 = length(n12345),
            category = categories,
            fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
            cat.col = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
            cat.cex = 2,
            margin = 0.05,
            cex = c(1.5, 1.5, 1.5, 1.5, 1.5, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.8, 
            1, 0.55, 1, 0.55, 1, 0.55, 1, 0.55, 1, 0.55, 1, 1, 1, 1, 1, 1.5),
            ind = TRUE
        ))
    }
}

