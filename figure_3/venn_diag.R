library(VennDiagram)
library(eulerr)

  # This is the four zone venn diagram, use this one
  # Note area 1,2,3,4 are not displayed in order
  # 1 -> 1
  # 2 -> 4
  # 3 -> 2
  # 4 -> 3
  # I hate this package, true torture
  venn.plot <- draw.quad.venn(
    area1 = 35,
    area2 = 184,
    area3 = 52,
    area4 = 115,
    n14 = 28,
    n12 = 33,
    n13 = 21,
    n24 = 110,
    n34 = 48,
    n23 = 50,
    n124 = 28,
    n134 = 21,
    n123 = 21,
    n234 = 48,
    n1234 = 21,
    category = c("Zone 1", "Zone 4" , "Zone 2", "Zone 3"),
    fill = c("goldenrod1", "azure4", "tomato", "deepskyblue3"),
    cat.fontfamily = rep("mono", 4),
    cat.fontface = rep("bold", 4),
    cat.just = list(c(0.5,-0.8),c(0.5,-0.8),c(0.6,-0.7),c(0.6,-0.7)),
#    cat.dist = c(0.28,0.28,0.18,0.18),
    fontfamily = rep("mono", 15),
    #fontface = rep("bold", 15),
    cex = 2,
    cat.cex = 2,
    cat.col = c("goldenrod1", "azure4", "tomato", "deepskyblue3")
  )

  