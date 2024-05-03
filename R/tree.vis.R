#' @title tree.vis
#' @description This function creates a simple phylogenetic tree, complete with branch length. It relies on the K80 model of distance
#' @param x - data
#' @keywords - DNA
#' @import ape
#' @import ggplot2
#' @import ggtree
#' @export

tree.vis <- function(x) {
  xbin <- as.DNAbin(x)
  dnbin<-dist.dna(xbin, model = "K80")
  tree<-nj(dnbin)
  ggt <- ggtree(tree, cex = 0.8, aes(color = branch.length)) +
    scale_color_continuous(high = "skyblue",low = "black") + #this colors the scale of the branches
    geom_tiplab(align = TRUE, size = 5) + #changes size and alignment of branches
    theme(legend.position = "right") + #positions legend
    geom_treescale(y = -5, color = "red", fontsize = 3) #adds scale for tree at bottom
  msaplot(ggt, xbin, offset = 0.25, width = 0.75, height = 0.5) #Offset, width, and height adjusts the sequence illustration to right of the tree
  color = c(rep("rosybrown", 1), rep("sienna1", 1),
            rep("lightgoldenrod1", 1), rep("lightskyblue1", 1), rep("palegreen", 1))
  ggt
}
