library(ggplot2)
library(gggenes)
library(ggfittext)
rm(list=ls())


data<-read.csv('ARG_MGE_saline_contig.txt',head=TRUE,sep = '\t')



p1<-ggplot(data, aes(xmin = start, xmax = end, y = contig, fill = type,forward = orientation == 1,label = gene)) + 
  
  geom_gene_arrow(arrowhead_height = unit(3, "mm"), arrowhead_width = unit(1, "mm")) +
  geom_gene_label(align = "left") +
  facet_wrap(~ contig, scales = "free", ncol = 1) + 
  scale_fill_manual(
    values = c(
      ARG   = "#D55E00",
      transposase   = "#9BBB59",
      integrase   = "#8064A2"
      
    )
  )+
  geom_gene_label(
    aes(label = gene),
    size = 3,
    family = "Arial",
    fontface = "italic"   # 可选：italic / bold
  )+
  theme_genes()

p1


n_contig <- length(unique(data$contig))

ggsave(
    "ARG_MGE_saline.pdf",
     plot = p1,
     width = 8,
     height = max(5, n_contig * 0.4),
     limitsize = FALSE
   )


