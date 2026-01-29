plotter <- function(data, width=0.6, plotsize=16) {
  # set colors
  colors <- RColorBrewer::brewer.pal(8, "Set1")[c(3, 5, 8)]
  names(colors) <- c("0", "10", "100")

  plot_list <- lapply(data, function(dat) {
    gene <- unique(dat$gene)
    gen <- unique(dat$gen)
    extra_gene <- ifelse( "sign" %in% dat$sign, FALSE, TRUE )

    anno <- build_annotations(gene, gen, data, size_factor=0.1)

    if (all(is.na(dat$count))){
      p <- ggplot2::ggplot(dat, aes(diet, count, fill = treatment)) +
        ggplot2::theme_void(plotsize) +
        ggplot2::geom_text(aes(1.5,0, label = "N/A")) +
        ggplot2::scale_x_discrete(limits = c("normal", "high")) +
        ggplot2::labs(
          title = gen,
          fill = "DBP [mg/kg]",
          x = "Fat diet"
        ) +
        ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
        cowplot::background_grid()
    } else {
      p <- ggplot2::ggplot(dat, aes(diet, count, fill = treatment)) +
        ggplot2::stat_boxplot(
          geom = "errorbar",
          width = 0.2,
          position = position_dodge(width = width)) +
        ggplot2::geom_boxplot(
          outlier.shape = NA,
          coef = 0,
          width = 0.5,
          position = position_dodge(width = width),
          show.legend = FALSE) +
        ggplot2::geom_point(
          shape = 21, 
          size = 2.5,
          position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0, dodge.width = width)) +
        ggplot2::scale_fill_manual(values = colors) +
        { if (nrow(dat) == 3) ggplot2::scale_x_discrete(limits = c("normal", "high")) } +
        ggplot2::guides(
          color = "none",
          errorbar = "none",
          fill = ggplot2::guide_legend(
            override.aes = list(
              shape = 22,
              color = "black",
              size = 7
            )
          )
        ) +
        ggplot2::labs(
          title = gen,
          fill = "DBP [mg/kg]",
          x = "Fat diet",
          y = expression(log[2]("norm. counts"))
        ) +
        ggplot2::theme_linedraw(plotsize) +
        ggplot2::theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
        cowplot::background_grid()
    }

    if (!is.null(anno)){
      p <- p + scale_y_continuous(limits = c(NA, max(anno$y_max))) +
        geom_signif(
          data=anno,
          manual=TRUE,
          inherit.aes=FALSE,
          textsize = 6,
          vjust = 0.5,
          aes(
            xmin=start,
            xmax=end,
            annotations=label,
            y_position = y,
            group = group
          )
        )
    }

    return(p)
  })

  return(plot_list)
}
