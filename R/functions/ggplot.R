##==============================================================================
## DEFINE CUSTOM GGPLOT FUNCTION
##==============================================================================
# cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
#                "#0072B2", "#D55E00", "#CC79A7")
ggplot <- function(...) {
    cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                   "#0072B2", "#D55E00", "#CC79A7")
    cbPalette <- rep(cbPalette, 2)
    ggplot2::ggplot(...) +
        theme_grey()+
        scale_fill_manual(values=cbPalette) +
        scale_colour_manual(values=cbPalette) +
        theme(plot.title = element_text(size = 15)) +
        theme(plot.title = element_text(hjust = 0.5))
}
