
# Figures

library(mixchar)
source('my_functions.R')

deriv_juncus <- process(juncus, init_mass = 18.96,
               temp = 'temp_C', mass_loss = 'mass_loss')

output_juncus <- deconvolve(deriv_juncus)

png('figs/mass_loss.png')
plot(deriv_juncus, plot_type = 'mass', cex = 1.5)
dev.off()

png('figs/dtg.png')
dtg_plot(juncus)
dev.off()

png('figs/simulation.png', height = 600, width = 600)
simulate_fraser_suzuki()
dev.off()

png('figs/default_tests.png', height = 600, width = 650)
test_plots()
dev.off()

png('figs/eradiata.png', height = 400, width = 750)
e.rad()
dev.off()

png('figs/deconvolved.png')
plot(output_juncus, bw = FALSE)
dev.off()

knitr::knit('ms/manuscript.Rnw', 'ms/manuscript.tex')
tinytex::pdflatex('ms/manuscript.tex', 
                  pdf_file = 'ms/manuscript.pdf')

