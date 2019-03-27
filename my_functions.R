
# Process test data

process_test_samples <- function (file) {

  # this is the line you're already familiar with that reads in your data
  df <- read.table(file, fill = TRUE, header = FALSE, skip = 46,
                   skipNul = TRUE, fileEncoding="UTF-16LE")

  # assigns column names
  colnames(df) <- c("time", "temp", "weight", "deriv")

  # It looks like the initial mass is stored on line 12, so this line just
  # reads that so it can be passed into the next function automatically
  init_mass <- read.table(file, nrows = 1, fill = TRUE, header = FALSE, skip = 11,
                          skipNul = TRUE, fileEncoding="UTF-16LE")[,2]

  # process data given the initial mass, and column names given
  tmp <- mixchar::process(df, init_mass = init_mass,
                          temp = 'temp',
                          mass = 'weight')

  # return this object
  tmp
}

# Figures

legend_subfig <- function (subfig, cex) {
  legend(170, .01, legend = paste0('(', subfig, ')'), bty = 'n', cex = cex)
}

decon_plot <- function (x, bw = TRUE, yaxt = 's') {

  temp <- seq(x$temp_bounds[1], x$temp_bounds[2], length.out = nrow(x$data))
  data <- x$data
  fit <- x$model_fit

  params <- as.data.frame(summary(fit)$coefficients[,1])

  par(mar = c(5, 5, 1, 1))
  plot(data$temp_C, data$deriv, xlab = '',
       ylab = '', yaxt = yaxt,
       yaxs = 'i', ylim = c(0, max(data$deriv) + 0.06*max(data$deriv)),
       pch = 20, cex = 0.5, cex.axis = 1.2, cex.lab = 1.5)

  if (isTRUE(bw)) {

    if (x$n_peaks == 4) {
      y1 <- fs_mixture(temp = temp,
                       height_1 = params['height_1',],
                       skew_1 = params['skew_1',],
                       position_1 = params['position_1',],
                       width_1 = params['width_1',],
                       height_2 = params['height_2',],
                       skew_2 = params['skew_2',],
                       position_2 = params['position_2',],
                       width_2 = params['width_2',],
                       height_3 = params['height_3',],
                       skew_3 = params['skew_3',],
                       position_3 = params['position_3',],
                       width_3 = params['width_3',],
                       height_0 = params['height_0',],
                       skew_0 = params['skew_0',],
                       position_0 = params['position_0',],
                       width_0 = params['width_0',])

      y5 <- fs_function(temp,
                        params['height_0',], params['skew_0',],
                        params['position_0',], params['width_0',])
      lines(temp, y5, lty = 5, lwd = 2)

      legend('topright',
             legend = c('DTG data', 'DTG modelled',
                        'Hemicellulose 1', 'Hemicellulose 2',
                        'Cellulose', 'Lignin'),
             ncol = 1,
             cex = 1.2,
             bty = 'n',
             lty = c(NA, 1, 6, 3, 4, 5),
             pch = c(20, NA, NA, NA, NA, NA),
             lwd = 2)

    }

    if (x$n_peaks == 3) {

      y1 <- fs_mixture(temp = temp,
                       height_1 = params['height_1',],
                       skew_1 = params['skew_1',],
                       position_1 = params['position_1',],
                       width_1 = params['width_1',],
                       height_2 = params['height_2',],
                       skew_2 = params['skew_2',],
                       position_2 = params['position_2',],
                       width_2 = params['width_2',],
                       height_3 = params['height_3',],
                       skew_3 = params['skew_3',],
                       position_3 = params['position_3',],
                       width_3 = params['width_3',])

      legend('topright',
             legend = c('DTG data', 'DTG modelled',
                        'Hemicellulose', 'Cellulose', 'Lignin'),
             ncol = 1,
             cex = 1.2,
             bty = 'n',
             lty = c(NA, 1, 3, 4, 5),
             pch = c(20, NA, NA, NA, NA),
             lwd = 2)

    }

    y2 <- fs_function(temp,
                      params['height_1',], params['skew_1',],
                      params['position_1',], params['width_1',])
    y3 <- fs_function(temp,
                      params['height_2',], params['skew_2',],
                      params['position_2',], params['width_2',])
    y4 <- fs_function(temp,
                      params['height_3',], params['skew_3',],
                      params['position_3',], params['width_3',])

    lines(temp, y1, lty = 1, lwd = 1.7)
    lines(temp, y2, lty = 3, lwd = 2)
    lines(temp, y3, lty = 4, lwd = 2)
    lines(temp, y4, lty = 5, lwd = 2)

  }

  if(!isTRUE(bw)) {

    if (x$n_peaks == 4) {
      y1 <- fs_mixture(temp = temp,
                       height_1 = params['height_1',],
                       skew_1 = params['skew_1',],
                       position_1 = params['position_1',],
                       width_1 = params['width_1',],
                       height_2 = params['height_2',],
                       skew_2 = params['skew_2',],
                       position_2 = params['position_2',],
                       width_2 = params['width_2',],
                       height_3 = params['height_3',],
                       skew_3 = params['skew_3',],
                       position_3 = params['position_3',],
                       width_3 = params['width_3',],
                       height_0 = params['height_0',],
                       skew_0 = params['skew_0',],
                       position_0 = params['position_0',],
                       width_0 = params['width_0',])

      y5 <- fs_function(temp,
                        params['height_0',], params['skew_0',],
                        params['position_0',], params['width_0',])

      lines(temp, y5, lty = 6, lwd = 2.5, col = '#33638DFF')

      legend('topright',
             legend = c('DTG data', 'DTG modelled',
                        'Hemicellulose 1', 'Hemicellulose 2',
                        'Cellulose', 'Lignin'),
             ncol = 1,
             cex = 1.2,
             bty = 'n',
             lty = c(NA, 1, 6, 3, 4, 5),
             pch = c(20, NA, NA, NA, NA, NA),
             col = c('black', 'black', '#33638DFF',
                     '#440154FF', '#B8DE29FF', '#3CBB75FF'),
             lwd = 2)

    }

    if (x$n_peaks == 3) {

      y1 <- fs_mixture(temp = temp,
                       height_1 = params['height_1',],
                       skew_1 = params['skew_1',],
                       position_1 = params['position_1',],
                       width_1 = params['width_1',],
                       height_2 = params['height_2',],
                       skew_2 = params['skew_2',],
                       position_2 = params['position_2',],
                       width_2 = params['width_2',],
                       height_3 = params['height_3',],
                       skew_3 = params['skew_3',],
                       position_3 = params['position_3',],
                       width_3 = params['width_3',])

      legend('topright',
             legend = c('DTG data', 'DTG modelled',
                        'Hemicellulose', 'Cellulose', 'Lignin'),
             ncol = 1,
             cex = 1.2,
             bty = 'n',
             lty = c(NA, 1, 3, 4, 5),
             pch = c(20, NA, NA, NA, NA),
             col = c('black', 'black', '#440154FF',
                     '#B8DE29FF', '#3CBB75FF'),
             lwd = 2)

    }

    y2 <- fs_function(temp,
                      params['height_1',], params['skew_1',],
                      params['position_1',], params['width_1',])
    y3 <- fs_function(temp,
                      params['height_2',], params['skew_2',],
                      params['position_2',], params['width_2',])
    y4 <- fs_function(temp,
                      params['height_3',], params['skew_3',],
                      params['position_3',], params['width_3',])

    lines(temp, y1, lty = 1, lwd = 2)
    lines(temp, y2, lty = 3, lwd = 3.5, col = '#440154FF')
    lines(temp, y3, lty = 4, lwd = 3.5, col = '#B8DE29FF')
    lines(temp, y4, lty = 5, lwd = 3.5, col = '#3CBB75FF')

}}

dtg_plot <- function (df) {

  deriv_juncus <- process(juncus,
                          init_mass = 18.96,        # initial mass of sample
                          temp = 'temp_C',          # temperature data column name
                          mass_loss = 'mass_loss',  # mass loss data column name
                          temp_units = 'C')         # 'C' is the default setting
  deriv_juncus

  plot(deriv_juncus, plot_type = 'rate', cex = 1.5)
  segments(x0 = 40, y0 = 0.0018, x1 = 40, y1 = 0.002, lwd = 3, col = 'darkgrey')
  segments(x0 = 40, y0 = 0.002, x1 = 120, y1 = 0.002, lwd = 3, col = 'darkgrey')
  segments(x0 = 120, y0 = 0.0018, x1 = 120, y1 = 0.002, lwd = 3, col = 'darkgrey')
  text(x = ((120-40)/2+40), y = 0.0024, '1', cex = 2, col = 'darkgrey')

  segments(x0 = 120, y0 = 0.0078, x1 = 120, y1 = 0.008, lwd = 3, col = 'darkgrey')
  segments(x0 = 120, y0 = 0.008, x1 = 650, y1 = 0.008, lwd = 3, col = 'darkgrey')
  segments(x0 = 650, y0 = 0.0078, x1 = 650, y1 = 0.008, lwd = 3, col = 'darkgrey')
  text(x = 400, y = 0.0076, '2', cex = 2, col = 'darkgrey')

  segments(x0 = 650, y0 = 0.0008, x1 = 650, y1 = 0.001, lwd = 3, col = 'darkgrey')
  segments(x0 = 650, y0 = 0.001, x1 = 790, y1 = 0.001, lwd = 3, col = 'darkgrey')
  segments(x0 = 790, y0 = 0.0008, x1 = 790, y1 = 0.001, lwd = 3, col = 'darkgrey')
  text(x = ((790-650)/2+650), y = 0.0014, '3', cex = 2, col = 'darkgrey')

}

simulate_fraser_suzuki <- function () {

  x <- seq(200, 700)
  h1 <- mixchar::fs_function(x, 0.004, -0.25, 400, 60)
  h2 <- mixchar::fs_function(x, 0.006, -0.25, 400, 60)
  h3 <- mixchar::fs_function(x, 0.008, -0.25, 400, 60)
  h4 <- mixchar::fs_function(x, 0.010, -0.25, 400, 60)

  s1 <- mixchar::fs_function(x, 0.010, -0.55, 400, 60)
  s2 <- mixchar::fs_function(x, 0.010, -0.25, 400, 60)
  s3 <- mixchar::fs_function(x, 0.010, 0.25, 400, 60)
  s4 <- mixchar::fs_function(x, 0.010, 0.55, 400, 60)

  p1 <- mixchar::fs_function(x, 0.010, -0.25, 350, 60)
  p2 <- mixchar::fs_function(x, 0.010, -0.25, 400, 60)
  p3 <- mixchar::fs_function(x, 0.010, -0.25, 450, 60)
  p4 <- mixchar::fs_function(x, 0.010, -0.25, 500, 60)

  w1 <- mixchar::fs_function(x, 0.010, -0.25, 400, 30)
  w2 <- mixchar::fs_function(x, 0.010, -0.25, 400, 60)
  w3 <- mixchar::fs_function(x, 0.010, -0.25, 400, 90)
  w4 <- mixchar::fs_function(x, 0.010, -0.25, 400, 120)

  par(oma = c(4, 3, 0, 1), mar = c(1, 2, 1, 0), mfrow = c(2, 2))

  plot(x, h4, type = 'l', lty = 4, xaxt = 'n', yaxt = 'n', cex = 1.6,
       xlab = '',
       ylab = '')
  axis(side = 2, at = c(0, 0.005, 0.010), cex.axis = 1.2,
       labels = c(0, 0.005, 0.010))
  lines(x, h2, lty = 2)
  lines(x, h3, lty = 3)
  lines(x, h1, lty = 1)
  legend_subfig('a', 1.5)
  legend('topright', legend = c(expression(paste('h = 0.004 C'^'-1')),
                                expression(paste('h = 0.006 C'^'-1')),
                                expression(paste('h = 0.008 C'^'-1')),
                                expression(paste('h = 0.010 C'^'-1')),
                                's = -0.25',
                                'p = 400 C',
                                'w = 60 C'),
         bty = 'n', cex = 1,
         lty = c(1, 2, 3, 4, NA, NA, NA)
  )

  plot(x, s1, type = 'l', lty = 1, xaxt = 'n', yaxt = 'n', cex = 1.6,
       xlab = '',
       ylab = '')
  lines(x, s2, lty = 2)
  lines(x, s3, lty = 3)
  lines(x, s4, lty = 4)
  legend_subfig('b', 1.5)
  legend('topright', legend = c(expression(paste('h = 0.010 C'^'-1')),
                                's = -0.5',
                                's = -0.25',
                                's = 0.25',
                                's = 0.55',
                                'p = 400 C',
                                'w = 60 C'),
         bty = 'n', cex = 1,
         lty = c(NA, 1, 2, 3, 4, NA, NA)
  )

  plot(x, p1, type = 'l', lty = 1, xaxt = 'n', yaxt = 'n', cex = 1.6,
       xlab = '',
       ylab = '')
  axis(side = 1, at = c(200, 325, 450, 575, 700), cex.axis = 1.2,
       labels = c(200, 325, 450, 575, 700))
  axis(side = 2, at = c(0, 0.005, 0.010), cex.axis = 1.2,
       labels = c(0, 0.005, 0.010))
  lines(x, p2, lty = 2)
  lines(x, p3, lty = 3)
  lines(x, p4, lty = 4)
  legend_subfig('c', 1.5)
  legend('topright', legend = c(expression(paste('h = 0.010 C'^'-1')),
                                's = -0.25',
                                'p = 350 C',
                                'p = 400 C',
                                'p = 450 C',
                                'p = 500 C',
                                'w = 60 C'),
         bty = 'n', cex = 1,
         lty = c(NA, NA, 1, 2, 3, 4, NA)
  )

  plot(x, w1, type = 'l', lty = 1, xaxt = 'n', yaxt = 'n', cex = 1.6,
       xlab = '',
       ylab = '')
  axis(side = 1, at = c(200, 325, 450, 575, 700), cex.axis = 1.2,
       labels = c(200, 325, 450, 575, 700))
  lines(x, w2, lty = 2)
  lines(x, w3, lty = 3)
  lines(x, w4, lty = 4)
  legend_subfig('d', 1.5)
  legend('topright', legend = c(expression(paste('h = 0.010 C'^'-1')),
                                's = -0.25',
                                'p = 400 C',
                                'w = 30 C',
                                'w = 60 C',
                                'w = 90 C',
                                'w = 120'),
         bty = 'n', cex = 1,
         lty = c(NA, NA, NA, 1, 2, 3, 4))

  mtext(text = 'Temperature (C)',
        side = 1,
        line = 2.1,
        outer = TRUE,
        cex = 1.5)
  mtext(text = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')),
        side = 2,
        line = 0.9,
        outer = TRUE,
        cex = 1.5)
}

deconvolve_plot <- function (x) {

  temp <- seq(x$temp_bounds[1], x$temp_bounds[2], length.out = nrow(x$data))
  data <- x$data
  fit <- x$model_fit

  params <- as.data.frame(summary(fit)$coefficients[,1])

  plot(data$temp_C, data$deriv,
       yaxs = 'i', ylim = c(0, max(data$deriv) + 0.06*max(data$deriv)),
       pch = 20, cex = 0.5, cex.axis = 1.2, cex.lab = 1.5)

  if (x$n_peaks == 4) {
    y1 <- fs_mixture(temp = temp,
                     height_1 = params['height_1',],
                     skew_1 = params['skew_1',],
                     position_1 = params['position_1',],
                     width_1 = params['width_1',],
                     height_2 = params['height_2',],
                     skew_2 = params['skew_2',],
                     position_2 = params['position_2',],
                     width_2 = params['width_2',],
                     height_3 = params['height_3',],
                     skew_3 = params['skew_3',],
                     position_3 = params['position_3',],
                     width_3 = params['width_3',],
                     height_0 = params['height_0',],
                     skew_0 = params['skew_0',],
                     position_0 = params['position_0',],
                     width_0 = params['width_0',])

    y5 <- fs_function(temp,
                      params['height_0',], params['skew_0',],
                      params['position_0',], params['width_0',])

    lines(temp, y5, lty = 6, lwd = 2.5, col = '#33638DFF')

  }

  if (x$n_peaks == 3) {

    y1 <- fs_mixture(temp = temp,
                     height_1 = params['height_1',],
                     skew_1 = params['skew_1',],
                     position_1 = params['position_1',],
                     width_1 = params['width_1',],
                     height_2 = params['height_2',],
                     skew_2 = params['skew_2',],
                     position_2 = params['position_2',],
                     width_2 = params['width_2',],
                     height_3 = params['height_3',],
                     skew_3 = params['skew_3',],
                     position_3 = params['position_3',],
                     width_3 = params['width_3',])

  }

  y2 <- fs_function(temp,
                    params['height_1',], params['skew_1',],
                    params['position_1',], params['width_1',])
  y3 <- fs_function(temp,
                    params['height_2',], params['skew_2',],
                    params['position_2',], params['width_2',])
  y4 <- fs_function(temp,
                    params['height_3',], params['skew_3',],
                    params['position_3',], params['width_3',])

  lines(temp, y1, lty = 1, lwd = 2)
  lines(temp, y2, lty = 3, lwd = 3.5, col = '#440154FF')
  lines(temp, y3, lty = 4, lwd = 3.5, col = '#B8DE29FF')
  lines(temp, y4, lty = 5, lwd = 3.5, col = '#3CBB75FF')

}

legend_species <- function (spname, plant_part, y) {
  legend(600, y,
         xjust = 1,
         legend = c(spname, plant_part),
         text.font = c(3, 1),
         cex = 1.2,
         bty = 'n')
}

test_plots <- function () {

  t.testudinum <- process_test_samples('data/Tt_leaf.txt')
  t.testudinum_output <- deconvolve(t.testudinum)

  z.marina_rh <- process_test_samples('data/Zm_rhizome.txt')
  z.marina_rh_output <- deconvolve(z.marina_rh)

  z.marina_ro <- process_test_samples('data/Zm_root.txt')
  z.marina_ro_output <- deconvolve(z.marina_ro)

  par(oma = c(4, 3, 0, 1), mar = c(2, 2, 1, 1), mfrow = c(2, 2))
  
  deconvolve_plot(t.testudinum_output)
  text(170, max(t.testudinum_output$data$deriv), '(a)', cex = 1.5)
  legend_species('Thalassia testudinum', 'leaves', max(t.testudinum_output$data$deriv))

  deconvolve_plot(z.marina_rh_output)
  text(170, max(z.marina_rh_output$data$deriv), '(b)', cex = 1.5)
  legend_species('Zostera marina', 'rhizome', max(z.marina_rh_output$data$deriv))

  deconvolve_plot(z.marina_ro_output)
  text(170, max(z.marina_ro_output$data$deriv), '(c)', cex = 1.5)
  legend_species('Zostera marina', 'root', max(z.marina_ro_output$data$deriv))

  plot(1, type = 'n', axes = FALSE, xlab = '', ylab = '')
  
  legend(x = "top", inset = 0,
         legend = c('DTG data', 'DTG modelled', 'Hemicellulose 1', 
                    'Hemicellulose 2', 'Cellulose', 'Lignin'),
         cex = 1.5,
         ncol = 1,
         bty = 'n',
         lty = c(NA, 1, 6, 3, 4, 5),
         pch = c(20, NA, NA, NA, NA, NA),
         col = c('black', 'black', '#33638DFF', '#440154FF', '#B8DE29FF', '#3CBB75FF'),
         lwd = 2) 

  mtext(text = 'Temperature (C)',
        side = 1,
        line = 2.1,
        outer = TRUE,
        cex = 1.5)
  mtext(text = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')),
        side = 2,
        line = 0.9,
        outer = TRUE,
        cex = 1.5)

}

e.rad <- function () {

  e.radiata <- process_test_samples('data/Er_blade.txt')
  e.radiata_output <- deconvolve(e.radiata,
                                 lower_temp = 150,
                                 upper_temp = 600)

  start_vec <- c(0.002, -0.15, 250, 50,           # for hemicellulose 1
                 0.003, -0.15, 310, 50,           # for hemicellulose 2
                 0.006, -0.15, 350, 30,           # for cellulose
                 0.001, -0.15, 410, 200)          # for lignin

  # change the upper bounds to ensure the starting vector values are within
  # the allowed range
  ub <- c(2, 0.2, 260, 80,
          2, 0.2, 330, 90,
          2, 0.2, 380, 50,
          2, 0.2, 430, 250)

  e.radiata_decon <- deconvolve(e.radiata,
                                n_peaks = 4,
                                start_vec = start_vec,
                                upper_vec = ub,
                                lower_temp = 150,
                                upper_temp = 600)

  layout(matrix(c(1,2,3,3), nrow = 2, ncol = 2, byrow = TRUE), heights = c(0.8, 0.2))
  par(oma = c(2, 3, 0, 1), mar = c(1, 2, 1, 0))
  
  x <- e.radiata_output
  temp <- seq(x$temp_bounds[1], x$temp_bounds[2], length.out = nrow(x$data))
  data <- x$data
  fit <- x$model_fit
  
  params <- as.data.frame(summary(fit)$coefficients[,1])
  plot(data$temp_C, data$deriv, xlab = '',
       ylab = '', yaxt = 's',
       yaxs = 'i', ylim = c(0, max(data$deriv) + 0.06*max(data$deriv)),
       pch = 20, cex = 0.5, cex.axis = 1.2, cex.lab = 1.5)
    
  y1 <- fs_mixture(temp = temp,
                   height_1 = params['height_1',],
                   skew_1 = params['skew_1',],
                   position_1 = params['position_1',],
                   width_1 = params['width_1',],
                   height_2 = params['height_2',],
                   skew_2 = params['skew_2',],
                   position_2 = params['position_2',],
                   width_2 = params['width_2',],
                   height_3 = params['height_3',],
                   skew_3 = params['skew_3',],
                   position_3 = params['position_3',],
                   width_3 = params['width_3',])
  
  y2 <- fs_function(temp,
                    params['height_1',], params['skew_1',],
                    params['position_1',], params['width_1',])
  y3 <- fs_function(temp,
                    params['height_2',], params['skew_2',],
                    params['position_2',], params['width_2',])
  y4 <- fs_function(temp,
                    params['height_3',], params['skew_3',],
                    params['position_3',], params['width_3',])
  
  lines(temp, y1, lty = 1, lwd = 2)
  lines(temp, y2, lty = 3, lwd = 3.5, col = '#440154FF')
  lines(temp, y3, lty = 4, lwd = 3.5, col = '#B8DE29FF')
  lines(temp, y4, lty = 5, lwd = 3.5, col = '#3CBB75FF')
  
  text(170, max(e.radiata_output$data$deriv), '(a)', cex = 1.5)
  
  # second plot
  x <- e.radiata_decon
  temp <- seq(x$temp_bounds[1], x$temp_bounds[2], length.out = nrow(x$data))
  data <- x$data
  fit <- x$model_fit
  
  params <- as.data.frame(summary(fit)$coefficients[,1])
  
  plot(data$temp_C, data$deriv, xlab = '',
       ylab = '', yaxt = 'n',
       yaxs = 'i', ylim = c(0, max(data$deriv) + 0.06*max(data$deriv)),
       pch = 20, cex = 0.5, cex.axis = 1.2, cex.lab = 1.5)
  
  y1 <- fs_mixture(temp = temp,
                   height_1 = params['height_1',],
                   skew_1 = params['skew_1',],
                   position_1 = params['position_1',],
                   width_1 = params['width_1',],
                   height_2 = params['height_2',],
                   skew_2 = params['skew_2',],
                   position_2 = params['position_2',],
                   width_2 = params['width_2',],
                   height_3 = params['height_3',],
                   skew_3 = params['skew_3',],
                   position_3 = params['position_3',],
                   width_3 = params['width_3',],
                   height_0 = params['height_0',],
                   skew_0 = params['skew_0',],
                   position_0 = params['position_0',],
                   width_0 = params['width_0',])
  
  y5 <- fs_function(temp,
                    params['height_0',], params['skew_0',],
                    params['position_0',], params['width_0',])
  lines(temp, y5, lty = 6, lwd = 2.5, col = '#33638DFF')
  
  y2 <- fs_function(temp,
                    params['height_1',], params['skew_1',],
                    params['position_1',], params['width_1',])
  y3 <- fs_function(temp,
                    params['height_2',], params['skew_2',],
                    params['position_2',], params['width_2',])
  y4 <- fs_function(temp,
                    params['height_3',], params['skew_3',],
                    params['position_3',], params['width_3',])
  
  lines(temp, y1, lty = 1, lwd = 2)
  lines(temp, y2, lty = 3, lwd = 3.5, col = '#440154FF')
  lines(temp, y3, lty = 4, lwd = 3.5, col = '#B8DE29FF')
  lines(temp, y4, lty = 5, lwd = 3.5, col = '#3CBB75FF')
  
  text(170, max(e.radiata_output$data$deriv), '(b)', cex = 1.5)
  
  mtext(text = 'Temperature (C)',
        side = 1,
        line = 0,
        outer = TRUE,
        cex = 1.5)
  mtext(text = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')),
        side = 2,
        line = 0.7,
        adj = 0.7,
        outer = TRUE,
        cex = 1.5)
  
  legend_four_curves_horizontal(cex = 1.5)
}

# Three four horizontal legend
legend_four_curves_horizontal <- function (cex) {
  
  plot(1, type = 'n', axes = FALSE, xlab = '', ylab = '')
  
  legend(x = "top", inset = 0,
         legend = c('DTG data', 'DTG modelled', 'Hemicellulose 1', 
                    'Hemicellulose 2', 'Cellulose', 'Lignin'),
         cex = cex,
         ncol = 3,
         bty = 'n',
         lty = c(NA, 1, 6, 3, 4, 5),
         pch = c(20, NA, NA, NA, NA, NA),
         col = c('black', 'black', '#33638DFF', '#440154FF', '#B8DE29FF', '#3CBB75FF'),
         lwd = 2) 
}