#' @title Order Peaks by Retention Time
#' @description Orders a data.frame of detected peaks by their retention time (RT)
#' @param peaks Matrix or data.frame with columns: height, max, start, end
#' @param RT Numeric vector of retention times
#' @return A data.frame with ordered peaks and calculated RT
#' @export
findpeaks_order <- function(peaks, RT){
  peaks <- data.frame(peaks)
  colnames(peaks) <- c("height", "max", "start", "end")
  peaks$RT <- RT[ peaks$max ]
  peaks$area <- NA
  peaks <- peaks[ order(peaks$RT) , ]
  rownames(peaks) <- c(1:nrow(peaks))
  peaks <- peaks[ , c("RT", "area", "height", "start", "end", "max")]
  return(peaks)
}

#' @title Trapezoidal Peak Integration
#' @description Integrates a signal using the trapezoidal rule
#' @param x Numeric vector for x-axis (e.g. RT)
#' @param y Numeric vector for y-axis (e.g. intensity)
#' @return Numeric value representing the area under the curve
#' @export
integrate_peaks <- function(x, y) {
  y[y < 0] <- 0
  n <- length(x)
  area <- 0
  for (i in 1:(n-1)) {
    area <- area + (x[i+1] - x[i]) * (y[i] + y[i+1]) / 2
  }
  return(area)
}

#' @title Segment-wise Chromatographic Peak Integration
#' @description Integrates chromatographic data over defined RT segments with baseline correction, optional peak addition and reprocessing.
#' @param RT Retention time vector
#' @param Intensity Intensity vector
#' @param segment Numeric vector defining segment boundaries
#' @param findpeak Data.frame with parameters per segment
#' @param reprocess Optional data.frame with peaks to reprocess
#' @param addpeak Optional data.frame with peaks to manually add
#' @param removepeak Optional data.frame with peaks to manually remove
#' @param name Sample name for plot title
#' @param ID Sample ID for filename
#' @param png Logical, if TRUE saves plots to PNG
#' @return Data.frame of integrated peaks
#' @export
#' @import baseline
#' @importFrom utils head
segment_integration <- function(RT, Intensity,
                                segment,
                                findpeak,
                                reprocess = NULL,
                                addpeak = NULL,
                                removepeak = NULL,
                                name,
                                ID,
                                png = T

) {

  segment <- sort(unique(segment))
  peaks <- list()
  n.seg.plot <- length(segment) - 1

  windowsFonts(Verdana = windowsFont("Verdana"))
  graphics.off()
  if(png) png(filename  = file.path(wd$data$integration, paste0(datetime(), "_", ID, "_", name, ".png")), width = width <- 480 * 4, height = width*.6
              , family = r4apl$font)

  if(png) par(mar = c(6, 13, 6, 0.25), mfrow = c(parmfrow(length(segment) ))
              , cex.axis = 2, cex.main = 2)

  if(!png) par(mar = c(3, 6, 3, 0.25), mfrow = c(parmfrow(length(segment) ))
               , cex.axis = 1, cex.main = 1)

  options(scipen = 1)
  plot(RT, Intensity, type = "l", xlab = "", ylab = "",
       ylim = c(0, rangexy(Intensity)[2]), axes = FALSE, main = name)
  axis(2, las = 2)
  x.segments <- seq(0, round(range(RT)[2], 1), 0.5)
  segments(x0 = x.segments, y0 = par("usr")[3], x1 = x.segments,
           y1 = par("usr")[3] - diff(par("usr")[3:4]) * .02, xpd = TRUE)
  x.segments <- seq(0, round(range(RT)[2], 1), 1)
  segments(x0 = x.segments, y0 = par("usr")[3], x1 = x.segments,
           y1 = par("usr")[3] - diff(par("usr")[3:4]) * .04, xpd = TRUE, lwd = 2)
  text(x.segments, par("usr")[3] - diff(par("usr")[3:4]) * .08, xpd = T, cex = 2)
  if(png) mtext("Retention Time", 1, 5, cex = 1.5)
  if(png) mtext("Intensity", 2, 10, cex = 1.5)
  box()
  if(!png) mtext("Retention Time", 1, 1.75, cex = 1)
  if(!png) mtext("Intensity", 2, 4, cex = 1)

  for (seg in seq_along(segment)[-1]) {
    i.seg <- seg - 1
    xrange <- which(RT > segment[i.seg] & RT < segment[seg])
    x1.range <- RT[xrange]
    y1.range <- Intensity[xrange]

    this.para <- findpeak[findpeak$seg == i.seg, ]
    if (nrow(this.para) == 0) this.para <- findpeak[1, ]

    if (length(x1.range) < 3) {
      warning(paste("Segment", i.seg, "hat zu wenige Datenpunkte."))
      next
    }

    raw.peaks <- tryCatch({
      ppp <- findpeaks(y1.range - min(y1.range),
                       minpeakheight = this.para$minpeakheight,
                       minpeakdistance = this.para$minpeakdistance,
                       nups = this.para$nups,
                       ndowns = this.para$ndowns)
      ppp[ , 1] <- ppp[ , 1] + min(y1.range)
      ppp
    }, error = function(e) {
      warning(paste("Fehler bei findpeaks in Segment", i.seg, ":", e$message))
      return(NULL)
    })

    if (is.null(raw.peaks) || nrow(raw.peaks) == 0) {
      peaks[[i.seg]] <- data.frame()
      next
    }

    peaks[[i.seg]] <- tryCatch({
      findpeaks_order(raw.peaks, RT = x1.range)
    }, error = function(e) {
      warning(paste("Fehler bei findpeaks_order in Segment", i.seg, ":", e$message))
      return(data.frame())
    })

    baseline.correction <- tryCatch({
      baseline(matrix(y1.range, nrow = 1), method = this.para$baseline.method)
    }, error = function(e) {
      warning(paste("Baseline-Korrektur fehlgeschlagen in Segment", i.seg))
      baseline(matrix(rep(0, length(y1.range)), nrow = 1), method = "modpolyfit")
    })

    y1.corrected <- baseline.correction@corrected[1, ]
    baseline.corrected <- baseline.correction@baseline[1, ]

    x.peak <- list(); y.peak <- list(); y.baseline <- list()

    for (j in seq_len(nrow(peaks[[i.seg]]))) {
      idx.start <- peaks[[i.seg]]$start[j]
      idx.end <- peaks[[i.seg]]$end[j]
      if (idx.end > length(x1.range)) next
      x.peak[[j]] <- x1.range[idx.start:idx.end]
      y.peak[[j]] <- y1.corrected[idx.start:idx.end]
      y.baseline[[j]] <- baseline.corrected[idx.start:idx.end]

      peaks[[i.seg]]$height[j] <- y1.corrected[ peaks[[i.seg]]$max[j] ]

      if (diff(range(y.baseline[[j]] + y.peak[[j]])) < this.para$minarea) next

      peaks[[i.seg]]$area[j] <- integrate_peaks(x.peak[[j]], y.peak[[j]])
    }

    peaks[[i.seg]] <- subset(peaks[[i.seg]], !is.na(area) & area > this.para$minarea & height > this.para$minpeakheight)

    # Neue Peaks hinzufügen ####
    if (!is.null(addpeak) && nrow(addpeak) > 0 && any(addpeak$RT > min(x1.range) & addpeak$RT < max(x1.range))) {
      for (p in seq_len(nrow(addpeak))) {
        if (!(addpeak$RT[p] > min(x1.range) & addpeak$RT[p] < max(x1.range))) next

        new.peak <- data.frame(RT = NA, area = NA, height = NA, start = NA, end = NA, max = NA)

        new.peak$RT <- addpeak$RT[ p ]
        new.peak$start <- which.min( abs(x1.range - new.peak$RT) ) + addpeak$left[ p ]
        new.peak$end <- which.min( abs(x1.range - new.peak$RT) ) + addpeak$right[ p ]

        new.peak$max <- new.peak$start + which.max( y1.range[ new.peak$start : new.peak$end ] )

        new.peak$RT <- x1.range[ new.peak$max ]

        new.peak$height <- max( y1.corrected[ new.peak$start : new.peak$end ] )
        new.peak$area <- integrate_peaks(x1.range[ new.peak$start : new.peak$end ]
                                         , y1.corrected[ new.peak$start : new.peak$end ])

        if(new.peak$area <= 0){new.peak$area <- integrate_peaks(x1.range[ new.peak$start : new.peak$end ]
                                                                , y1.range[ new.peak$start : new.peak$end ])}

        x.peak <- append(x.peak, list(x1.range[ new.peak$start : new.peak$end ]))
        y.peak <- append(y.peak, list(y1.corrected[ new.peak$start : new.peak$end ]))
        y.baseline <- append(y.baseline, list(baseline.corrected[ new.peak$start : new.peak$end ]))

        peaks[[i.seg]] <- rbind(peaks[[i.seg]], new.peak)
        rownames(peaks[[i.seg]])[nrow(peaks[[i.seg]])] <- as.character(length(x.peak))
        peaks[[i.seg]] <- peaks[[i.seg]][order(peaks[[i.seg]]$RT), ]
      }
    }

    # Reprocessing ####
    if (!is.null(reprocess) && nrow(reprocess) > 0 && any(reprocess$RT > min(x1.range) & reprocess$RT < max(x1.range))) {
      for (p in seq_len(nrow(reprocess))) {
        if (!(reprocess$RT[p] > min(x1.range) & reprocess$RT[p] < max(x1.range))) next
        peak.idx <- which(abs(peaks[[i.seg]]$RT - reprocess$RT[p]) < 0.3)
        peak.idx <- peak.idx[ which.min(abs(peaks[[i.seg]]$RT[ peak.idx ] - reprocess$RT[p])) ]

        if (length(peak.idx) == 0) next

        j <- peak.idx[1]
        o <- as.numeric(rownames(peaks[[i.seg]])[j])

        idx.start <- peaks[[i.seg]]$start[j] + reprocess$left[p]
        idx.end <- peaks[[i.seg]]$end[j] + reprocess$right[p]

        if(idx.start <= 0) idx.start <- 1

        x.peak[[o]] <- x1.range[idx.start:idx.end]
        y.peak[[o]] <- y1.corrected[idx.start:idx.end]
        y.baseline[[o]] <- baseline.corrected[idx.start:idx.end]
        peaks[[i.seg]]$area[j] <- integrate_peaks(x.peak[[o]], y.peak[[o]])
      }
    }

    # Peak löschen ####
    if (!is.null(removepeak) && nrow(removepeak) > 0) {
      for (p in seq_len(nrow(removepeak))) {
        if (!(removepeak$RT[p] > min(x1.range) & removepeak$RT[p] < max(x1.range))) next

        peak.idx <- which.min( abs( peaks[[i.seg]]$RT - removepeak$RT[p] ))
        if (length(peak.idx) == 0) next

        j <- peak.idx[1]
        o <- as.numeric(rownames(peaks[[i.seg]])[j])

        # x.peak[[o]] <- NULL
        # y.peak[[o]] <- NULL
        # y.baseline[[o]]  <- NULL

        peaks[[i.seg]] <- peaks[[i.seg]][ - j , ]

      }
    }

    # Plot mit Baseline
    plot(x1.range, y1.range, type = "l", xlab = "", ylab = "",
         ylim = c(0, rangexy(y1.range)[2]), axes = FALSE, main = paste0("Segment ", i.seg))
    lines(x1.range, baseline.corrected, col = "blue", lwd = 1)
    points(peaks[[i.seg]]$RT, peaks[[i.seg]]$height + baseline.corrected[ peaks[[i.seg]]$max ], pch = 20, col = "red", cex = 2.5)
    axis(2, las = 2)

    mtext("Retention Time", 1, 5, cex = 1.5)
    mtext("Intensity", 2, 10, cex = 1.5)

    # Markierungen
    x.segments <- seq(round(range(x1.range)[1], 1), round(range(x1.range)[2], 1), 0.1)
    segments(x0 = x.segments, y0 = par("usr")[3], x1 = x.segments,
             y1 = par("usr")[3] - diff(par("usr")[3:4]) * .02, xpd = TRUE)
    x.segments <- seq(round(range(x1.range)[1], 1), round(range(x1.range)[2], 1), 0.5)
    segments(x0 = x.segments, y0 = par("usr")[3], x1 = x.segments,
             y1 = par("usr")[3] - diff(par("usr")[3:4]) * .04, xpd = TRUE, lwd = 2)
    text(x.segments, par("usr")[3] - diff(par("usr")[3:4]) * .08, x.segments, xpd = T, cex = 2)
    x.segments <- seq(round(range(x1.range)[1], 1), round(range(x1.range)[2], 1), 1)
    segments(x0 = x.segments, y0 = par("usr")[3], x1 = x.segments,
             y1 = par("usr")[3] - diff(par("usr")[3:4]) * .04, xpd = TRUE, lwd = 2)
    box()



    for (o in as.numeric(rownames(peaks[[i.seg]]))) {
      polygon(c(x.peak[[o]], rev(x.peak[[o]])),
              c(y.peak[[o]] + y.baseline[[o]], rev(y.baseline[[o]])),
              col = rgb(1, 0, 0, 0.3), border = NA)
      abline(v = range(x.peak[[o]]), lty = 3, col = "blue")
    }

  }
  if(png) dev.off()
  # Ergebnis zusammenfassen
  peaks <- do.call(rbind, peaks)
  if(nrow(peaks) > 0)peaks <- peaks[order(peaks$RT), ]
  rownames(peaks) <- seq_len(nrow(peaks))
  return(peaks)
}

#' @title Group Peaks Globally by RT Clustering
#' @description Clusters peaks globally based on RT similarity using hierarchical clustering
#' @param integration.results Named list of data.frames with integrated peak info
#' @param rt_min Lower RT limit
#' @param rt_max Upper RT limit
#' @param h Height parameter for clustering (see cutree)
#' @param prefix Prefix for group name
#' @return Data.frame with compound_group and median_RT
#' @export
group_peaks_global <- function(integration.results, rt_min = 3, rt_max = 11, h = 0.25, prefix = "G1") {
  library(dplyr)
  all_peaks <- bind_rows(integration.results, .id = "sample") %>% filter(RT > rt_min & RT < rt_max)
  if (nrow(all_peaks) == 0) return(data.frame())
  dist_matrix <- dist(all_peaks$RT)
  clust <- hclust(dist_matrix, method = "single")
  groups <- cutree(clust, h = h)
  groups <- formatC(groups, width = 2, format = "d", flag = "0")
  all_peaks$compound_group <- paste0(prefix, ".", groups)
  rt_medians <- all_peaks %>% group_by(compound_group) %>% summarise(median_RT = median(RT), .groups = "drop")
  all_peaks <- left_join(all_peaks, rt_medians, by = "compound_group")
  return(all_peaks)
}

#' @title Group Peaks by Elution Order
#' @description Groups peaks within a narrow RT window based on elution rank per sample
#' @param integration.results Named list of data.frames with integrated peak info
#' @param rt_min Lower RT limit
#' @param rt_max Upper RT limit
#' @param prefix Prefix for group name
#' @return Data.frame with compound_group and median_RT
#' @export
group_peaks_by_elution <- function(integration.results, rt_min, rt_max, prefix = "F1") {
  library(dplyr)
  all_peaks <- bind_rows(integration.results, .id = "sample") %>%
    filter(RT >= rt_min & RT <= rt_max) %>%
    arrange(sample, RT) %>%
    group_by(sample) %>%
    mutate(elution_rank = row_number()) %>%
    ungroup() %>%
    mutate(compound_group = paste0(prefix, ".", formatC(elution_rank, width = 2, format = "d", flag = "0")))
  rt_medians <- all_peaks %>% group_by(compound_group) %>% summarise(median_RT = median(RT), .groups = "drop")
  all_peaks <- left_join(all_peaks, rt_medians, by = "compound_group")
  return(all_peaks)
}

#' @title Custom Barplot for GC/MS Peak Areas
#' @description Creates a grouped barplot per compound with color-coded samples
#' @param summary.df A data.frame with columns: sample, compound_group, Area, median_RT
#' @param bar_width Width of bars (default 1.5)
#' @param colfun A vector of colors (e.g. r4apl$col)
#' @export
plot_bar_custom <- function(summary.df, bar_width = 1.5, colfun = r4apl$col) {
  compound_levels <- unique(summary.df$compound_group[order(summary.df$median_RT)])
  samples <- unique(summary.df$sample)
  n_compounds <- length(compound_levels)
  n_samples <- length(samples)
  bar_colors <- setNames(colfun[ 1:n_samples], samples)
  byp <- 2.5
  x_positions <- seq(1, by = byp, length.out = n_compounds)
  png(filename = file.path(wd$plot$Audi, paste0("Barplot_GC_MS", ".png")), width = 480 * 4, height = 480 * 2.4, family = r4apl$font)
  par(mar = c(15,9,4,1))
  plot(NULL, xlim = c(min(x_positions) - 1, max(x_positions) + 1),
       ylim = c(0, max(summary.df$Area) * 1.2),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
  rect(xleft = par("usr")[1], xright = par("usr")[2], ybottom = 0, ytop = par("usr")[4], xpd = T)
  segments(x_positions - byp / 2, 0, x_positions - byp / 2, - diff(par("usr")[3:4]) * .025, xpd = T)
  axis(2, las = 2, lwd = 0.5)
  mtext("Compound", 1, 3, font = 2)
  mtext("Area", 2, 4.5, font = 2)
  dx <- bar_width / n_samples
  offsets <- seq(-bar_width/2, bar_width/2, length.out = n_samples) * byp / 2
  for (i in seq_along(compound_levels)) {
    compound <- compound_levels[i]
    x_center <- x_positions[i]
    sub <- summary.df[summary.df$compound_group == compound, ]
    for (j in seq_len(nrow(sub))) {
      samp <- sub$sample[j]
      area <- sub$Area[j]
      x_left <- x_center + offsets[which(samples == samp)] - dx / 2
      x_right <- x_left + dx
      rect(x_left, 0, x_right, area, col = bar_colors[samp], border = NA)
    }
    text(x = x_center, y = -0.015 * max(summary.df$Area), labels = compound, xpd = TRUE, srt = 60, adj = 1, cex = 1.25)
  }
  legend("topright", legend = samples, fill = bar_colors, border = NA, bty = "n", cex = 0.8)
  dev.off()
}
