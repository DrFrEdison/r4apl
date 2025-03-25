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

integrate_peaks <- function(x, y) {
  # Ensure only positive values are integrated (optional)
  y[y < 0] <- 0
  # Calculate area using trapezoid rule
  n <- length(x)
  area <- 0
  for (i in 1:(n-1)) {
    area <- area + (x[i+1] - x[i]) * (y[i] + y[i+1]) / 2
  }
  return(area)
}

# Teile das Chromatogramm in bis zu 6 Segmente ####
segment_integration <- function(RT, Intensity,
                                segment = c(1, 2.5, 8, 14, 21),
                                findpeak,
                                reprocess = NULL,
                                addpeak = NULL,
                                removepeak = NULL,
                                name = NULL,
                                png = F

) {

  segment <- sort(unique(segment))
  peaks <- list()
  n.seg.plot <- length(segment) - 1

  graphics.off()
  if(png) png(filename  = file.path(wd$data$integration, paste0(name, ".png")), width = width <- 480 * 4, height = width*.6
              , family = r4apl$font)

  par(mar = c(3, 4, 2, 0), mfrow = c(parmfrow(length(segment) )))

  plot(RT, Intensity, type = "l", xlab = "Retention Time", ylab = "Intensity",
       ylim = c(0, rangexy(Intensity)[2]), axes = FALSE, main = name)
  axis(2, las = 2); axis(1, lwd.ticks = 3, at = seq(0, round(range(RT)[2], 1), 1)); box()
  x.segments <- seq(0, round(range(RT)[2], 1), 0.5)
  segments(x0 = x.segments, y0 = par("usr")[3], x1 = x.segments,
           y1 = par("usr")[3] - diff(par("usr")[3:4]) * .02, xpd = TRUE)

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
      findpeaks(y1.range,
                minpeakheight = this.para$minpeakheight,
                minpeakdistance = this.para$minpeakdistance,
                nups = this.para$nups,
                ndowns = this.para$ndowns)
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

      if (diff(range(y.baseline[[j]] + y.peak[[j]])) < this.para$area.threshold) next

      peaks[[i.seg]]$area[j] <- integrate_peaks(x.peak[[j]], y.peak[[j]])
    }

    peaks[[i.seg]] <- subset(peaks[[i.seg]], !is.na(area) & area > this.para$area.threshold)

    # Reprocessing anwenden
    if (!is.null(reprocess) && nrow(reprocess) > 0 && any(reprocess$RT > min(x1.range) & reprocess$RT < max(x1.range))) {
      for (p in seq_len(nrow(reprocess))) {
        if (!(reprocess$RT[p] > min(x1.range) & reprocess$RT[p] < max(x1.range))) next
        peak.idx <- which(abs(peaks[[i.seg]]$RT - reprocess$RT[p]) < 0.3)
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

    # Neue Peaks hinzufügen
    if (!is.null(addpeak) && nrow(addpeak) > 0 && any(addpeak$RT > min(x1.range) & addpeak$RT < max(x1.range))) {
      for (p in seq_len(nrow(addpeak))) {
        if (!(addpeak$RT[p] > min(x1.range) & addpeak$RT[p] < max(x1.range))) next

        idx.center <- min(which(x1.range > addpeak$RT[p]))
        x.whichp <- (idx.center + addpeak$left[p]):(idx.center + addpeak$right[p])

        if (any(x.whichp < 1 | x.whichp > length(x1.range))) next

        x.add <- x1.range[x.whichp]
        y.add <- y1.range[x.whichp]
        y.add.corrected <- y1.corrected[x.whichp]
        base.add <- baseline.corrected[x.whichp]

        new.peak.raw <- tryCatch({
          findpeaks(y.add,
                    minpeakheight = this.para$minpeakheight,
                    minpeakdistance = this.para$minpeakdistance,
                    nups = this.para$nups,
                    ndowns = this.para$ndowns
                    , npeaks = 1)
        }, error = function(e) {
          warning(paste("Fehler bei findpeaks (addpeak) in Segment", i.seg, ":", e$message))
          return(NULL)
        })

        if (is.null(new.peak.raw) || nrow(new.peak.raw) == 0) next

        new.peak <- tryCatch({
          findpeaks_order(new.peak.raw, RT = x.add)
        }, error = function(e) {
          warning(paste("Fehler bei findpeaks_order (addpeak) in Segment", i.seg, ":", e$message))
          return(data.frame())
        })

        if (nrow(new.peak) == 0) next

        idxs <- new.peak$start:new.peak$end
        x.add <- x.add[idxs]
        y.add <- y.add[idxs]
        y.add.corrected <- y.add.corrected[idxs]
        base.add <- base.add[idxs]

        new.peak$area <- integrate_peaks(x.add, y.add.corrected)
        offset <- min(x.whichp)
        new.peak$start <- new.peak$start + offset
        new.peak$end <- new.peak$end + offset
        new.peak$max <- new.peak$max + offset

        x.peak <- append(x.peak, list(x.add))
        y.peak <- append(y.peak, list(y.add.corrected))
        y.baseline <- append(y.baseline, list(base.add))

        peaks[[i.seg]] <- rbind(peaks[[i.seg]], new.peak)
        rownames(peaks[[i.seg]])[nrow(peaks[[i.seg]])] <- as.character(length(x.peak))
        peaks[[i.seg]] <- peaks[[i.seg]][order(peaks[[i.seg]]$RT), ]
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
    plot(x1.range, y1.range, type = "l", xlab = "Retention Time", ylab = "Intensity",
         ylim = c(0, rangexy(y1.range)[2]), axes = FALSE)
    lines(x1.range, baseline.corrected, col = "blue", lwd = 1)
    points(peaks[[i.seg]]$RT, peaks[[i.seg]]$height, pch = 20, col = "red", cex = 2.5)
    axis(2, las = 2); axis(1, lwd.ticks = 2); box()

    # Markierungen
    x.segments <- seq(round(range(x1.range)[1], 1), round(range(x1.range)[2], 1), 0.1)
    segments(x0 = x.segments, y0 = par("usr")[3], x1 = x.segments,
             y1 = par("usr")[3] - diff(par("usr")[3:4]) * .01, xpd = TRUE)
    x.segments <- seq(round(range(x1.range)[1], 1), round(range(x1.range)[2], 1), 0.5)
    segments(x0 = x.segments, y0 = par("usr")[3], x1 = x.segments,
             y1 = par("usr")[3] - diff(par("usr")[3:4]) * .02, xpd = TRUE, lwd = 2)

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
  peaks <- peaks[order(peaks$RT), ]
  rownames(peaks) <- seq_len(nrow(peaks))
  return(peaks)
}
