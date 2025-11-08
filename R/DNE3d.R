#' Plot results of a DNE analysis of a surface
#'
#' A molaR surface plotting function.
#'
#' @param DNE_File An object that stores the output of the `DNE()` function
#' @param setMax User-defined upper range for plotting color scheme, see Details
#' @param logColors Logical that log transforms the color scheme
#' @param signColor Logical indicating whether or not to plot by concavity vs convexity. Plotting by curve orientation is the default.
#' @param concaveMute Logical indicating whether or not to mute the concave portion of the Dirichlet normal density coloration. Default is FALSE.
#' @param cuttingFocus Logical indicating whether or not to mute the concave portion and faces with Dirichlet normal density below the top quartile. Default is FALSE.
#' @param main String indicating plot title
#' @param cex Numeric setting the relative size of the legend
#' @param legend Logical indicating whether or not a legend should be displayed
#' @param widget_size_px Sets the plot size in pixels. Default is 768 and always square
#' @param scene_zoom Set the initial viewing window of the 3d scene. Default is 1.5
#' @param leftOffset Numeric between -1 and 1 setting the amount of offset for the plotted surface to the left. Larger values push surface farther to right.
#' @param fieldofview Passes an argument to `par3d()` changing the field of view (in degrees) of the resulting 3D plot
#' @param title_font_size_px Sets the title font size in pixels. Default is 30
#' @param legend_magnify Default set to 1, changes the scaling on legend.
#' @param fileName String indicating a name to save the plotted surface to as a *.ply file; default of 'NA' will not save a file
#' @param binary Logical indicating whether or not the saved surface plot should be binary, passed to `vcgPlyWrite()`
#'
#' @import grDevices graphics utils
#' @import htmltools
#' @export
#' @examples
#' \dontrun{
#' DNE_output <- DNE(Tooth)
#' DNE3d(DNE_output)
#' }
DNE3d <- function(
  DNE_File,
  setMax = 0,
  logColors = TRUE,
  signColor = TRUE,
  concaveMute = FALSE,
  cuttingFocus = FALSE,
  main = "",
  cex = 1,
  legend = TRUE,
  widget_size_px = 768,
  scene_zoom = 1.5,
  leftOffset = 0,
  fieldofview = 0,
  title_font_size_px = 30,
  legend_magnify = 1,
  fileName = NA,
  binary = FALSE
) {
  # --- rgl device setup (headless): minimal, scoped, auto-restore ---
  old_opts <- options(c("rgl.useNULL", "rgl.printRglwidget"))
  options(rgl.useNULL = TRUE, rgl.printRglwidget = FALSE)
  on.exit(options(old_opts), add = TRUE)
  
  # --- inputs ---
  plyFile <- DNE_File$plyFile
  DNEs <- DNE_File$Face_Values$Dirichlet_Energy_Densities * DNE_File$Face_Values$Face_Areas
  Kappa <- DNE_File$Kappa
  kappas <- DNE_File$Face_Values$Kappa_Values
  
  # --- validate ---
  if (!is.numeric(setMax) || length(setMax) != 1L) stop("setMax must be numeric length 1.")
  if (setMax < 0) stop("Negative values not accepted for face energy maximum.")
  if (!is.logical(logColors) || length(logColors) != 1L) stop("logColors must be TRUE/FALSE.")
  if (!is.logical(signColor) || length(signColor) != 1L) stop("signColor must be TRUE/FALSE.")
  if (!is.logical(legend) || length(legend) != 1L) stop("legend must be TRUE/FALSE.")
  if (!is.numeric(cex) || cex <= 0) stop("cex must be > 0.")
  if (!is.numeric(leftOffset) || length(leftOffset) != 1L) stop("leftOffset must be numeric.")
  if (!is.numeric(scene_zoom) || scene_zoom <= 0) stop("scene_zoom must be > 0.")
  if (!is.numeric(widget_size_px) || widget_size_px <= 0) stop("widget_size_px must be > 0.")
  if (!is.numeric(title_font_size_px) || title_font_size_px <= 0) stop("title_font_size_px must be > 0.")
  if (!is.numeric(legend_magnify) || legend_magnify <= 0) stop("legend_magnify must be > 0.")
  if (!inherits(plyFile, "mesh3d")) stop("DNE_File$plyFile must be an rgl mesh3d.")
  
  # --- color ranges ---
  userMax <- setMax
  if (setMax == 0) setMax <- max(DNEs)
  if (setMax < max(DNEs)) DNEs[DNEs > setMax] <- setMax
  if (logColors) {
    logcut <- 1e-6
    DNEs[DNEs < logcut] <- logcut
    logDNEs <- log(DNEs); shift <- -min(logDNEs)
    color_range <- (logDNEs + shift) / (log(setMax) + shift)
  } else {
    color_range <- DNEs / setMax
  }
  # split convex/concave
  neg_idx <- which(kappas < Kappa)
  signed_range <- color_range
  signed_range[neg_idx] <- -color_range[neg_idx]
  signed_range <- (signed_range + 1) / 2
  # per-vertex colors
  if (isTRUE(signColor)) {
    DNE_colors <- signedcolor.gradient(
      interpolate_faces_to_vertices(signed_range, plyFile)
    )
  } else {
    DNE_colors <- ccolor.gradient(
      interpolate_faces_to_vertices(color_range, plyFile)
    )
  }
  if (isTRUE(concaveMute)) {
    DNE_colors[which(interpolate_faces_to_vertices(
      (DNE_File$Face_Values$Kappa_Values), plyFile) < 0)] <- "gray"
  }
  if (isTRUE(cuttingFocus)) {
    breakValue <- quantile(interpolate_faces_to_vertices(
      (DNE_File$Face_Values$Dirichlet_Energy_Densities *
         DNE_File$Face_Values$Face_Areas), plyFile))[4]
    DNE_colors[which(interpolate_faces_to_vertices(
      (DNE_File$Face_Values$Dirichlet_Energy_Densities *
         DNE_File$Face_Values$Face_Areas), plyFile) < breakValue)] <- "gray"
    DNE_colors[which(interpolate_faces_to_vertices(
      (DNE_File$Face_Values$Kappa_Values), plyFile) < 0)] <- "gray"
  }
  # --- centering ---
  vb <- plyFile$vb
  mesh_centered <- plyFile
  xr <- range(vb[1, ], na.rm = TRUE)
  zr <- range(vb[3, ], na.rm = TRUE)
  T_center_xz <- rgl::translationMatrix(-mean(xr), 0, -mean(zr))
  mesh_centered <- rgl::transform3d(mesh_centered, T_center_xz)
  {
    LEFT_BIAS_FRAC <- 0.08
    vb2 <- mesh_centered$vb
    xr2 <- range(vb2[1, ], na.rm = TRUE)
    x_width <- diff(xr2)
    x_left <- -LEFT_BIAS_FRAC * x_width
    T_left <- rgl::translationMatrix(x_left, 0, 0)
    mesh_centered <- rgl::transform3d(mesh_centered, T_left)
  }
  vb3 <- mesh_centered$vb
  yr <- range(vb3[2, ], na.rm = TRUE)
  T_center_y <- rgl::translationMatrix(0, -mean(yr), 0)
  mesh_centered <- rgl::transform3d(mesh_centered, T_center_y)
  
  # --- Only run plotting and file output in interactive sessions ---
  if (interactive()) {
    # --- scene ---
    rgl::open3d()
    rgl::par3d(windowRect = c(100, 100, 100 + widget_size_px, 100 + widget_size_px))
    rgl::par3d(userMatrix = diag(4), zoom = 1)
    rgl::shade3d(mesh_centered, color = DNE_colors, meshColor = "vertices", shininess = 110)
    rgl::view3d(fov = fieldofview)
    rgl::aspect3d("iso")
    rgl::par3d(zoom = scene_zoom)
    rgl::bg3d(col = "white")
    # Optional small horizontal camera nudge
    if (!isTRUE(all.equal(leftOffset, 0))) {
      ZView <- rgl::par3d("observer")[3]
      XView <- leftOffset * ZView * 0.05
      rgl::observer3d(XView, 0, ZView)
    }
    # --- legend labels (top->bottom = high->low) ---
    legend_labels <- NULL
    if (isTRUE(legend)) {
      if (logColors) {
        logcut <- 1e-6
        x <- exp(seq(log(setMax), log(logcut), length.out = 5)); x <- c(x, rev(-x))
        legend_labels <- if (isTRUE(signColor)) rev(x) else {
          y <- exp(seq(log(setMax), log(logcut), length.out = 10)); rev(y)
        }
      } else {
        x <- seq(setMax, 0, length.out = 5); x <- c(x, rev(-x))
        legend_labels <- if (isTRUE(signColor)) rev(x) else rev(seq(setMax, 0, length.out = 10))
      }
      legend_labels <- signif(legend_labels, 3)
      legend_labels <- sort(legend_labels, decreasing = TRUE)
    }
    # --- rglwidget ---
    w <- rgl::rglwidget(minimal = FALSE, width = widget_size_px, height = widget_size_px)
    # --- legend title text (dynamic) ---
    legend_title <- if (isTRUE(logColors)) "Log Dirichlet Normal Density" else "Dirichlet Normal Density"
    # --- HTML legend (vertical, right; centered title) ---
    legend_html_vertical <- function(labels, logColors, signColor, cex = 1, magnify = 1,
                                     bar_height_px = round(widget_size_px / 2),
                                     bar_width_px = 36) {
      n <- 101
      x <- seq(0, 1, length.out = n)
      cols <- if (isTRUE(signColor)) signedcolor.gradient(x) else ccolor.gradient(x)
      cols <- rev(cols) # keep positive/high at TOP
      pct <- seq(0, 100, length.out = n)
      stops <- paste0(cols, " ", sprintf("%.2f%%", pct))
      gradient_css <- paste0("linear-gradient(180deg,", paste(stops, collapse = ","), ")")
      font_px <- 12 * cex * magnify
      htmltools::div(
        style = paste0(
          "display:flex; flex-direction:column; align-items:stretch; gap:6px;"
        ),
        # centered title
        htmltools::div(
          style = paste0(
            "font-weight:600; font-size:", round(font_px * 1.05), "px;",
            "line-height:1.2; text-align:center;"
          ),
          legend_title
        ),
        # bar + ticks
        htmltools::div(
          style = "display:flex; flex-direction:row; align-items:stretch; gap:8px;",
          # color bar
          htmltools::div(
            style = paste0(
              "width:", bar_width_px, "px; min-width:", bar_width_px, "px;",
              "border:1px solid #999; background:", gradient_css, "; ",
              "height:", bar_height_px, "px;"
            )
          ),
          # ticks stacked (labels already sorted high -> low)
          htmltools::div(
            style = paste0(
              "display:flex; flex-direction:column; justify-content:space-between; ",
              "font-size:", sprintf("%.0fpx;", font_px),
              "line-height:1.1; color:#222; white-space:nowrap;"
            ),
            lapply(labels, function(z) htmltools::tags$span(format(z)))
          )
        )
      )
    }
    # --- title + side-by-side layout ---
    title_div <- if (nzchar(main)) {
      htmltools::div(
        style = paste0(
          "font-weight:700;",
          "font-size:", title_font_size_px, "px;",
          "line-height:1.25;",
          "margin:0 0 10px 0;",
          "text-align:center;"
        ),
        main
      )
    } else NULL
    # right-column legend width (text + padding)
    legend_panel_px <- max(140, round(70 * cex * legend_magnify))
    # layout row
    row_div <- if (isTRUE(legend)) {
      htmltools::div(
        style = paste0(
          "display:flex; flex-direction:row; align-items:center; gap:12px; ",
          "width:", widget_size_px + legend_panel_px, "px; margin:0 auto;"
        ),
        # left: 3D scene
        htmltools::div(
          style = paste0("width:", widget_size_px, "px; height:", widget_size_px, "px;"),
          w
        ),
        # right: vertical legend (centered vertically; dynamic title)
        htmltools::div(
          style = paste0("width:", legend_panel_px, "px;"),
          legend_html_vertical(legend_labels, logColors, signColor,
                               cex = cex, magnify = legend_magnify)
        )
      )
    } else {
      htmltools::div(
        style = paste0("width:", widget_size_px, "px; margin:0 auto;"),
        w
      )
    }
    container <- htmltools::div(
      style = "width:100%;",
      title_div,
      row_div
    )
    # --- optional: save colored PLY ---
    if (!is.na(fileName)) {
      if (!is.character(fileName) || length(fileName) != 1L)
        stop("Enter a single character string for fileName.")
      if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) != ".ply") {
        fileName <- paste0(fileName, ".ply")
      }
      plyFile$material$color <- DNE_colors
      Rvcg::vcgPlyWrite(mesh = plyFile, filename = fileName, binary = binary)
      if (!binary) {
        path <- file.path(getwd(), fileName)
        txt <- readLines(con = path, warn = FALSE)
        com <- paste("comment DNE plot generated in molaR",
                     utils::packageVersion("molaR"), "for", R.version.string)
        com <- unlist(strsplit(com, split = "\n"))
        out <- c(txt[1:3], com, txt[4:length(txt)])
        writeLines(out, con = path)
      }
    }
    htmltools::browsable(container)
  }
}
