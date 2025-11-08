#' Plot Area Relative Curvature (ARC) on a surface (HTML widget; continuous bar legend)
#'
#' Creates a three-dimensional rendering of Area Relative Curvature (ARC)
#' on a mesh and returns an HTML widget with a continuous, vertical color-bar
#' legend. Curvature is split into four negative and four positive bands
#' (quantiles on each side of zero) for per-face coloring. The legend uses a
#' single blended bar assembled from the same eight colors used on the surface,
#' with negative values at the bottom and positive values at the top.
#'
#' Run ARC() first to compute per-face ARC values.
#'
#' @param ARC_object An object that stores the output of ARC(), including
#'   $Mean_Face_Curvature and $plyFile (an rgl::mesh3d).
#' @param colors Character vector of length >= 8 with the eight display colors
#'   for the curvature bands. Defaults match the classic ARC3d:
#'   c("darkblue","blue","powderblue","gray","gray","tan","orange","darkorange1").
#'   These eight colors are used both for the surface and for the legend bar.
#' @param main Character; plot title (default "")
#' @param cex Numeric; relative size multiplier for legend text (default 1)
#' @param legend Logical; show legend panel (default TRUE)
#' @param widget_size_px Integer; square size of the 3D widget in pixels (default 768)
#' @param scene_zoom Numeric; initial zoom on the 3D scene (default 1.5)
#' @param leftOffset Numeric (-1..1); small horizontal camera nudge (default 0)
#' @param fieldofview Numeric; field of view in degrees; 0 = isometric (default 0)
#' @param title_font_size_px Integer; title font size in pixels (default 30)
#' @param legend_magnify Numeric; additional legend scale factor (default 1)
#' @param legendTextCol Legend text color (default "black")
#' @param legendLineCol Legend border color for the bar box (default "black")
#' @param fileName Character or NA; if non-NA, write a colorized *.ply to disk (default NA)
#' @param binary Logical; write PLY as binary (default FALSE, i.e., ascii)
#'
#' @details
#' Binning scheme:
#' The mean face curvature values are split by sign. For negative values, the
#' empirical quartiles (Q1..Q3) define three internal boundaries, creating four
#' bins below zero; the same is done for positive values above zero. The eight
#' bins map to the eight provided colors. The legend bar is a continuous CSS
#' gradient built from those same colors so that the bar visually matches the
#' surface and blends smoothly from one band to the next. Tick labels are shown
#' at LowQs, 0, and HighQs (top to bottom = high to low).
#'
#' @return An htmltools::browsable object containing the title, 3D widget, and legend.
#'
#' @examples
#' # ARC_out <- ARC(Tooth)
#' if(interactive()){ARC3d(ARC_out, main = "Area Relative Curvature")}
#'
#' @import htmltools grDevices utils
#' @importFrom Rvcg vcgPlyWrite
#' @export
ARC3d <- function(
  ARC_object,
  main = "",
  cex = 1,
  colors = c("darkblue","blue","powderblue","gray","gray","tan","orange","darkorange1"),
  fieldofview = 0,
  legend = TRUE,
  widget_size_px = 768,
  scene_zoom = 1.5,
  leftOffset = 0,
  title_font_size_px = 30,
  legend_magnify = 1,
  legendTextCol = "black",
  legendLineCol = "black",
  fileName = NA,
  binary = FALSE
) {
  old_opts <- options("rgl.useNULL", "rgl.printRglwidget")
  options(rgl.useNULL = TRUE, rgl.printRglwidget = FALSE)
  on.exit(options(old_opts), add = TRUE)
  if (!is.numeric(cex) || cex <= 0) stop("cex must be > 0.")
  if (!is.numeric(widget_size_px) || widget_size_px <= 0) stop("widget_size_px must be > 0.")
  if (!is.numeric(scene_zoom) || scene_zoom <= 0) stop("scene_zoom must be > 0.")
  if (!is.numeric(legend_magnify) || legend_magnify <= 0) stop("legend_magnify must be > 0.")
  if (!is.numeric(leftOffset) || length(leftOffset) != 1L) stop("leftOffset must be numeric length 1.")
  if (!is.logical(legend) || length(legend) != 1L) stop("legend must be TRUE/FALSE.")
  if (length(colors) < 8) {
    warning("Provided 'colors' has < 8 entries; recycling to length 8.")
    colors <- rep(colors, length.out = 8)
  } else if (length(colors) > 8) {
    colors <- colors[1:8]
  }
  ply <- ARC_object$plyFile
  if (!inherits(ply, "mesh3d")) stop("ARC_object$plyFile must be an rgl mesh3d.")
  curv <- ARC_object$Mean_Face_Curvature
  pos_idx <- which(curv >= 0)
  neg_idx <- which(curv < 0)
  HighQs <- if (length(pos_idx)) stats::quantile(curv[pos_idx], na.rm = TRUE) else rep(0, 5)
  LowQs <- if (length(neg_idx)) stats::quantile(curv[neg_idx], na.rm = TRUE) else rep(0, 5)
  cols_face <- character(length(curv))
  if (length(neg_idx)) {
    cols_face[curv <= LowQs[2]] <- colors[1]
    cols_face[curv > LowQs[2] & curv <= LowQs[3]] <- colors[2]
    cols_face[curv > LowQs[3] & curv <= LowQs[4]] <- colors[3]
    cols_face[curv < 0 & curv > LowQs[4]] <- colors[4]
  }
  if (length(pos_idx)) {
    cols_face[curv >= 0 & curv < HighQs[2]] <- colors[5]
    cols_face[curv >= HighQs[2] & curv < HighQs[3]] <- colors[6]
    cols_face[curv >= HighQs[3] & curv < HighQs[4]] <- colors[7]
    cols_face[curv >= HighQs[4]] <- colors[8]
  }
  cols_face[is.na(cols_face)] <- "gray80"
  mesh_centered <- ply
  vb <- mesh_centered$vb
  xr <- range(vb[1, ], na.rm = TRUE)
  zr <- range(vb[3, ], na.rm = TRUE)
  T_center_xz <- rgl::translationMatrix(-mean(xr), 0, -mean(zr))
  mesh_centered <- rgl::transform3d(mesh_centered, T_center_xz)
  {
    LEFT_BIAS_FRAC <- 0.08
    vb2 <- mesh_centered$vb
    xr2 <- range(vb2[1, ], na.rm = TRUE)
    x_left <- -LEFT_BIAS_FRAC * diff(xr2)
    mesh_centered <- rgl::transform3d(mesh_centered, rgl::translationMatrix(x_left, 0, 0))
  }
  vb3 <- mesh_centered$vb
  yr <- range(vb3[2, ], na.rm = TRUE)
  T_center_y <- rgl::translationMatrix(0, -mean(yr), 0)
  mesh_centered <- rgl::transform3d(mesh_centered, T_center_y)
  if (interactive()) {
    rgl::open3d()
    rgl::par3d(windowRect = c(100, 100, 100 + widget_size_px, 100 + widget_size_px))
    rgl::par3d(userMatrix = diag(4), zoom = 1)
    rgl::shade3d(mesh_centered, color = cols_face, meshColor = "faces", shininess = 110)
    rgl::view3d(fov = fieldofview)
    rgl::aspect3d("iso")
    rgl::par3d(zoom = scene_zoom)
    rgl::bg3d(col = "white")
    if (!isTRUE(all.equal(leftOffset, 0))) {
      ZView <- rgl::par3d("observer")[3]
      XView <- leftOffset * ZView * 0.05
      rgl::observer3d(XView, 0, ZView)
    }
    w <- rgl::rglwidget(minimal = FALSE, width = widget_size_px, height = widget_size_px)
    fmt_num <- function(x) {
      if (is.na(x) || is.infinite(x)) return(format(x))
      formatC(x, format = "g", digits = 3)
    }
    tick_vals <- c(HighQs[4], HighQs[3], HighQs[2], 0, LowQs[4], LowQs[3], LowQs[2])
    tick_labels <- sapply(tick_vals, fmt_num, USE.NAMES = FALSE)
    legend_html_vertical <- function(labels, palette, cex = 1, magnify = 1,
                                     bar_height_px = round(widget_size_px / 2),
                                     bar_width_px = 36) {
      n <- 101L
      cols <- grDevices::colorRampPalette(palette)(n)
      cols <- rev(cols)
      pct <- seq(0, 100, length.out = n)
      stops <- paste0(cols, " ", sprintf("%.2f%%", pct))
      gradient_css <- paste0("linear-gradient(180deg,", paste(stops, collapse = ","), ")")
      font_px <- 12 * cex * magnify
      htmltools::div(
        style = paste0("display:flex; flex-direction:column; align-items:stretch; gap:6px;"),
        htmltools::div(
          style = paste0("font-weight:600; font-size:", round(font_px * 1.05), "px;",
                         "line-height:1.2; text-align:center; color:", legendTextCol, ";"),
          "Area Relative Curvature"
        ),
        htmltools::div(
          style = "display:flex; flex-direction:row; align-items:stretch; gap:8px;",
          htmltools::div(
            style = paste0(
              "width:", bar_width_px, "px; min-width:", bar_width_px, "px;",
              "border:1px solid ", legendLineCol, "; background:", gradient_css, "; ",
              "height:", bar_height_px, "px;"
            )
          ),
          htmltools::div(
            style = paste0(
              "display:flex; flex-direction:column; justify-content:space-between; ",
              "font-size:", sprintf("%.0fpx;", font_px),
              "line-height:1.1; color:", legendTextCol, "; white-space:nowrap;"
            ),
            lapply(labels, function(z) htmltools::tags$span(z))
          )
        )
      )
    }
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
    legend_panel_px <- max(160, round(120 * cex * legend_magnify))
    row_div <- if (isTRUE(legend)) {
      htmltools::div(
        style = paste0(
          "display:flex; flex-direction:row; align-items:center; gap:12px;",
          "width:", widget_size_px + legend_panel_px, "px; margin:0 auto;"
        ),
        htmltools::div(
          style = paste0("width:", widget_size_px, "px; height:", widget_size_px, "px;"),
          w
        ),
        htmltools::div(
          style = paste0("width:", legend_panel_px, "px;"),
          legend_html_vertical(
            labels = tick_labels,
            palette = colors,
            cex = cex,
            magnify = legend_magnify
          )
        )
      )
    } else {
      htmltools::div(
        style = paste0("width:", widget_size_px, "px; margin:0 auto;"),
        w
      )
    }
    container <- htmltools::div(style = "width:100%;", title_div, row_div)
    if (!is.na(fileName)) {
      if (!is.character(fileName) || length(fileName) != 1L)
        stop("Enter a single character string for fileName.")
      if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) != ".ply")
        fileName <- paste0(fileName, ".ply")
      ply$material$color <- cols_face
      Rvcg::vcgPlyWrite(mesh = ply, filename = fileName, binary = binary)
      if (!binary) {
        path <- file.path(getwd(), fileName)
        txt <- readLines(con = path, warn = FALSE)
        com <- paste(
          "comment ARC plot generated in molaR",
          utils::packageVersion("molaR"), "for", R.version.string
        )
        com <- unlist(strsplit(com, split = "\n"))
        out <- c(txt[1:3], com, txt[4:length(txt)])
        writeLines(out, con = path)
      }
    }
    htmltools::browsable(container)
  }
}
