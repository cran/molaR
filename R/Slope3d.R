#' Plot results of a Slope analysis of a surface (HTML widget)
#'
#' A function that produces a three-dimensional HTML-based rendering of surface slope.
#' The `Slope()` function must be performed prior to using `Slope3d()`.
#'
#' @param Slope_File An object that stores the output of the Slope function
#' @param colors Character vector of colors to build a continuous color gradient
#' @param maskNegatives Logical; if TRUE, negative slopes are masked in black.
#' If FALSE, negatives are reflected into the positive range (original behavior).
#' @param legend Logical; draw a legend (right side of the widget)
#' @param main Plot title
#' @param cex Base scaling for legend/ticks (relative multiplier)
#' @param widget_size_px Square widget size in pixels (default 768)
#' @param scene_zoom Initial zoom for the 3d scene (default 1.5)
#' @param leftOffset Horizontal camera nudge (-1..1 recommended)
#' @param fieldofview Field of view in degrees (0 = isometric)
#' @param title_font_size_px Title font size in pixels (default 30)
#' @param legend_magnify Additional scale factor for the legend/tick text
#' @param fileName Optional file base name to save a colored *.ply (no legend)
#' @param binary Binary PLY if TRUE (smaller files)
#'
#' @details
#' Colors represent face slope magnitudes. With `maskNegatives = TRUE`, negative
#' slopes are shown in black. With `maskNegatives = FALSE`, negative magnitudes
#' are reflected into the positive range for coloring continuity (original behavior).
#'
#' The plotting window is an HTML widget (no Quartz/rgl window). The legend is
#' rendered in HTML/CSS and aligned vertically at the right of the scene.
#'
#' @import grDevices graphics utils
#' @import htmltools
#' @export
#' @examples
#' s <- Slope(Tooth)
#' if(interactive()){Slope3d(s)}
Slope3d <- function(
  Slope_File,
  colors = c("blue", "cornflowerblue", "green", "yellowgreen", "yellow", "orangered", "red"),
  maskNegatives = TRUE,
  legend = TRUE,
  main = "",
  cex = 1,
  widget_size_px = 768,
  scene_zoom = 1.5,
  leftOffset = 0,
  fieldofview = 0,
  title_font_size_px = 30,
  legend_magnify = 1,
  fileName = NA,
  binary = FALSE
) {
  old_opts <- options("rgl.useNULL", "rgl.printRglwidget")
  options(rgl.useNULL = TRUE, rgl.printRglwidget = FALSE)
  on.exit(options(old_opts), add = TRUE)
  plyFile <- Slope_File$plyFile
  face_slopes <- Slope_File$Face_Slopes
  if (!inherits(plyFile, "mesh3d")) stop("Slope_File$plyFile must be an rgl 'mesh3d'.")
  if (!is.logical(maskNegatives) || length(maskNegatives) != 1L) stop("maskNegatives must be TRUE/FALSE.")
  if (!is.logical(legend) || length(legend) != 1L) stop("legend must be TRUE/FALSE.")
  if (!is.numeric(cex) || cex <= 0) stop("cex must be > 0.")
  if (!is.numeric(scene_zoom) || scene_zoom <= 0) stop("scene_zoom must be > 0.")
  if (!is.numeric(widget_size_px) || widget_size_px <= 0) stop("widget_size_px must be > 0.")
  if (!is.numeric(title_font_size_px) || title_font_size_px <= 0) stop("title_font_size_px must be > 0.")
  if (!is.numeric(legend_magnify) || legend_magnify <= 0) stop("legend_magnify must be > 0.")
  if (!is.numeric(leftOffset) || length(leftOffset) != 1L) stop("leftOffset must be numeric length 1.")
  colorsfunc <- grDevices::colorRamp(colors)
  color_range <- face_slopes / 90
  if (isFALSE(maskNegatives)) {
    idx <- which(color_range > 1)
    if (length(idx)) color_range[idx] <- abs(1 - (color_range[idx] - 1))
    col_mat <- colorsfunc(abs(color_range))
  } else {
    col_mat <- colorsfunc(color_range)
    na_rows <- is.na(col_mat[, 1])
    if (any(na_rows)) col_mat[na_rows, ] <- c(0, 0, 0)
  }
  slope_colors <- apply(
    col_mat, 1,
    function(x) grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255)
  )
  vb <- plyFile$vb
  mesh_centered <- plyFile
  xr <- range(vb[1, ], na.rm = TRUE)
  zr <- range(vb[3, ], na.rm = TRUE)
  T_center_xz <- rgl::translationMatrix(-mean(xr), 0, -mean(zr))
  mesh_centered <- rgl::transform3d(mesh_centered, T_center_xz)
  LEFT_BIAS_FRAC <- 0.08
  vb2 <- mesh_centered$vb
  xr2 <- range(vb2[1, ], na.rm = TRUE)
  x_width <- diff(xr2)
  x_left <- -LEFT_BIAS_FRAC * x_width
  T_left <- rgl::translationMatrix(x_left, 0, 0)
  mesh_centered <- rgl::transform3d(mesh_centered, T_left)
  vb3 <- mesh_centered$vb
  yr <- range(vb3[2, ], na.rm = TRUE)
  T_center_y <- rgl::translationMatrix(0, -mean(yr), 0)
  mesh_centered <- rgl::transform3d(mesh_centered, T_center_y)
  if (interactive()) {
    rgl::open3d()
    rgl::par3d(windowRect = c(100, 100, 100 + widget_size_px, 100 + widget_size_px))
    rgl::par3d(userMatrix = diag(4), zoom = 1)
    rgl::shade3d(mesh_centered, color = slope_colors, meshColor = "faces", shininess = 110)
    rgl::view3d(fov = fieldofview)
    rgl::aspect3d("iso")
    rgl::par3d(zoom = scene_zoom)
    rgl::bg3d(col = "white")
    if (!isTRUE(all.equal(leftOffset, 0))) {
      ZView <- rgl::par3d("observer")[3]
      XView <- leftOffset * ZView * 0.05
      rgl::observer3d(XView, 0, ZView)
    }
    legend_labels <- NULL
    if (isTRUE(legend)) {
      legend_labels <- seq(90, 0, length.out = 6)
      legend_labels <- format(round(legend_labels, 0))
    }
    w <- rgl::rglwidget(minimal = FALSE, width = widget_size_px, height = widget_size_px)
    legend_html_vertical <- function(labels, palette, cex = 1, magnify = 1,
                                     bar_height_px = round(widget_size_px / 2),
                                     bar_width_px = 36,
                                     show_neg_mask_note = FALSE) {
      n <- 101L
      cols <- grDevices::colorRampPalette(palette)(n)
      cols <- rev(cols)
      pct <- seq(0, 100, length.out = n)
      stops <- paste0(cols, " ", sprintf("%.2f%%", pct))
      gradient_css <- paste0("linear-gradient(180deg,", paste(stops, collapse = ","), ")")
      font_px <- 12 * cex * magnify
      note_div <- if (isTRUE(show_neg_mask_note)) {
        htmltools::div(
          style = paste0("font-size:", round(font_px * 0.92), "px; color:#444;"),
          htmltools::tags$span(
            htmltools::tags$span(
              style = "display:inline-block;width:10px;height:10px;background:#000;margin-right:6px;border:1px solid #999;"
            ),
            "negative slopes masked"
          )
        )
      } else NULL
      htmltools::div(
        style = paste0("display:flex; flex-direction:column; align-items:stretch; gap:6px;"),
        htmltools::div(
          style = paste0("font-weight:600; font-size:", round(font_px * 1.05), "px;",
                         "line-height:1.2; text-align:center;"),
          "Slope (deg)"
        ),
        htmltools::div(
          style = "display:flex; flex-direction:row; align-items:stretch; gap:8px;",
          htmltools::div(
            style = paste0(
              "width:", bar_width_px, "px; min-width:", bar_width_px, "px;",
              "border:1px solid #999; background:", gradient_css, "; ",
              "height:", bar_height_px, "px;"
            )
          ),
          htmltools::div(
            style = paste0(
              "display:flex; flex-direction:column; justify-content:space-between; ",
              "font-size:", sprintf("%.0fpx;", font_px),
              "line-height:1.1; color:#222; white-space:nowrap;"
            ),
            lapply(labels, function(z) htmltools::tags$span(paste0(z, " deg")))
          )
        ),
        note_div
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
    legend_panel_px <- max(140, round(70 * cex * legend_magnify))
    show_neg_note <- isTRUE(maskNegatives) && any(is.na(colorsfunc(face_slopes / 90)[, 1]))
    row_div <- if (isTRUE(legend)) {
      htmltools::div(
        style = paste0(
          "display:flex; flex-direction:row; align-items:center; gap:12px; ",
          "width:", widget_size_px + legend_panel_px, "px; margin:0 auto;"
        ),
        htmltools::div(
          style = paste0("width:", widget_size_px, "px; height:", widget_size_px, "px;"),
          w
        ),
        htmltools::div(
          style = paste0("width:", legend_panel_px, "px;"),
          legend_html_vertical(
            labels = legend_labels,
            palette = colors,
            cex = cex,
            magnify = legend_magnify,
            show_neg_mask_note = show_neg_note
          )
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
    if (!is.na(fileName)) {
      if (!is.character(fileName) || length(fileName) != 1L)
        stop("Enter a single character string for fileName.")
      if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) != ".ply") {
        fileName <- paste0(fileName, ".ply")
      }
      OutPly <- plyFile
      NewVertList <- plyFile$vb[, plyFile$it[1:length(plyFile$it)]]
      NewNormList <- plyFile$normals[, plyFile$it[1:length(plyFile$it)]]
      NewFaceList <- matrix(1:ncol(NewVertList), nrow = 3)
      colormatrix <- matrix(rep(slope_colors, 3), nrow = 3, byrow = TRUE)
      NewColorList <- colormatrix[1:length(colormatrix)]
      OutPly$vb <- NewVertList
      OutPly$it <- NewFaceList
      OutPly$normals <- NewNormList
      OutPly$material$color <- NewColorList
      Rvcg::vcgPlyWrite(mesh = OutPly, filename = fileName, binary = binary)
      if (!binary) {
        path <- file.path(getwd(), fileName)
        txt <- readLines(con = path, warn = FALSE)
        com <- paste("comment Slope plot generated in molaR",
                     utils::packageVersion("molaR"), "for", R.version.string)
        com <- unlist(strsplit(com, split = "\n"))
        out <- c(txt[1:3], com, txt[4:length(txt)])
        writeLines(out, con = path)
      }
    }
    htmltools::browsable(container)
  }
}
