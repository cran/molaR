#' Plot results of OPC analysis of a surface (3D HTML widget; numbered legend; distinct colors)
#'
#' Produces a three-dimensional rendering of face orientation on a surface and
#' returns an HTML widget with a clean, numbered pie legend
#' Wedges can be scaled by patch counts. Default colors emphasize clarity.
#' 
#'
#' Run `OPC()` first to compute patches and directional bins.
#'
#' @param OPC_File An object that stores the output of `OPC()`
#' @param binColors Optional character vector of colors for each directional bin. If not supplied,
#' a high-contrast set is generated automatically (Okabe–Ito <= 12 bins; vivid HCL rainbow for more).
#' @param patchOutline Logical; draw patch boundary outlines on the mesh (default `FALSE`)
#' @param outlineColor Color for patch outlines (default `"black"`)
#' @param maskDiscard Logical; color discarded/small patches black in the mesh (default `FALSE`)
#' @param legend Logical; show a right-hand legend (default `TRUE`)
#' @param main Character; plot title (default `""`)
#' @param cex Numeric; relative size multiplier for legend text (default `1`)
#' @param scaleLegend Logical; scale legend pie wedges by patch counts (default `FALSE`)
#' @param legendTextCol Color for legend text (default `"black"`)
#' @param legendLineCol Color for legend ring/lines (default `"black"`)
#' @param leftOffset Numeric (-1..1); small horizontal camera nudge (default `0`)
#' @param fieldofview Numeric; field of view in degrees; 0 = isometric (default `0`)
#' @param widget_size_px Integer; square size of the 3D widget in pixels (default `768`)
#' @param scene_zoom Numeric; initial zoom on the 3D scene (default `1.5`)
#' @param title_font_size_px Integer; title font size in pixels (default `30`)
#' @param title_font_family CSS font-family for title/legend (default system UI stack)
#' @param legend_magnify Numeric; additional legend scale factor (default `1`)
#' @param fileName Character or `NA`; if non-`NA`, write a colored *.ply to disk (default `NA`)
#' @param binary Logical; write PLY as binary (default `FALSE`, i.e., ascii)
#'
#' @details
#'
#' **Legend**
#' Wedges are **numbered (1..N)** and can be **scaled** to patch counts.
#'
#' **Saving a PLY**
#' When `fileName` is provided, a colorized PLY is written. Colors are assigned per face via a
#' vertex-duplication strategy so external tools (e.g., MeshLab) show the same appearance. For ascii
#' PLY, a comment line notes the generator/version.
#'
#' @return
#' - **Interactive**: an `htmltools`-browsable object (title, 3D widget, legend).
#' - **Non-interactive**: (invisibly) a list with class `"OPC3d_spec"` containing:
#'   `title`, `bin_colors`, `face_colors`, `bin_sizes`, `bin_labels`, `legend`, `scaled`.
#'
#' @examples
#' # OPC_out <- OPC(Tooth)
#' if (interactive()) {
#'   OPC3d(OPC_out, legend = TRUE, scaleLegend = TRUE,
#'         main = "OPC -- Numbered Bins")
#' }
#'
#' @import htmltools grDevices utils
#' @importFrom Rvcg vcgPlyWrite
#' @export
OPC3d <- function(
  OPC_File,
  binColors = NULL,
  patchOutline = FALSE, outlineColor = "black", maskDiscard = FALSE,
  legend = TRUE, main = "", cex = 1, scaleLegend = FALSE,
  legendTextCol = "black", legendLineCol = "black",
  leftOffset = 0, fieldofview = 0,
  widget_size_px = 768, scene_zoom = 1.5,
  title_font_size_px = 30,
  title_font_family = "system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif",
  legend_magnify = 1,
  fileName = NA, binary = FALSE
) {
  # --- input checks -----------------------------------------------------------
  if (!is.numeric(cex) || cex <= 0) stop("cex must be > 0.")
  if (!is.numeric(widget_size_px) || widget_size_px <= 0) stop("widget_size_px must be > 0.")
  if (!is.numeric(scene_zoom) || scene_zoom <= 0) stop("scene_zoom must be > 0.")
  if (!is.numeric(legend_magnify) || legend_magnify <= 0) stop("legend_magnify must be > 0.")
  if (!is.numeric(leftOffset) || length(leftOffset) != 1L) stop("leftOffset must be numeric length 1.")
  if (!is.logical(legend) || length(legend) != 1L) stop("legend must be TRUE/FALSE.")
  if (!is.logical(patchOutline) || length(patchOutline) != 1L) stop("patchOutline must be TRUE/FALSE.")
  if (!is.logical(maskDiscard) || length(maskDiscard) != 1L) stop("maskDiscard must be TRUE/FALSE.")

  PD  <- OPC_File$Patch_Details
  ply <- OPC_File$plyFile
  if (!inherits(ply, "mesh3d")) stop("OPC_File$plyFile must be an rgl mesh3d.")

  # --- Bin sizes (as in original) --------------------------------------------
  bins_vec <- numeric(length = length(PD))
  param    <- OPC_File$Parameters
  criteria <- which(param != 0)

  if (length(criteria) == 1) {
    criteria <- param[[2]]
    for (i in seq_along(PD)) {
      legit         <- which(PD[[i]][, 1] >= criteria)
      bins_vec[i]   <- sum(PD[[i]][legit, 2])
      names(bins_vec) <- names(PD)
    }
  }
  if (length(criteria) == 2) {
    criteria <- param[[3]]
    for (i in seq_along(PD)) {
      legit         <- which(PD[[i]][, 2] >= criteria)
      bins_vec[i]   <- sum(PD[[i]][legit, 2])
      names(bins_vec) <- names(PD)
    }
  }
  BinSizes <- bins_vec[order(names(bins_vec))]

  # --- Per-face color assignment ---------------------------------------------
  dir_bins  <- ply$Directional_Bins
  BinCount  <- length(unique(dir_bins))

  if (is.null(binColors)) {
    okabe_ito_12 <- c(
      "#E69F00", "#56B4E9", "#009E73", "#F0E442",
      "#0072B2", "#D55E00", "#CC79A7", "#999999",
      "#8DD3C7", "#FB8072", "#80B1D3", "#FDB462"
    )
    if (BinCount <= length(okabe_ito_12)) {
      binColors <- okabe_ito_12[seq_len(BinCount)]
    } else {
      binColors <- grDevices::hcl(
        h = seq(15, 375, length.out = BinCount + 1)[-1],
        c = 100, l = 60
      )
    }
  }
  if (length(binColors) < BinCount) {
    binColors <- c(binColors, rep("#FFFFFF", BinCount - length(binColors)))
  }

  # Mask discarded patches if requested
  BlackPatch <- integer(0)
  if (isTRUE(maskDiscard)) {
    for (i in seq_len(BinCount)) {
      if (OPC_File$Parameters$Minimum_Area == 0) {
        PatchList  <- unlist(OPC_File$Patches[i], recursive = FALSE)
        SmallPatch <- names(which(lapply(PatchList, length) < OPC_File$Parameters$Minimum_Faces))
        Discarded  <- as.numeric(unlist(PatchList[SmallPatch]))
      } else {
        AreaList          <- as.vector(OPC_File$Patch_Details[[i]][, 2])
        MinAreaPercentage <- sum(OPC_File$plyFile$Face_Areas) * OPC_File$Parameters$Minimum_Area
        SmallPatchList    <- which(AreaList < MinAreaPercentage)
        Discarded         <- as.numeric(unlist(OPC_File$Patches[[i]][SmallPatchList]))
      }
      BlackPatch <- c(BlackPatch, Discarded)
    }
  }

  face_cols <- vapply(dir_bins, function(b) binColors[b], character(1))
  if (isTRUE(maskDiscard) && length(BlackPatch)) face_cols[BlackPatch] <- "#000000"

  # --- Optional: write colored PLY (works in both modes) ---------------------
  if (!is.na(fileName)) {
    if (!is.character(fileName) || length(fileName) != 1L)
      stop("Enter a single character string for fileName.")
    if (substr(fileName, nchar(fileName) - 3, nchar(fileName)) != ".ply")
      fileName <- paste0(fileName, ".ply")

    OutPly        <- ply
    NewVertList   <- ply$vb[, ply$it[1:length(ply$it)]]
    NewNormList   <- ply$normals[, ply$it[1:length(ply$it)]]
    NewFaceList   <- matrix(1:ncol(NewVertList), nrow = 3)
    col_mat       <- matrix(rep(face_cols, 3), nrow = 3, byrow = TRUE)
    NewColorList  <- col_mat[1:length(col_mat)]

    OutPly$vb               <- NewVertList
    OutPly$it               <- NewFaceList
    OutPly$normals          <- NewNormList
    OutPly$material$color   <- NewColorList

    Rvcg::vcgPlyWrite(mesh = OutPly, filename = fileName, binary = binary)
    if (!binary) {
      path  <- file.path(getwd(), fileName)
      txt   <- readLines(con = path, warn = FALSE)
      NewCom <- paste(
        "comment OPC plot generated in molaR",
        utils::packageVersion("molaR"),
        "for", R.version.string
      )
      NewOut <- c(txt[1:3], unlist(strsplit(NewCom, "\n")), txt[4:length(txt)])
      writeLines(NewOut, con = path)
    }
  }

  # --- INTERACTIVE BLOCK: all HTML/UI calls are confined here ----------------
  if (interactive()) {
    # Isolate any rgl device interaction to interactive sessions
    old_opts <- options("rgl.useNULL", "rgl.printRglwidget")
    options(rgl.useNULL = TRUE, rgl.printRglwidget = FALSE)
    on.exit(options(old_opts), add = TRUE)

    # Prepare a centered mesh for viewing (no device side-effects)
    mesh_centered <- ply
    vb <- mesh_centered$vb
    xr <- range(vb[1, ], na.rm = TRUE)
    zr <- range(vb[3, ], na.rm = TRUE)
    T_center_xz   <- rgl::translationMatrix(-mean(xr), 0, -mean(zr))
    mesh_centered <- rgl::transform3d(mesh_centered, T_center_xz)

    # small left bias to leave room for legend
    LEFT_BIAS_FRAC <- 0.08
    vb2     <- mesh_centered$vb
    xr2     <- range(vb2[1, ], na.rm = TRUE)
    x_left  <- -LEFT_BIAS_FRAC * diff(xr2)
    mesh_centered <- rgl::transform3d(mesh_centered, rgl::translationMatrix(x_left, 0, 0))

    vb3 <- mesh_centered$vb
    yr  <- range(vb3[2, ], na.rm = TRUE)
    T_center_y   <- rgl::translationMatrix(0, -mean(yr), 0)
    mesh_centered <- rgl::transform3d(mesh_centered, T_center_y)

    # --- Scene (rgl) ---------------------------------------------------------
    rgl::open3d()
    rgl::par3d(windowRect = c(100, 100, 100 + widget_size_px, 100 + widget_size_px))
    rgl::par3d(userMatrix = diag(4), zoom = 1)
    rgl::shade3d(mesh_centered, color = face_cols, meshColor = "faces", shininess = 110)
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

    # Optional patch outlines
    if (isTRUE(patchOutline)) {
      for (i in seq_len(BinCount)) {
        Orientation <- OPC_File$Patches[i]
        PatchCount  <- length(Orientation[[1]])
        if (PatchCount == 0) next
        for (j in seq_len(PatchCount)) {
          Patch  <- as.numeric(Orientation[[1]][[j]])
          Faces  <- t(mesh_centered$it[, Patch])
          fnum   <- nrow(Faces)
          vorder <- lapply(seq_len(fnum), function(k) sort(Faces[k, ]))
          edges  <- lapply(
            vorder,
            function(o)
              c(paste(o[1], o[2], sep = "_"),
                paste(o[1], o[3], sep = "_"),
                paste(o[2], o[3], sep = "_"))
          )
          string     <- unlist(edges)
          edgeframe  <- data.frame(names = string)
          UniqueEdge <- aggregate(edgeframe, list(edgeframe$names), FUN = length)
          PatchEdge  <- subset(UniqueEdge, UniqueEdge$names == 1)
          if (!nrow(PatchEdge)) next
          EdgeVerts  <- as.numeric(unlist(strsplit(as.character(unlist(PatchEdge$Group.1)), "_")))
          EdgeCoords <- mesh_centered$vb[1:3, EdgeVerts]
          rgl::segments3d(t(EdgeCoords), color = outlineColor, lwd = 1.25, shininess = 120)
        }
      }
    }

    # Build the widget
    w <- rgl::rglwidget(minimal = FALSE, width = widget_size_px, height = widget_size_px)

    # --- Legend/UI helpers (HTML only inside interactive) --------------------
    svg_pie_legend <- function(colors, sizes, labels = NULL,
                               scaleLegend = FALSE,
                               text_col = "black", line_col = "black",
                               size_px = round(260 * legend_magnify),
                               cex = 1, font_family = title_font_family) {
      n <- length(colors)
      if (is.null(labels) || length(labels) != n) labels <- as.character(seq_len(n))
      total   <- if (isTRUE(scaleLegend)) sum(sizes) else n
      weights <- if (isTRUE(scaleLegend)) sizes else rep(1, n)
      if (total <= 0) { total <- n; weights <- rep(1, n) }
      pad_px <- 14
      cx <- cy <- size_px/2
      R  <- size_px/2 - pad_px
      stroke_w_sep  <- 2.2  # white separators
      stroke_w_ring <- 1.2  # outer ring
      angs <- c(0, cumsum(weights) / total * 2*pi) - pi/2  # start at -pi/2 (top), clockwise
      A <- function(theta) c(x = cx + R * cos(theta), y = cy + R * sin(theta))
      arc_path <- function(a1, a2) {
        p1 <- A(a1); p2 <- A(a2)
        large <- ifelse(abs(a2 - a1) > pi, 1, 0)
        paste0(
          "M ", cx, " ", cy, " ",
          "L ", sprintf("%.3f %.3f", p1["x"], p1["y"]), " ",
          "A ", R, " ", R, " 0 ", large, " 1 ",
          sprintf("%.3f %.3f", p2["x"], p2["y"]), " Z"
        )
      }
      mids  <- (angs[-1] + angs[-length(angs)]) / 2
      lab_r <- 0.70 * R
      L <- function(theta) c(x = cx + lab_r * cos(theta), y = cy + lab_r * sin(theta))
      paths <- lapply(seq_len(n), function(i) {
        d <- arc_path(angs[i], angs[i+1])
        htmltools::tags$path(
          d = d, fill = colors[i],
          stroke = "#FFFFFF", `stroke-width` = stroke_w_sep
        )
      })
      ring <- htmltools::tags$circle(
        cx = cx, cy = cy, r = R,
        fill = "none", stroke = line_col, `stroke-width` = stroke_w_ring
      )
      labs <- lapply(seq_len(n), function(i) {
        p <- L(mids[i])
        htmltools::tags$text(
          x = sprintf("%.1f", p["x"]), y = sprintf("%.1f", p["y"]),
          style = sprintf(
            "font-size: %.0fpx; font-family: %s; fill: %s; text-anchor: middle; dominant-baseline: middle;",
            12 * cex * legend_magnify, font_family, text_col
          ),
          labels[i]
        )
      })
      htmltools::tags$svg(
        width = size_px, height = size_px, viewBox = sprintf("0 0 %d %d", size_px, size_px),
        style = "display:block;",
        paths, ring, labs
      )
    }

    bin_labels      <- as.character(seq_len(BinCount))
    legend_panel_px <- max(200, round(120 * cex * legend_magnify)) # room for title above the pie

    legend_widget <- if (isTRUE(legend)) {
      htmltools::div(
        style = sprintf("width:%dpx; display:flex; flex-direction:column; align-items:center;", legend_panel_px),
        # Title above the pie
        htmltools::div(
          style = paste0(
            "font-weight:600;",
            "font-size:", round(12 * cex * legend_magnify), "px;",
            "font-family:", title_font_family, ";",
            "color:", legendTextCol, ";",
            "line-height:1.2;",
            "margin:0 0 6px 0;",
            "text-align:center;"
          ),
          if (isTRUE(scaleLegend)) "Orientation (scaled)" else "Orientation"
        ),
        # The pie itself
        svg_pie_legend(
          colors = binColors[seq_len(BinCount)],
          sizes  = if (length(BinSizes)) as.numeric(BinSizes) else rep(1, BinCount),
          labels = bin_labels,
          scaleLegend = scaleLegend,
          text_col = legendTextCol, line_col = legendLineCol,
          size_px = round(260 * legend_magnify),
          cex = cex, font_family = title_font_family
        ),
        if (isTRUE(maskDiscard)) htmltools::div(
          style = paste0(
            "display:flex;align-items:center;gap:8px;",
            "font-family:", title_font_family, ";",
            "color:", legendTextCol, ";",
            "font-size:", round(12 * cex * legend_magnify), "px;",
            "margin-top:6px;"
          ),
          htmltools::tags$span(
            style = "display:inline-block;width:12px;height:12px;background:#000;border:1px solid #444;"
          ),
          "Discarded"
        )
      )
    } else NULL

    title_div <- if (nzchar(main)) {
      htmltools::div(
        style = paste0(
          "font-weight:700;",
          "font-size:", title_font_size_px, "px;",
          "font-family:", title_font_family, ";",
          "line-height:1.25;",
          "margin:0 0 10px 0;",
          "text-align:center;"
        ),
        main
      )
    } else NULL

    row_div <- if (isTRUE(legend)) {
      htmltools::div(
        style = paste0(
          "display:flex;flex-direction:row;align-items:center;gap:12px;",
          "width:", widget_size_px + legend_panel_px, "px;margin:0 auto;"
        ),
        htmltools::div(style = sprintf("width:%dpx;height:%dpx;", widget_size_px, widget_size_px), w),
        legend_widget
      )
    } else {
      htmltools::div(style = sprintf("width:%dpx;margin:0 auto;", widget_size_px), w)
    }

    container <- htmltools::div(style = "width:100%;", title_div, row_div)
    return(htmltools::browsable(container))
  }

  # --- NON-INTERACTIVE RETURN: no HTML created -------------------------------
  bin_labels <- as.character(seq_len(BinCount))
  out <- structure(
    list(
      title       = main,
      bin_colors  = binColors[seq_len(BinCount)],
      face_colors = face_cols,
      bin_sizes   = BinSizes,
      bin_labels  = bin_labels,
      legend      = legend,
      scaled      = scaleLegend
    ),
    class = "OPC3d_spec"
  )
  invisible(out)
}
