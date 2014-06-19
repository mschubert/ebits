# Graphics helpers

import('./functional', attach = TRUE)

# Color helper functions {{{
transparent = function (colors, alpha = 0.5) {
    c = col2rgb(colors)
    rgb(c['red', ], c['green', ], c['blue', ], alpha * 255, maxColorValue = 255)
}

lighten = function (colors, factor = 0.5) {
    c = col2rgb(colors)
    l = function (c) 255 * factor + c * (1 - factor)
    rgb(l(c['red', ]), l(c['green', ]), l(c['blue', ]), maxColorValue = 255)
}

darken = function (colors, factor = 0.5) {
    c = col2rgb(colors)
    d = function (c) c * (1 - factor)
    rgb(d(c['red', ]), d(c['green', ]), d(c['blue', ]), maxColorValue = 255)
}

hsv2col = function (col)
    apply(col, COLS, lpartial(do.call, hsv) %.% as.list)
# }}}
