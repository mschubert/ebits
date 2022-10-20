# not attaching this might have unforseen consequences
# do it once, so we can refactor later
library(ggplot2)

#export_submodule('./box') # broken
export_submodule('./volcano')
export_submodule('./linear_fit')
export_submodule('./pca')
#export_submodule('./heatmap') # empty
export_submodule('./matrix')
export_submodule('./venn')
export_submodule('./color')
export_submodule('./symmetrise_scale')
export_submodule('./build_or_spacer')
export_submodule('./denspt')

error = import('./error')$error
text = import('./text')$text
try = import('./try')$try

brew = import('./brew')
theme = import('./theme')
genome = import('./genome')
