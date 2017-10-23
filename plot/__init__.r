# not attaching this might have unforseen consequences
# do it once, so we can refactor later
library(cowplot)

#export_submodule('./box') # broken
export_submodule('./volcano')
export_submodule('./linear_fit')
#export_submodule('./heatmap') # empty
export_submodule('./matrix')
export_submodule('./venn') # bad style
export_submodule('./color')
export_submodule('./symmetrise_scale')

brew = import('./brew')
