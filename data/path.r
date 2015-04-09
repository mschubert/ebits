.b = import('../base')

path = function(mod_str = NULL) {
    getOption(paste("data.dir", mod_str, sep=".")) %or%
        getOption("data.dir") %or%
        file.path(module_file(), mod_str)
}
