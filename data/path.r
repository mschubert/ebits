.b = import('../base')
.io = import('../io')

path = function(mod_str) {
    getOption(paste("data.dir", mod_str, sep=".")) %or%
        getOption("data.dir") %or%
        file.path(module_file(), mod_str)
}

file = function(mod_str, file) {
    file.path(path(mod_str), file)
}

load = function(mod_str, file) {
    .io$load(file(mod_str, file))
}
