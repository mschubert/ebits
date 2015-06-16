process_formula = function(form, data=parent.frame()) {
    extract_calls = function(f, ops=c("~","+")) {
        if (length(f) > 1 && as.character(f[[1]]) %in% ops)
            sapply(f[2:length(f)], extract_calls)
        else {
            if (is.call(f))
                name = paste(f[2:length(f)], collapse="_")
            else
                name = as.character(f)
            attr(f, "name") = name
            attr(f, "str") = deparse(f)
            f
        }
    }

    # extract all names/calls that are atomic for the formula
    vars = unlist(extract_calls(form))

    # go through vars, create data list w/ all evals
    processed_data = lapply(vars, function(v) base::eval(v, envir=data))
    names(processed_data) = sapply(vars, function(v) attr(v, "name"))

    # go through formula, replace every call by name
    form_str = deparse(form)
    for (v in vars)
        form_str = gsub(attr(v,"str"), attr(v,"name"), form_str, fixed=TRUE)

    list(form=as.formula(form_str), data=processed_data)
}
