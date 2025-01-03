radian.editing_mode = "vi"
radian.auto_suggest = TRUE
radian.highlight_matching_bracket = TRUE
radian.global_history_file = "~/.logs/radian_history"
radian.history_search_no_duplicates = TRUE
radian.history_search_ignore_case = TRUE

# Custom keymaps via prompt_toolkit
radian.on_load_hooks = list(function() {
    getOption("rchitect.py_tools")$attach()
    radian <- import("radian")
    prompt_toolkit <- import("prompt_toolkit")
    KeyPress <- prompt_toolkit$key_binding$key_processor$KeyPress
    Keys <- prompt_toolkit$keys$Keys
    insert_mode <- radian$key_bindings$insert_mode
    app <- radian$get_app()
    kb <- app$session$modes$r$prompt_key_bindings

    kb$add("k", "v", filter = insert_mode)(function(event) {
        event$app$key_processor$feed(KeyPress(Keys$Escape))
    })
    kb$add("v", "k", filter = insert_mode)(function(event) {
        event$app$key_processor$feed(KeyPress(Keys$Escape))
    })
})

do.call(options, c(as.list(environment())))
