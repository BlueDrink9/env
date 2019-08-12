// leave at least 2 line with only a star on it below, or doc generation fails
/**
 *
 *
 * Placeholder for custom user javascript
 * mainly to be overridden in profile/static/custom/custom.js
 * This will always be an empty file in IPython
 *
 * User could add any javascript in the `profile/static/custom/custom.js` file.
 * It will be executed by the ipython notebook at load time.
 *
 * Same thing with `profile/static/custom/custom.css` to inject custom css into the notebook.
 *
 *
 * The object available at load time depend on the version of IPython in use.
 * there is no guaranties of API stability.
 *
 * The example below explain the principle, and might not be valid.
 *
 * Instances are created after the loading of this file and might need to be accessed using events:
 *     define([
 *        'base/js/namespace',
 *        'base/js/events'
 *     ], function(IPython, events) {
 *         events.on("app_initialized.NotebookApp", function () {
 *             IPython.keyboard_manager....
 *         });
 *     });
 *
 * __Example 1:__
 *
 * Create a custom button in toolbar that execute `%qtconsole` in kernel
 * and hence open a qtconsole attached to the same kernel as the current notebook
 *
 *    define([
 *        'base/js/namespace',
 *        'base/js/events'
 *    ], function(IPython, events) {
 *        events.on('app_initialized.NotebookApp', function(){
 *            IPython.toolbar.add_buttons_group([
 *                {
 *                    'label'   : 'run qtconsole',
 *                    'icon'    : 'icon-terminal', // select your icon from http://fortawesome.github.io/Font-Awesome/icons
 *                    'callback': function () {
 *                        IPython.notebook.kernel.execute('%qtconsole')
 *                    }
 *                }
 *                // add more button here if needed.
 *                ]);
 *        });
 *    });
 *
 * __Example 2:__
 *
 * At the completion of the dashboard loading, load an unofficial javascript extension
 * that is installed in profile/static/custom/
 *
 *    define([
 *        'base/js/events'
 *    ], function(events) {
 *        events.on('app_initialized.DashboardApp', function(){
 *            require(['custom/unofficial_extension.js'])
 *        });
 *    });
 *
 * __Example 3:__
 *
 *  Use `jQuery.getScript(url [, success(script, textStatus, jqXHR)] );`
 *  to load custom script into the notebook.
 *
 *    // to load the metadata ui extension example.
 *    $.getScript('/static/notebook/js/celltoolbarpresets/example.js');
*    // or
*    // to load the metadata ui extension to control slideshow mode / reveal js for nbconvert
*    $.getScript('/static/notebook/js/celltoolbarpresets/slideshow.js');
*
*
* @module IPython
* @namespace IPython
* @class customjs
* @static
*/
// Configure CodeMirror Keymap
require([
    'nbextensions/vim_binding/vim_binding',   // depends your installation
], function() {
    // Map jj to <Esc>
    CodeMirror.Vim.map("kv", "<Esc>", "insert");
    CodeMirror.Vim.map("vk", "<Esc>", "insert");
    CodeMirror.Vim.map(";", ":", "normal");
    // CodeMirror.Vim.map(":", ";", "normal");
    // Swap j/k and gj/gk (Note that <Plug> mappings)
    CodeMirror.Vim.map("j", "<Plug>(vim-binding-gj)", "normal");
    CodeMirror.Vim.map("k", "<Plug>(vim-binding-gk)", "normal");
    CodeMirror.Vim.map("gj", "<Plug>(vim-binding-j)", "normal");
    CodeMirror.Vim.map("gk", "<Plug>(vim-binding-k)", "normal");
    // I cannot get these to work at all.
    // CodeMirror.Vim.map("(", "(<Enter><Enter>)<Up>", "insert");
    CodeMirror.Vim.map("Y", "y$", "normal");

    // custom operator for commenting
    // (similar to commentary by Tim Pope)
    // this woks with visual selection ('vipgc') and with motions ('gcip')
    CodeMirror.Vim.defineOperator("comment_op", function(cm) {
        cm.toggleComment();
    });
    CodeMirror.Vim.mapCommand("gc", "operator", "comment_op", {});
});

// Configure Jupyter Keymap
require([
    'nbextensions/vim_binding/vim_binding',
    'base/js/namespace',
], function(vim_binding, ns) {
    // Add post callback
    vim_binding.on_ready_callbacks.push(function(){
        var km = ns.keyboard_manager;
        // Allow Ctrl-2 to change the cell mode into Markdown in Vim normal mode
        km.edit_shortcuts.add_shortcut('ctrl-2', 'vim-binding:change-cell-to-markdown', true);
        // Update Help
        km.edit_shortcuts.events.trigger('rebuild.QuickHelp');
    });
});

// Using :q to leave Vim mode and re-enter Jupyter mode
require([
    'base/js/namespace',
    'codemirror/keymap/vim',
    'nbextensions/vim_binding/vim_binding'
], function(ns) {
    CodeMirror.Vim.defineEx("quit", "q", function(cm){
        ns.notebook.command_mode();
        ns.notebook.focus_cell();
    });
});
