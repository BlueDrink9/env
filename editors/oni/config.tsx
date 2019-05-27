
import * as React from "react"
import * as Oni from "oni-api"

export const activate = (oni: Oni.Plugin.Api) => {
    console.log("config activated")

    // Input
    //
    // Add input bindings here:
    //
    // oni.input.bind("<c-enter>", () => console.log("Control+Enter was pressed"))
    // oni.input.bind("<leader>ir", () => "rename variable"
    oni.input.bind("<leader>if", "language.findAllReferences"),
    // bind to some provided commands
    // ctrl-space to jump to a file
    // oni.input.bind("<c-space>", "quickOpen.show")
    // shift-F8 to jump to the previous error in the buffer
    oni.input.bind("[e", "oni.editor.previousError")
    // F8 to jump to the next error in the buffer
    oni.input.bind("]e", "oni.editor.nextError")

    //
    // Or remove the default bindings here by uncommenting the below line:
    //
    // oni.input.unbind("<c-p>")

}

export const deactivate = (oni: Oni.Plugin.Api) => {
    console.log("config deactivated")
}

export const configuration = {
    //add custom config here, such as
    "oni.useDefaultConfig": false,
    "oni.loadInitVim": true,

    // Own vim mappings solve this
    "autoClosingPairs.enabled": false,

    "experimental.indentLines.enabled": true
    "experimental.markdownPreview.enabled": true
    "experimental.vcs.sidebar": true,
    "editor.quickOpen.showHidden": true,
    // Use textmate grammars for syntax highlighting
    "editor.textMateHighlighting.enabled" : true,

    // `buffers` will show every buffer in its own tab.
    // If you use tabs as part of your Vim workflow, you'll likely want to set this to `tabs`, which will instead only show tabs.
    // To disable Oni's tab UI entirely and revert to the native Vim tab bar, use `native`. Hidden removes both.
    // "tabs.mode": "buffers",
    "tabs.mode": "native",
    "tabs.height": "2.0em",
    // max width
    // "tabs.width": "30.0em",
    "tabs.showIndex": true,
    // Wrap when screen is full?
    // "tabs.wrap": true,
    // Maybe change this later
    "statusbar.enabled": true,
    // "sidebar.width": "15.0em",
    "sidebar.default.open": true,
    // If the externalised commandline should be enabled or not.
    // Setting this to false will revert to the standard Vim commandline at the bottom of the screen.
    "commandline.mode": true,
    //"oni.bookmarks": ["~/Documents"],

    "ui.colorscheme": "solarized8_light",
    // "editor.fontFamily": "Menlo",
    "editor.fontFamily": "SauceCodePro NF",
    "editor.fontSize": "14px",

    // Enables / disables cursor line highlight.
    "editor.cursorLine": true,
    // Defines opacity of cursor line highlight. Only valid when editor.cursorLine: true.
    "editor.cursorLineOpacity": 0.1,
    // Enables / disables cursor column highlight.
    "editor.cursorColumn": false,
    // Defines opacity of cursor column highlight. Only valid when editor.cursorColumn: true
    "editor.cursorColumnOpacity": 0.1,

    // UI customizations
    "ui.animations.enabled": true,
    "ui.fontSmoothing": "auto",
    // Padding between lines, in pixels
    "editor.linePadding": 2,
    // Path to a custom background image
    // "editor.backgroundImageUrl" : null,
    // Opacity of background image.
    "editor.backgroundOpacity" : 1.0,
    "achievements.enabled": false,
    "learning.enabled": false,

    //The default config Oni ships with
    "oni.plugins.prettier": {
        settings: {
            semi: false,
            tabWidth: 4,
            useTabs: false,
            singleQuote: false,
            trailingComma: "es5",
            bracketSpacing: true,
            jsxBracketSameLine: false,
            arrowParens: "avoid",
            printWidth: 80,
        },
		// TODO: Set this to true to enable formatting on save
        formatOnSave: false,

		// TODO: Set this to true to enable the plugin
        enabled: true,
    },
}
