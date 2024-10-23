
{ config, pkgs, ... }:

{
  programs.firefox = {
    enable = true;
    # Allow overriding settings set here
    preferencesStatus = "user";

    /* ---- POLICIES ---- */
    # Check about:policies#documentation for options.
    policies = {
      DisableTelemetry = true;
      DisableFirefoxStudies = true;
      EnableTrackingProtection = {
        Value= true;
        Locked = true;
        Cryptomining = true;
        Fingerprinting = true;
      };
      DisablePocket = true;
      OverrideFirstRunPage = "";
      OverridePostUpdatePage = "";
      # DontCheckDefaultBrowser = true;
      DisplayBookmarksToolbar = "newtab";
      DisplayMenuBar = "default-off"; # alternatives: "always", "never" or "default-on"
      SearchBar = "unified"; # alternative: "separate"

      /* ---- EXTENSIONS ---- */
      # Check about:support for extension/add-on ID strings.
      # Valid strings for installation_mode are "allowed", "blocked",
      # "force_installed" and "normal_installed".
      ExtensionSettings = with builtins;
        let extension = shortId: uuid: {
          name = uuid;
          value = {
            install_url = "https://addons.mozilla.org/en-US/firefox/downloads/latest/${shortId}/latest.xpi";
            installation_mode = "normal_installed";
          };
        };
        in listToAttrs [
            # "*".installation_mode = "blocked"; # blocks all addons except the ones specified below
            # uBlock Origin:
            (extension "ublock-origin" "uBlock0@raymondhill.net")
            # Privacy Badger:
            (extension "privacy-badger17" "jid1-MnnxcxisBPnSXQ@jetpack")
            # vimium:
            (extension "vimium-c" "vimium-c@gdh1995.cn")
            (extension "multi-account-containers" "firefox")
          ];

      /* ---- PREFERENCES ---- */
      # Check about:config for options.
      Preferences = {
        # "browser.contentblocking.category" = { Value = "strict"; Status = "locked"; };
        "widget.use-xdg-desktop-portal.file-picker" = 1;
        "extensions.pocket.enabled" = false;
        # Ask where to save downloads.
        "browser.download.useDownloadDir" = false;
        # "extensions.screenshots.disabled" = lock-true;
        # "browser.topsites.contile.enabled" = lock-false;
        # "browser.formfill.enable" = lock-false;
        # "browser.search.suggest.enabled" = lock-false;
        # # "browser.search.suggest.enabled.private" = lock-false;
        # "browser.urlbar.suggest.searches" = lock-false;
        # "browser.urlbar.showSearchSuggestionsFirst" = lock-false;
        # "browser.newtabpage.activity-stream.feeds.section.topstories" = lock-false;
        # "browser.newtabpage.activity-stream.feeds.snippets" = lock-false;
        # "browser.newtabpage.activity-stream.section.highlights.includePocket" = lock-false;
        # "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" = lock-false;
        # "browser.newtabpage.activity-stream.section.highlights.includeDownloads" = lock-false;
        # "browser.newtabpage.activity-stream.section.highlights.includeVisited" = lock-false;
        "browser.newtabpage.activity-stream.showSponsored" = false;
        "browser.newtabpage.activity-stream.system.showSponsored" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        # disable all the annoying quick actions
        "browser.urlbar.quickactions.enabled" = false;
        "browser.urlbar.quickactions.showPrefs" = false;
        "browser.urlbar.shortcuts.quickactions" = false;
        "browser.urlbar.suggest.quickactions" = false;
        # Default search engine?
        "browser.urlbar.placeholderName" = "DuckDuckGo";
        "browser.urlbar.placeholderName.private" = "DuckDuckGo";
        "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.havePinned" = "DuckDuckGo";
        "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.searchEngines" = "DuckDuckGo";
        # Don't save passwords
        "signon.rememberSignons" = false;
      };
    };
  };
}
