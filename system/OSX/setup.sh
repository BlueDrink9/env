#!/usr/bin/env bash

source "$DOTFILES_DIR/bash/script_functions.sh"

# Close any open System Preferences panes, to prevent them from overriding
# settings we’re about to change
osascript -e 'tell application "System Preferences" to quit'

setKeyboardShortcuts(){
  # https://ryanmo.co/2017/01/05/setting-keyboard-shortcuts-from-terminal-in-macos/
  # Character         Special Key              Abbreviation
  #--------------------------------------------------------
  # @                 Command (Apple)		 CMD
  # ~                 Option         		 OPT
  # $                 Shift          		 SHIFT
  # ^                 Control        		 CTRL

  # http://apple.stackexchange.com/questions/91679/is-there-a-way-to-set-an-application-shortcut-in-the-keyboard-preference-pane-vi
# http://krypted.com/mac-os-x/defaults-symbolichotkeys/
  # These won't work. Not right keys. Apparently this is a very complex thing to change.
  # https://apple.stackexchange.com/questions/344494/how-to-disable-default-mission-control-shortcuts-in-terminal/344504
  # defaults write -globalDomain NSUserKeyEquivalents  -dict-add "Switch to Desktop 1" "~1";
  # defaults write -globalDomain NSUserKeyEquivalents  -dict-add "Switch to Desktop 2" "~2";
  # defaults write -globalDomain NSUserKeyEquivalents  -dict-add "Switch to Desktop 3" "~3";
  # defaults write -globalDomain NSUserKeyEquivalents  -dict-add "Switch to Desktop 4" "~4";
  # defaults write -globalDomain NSUserKeyEquivalents  -dict-add "Switch to Desktop 5" "~5";
  # defaults write -globalDomain NSUserKeyEquivalents  -dict-add "Switch to Desktop 6" "~6";
  #
  # This dict has more complete settings. Keys here:
  # https://web.archive.org/web/20141112224103/http://hintsforums.macworld.com/showthread.php?t=114785
  # For writing to this dict, param 3 is the modifier key:
  # MODS = {
  #   0=>"No modifier",
  #   131072=>"Shift",
  #   262144=>"Control",
  #   524288=>"Option",
  #   1048576=>"Command",
  #   393216=>"Shift + Control",
  #   655360=>"Shift + Option",
  #   1179648=>"Shift + Command",
  #   786432=>"Control + Option",
  #   1310720=>"Control + Command",
  #   1572864=>"Option + Command",
  #   917504=>"Shift + Control + Option",
  #   1441792=>"Shift + Control + Command",
  #   1703936=>"Shift + Option + Command",
  #   1835008=>"Control + Option + Command",
  #   1966080=>"Shift + Control + Option + Command",
  # }
  # Param 2 is the base key, param 1 is its ascii value
  # https://web.archive.org/web/20141112224103/http://hintsforums.macworld.com/showthread.php?t=114785
  # Enable 'change input source' shortcut, set to option+space
  # No idea if this will actually work.
  defaults write "com.apple.symbolichotkeys" "AppleSymbolicHotKeys" -dict-add 60 "{ enabled = 1; value = { parameters = (32, 49, 524288); type = 'standard'; }; }"

  # Enable 'switch to desktop 1-6' as option+num.
  defaults write "com.apple.symbolichotkeys" "AppleSymbolicHotKeys" -dict-add 118 "{ enabled = 1; value = { parameters = (49, 18, 524288); type = 'standard'; }; }"
  defaults write "com.apple.symbolichotkeys" "AppleSymbolicHotKeys" -dict-add 119 "{ enabled = 1; value = { parameters = (50, 19, 524288); type = 'standard'; }; }"
  defaults write "com.apple.symbolichotkeys" "AppleSymbolicHotKeys" -dict-add 120 "{ enabled = 1; value = { parameters = (51, 20, 524288); type = 'standard'; }; }"
  defaults write "com.apple.symbolichotkeys" "AppleSymbolicHotKeys" -dict-add 121 "{ enabled = 1; value = { parameters = (52, 21, 524288); type = 'standard'; }; }"
  defaults write "com.apple.symbolichotkeys" "AppleSymbolicHotKeys" -dict-add 122 "{ enabled = 1; value = { parameters = (53, 22, 524288); type = 'standard'; }; }"
  defaults write "com.apple.symbolichotkeys" "AppleSymbolicHotKeys" -dict-add 123 "{ enabled = 1; value = { parameters = (54, 23, 524288); type = 'standard'; }; }"

}

###############################################################################
# General UI/UX                                                               #
###############################################################################

# Disable smart dashes as they’re annoying when typing code.
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false
# Disable automatic periods with a double space:
defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false
# Disable smart quotes as they’re annoying when typing code.
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false
# Enable Text Selection in Quick Look Windows
defaults write com.apple.finder QLEnableTextSelection -bool TRUE
# Change Where Screen Shots Are Saved To
defaults write com.apple.screencapture location ~/Pictures/Screenshots
# Always Show the User Library Folder
chflags nohidden ~/Library/
# Show the /Volumes folder
sudo chflags nohidden /Volumes

# System Preferences > General > Click in the scrollbar to: Jump to the spot that's clicked
defaults write -globalDomain "AppleScrollerPagingBehavior" -bool true
# System Preferences > Mission Control > Automatically rearrange Spaces based on most recent use (don't)
defaults write com.apple.dock mru-spaces -bool false
# System Preferences > Mission Controll > Dashboard
defaults write com.apple.dock dashboard-in-overlay -bool true
# Finder > Preferences > Show all filename extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true
# Finder > Preferences > Show wraning before changing an extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false
# Finder > View > Show Path Bar
defaults write com.apple.finder ShowPathbar -bool true
# No natural scrolling
defaults write -g com.apple.swipescrolldirection -bool false
# Disable press-and-hold for keys in favor of key repeat
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false
# Set a blazingly fast keyboard repeat rate
defaults write NSGlobalDomain KeyRepeat -int 1
defaults write NSGlobalDomain InitialKeyRepeat -int 10
# Require password delay after sleep or screen saver begins
# defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 5

# Set Home as the default location for new Finder windows
# For other paths, use `PfLo` and `file:///full/path/here/`
defaults write com.apple.finder NewWindowTarget -string "PfLo"
defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/"
# When performing a search, search the current folder by default
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"
# Avoid creating .DS_Store files on network or USB volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true
# Automatically open a new Finder window when a volume is mounted
defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true
defaults write com.apple.frameworks.diskimages auto-open-rw-root -bool true
defaults write com.apple.finder OpenWindowForNewRemovableDisk -bool true
# Enable snap-to-grid for icons on the desktop and in other icon views
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
# Expand the following File Info panes:
# “General”, “Open with”, and “Sharing & Permissions”
defaults write com.apple.finder FXInfoPanesExpanded -dict \
	General -bool true \
	OpenWith -bool true \
	Privileges -bool true

# Chrome: Use the system-native print preview dialog
defaults write com.google.Chrome DisablePrintPreview -bool true
defaults write com.google.Chrome.canary DisablePrintPreview -bool true
# Expand the print dialog by default
defaults write com.google.Chrome PMPrintingExpandedStateForPrint2 -bool true
defaults write com.google.Chrome.canary PMPrintingExpandedStateForPrint2 -bool true



# Add colemak, set as current keyboard.
defaults write "com.apple.HIToolbox" "AppleCurrentKeyboardLayoutInputSourceID" -string  "com.apple.keylayout.Colemak"
defaults write "com.apple.HIToolbox" "AppleEnabledInputSources" -array '({ InputSourceKind = "Keyboard Layout"; "KeyboardLayout ID" = 252; "KeyboardLayout Name" = ABC; })'
defaults write "com.apple.HIToolbox" "AppleEnabledInputSources" -array-add '{ InputSourceKind = "Keyboard Layout"; "KeyboardLayout ID" = 12825; "KeyboardLayout Name" = Colemak; }'
defaults write "com.apple.HIToolbox" "AppleInputSourceHistory" -array-add '{ InputSourceKind = "Keyboard Layout"; "KeyboardLayout ID" = 12825; "KeyboardLayout Name" = Colemak; }'
defaults write "com.apple.HIToolbox" "AppleSelectedInputSources" -array '({ InputSourceKind = "Keyboard Layout"; "KeyboardLayout ID" = 12825; "KeyboardLayout Name" = Colemak; })'

setKeyboardShortcuts

cp "$($SCRIPTDIR_CMD)"/defaults/* ~/Library/LaunchAgents
for fil in ~/Library/LaunchAgents/*; do
  sed -i '' "s,@{HOME},$HOME,g" "$HOME/Library/LaunchAgents/$fil"
  sed -i '' "s,@{DOTFILES_DIR},$DOTFILES_DIR/g" "$HOME/Library/LaunchAgents/$fil"
done

# killall Finder
echo "Restart is needed for OSX shortcuts and settings to take effect"
