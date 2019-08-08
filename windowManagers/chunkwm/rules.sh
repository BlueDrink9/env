#!/usr/bin/env sh

chunkc tiling::rule --owner workrave --state float &
chunkc tiling::rule --owner \"System Preferences\" --subrole AXStandardWindow --state tile
chunkc tiling::rule --owner Finder --name Copy --state float &
chunkc tiling::rule --owner Finder --name Move --state float &
chunkc tiling::rule --owner \"Archive Utility\" --state float &
chunkc tiling::rule --owner Archive Utility --state float &
chunkc tiling::rule --owner \"App Store\" --state float &
chunkc tiling::rule --owner \"Word*\" --state tile &
chunkc tiling::rule --owner \"OneNote*\" --state tile &
chunkc tiling::rule --owner \"PowerPoint*\" --state tile &
chunkc tiling::rule --owner \"Excel*\" --state tile &
chunkc tiling::rule --owner PowerPoint --state tile &
chunkc tiling::rule --owner PowerPoint --subrole AXUnknown --state float &
chunkc tiling::rule --owner PowerPoint --subrole AXDialog --state float &
chunkc tiling::rule --subrole AXUnknown --state float &
chunkc tiling::rule --subrole AXDialog --state float &
chunkc tiling::rule --owner Excel --state tile &
chunkc tiling::rule --owner Word --state tile &
chunkc tiling::rule --owner OneNote --state tile &
chunkc tiling::rule --owner kitty --alpha 0.9 &

