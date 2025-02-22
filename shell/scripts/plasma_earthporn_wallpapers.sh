#!/bin/sh
# This script needs imagemagick, plasma and wget
wallpaper_dir="$HOME/.wallpapers"
mkdir -p $wallpaper_dir
wallpaper_name="current-wallpaper.jpg"

# subreddit=$(
#   shuf -n1 <<-END
# earthporn/top
# exposureporn/hot
# cityporn/top
# skyporn/top
# beachporn/top
# waterporn/top
# auroraporn/hot
# END
# )

# wait until network is up
i=0
while ip addr | grep -qv 'inet.*global' && [ "$i" -le 120 ]; do
  i=$(("$i" + 1))
  echo "Waiting for network..."
  sleep 1
done

subreddit="earthporn/top"
# echo "Choosing $subreddit"
feed_url="https://www.reddit.com/r/$subreddit/.rss"
# Not easy to get a feed from reddit without being blocked.
feed=$(curl "$feed_url" -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:134.0) Gecko/20100101 Firefox/134.0' -H 'Accept: text/html,application/xhtml+xml,application/xml' -H 'Accept-Language: en-GB,en' -H 'Accept-Encoding: gzip, deflate, br, zstd' -H 'DNT: 1' -H 'Upgrade-Insecure-Requests: 1' -H 'Sec-Fetch-Dest: document' -H 'Sec-Fetch-Mode: navigate' -H 'Sec-Fetch-Site: none' -H 'Sec-Fetch-User: ?1' -H 'Connection: keep-alive')
urls=$(echo $feed | grep -oP 'https://i.redd.it/\w{13}\.(jpe?g)')


screen_i=0
for res in $(xrandr | grep "\*" | awk '{print $1}'); do
  screen_i=$((screen_i+1))
  url=$(echo "$urls" | shuf -n1)
  tmpfile=$(mktemp).png
  wget "$url" -qO "$tmpfile"
  mv $tmpfile "$wallpaper_dir/${screen_i}_$wallpaper_name"
  # ^ is the fill area flag
  # magick "$tmpfile" -sample ${res}^ -filter Triangle -define filter:support=2 -unsharp 0.25x0.25+8+0.065 -dither None -posterize 136 -quality 82 -define jpeg:fancy-upsampling=off -define png:compression-filter=5 -define png:compression-level=9 -define png:compression-strategy=1 -define png:exclude-chunk=all -interlace none -colorspace sRGB JPG:"$wallpaper_dir/${i}_$wallpaper_name"
done

# Set dev/null first to ensure a refresh is triggered; it won't if the file
# path is the same but the contents differs.
dbus-send --session --dest=org.kde.plasmashell --type=method_call /PlasmaShell org.kde.PlasmaShell.evaluateScript "string:
var Desktops = desktops();
for (i=0;i<Desktops.length; i++) {
        let wp_i = i % $screen_i + 1;
        d = Desktops[i];
        d.wallpaperPlugin = 'org.kde.image';
        d.currentConfigGroup = Array('Wallpaper', 'org.kde.image', 'General');
        d.writeConfig('Image', 'file:///dev/null');
        d.writeConfig('Image', 'file://$wallpaper_dir/' + wp_i + '_$wallpaper_name');
}"
# Optional, change lockscreen if you want
# # kwriteconfig5 --file kscreenlockerrc --group Greeter --group Wallpaper --group org.kde.image --group General --key Image "file://$full_image_path"
