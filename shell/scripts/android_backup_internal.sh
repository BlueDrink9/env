#!/usr/bin/env bash

# Backs up a connected android devices /data/media (which is not backup up by
# TWRP nandroid backups).

adb forward tcp:7080 tcp:8080 && \
  adb shell 'su; \
  tar -zcvf --exclude="cache*" - /storage/emulated/0 \
    | nc -p 8080 -l 1>/dev/null' & \
  sleep 1; nc localhost 7080 > "android_internal_backup_$(date -I).tar.gz" && \
  adb forward --remove tcp:7080
