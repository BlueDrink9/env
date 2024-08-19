in { config, pkgs, ... }:
{
  systemd.services = {
    # media-perm-updater = {
    #   script = "mkdir /run/media -p && chown root /run/media && chmod 700 /run/media";
    #   wantedBy = ["default.target"];
    # };
  };
}
