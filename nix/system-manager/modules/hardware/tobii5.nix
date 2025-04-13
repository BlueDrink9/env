
{ config, lib, pkgs, ... }:

{
  config = {
    services.udev.extraRules = ''
      # Tobii5
      SUBSYSTEM=="usb", ATTRS{idVendor}=="2104", ATTRS{idProduct}=="0127", GROUP="plugdev", TAG+="uaccess"
      SUBSYSTEM=="usb", ATTRS{idVendor}=="2104", ATTRS{idProduct}=="0118", GROUP="plugdev", TAG+="uaccess"
      SUBSYSTEM=="usb", ATTRS{idVendor}=="2104", ATTRS{idProduct}=="0106", GROUP="plugdev", TAG+="uaccess"
      SUBSYSTEM=="usb", ATTRS{idVendor}=="2104", ATTRS{idProduct}=="0128", GROUP="plugdev", TAG+="uaccess"
      SUBSYSTEM=="usb", ATTRS{idVendor}=="2104", ATTRS{idProduct}=="010a", GROUP="plugdev", TAG+="uaccess"
      SUBSYSTEM=="usb", ATTRS{idVendor}=="2104", ATTRS{idProduct}=="0102", GROUP="plugdev", TAG+="uaccess"
      SUBSYSTEM=="usb", ATTRS{idVendor}=="2104", ATTRS{idProduct}=="0313", GROUP="plugdev", TAG+="uaccess"
      SUBSYSTEM=="usb", ATTRS{idVendor}=="2104", ATTRS{idProduct}=="0318", GROUP="plugdev", TAG+="uaccess"
    '';

  };
}
