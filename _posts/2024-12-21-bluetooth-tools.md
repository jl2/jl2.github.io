---
layout: post
published: false
---

## About

I recently created a small Common Lisp package for working with Bluetooth devices on Linux.



For a project I was working on, I needed to read GPS messages being broadcast by a [RaceBox Mini](https://www.racebox.pro/products/racebox-mini).
After a bit of research I learned that Bluetooth on Linux uses [BlueZ](https://www.bluez.org/), and it provides a D-Bus API.
Common Lisp has a library for using D-Bus, so I was able to create this `bluetooth-tools` package.

Right now it uses the D-Bus API to:

* list adapters
* scan for devices
* list devices
* read GATT data
* connect and disconnect devices
* read battery levels
* and a bit more


It worked great for the [RaceBox Logger](https://github.com/jl2/racebox-tools) project, and I've also used it from the Lisp REPL to
scan for devices, check battery levels, etc.

It's useful as it is, but I'd like to improve the useability, especially from the REPL.  Right now it doesn't support looking up a device by name,
which makes it a little clunky to use for some things.  It would also be nice to lookup human readable information about UUIDs.

Eventually I'd like to have functions for all of the functionality of bluetoothctl exported from bluetooth-tools.

I also want to use this library from StumpWM and assign keyboard shortcuts to various Bluetooth commands, like switching headphone modes,
disconnecting a certain device, etc.

## Examples

The first thing to do is load the packaged:
``` common-lisp
CL-USER> (ql:quickload :bluetooth-tools)
```

After that, if I want to check my headphone's battery level, I can do this:

``` common-lisp
CL-USER> (bluetooth:battery-levels)
(("LE_ATH-M50xBT2" . 80) ("SpaceMouse Pro Wireless BT" . 56))
```

And see that they're at 80%, and my 3D mouse is at 56%.

I can also programmatically scan for nearby devices and list them:

``` common-lisp
CL-USER> (bluetooth:scan :timeout 5)
; No value
CL-USER> (bluetooth:list-devices)
("Ion Pro RT" "Ion Pro RT" "LE-Bose SoundLink Micro" "/org/bluez/hci0/dev_72_50_3D_BA_6D_77"
 "/org/bluez/hci0/dev_62_1D_FB_87_FD_06" "/org/bluez/hci0/dev_70_49_F0_16_04_04" "Flare RT"
 "/org/bluez/hci0/dev_EC_81_93_2A_8E_E0" "Flare RT" "/org/bluez/hci0/dev_C4_B2_1B_1F_46_37" "Ion Pro RT" "Flare RT"
 "/org/bluez/hci0/dev_98_E0_D9_AE_13_FA" "S36 2E97 LE" "/org/bluez/hci0/dev_54_8A_4B_D6_7B_39" "Ion Pro RT" "CS100-AO"
 "Apple Wireless Keyboard" "GBK_H613E_FC76" "Flare RT" "RaceBox Mini 1221405078" "Govee_H617A_2611" "110092_603C"
 "LE_ATH-M50xBT2" "ERGO M575" "/org/bluez/hci1/dev_EC_81_93_68_7F_54" "Wacom One pen tablet medium"
 "Jeremiah’s Trackpad" "LE_ATH-M50xBT2" "SpaceMouse Pro Wireless BT")
CL-USER> 
```

