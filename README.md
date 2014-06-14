cl-qrencode
=====================
QR code 2005 encoder in Common Lisp

Usage
---------
There are some examples in [demo.lisp](demo.lisp). More detailed explanations are
listed below.

```lisp
(in-package :cl-qrencode)
(defclass qr-symbol ()
  (matrix modules))
(defun dark-module-p (matrix i j))
```
QR code is represented by a ```QR-SYMBOL``` object in this package, where
```MATRIX``` is an ```MODULES * MODULES``` array, and the color of each
module (dark or light) is indicated by ```dark-module-p```.

```lisp
(use-package :cl-qrencode)
(defun encode-symbol (text &key (version 1) (level :level-m) (mode nil)))
(defun encode-symbol-bytes (bytes &key (version 1) (level :level-m) (mode nil)))
```
```encode-symbol``` encodes ```TEXT``` into a ```QR-SYMBOL``` object.
```VERSION``` may be adapted to accommodate the encoding data.
```LEVEL``` is the error correction level, which should be one of ```:level-l```,
```:level-m```, ```:level-q```, or ```:level-h```.
For most of the time, ```MODE``` should be left ```nil```.

```lisp
(defun encode-png (text &key (fpath "qrcode.png") (version 1) (level :level-m)
                   (mode nil) (pixsize 9) (margin 8)))
```
This package also supports writing ```QR-SYMBOL``` into png files.
```PIXSIZE``` is number of pixels for each ```QR-SYMBOL``` module, ```MARGIN```
is number of pixels for each side of the QRcode quiet zone.

COPYING
---------
Copyright (c) 2011-2014 jnjcc, [Yste.org](http://www.yste.org)

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
