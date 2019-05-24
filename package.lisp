(defpackage :cl-ipfs-api²
  (:use :cl :arnesi)
  (:nicknames :cl-ipfs :ipfs :cl-ipfs-api2)
  (:export
    *api-host*
    *api-root*

    ;; / calls
    :dl
    :cat
    :add
    :dns
    :id
    :ls
    :resolve
    :shutdown

    ;; / config calls
    :config
    config/show

    ;; /version calls
    :version
    :version/deps))

(in-package :cl-ipfs-api²)
