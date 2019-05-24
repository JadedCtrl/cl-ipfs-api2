(defpackage :cl-ipfs-api2
  (:use :cl :arnesi)
  (:nicknames :cl-ipfs :ipfs :cl-ipfs-api²)
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

    ;; / block calls
    :block/get
    :block/put
    :block/rm
    :block/stat

    ;; / config calls
    :config
    :config/show

    ;; /version calls
    :version
    :version/deps))

(in-package :cl-ipfs-api²)
