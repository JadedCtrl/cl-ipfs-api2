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

    ;; / bootstrap calls
    :bootstrap
    :bootstrap/list
    :bootstrap/add
    :bootstrap/add/default
    :bootstrap/rm
    :bootstrap/rm/all

    ;; / config calls
    :config
    :config/show

    ;; /version calls
    :version
    :version/deps))

(in-package :cl-ipfs-api²)
