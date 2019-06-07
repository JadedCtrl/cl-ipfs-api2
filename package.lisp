(defpackage :cl-ipfs-api2
  (:use :cl :arnesi)
  (:nicknames :cl-ipfs :ipfs :cl-ipfs-api²)
  (:export
    *api-host*
    *api-root*

    ;; calls
    :dl
    :cat
    :add
    :dns
    :id
    :ls
    :resolve
    :shutdown

    ;; bitswap calls
    :bitswap-ledger
    :bitswap-reprovide
    :bitswap-stat
    :bitswap-wantlist

    ;; block calls
    :block-get
    :block-put
    :block-rm
    :block-stat

    ;; bootstrap calls
    :bootstrap
    :bootstrap-list
    :bootstrap-add
    :bootstrap-add-default
    :bootstrap-rm
    :bootstrap-rm-all

    ;; cid calls
    :cid-base32
    :cid-bases

    ;; config calls
    :config
    :config-show

    ;; dag calls
    :dag-get
    :dag-put
    :dag-resolve

    ;; version calls
    :version
    :version-deps))
