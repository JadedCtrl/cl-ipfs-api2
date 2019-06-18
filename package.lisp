(defpackage :cl-ipfs-api2
  (:use :cl :arnesi)
  (:nicknames :cl-ipfs :ipfs :cl-ipfs-api²)
  (:export
    *api-host*
    *api-root*
    *ipfs-root*

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

    ;; dht calls
    :dht-findpeer
    :dht-findprovs
    :dht-get
    :dht-provide
    :dht-put
    :dht-query

    ;; diag calls
    :diag-cmds
    :diag-cmds-clear
    :diag-cmds-set-time
    :diag-sys

    ;; file calls
    :file-ls

    ;; files calls
    :files-chcid
    :files-cp
    :files-flush
    :files-ls
    :files-mkdir
    :files-mv
    :files-read
    :files-rm
    :files-stat
    :files-write

    ;; filestore calls
    :filestore-dups
    :filestore-ls
    :filestore-verify
    
    ;; key calls
    :key-gen
    :key-list
    :key-rename
    :key-remove

    ;; log calls
    :log-level
    :log-ls
    :log-tail

    ;; name calls
    :name-publish
    :name-pubsub-cancel
    :name-pubsub-state
    :name-pubsub-subs
    :name-resolve

    ;; object calls
    :object-data
    :object-diff
    :object-get
    :object-links
    :object-new
    :object-patch-add-link
    :object-patch-rm-link
    :object-stat
    ;; absentees— :object-put, :object-set-data, :object-patch-append-data

    ;; p2p calls
    :p2p-close
    :p2p-listen
    :p2p-ls
    :p2p-stream-close
    :p2p-stream-ls

    ;; pin calls
    :pin-add
    :pin-ls
    :pin-rm
    :pin-update
    :pin-verify

    ;; pubsub calls
    :pubsub-sub
    :pubsub-sub-process
    :pubsub-sub-read-char
    :pubsub-sub-listen
    :pubsub-sub-close
    :pubsub-pub
    :pubsub-ls
    :pubsub-peers

    ;; version calls
    :version
    :version-deps))
