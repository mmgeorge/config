
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "/home/matt/wgslx/target/debug/wgslx")
                  :activation-fn (lsp-activate-on "wgsl")
                  :server-id 'wgslx))
