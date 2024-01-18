;;; lsp-wgslx.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Matt George

;; Author: Matt George
;; Keywords: wgsl, wgslx, webgpu, lsp, shading, shader

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; LSP Client for WGSLX (WebGPU Shading Language eXtended)

;;; Code:
(when (require 'projectile nil 'noerror)
  (projectile-register-project-type 'wgslx '("wgslx.toml")
                                    :project-file "wgslx.toml"
                                    :src-dir "src"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "/home/matt/wgslx/target/debug/wgslx-lsp")
                  :activation-fn (lsp-activate-on "wgsl")
                  :server-id 'wgslx))



(provide 'lsp-wgslx)
;;; lsp-wgslx.el ends here
