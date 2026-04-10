;;;; protocol-version.lisp — MCP protocol version pinned for slice 009

;;;; Atelier (https://github.com/melusina-org/cl-atelier)
;;;; This file is part of Atelier.
;;;;
;;;; Copyright © 2017–2026 Michaël Le Barbier
;;;; All rights reserved.

;;;; SPDX-License-Identifier: MIT

(in-package #:atelier/mcp)

(alexandria:define-constant +mcp-protocol-version+ "2024-11-05"
  :test #'string=
  :documentation
  "The MCP protocol version Atelier targets in slice 009. Single point
   of change when the spec moves. See references/mcp-protocol.md for the
   pinned envelope shapes.")

;;;; End of file `protocol-version.lisp'
