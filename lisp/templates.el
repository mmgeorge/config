
;;; Code:

(define-skeleton doc
  "template for documenation"
  > '(setq docstr (skeleton-read "Documentation:")) \n
  > ";; " docstr \n
  > ";; ----------------------------------------------------------------" \n
  )

(define-skeleton docjs
  "templace for js documentation"
  > "/** \n"
  > "* "@ _ "\n"
  > "* @param" @ _ "n"
  > "* @return "@ _" \n"
  > "*/"
  )

(define-skeleton docc
  "templace for documentation"
  > "/** " @ _ " */"
  )


(provide 'emacs-templates)
;;; emacs-templates ends here
