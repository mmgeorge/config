
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


(define-skeleton react
  "Template for owny component"
  > '(setq name (skeleton-read "Class Name: "))
  > "import { createElement as e } from 'react';" \n
  > "import { withStyles } from 'material-ui/styles';" \n
  > "import { IStyled } from 'owny/interfaces';" \n
  > \n
  > "const styles = () => ({" \n
  > "root: {}" \n
  > "});"\n
  > \n
  > "interface I" name " extends IStyled<typeof styles> {}" \n
  > \n
  > "const " name " = (props: I" name ") =>" \n
  > "e('div', { className: props.classes.root })" \n
  > \n
  > "export default withStyles(styles)(" name ");"
);

(provide 'emacs-templates)
;;; emacs-templates ends here
