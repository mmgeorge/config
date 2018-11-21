
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

(define-skeleton mc
  "Add material-ui import"
  > '(setq name (skeleton-read "Class Name: "))
  > "import "name" from '@material-ui/core/"name"';" \n
  )


(define-skeleton react
  "Template for owny component"
  > '(setq name (skeleton-read "Class Name: "))
  > "import { createElement as e } from 'react';" \n
  > "import withStyles from '@material-ui/core/styles/withStyles';" \n
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

(define-skeleton container
  "Template for owny component"
  > '(setq name (skeleton-read "Class Name: "))
  > "import { connect } from 'react-redux';" \n
  > "import { IState } from '../../../reducers';" \n
  > "import { Dispatch } from 'redux';" \n
  > "import "name" from './"name"';" \n
  > \n
  > "const mapState = (_state: IState) => ({});" \n
  > "const mapDispatch = (_dispatch: Dispatch) => ({});" \n
  > \n
  > "export default connect(mapState, mapDispatch)("name");"
);

(define-skeleton clpack
  "Template for owny component"
  > '(setq path (skeleton-read "path: "))
  > '(setq name (skeleton-read "class name: "))
  > "(defpackage :" path "/" name \n
  > "(:use :cl)" \n
  > "(:export))" \n
  > \n
  > "(in-package :"path "/" name")"
  > \n
  > \n
  > "(defclass "name" ()" \n
  > "())"
);


(provide 'emacs-templates)
;;; emacs-templates ends here
