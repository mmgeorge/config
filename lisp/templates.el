
;;; Code:

(define-skeleton doc
  "template for documenation"
  > '(setq docstr (skeleton-read "Header:")) \n
  > "//--------------------------------------------------------------------------" \n
  > "//" \n
  > "//  " docstr \n
  > "//" \n
  > "//--------------------------------------------------------------------------" \n
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

;; ;;(skeleton-read "Class Name: ")
;; (define-skeleton fc
;;   "Template for a react functional component"
;;   > '(setq name (file-name-base (buffer-file-name)))
;;   > '(setq styles-path (s-join "-" (append (s-split-words name) (list "styles"))))
;;   > "import * as React from 'react';" \n
;;   > "import { use"(s-upper-camel-case styles-path)" } from './"styles-path"';" \n
;;   > "import { useTheme } from 'components/styles';" \n
;;   > \n
;;   > "interface I"(s-upper-camel-case name)"Props {}" \n
;;   > \n
;;   > "export const " (s-upper-camel-case name)": React.FunctionComponent<I"(s-upper-camel-case name)"Props> = props => {" \n
;;   > "const theme = useTheme();"\n
;;   > "const classes = use"(s-upper-camel-case styles-path)"(theme);"\n
;;   > "" \n
;;   > "return (" \n
;;   > "<div className={classes."(s-lower-camel-case name)"}>" \n
;;   > "</div>" \n
;;   > ")" \n
;;   > "}"
;;   );

(define-skeleton fc
  "Template for a react functional component"
  > '(setq name (file-name-base (buffer-file-name)))
  > "import { FunctionComponent } from 'react';" \n
  > "import { css } from '@compiled/react';" \n
  > "import type { CssFunction } from '@compiled/react/dist/esm/types';" \n
  > \n
  > "export interface I"(s-upper-camel-case name)" {}" \n
  > \n
  > "const root: CssFunction = {}"
  > \n
  > \n
  > "export const " (s-upper-camel-case name)": FunctionComponent<I"(s-upper-camel-case name)"> = props => {" \n
  > "" \n
  > "return (" \n
  > "<div css={root}>" \n
  > "</div>" \n
  > ")" \n
  > "}"
  );

;;(skeleton-read "Class Name: ")
(define-skeleton rc
  "Template for a react functional component"
  > '(setq name (file-name-base (buffer-file-name)))
  > "import * as React from 'react';" \n
  > "import { makeStyles } from 'components/styles';" \n
  > \n
  > "export interface I"(s-upper-camel-case name)"Props {}" \n
  > \n
  > "const useStyles = makeStyles(\""(s-upper-camel-case name)"\", _theme => ({" \n
  > "root: {}" \n
  > "}));"\n
  > \n
  > "export const " (s-upper-camel-case name)": React.FunctionComponent<I"(s-upper-camel-case name)"Props> = props => {" \n
  > "const classes = useStyles(props);" \n
  > "" \n
  > "return (" \n
  > "<div className={classes.root}>" \n
  > "</div>" \n
  > ")" \n
  > "}" \n
  );

(define-skeleton fcs
  "Template for a react functional component"
  > '(setq name (file-name-base (buffer-file-name)))
  > '(setq styles-path (s-join "-" (append (s-split-words name) (list "styles"))))
  > "import { makeStyles } from 'components/styles';" \n
  > \n
  > "export const use"(s-upper-camel-case name) " = makeStyles(_theme => ({}))"
  )



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
