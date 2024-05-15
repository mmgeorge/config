"See https://github.com/esm7/obsidian-vimrc-support"

"Left"
noremap w q
noremap g w
noremap d a

noremap r b
noremap h w
noremap s gk
noremap t gj
noremap k g

noremap x u
noremap c r
noremap m c 
noremap p v
noremap v b

"Right"
noremap q x
noremap l y
noremap u i
nmap o <Nop>
noremap y p

noremap j d
noremap n h
noremap i l
noremap e 5<C-u>
noremap a 5<C-d>

noremap z n
noremap ; g0
noremap / g$
exmap back obcommand app:go-back
nmap , :back
exmap forward obcommand app:go-forward
nmap . :forward
 
"Open commands"
unmap <Space>

exmap openit obcommand switcher:open
nmap <Space>h :openit





