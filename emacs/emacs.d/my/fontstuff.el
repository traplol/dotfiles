
(defvar *current-font-idx* 0)
(defconst *fonts-array* [
                         "clearlyu pua 12"

                         "clearlyu 12"

                         "clean 12"

                         "fixed 12"

                         "open look cursor 12"

                         "open look glyph 12"

                         "newspaper 12"

                         "gothic 12"

                         "mincho 12"

                         "fangsong ti 12"

                         "song ti 12"

                         "fixed 12"

                         "nil 12"

                         "clearlyu alternate glyphs 12"

                         "clearlyu arabic extra 12"

                         "clearlyu arabic 12"

                         "clearlyu devanagari 12"

                         "clearlyu devangari extra 12"

                         "clearlyu ligature 12"

                         "clearlyu pua 12"

                         "clearlyu 12"

                         "clean 12"

                         "fixed 12"

                         "open look cursor 12"

                         "open look glyph 12"

                         "bitstream charter 12"

                         "courier 10 pitch 12"

                         "fixed 12"

                         "Noto Sans Mono CJK TC 12"

                         "monofur for Powerline 12"

                         "Lato 12"

                         "Lato 12"

                         "Century Schoolbook L 12"

                         "Khmer OS System 12"

                         "Nakula 12"

                         "Chandas 12"

                         "Roboto Mono Thin for Powerline 12"

                         "Keraleeyam 12"

                         "IBM 3270 Narrow 12"

                         "Noto Sans Mono CJK TC 12"

                         "Meera 12"

                         "Source Code Pro for Powerline 12"

                         "Lato 12"

                         "Meslo LG S DZ for Powerline 12"

                         "Noto Sans Mono CJK SC 12"

                         "Tibetan Machine Uni 12"

                         "Umpush 12"

                         "IBM 3270 Semi-Narrow 12"

                         "DejaVu Sans Mono 12"

                         "Purisa 12"

                         "Meslo LG L DZ for Powerline 12"

                         "Mitra Mono 12"

                         "Meslo LG M DZ for Powerline 12"

                         "Noto Serif CJK JP 12"

                         "Noto Serif CJK KR 12"

                         "Noto Sans CJK SC 12"

                         "Norasi 12"

                         "Loma 12"

                         "Karumbi 12"

                         "Droid Sans Mono for Powerline 12"

                         "Liberation Mono for Powerline 12"

                         "Roboto Mono Light for Powerline 12"

                         "Noto Mono for Powerline 12"

                         "URW Palladio L 12"

                         "Noto Serif CJK SC 12"

                         "Noto Serif CJK TC 12"

                         "Likhan 12"

                         "Fira Mono for Powerline 12"

                         "Source Code Pro for Powerline 12"

                         "Padauk Book 12"

                         "Phetsarath OT 12"

                         "Sawasdee 12"

                         "Sahadeva 12"

                         "Noto Sans CJK TC 12"

                         "Tlwg Typist 12"

                         "Tlwg Typewriter 12"

                         "Noto Sans CJK KR 12"

                         "Lato 12"

                         "URW Gothic L 12"

                         "URW Chancery L 12"

                         "Ubuntu 12"

                         "Mukti Narrow 12"

                         "Noto Sans CJK JP 12"

                         "Noto Sans CJK SC 12"

                         "Meslo LG S for Powerline 12"

                         "Chilanka 12"

                         "FreeSerif 12"

                         "Padauk 12"

                         "AnjaliOldLipi 12"

                         "DejaVu Sans Mono for Powerline 12"

                         "Ubuntu Condensed 12"

                         "Space Mono for Powerline 12"

                         "DejaVu Sans 12"

                         "Source Code Pro for Powerline 12"

                         "Kinnari 12"

                         "NovaMono for Powerline 12"

                         "padmaa 12"

                         "Tlwg Mono 12"

                         "Source Code Pro for Powerline 12"

                         "aakar 12"

                         "Bitstream Charter 12"

                         "Space Mono 12"

                         "Symbol Neu for Powerline 12"

                         "Hack 12"

                         "Droid Sans Mono Slashed for Powerline 12"

                         "Khmer OS 12"

                         "Courier 10 Pitch 12"

                         "Inconsolata-g for Powerline 12"

                         "Lato 12"

                         "Inconsolata for Powerline 12"

                         "Laksaman 12"

                         "Liberation Sans Narrow 12"

                         "Liberation Mono 12"

                         "Nimbus Sans L 12"

                         "Roboto Mono Medium for Powerline 12"

                         "Manjari 12"

                         "Noto Sans CJK TC 12"

                         "Rachana 12"

                         "Pagul 12"

                         "Lohit Telugu 12"

                         "Samanata 12"

                         "Roboto Mono for Powerline 12"

                         "Kalimati 12"

                         "padmaa 12"

                         "Lato 12"

                         "Terminess Powerline 12"

                         "Nimbus Mono L 12"

                         "Liberation Serif 12"

                         "Manjari 12"

                         "Ani 12"

                         "Lato 12"

                         "Meslo LG M for Powerline 12"

                         "Noto Sans Mono CJK SC 12"

                         "Nimbus Roman No9 L 12"

                         "Ubuntu 12"

                         "Cousine for Powerline 12"

                         "IBM 3270 12"

                         "Anonymous Pro for Powerline 12"

                         "Liberation Sans 12"

                         "Meslo LG L for Powerline 12"

                         "Manjari 12"

                         "FreeSans 12"

                         "Noto Sans Mono CJK JP 12"

                         "Arimo for Powerline 12"

                         "Sarai 12"

                         "Lohit Devanagari 12"

                         "Noto Color Emoji 12"

                         "Uroob 12"

                         "Source Code Pro for Powerline 12"

                         "Noto Mono 12"

                         "Dyuthi 12"

                         "Tlwg Typo 12"

                         "Droid Sans Mono Dotted for Powerline 12"

                         "Noto Sans CJK JP 12"

                         "Lato 12"

                         "Suruma 12"

                         "Abyssinica SIL 12"

                         "Jamrul 12"

                         "Noto Sans Mono CJK KR 12"

                         "Waree 12"

                         "Gargi 12"

                         "Mukti Narrow 12"

                         "Inconsolata-dz for Powerline 12"

                         "DejaVu Serif 12"

                         "Noto Sans CJK KR 12"

                         "Noto Sans Mono CJK KR 12"

                         "Source Code Pro for Powerline 12"

                         "Garuda 12"

                         "Rekha 12"

                         "Ubuntu Mono derivative Powerline 12"

                         "Tinos for Powerline 12"

                         "Go Mono for Powerline 12"

                         "FreeMono 12"

                         "Ubuntu Mono 12"

                         "ProFont for Powerline 12"

                         "URW Bookman L 12"

                         "Noto Sans Mono CJK JP 12"
                         ])



(defun font-forward ()
  (interactive)
  (setq *current-font-idx* (mod (1+  *current-font-idx*) (length *fonts-array*)))
  (let ((font-to-use (aref *fonts-array* *current-font-idx*)))
    (set-frame-font font-to-use)
    (message "Using font: %s" font-to-use)))

(defun font-backward ()
  (interactive)
  (setq *current-font-idx* (mod (1-  *current-font-idx*) (length *fonts-array*)))
  (let ((font-to-use (aref *fonts-array* *current-font-idx*)))
    (set-frame-font font-to-use)
    (message "Using font: %s" font-to-use)))

(progn
  (global-set-key (kbd "C-=") 'font-forward)
  (global-set-key (kbd "C--") 'font-backward))
