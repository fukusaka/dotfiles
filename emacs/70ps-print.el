(load "ps-print")

(setq ps-paper-type 'a4)
(setq ps-landscape-mode t)
(setq ps-number-of-columns 2)
(setq ps-printer-name "lw230d")


(defvar ps-kanji-ascii-ratio 0.8)

(defvar ps-print-prologue-kanji
  "
/KS {
  kfontarray exch get setfont

  /xx currentpoint dup Descent add /yy exch def
  Ascent add /YY exch def def
  dup stringwidth pop xx add /XX exch def
  Effect 8 and 0 ne {
    /yy yy Yshadow add def
    /XX XX Xshadow add def
  } if
  bg {
    true
    Effect 16 and 0 ne
      {SpaceBackground doBox}
      {xx yy XX YY doRect}
    ifelse
  } if							% background
  Effect 16 and 0 ne {false 0 doBox}if			% box
  Effect 8  and 0 ne {dup doShadow}if			% shadow
  Effect 32 and 0 ne
    {true doOutline}					% outline
    {
      KanjiRomanDiffHalf 0 rmoveto
      KanjiRomanDiff 0 3 -1 roll ashow
      KanjiRomanDiffHalf neg 0 rmoveto
    }
  ifelse
  Effect 1  and 0 ne {UnderlinePosition Hline}if	% underline
  Effect 2  and 0 ne {StrikeoutPosition Hline}if	% strikeout
  Effect 4  and 0 ne {OverlinePosition  Hline}if	% overline
} bind def

/getfont {exch findfont exch scalefont} bind def

% Initializing kanji fonts
/kanji_init {
  /bodyfontsize exch def
  /bodykfontsize bodyfontsize kanjiAsciiRatio mul def
  /bodyknfont /Ryumin-Light-H bodykfontsize getfont def
  /bodykbfont /GothicBBB-Medium-H bodykfontsize getfont def
  /bodykofont bodyknfont [ 1 0 .2 1 0 0 ] makefont def
  /bodykbofont bodykbfont [ 1 0 .2 1 0 0 ] makefont def
  /KanjiRomanDiff 1.2 bodyfontsize mul 1.0 bodykfontsize mul sub def
  /KanjiRomanDiffHalf KanjiRomanDiff 2 div def
  /kfontarray [ bodyknfont bodykbfont bodykofont bodykbofont ] def
} def
")


(defun ps-begin-file ()
  (ps-get-page-dimensions)
  (setq ps-page-postscript 0
	ps-background-text-count 0
	ps-background-image-count 0
	ps-background-pages nil
	ps-background-all-pages nil)

  (ps-output ps-adobe-tag
	     "%%Title: " (buffer-name)	; Take job name from name of
					; first buffer printed
	     "\n%%Creator: " (user-full-name)
	     " (using ps-print v" ps-print-version
	     ")\n%%CreationDate: "
	     (time-stamp-hh:mm:ss) " " (time-stamp-mon-dd-yyyy)
	     "\n%%Orientation: "
	     (if ps-landscape-mode "Landscape" "Portrait")
	     "\n%% DocumentFonts: Times-Roman Times-Italic "
	     (mapconcat 'identity
			(ps-remove-duplicates
			 (append (ps-fonts 'ps-font-for-text)
				 (list (ps-font 'ps-font-for-header 'normal)
				       (ps-font 'ps-font-for-header 'bold))))
			" ")
	     "\n%%Pages: (atend)\n"
	     "%%EndComments\n\n")

  (ps-output-boolean "LandscapeMode"             ps-landscape-mode)
  (ps-output (format "/NumberOfColumns %d def\n" ps-number-of-columns)

	     (format "/LandscapePageHeight %s def\n" ps-landscape-page-height)
	     (format "/PrintPageWidth      %s def\n"
		     (- (* (+ ps-print-width ps-inter-column)
			   ps-number-of-columns)
			ps-inter-column))
	     (format "/PrintWidth   %s def\n" ps-print-width)
	     (format "/PrintHeight  %s def\n" ps-print-height)

	     (format "/LeftMargin   %s def\n" ps-left-margin)
	     (format "/RightMargin  %s def\n" ps-right-margin) ; not used
	     (format "/InterColumn  %s def\n" ps-inter-column)

	     (format "/BottomMargin %s def\n" ps-bottom-margin)
	     (format "/TopMargin    %s def\n" ps-top-margin) ; not used
	     (format "/HeaderOffset %s def\n" ps-header-offset)
	     (format "/HeaderPad    %s def\n" ps-header-pad))

  (ps-output-boolean "PrintHeader"        ps-print-header)
  (ps-output-boolean "PrintOnlyOneHeader" ps-print-only-one-header)
  (ps-output-boolean "PrintHeaderFrame"   ps-print-header-frame)
  (ps-output-boolean "ShowNofN"           ps-show-n-of-n)
  (ps-output-boolean "Duplex"             ps-spool-duplex)

  (let ((line-height (ps-line-height 'ps-font-for-text)))
    (ps-output (format "/LineHeight   %s def\n" line-height)
	       (format "/LinesPerColumn %d def\n"
		       (round (/ (+ ps-print-height
				    (* line-height 0.45))
				 line-height)))))

  (ps-output-boolean "Zebra" ps-zebra-stripes)
  (ps-output-boolean "PrintLineNumber" ps-line-number)
  (ps-output (format "/ZebraHeight %d def\n" ps-zebra-stripe-height))
  (ps-output (format "/kanjiAsciiRatio %s def\n" ps-kanji-ascii-ratio))

  (ps-background-text)
  (ps-background-image)
  (setq ps-background-all-pages (nreverse ps-background-all-pages)
	ps-background-pages (nreverse ps-background-pages))

  (ps-output ps-print-prologue-1)

  (ps-output "/printGlobalBackground {\n")
  (ps-output-list ps-background-all-pages)
  (ps-output "} def\n/printLocalBackground {\n} def\n")

  ;; Header fonts
  (ps-output (format "/h0 %s /%s DefFont\n" ; /h0 14 /Helvetica-Bold DefFont
		     ps-header-title-font-size (ps-font 'ps-font-for-header
							'bold))
	     (format "/h1 %s /%s DefFont\n" ; /h1 12 /Helvetica DefFont
		     ps-header-font-size (ps-font 'ps-font-for-header
						  'normal)))

  (ps-output ps-print-prologue-2)

  ;; Text fonts
  (let ((font (ps-font-alist 'ps-font-for-text))
	(i 0))
    (while font
      (ps-output (format "/f%d %s /%s DefFont\n"
			 i
			 ps-font-size
			 (ps-font 'ps-font-for-text (car (car font)))))
      (setq font (cdr font)
	    i (1+ i))))

  (ps-output ps-print-prologue-kanji)

  ;; Kanji fonts
  (ps-output (format "%s kanji_init\n" ps-font-size))

  (ps-output "\nBeginDoc\n\n"
	     "%%EndPrologue\n"))


(defun split-string-with-coding (str)
  (let*
      ((strJ (encode-coding-string str 'iso-2022-jp))
       (staJ (string-match "$[B@]" strJ))
       (endJ (string-match "([BJ]" strJ))
       he ta)
    (cond
     ((not staJ)
      (setq he strJ
	    ta ""
	    cod 'ascii)
      )
     ((/= staJ 0)
      (setq he (substring strJ 0 staJ)
	    ta (substring strJ staJ)
	    cod 'ascii)
      )
     ((/= endJ 0)
      (setq he (substring strJ 0 (+ 3 endJ))
	    ta (substring strJ (+ 3 endJ))
	    cod 'jis)
      )
     )
    (cons (cons cod he)
	  (if (string= ta "")
	      nil
	    (split-string-with-coding ta)))
    ))

(defun ps-basic-plot-string (from to &optional bg-color)
  (let* ((wrappoint (ps-find-wrappoint from to
					    (ps-avg-char-width 'ps-font-for-text)))
	      (to (car wrappoint))
	      (string (buffer-substring-no-properties from to)))

    (mapcar (lambda (x)
	      (cond
	       ((eq (car x) 'ascii)
		(ps-output (format "/f%d F " ps-current-font))
		(ps-output-string (cdr x))
		(ps-output " S\n")
		)
	       ((eq (car x) 'jis )
		(string-match "^$[B@]\\(.*\\)([BJ]$" (cdr x))
		(ps-output-string (match-string 1 (cdr x)))
		(ps-output " 0 KS\n")
		)
	       )
	      )
	    (split-string-with-coding string))

    wrappoint))

(defun ps-basic-plot-whitespace (from to &optional bg-color)
  (let* ((wrappoint (ps-find-wrappoint from to
				       (ps-space-width 'ps-font-for-text)))
	 (to (car wrappoint)))
    (ps-output (format "/f%d F " ps-current-font))
    (ps-output (format "%d W\n" (- to from)))
    wrappoint))
