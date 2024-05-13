/*
 [The "BSD licence"]
 Copyright (c) 2014 Vlad Shlosberg
 Copyright (c) 2022 Sergei Russkikh
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

// $antlr-format alignTrailingComments true, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments false, useTab false


// $antlr-format allowShortRulesOnASingleLine true, allowShortBlocksOnASingleLine true, minEmptyLines 0, alignSemicolons ownLine


// $antlr-format alignColons trailing, singleLineOverrulesHangingColon true, alignLexerCommands true, alignLabels true, alignTrailers true

lexer grammar HTMLLexer;

fragment Hex
   : [0-9a-fA-F]
   ;

fragment NewlineOrSpace
   : '\r\n'
   | [ \t\r\n\f]
   |
   ;

fragment Unicode
   : '\\' Hex Hex? Hex? Hex? Hex? Hex? NewlineOrSpace
   ;

fragment Escape
   : Unicode
   | '\\' ~ [\r\n\f0-9a-fA-F]
   ;

fragment Whitespace
   : Space
   ;

fragment Newline
   : '\n'
   | '\r\n'
   | '\r'
   | '\f'
   ;

fragment ZeroToFourZeros
   : '0'? '0'? '0'? '0'?
   ;

fragment DashChar
   : '-'
   | '\\' ZeroToFourZeros '2d' NewlineOrSpace
   ;

fragment Nmstart
   : [_a-zA-Z]
   | Nonascii
   | Escape
   ;

fragment Nmchar
   : [_a-zA-Z0-9\-]
   | Nonascii
   | Escape
   ;

fragment Nonascii
   : ~ [\u0000-\u007f]
   ;

fragment Name
   : Nmchar+
   ;

fragment Url
   : ([!#$%&*-~] | Nonascii | Escape)*
   ;
   // comment from html && xml 
   
MultiLineComment
   : '<!--' .*? '-->' -> channel (HIDDEN)
   ;
   // long comment css js java 
   
COMMENTCSS
   : '/*' .*? '*/' -> channel (HIDDEN)
   ;
   // comment from js css and ... 
   
LineComment
   : '//' ~ [\r\n\u2028\u2029]* -> channel (HIDDEN)
   ;

Space
   : [ \t\r\n\f]+ -> skip
   ;

Uri
   : 'url(' Whitespace (Url | String_) (Space (Url | String_))* Whitespace ')'
   ;

SLASH_CLOSE
   : '/>' -> popMode
   ;

Format
   : 'format(' Whitespace String_ Whitespace ')'
   ;

NameRule
   : Hex '(' Whitespace Nmstart Whitespace ')'
   ;
   //from react js
   
Colen
   : ':'
   | '=>'
   | '==>'
   ;

MYCLASS
   : 'class2' [ \t]* [a-zA-Z_]+ [ \t]*
   ///for case mode js and php
   | [ \t]+ [a-zA-Z0-9_]+ '.'+ [a-zA-Z0-9_]+ ':'
   ;

HASHSTYLE
   : '#' [a-zA-Z_]* '-' [a-zA-Z_]*
   | '#' [a-zA-Z0-9_]*
   | '#'
   | '@' [a-zA-Z_]*
   ;

GREENBOLD
   : ':hover'
   | ':before'
   ;

FontRelative
   : 'em'
   | 'ex'
   | 'ch'
   | 'rem'
   | 'px'
   ;

ViewportRelative
   : 'vw'
   | 'vh'
   | 'vmin'
   | 'vmax'
   ;
   //io.github.rosemoe.sora.langs.html
   
JsKeyWord
   : 'abstract'
   | 'else'
   | 'instanceof'
   | 'super'
   | 'boolean'
   | 'enum'
   | 'int'
   | 'switch'
   | 'break'
   | 'export'
   ;

JsKeyWord1
   : 'interface'
   | 'synchronized'
   | 'byte'
   | 'extends'
   | 'let'
   | 'this'
   | 'case'
   | 'long'
   | 'throw'
   | 'catch'
   ;

JsKeyWord2
   : 'final'
   | 'native'
   | 'throws'
   | 'char'
   | 'finally'
   | 'new'
   | 'transient'
   | 'class'
   | 'null'
   | 'const'
   | 'for'
   ;

JsKeyWord3
   : 'package'
   | 'try'
   | 'continue'
   | 'function'
   | 'private'
   | 'typeof'
   | 'debugger'
   | 'goto'
   | 'protected'
   ;
   // from test
   
JsKeyWord4
   : 'var'
   | 'default'
   | 'public'
   | 'void'
   | 'delete'
   | 'implements'
   | 'return'
   | 'volatile'
   | 'do'
   | 'import'
   | 'short'
   | 'while'
   ;

JsKeyWord5
   : 'double'
   | 'static'
   | 'of'
   | 'console.log'
   | 'prototype'
   | 'alert'
   | 'support.function'
   | 'yield'
   | 'async'
   | 'document'
   | 'arguments'
   | 'parseInt'
   | 'await'
   ;

JsKeyWord6
   : 'undefined'
   | 'parseFloat'
   | 'get'
   | 'storage.type'
   | 'infinity'
   | 'escape'
   | '__count__'
   | '__parent__'
   | '__proto__'
   | 'window'
   | 'eval'
   | 'getElementById'
   | 'getElementByClass '
   ;

VAR_WS_EQUALS
   : [ \t]+ [a-zA-Z_0-9]+ [ \t]* '='
   | [ \t]+ [a-zA-Z_0-9]+ [ \t]* '(' [a-zA-Z_0-9]* ')'
   | '(' [a-zA-Z_0-9]* ')'
   //forNameEnd like in Myapp()
   | [a-zA-Z_0-9]* '()'
   | [a-zA-Z_0-9]* '(' .*? ')'
   ;

COLORUPPERCASE
   : [A-Z_]*
   ;

REACTBRACET
   : '{' [a-zA-Z0-9_]* '}'
   | '{' [ \t]+ [a-zA-Z0-9_]+ [ \t]* '}'
   | '[' [a-zA-Z0-9_]* ']'
   | '[' [ \t]+ [a-zA-Z0-9_]+ [ \t]* ']'
   //test
   | 'import '+ '{' [ \t]+ [a-zA-Z0-9_]+ [ \t]* '}'+ 'from'+ String_
   ;

HtmlTags
   : 'a'
   | 'abbr'
   | 'acronym'
   | 'address'
   | 'applet'
   | 'article'
   | 'aside'
   | 'audio'
   | 'b'
   | 'basefont'
   | 'bdi'
   | 'bdo'
   | 'bgsound'
   | 'big'
   | 'blink'
   | 'blockquote'
   | 'body'
   | 'button'
   | 'canvas'
   | 'caption'
   | 'center'
   | 'circle'
   | 'clipPath'
   | 'code'
   | 'colgroup'
   | 'command'
   | 'content'
   | 'data'
   | 'datalist'
   | 'dd'
   | 'defs'
   | 'del'
   | 'details'
   | 'dfn'
   | 'dialog'
   | 'dir'
   | 'div'
   | 'dl'
   | 'dt'
   | 'element'
   | 'ellipse'
   | 'fieldset'
   | 'figcaption'
   | 'figure'
   | 'font'
   | 'footer'
   | 'foreignObject'
   | 'frame'
   | 'frameset'
   | 'g'
   | 'h1'
   | 'h2'
   | 'h3'
   | 'h4'
   | 'h5'
   | 'h6'
   | 'head'
   | 'header'
   | 'hgroup'
   | 'html'
   | 'i'
   | 'iframe'
   | 'image'
   | 'ins'
   | 'isindex'
   | 'kbd'
   | 'label'
   | 'legend'
   | 'li'
   | 'line'
   | 'linearGradient'
   | 'listing'
   | 'main'
   | 'map'
   | 'mark'
   | 'marquee'
   | 'mask'
   | 'math'
   | 'menu'
   | 'menuitem'
   | 'meter'
   | 'multicol'
   | 'nav'
   | 'nextid'
   | 'nobr'
   | 'noembed'
   | 'noframes'
   | 'noscript'
   | 'object'
   | 'ol'
   | 'optgroup'
   | 'option'
   | 'output'
   | 'p'
   | 'path'
   | 'pattern'
   | 'picture'
   | 'plaintext'
   | 'polygon'
   | 'polyline'
   | 'pre'
   | 'progress'
   | 'radialGradient'
   | 'rb'
   | 'rbc'
   | 'rect'
   | 'rp'
   | 'rt'
   | 'rtc'
   | 'ruby'
   | 'samp'
   | 'script'
   | 'section'
   | 'select'
   | 'shadow'
   | 'slot'
   | 'small'
   | 'spacer'
   | 'stop'
   | 'strike'
   | 'strong'
   | 'style'
   | 'nostyle'
   | 'sub'
   | 'summary'
   | 'sup'
   | 'svg'
   | 'table'
   | 'tbody'
   | 'td' 'template'
   | 'text'
   | 'textarea'
   | 'tfoot'
   | 'th'
   | 'thead'
   | 'time'
   | 'tr'
   | 'tspan'
   | 'tt'
   | 'u'
   | 'ul'
   | 'video'
   | 'xmp'
   ;

HtmlTagOne
   : 'area'
   | 'base'
   | 'br'
   | 'col'
   | 'embed'
   | 'hr'
   | 'img'
   | 'input'
   | 'keygen'
   | 'link'
   | 'meta'
   | 'param'
   | 'source'
   | 'track'
   | 'wbr'
   | '<!DOCTYPE html>'
   ;

HtmlAttr
   : 'accept'
   | 'accept-charset'
   | 'accesskey'
   | 'action'
   | 'align'
   | 'alt'
   | 'autocomplete'
   | 'autofocus'
   | 'autoplay'
   | 'bgcolor'
   | 'charset'
   | 'checked'
   | 'cite'
   | 'cols'
   | 'colspan'
   | 'contenteditable'
   | 'controls'
   | 'coords'
   | 'data-*'
   | 'datetime'
   | 'defer'
   | 'dirname'
   | 'disabled'
   | 'draggable'
   | 'dropzone'
   | 'enctype'
   | 'face'
   | 'form'
   | 'formaction'
   | 'headers'
   | 'hidden'
   | 'high'
   | 'href'
   | 'hreflang'
   | 'http-equiv'
   | 'id'
   | 'ismap'
   | 'kind'
   | 'lang'
   | 'list'
   | 'loop'
   | 'low'
   | 'max'
   | 'maxlength'
   | 'media'
   | 'method'
   | 'min'
   | 'multiple'
   | 'muted'
   | 'name'
   | 'novalidate'
   | 'onabort'
   | 'onafterprint'
   | 'onbeforeprint'
   | 'onbeforeunload'
   | 'onblur'
   | 'oncanplay'
   | 'oncanplaythrough'
   | 'oncanplaythrough'
   | 'onchange'
   | 'onclick'
   | 'oncontextmenu'
   | 'oncopy'
   | 'oncuechange'
   | 'oncut'
   | 'ondblclick'
   | 'ondrag'
   | 'ondragend'
   | 'ondragenter'
   | 'ondragleave'
   | 'ondragover'
   | 'ondragstart'
   | 'ondrop'
   | 'ondurationchange'
   | 'onemptied'
   | 'onended'
   | 'onerror'
   | 'onfocus'
   | 'onhashchange'
   | 'oninput'
   | 'oninvalid'
   | 'onkeydown'
   | 'onkeypress'
   | 'onkeyup'
   | 'onload'
   | 'onloadeddata'
   | 'onloadedmetadata'
   | 'onloadstart'
   | 'onmousedown'
   | 'onmousemove'
   | 'onmouseout'
   | 'onmouseover'
   | 'onmouseup'
   | 'onmousewheel'
   | 'onoffline'
   | 'ononline'
   | 'onpagehide'
   | 'onpageshow'
   | 'onpaste'
   | 'onpause'
   | 'onplay'
   | 'onplaying'
   | 'onpopstate'
   | 'onprogress'
   | 'onratechange'
   | 'onreset'
   | 'onresize'
   | 'onscroll'
   | 'onsearch'
   | 'onseeked'
   | 'onseeking'
   | 'onselect'
   | 'onstalled'
   | 'onstorage'
   | 'onsubmit'
   | 'onsuspend'
   | 'ontimeupdate'
   | 'ontoggle'
   | 'onunload'
   | 'onvolumechange'
   | 'onwaiting'
   | 'onwheel'
   | 'open'
   | 'optimum'
   | 'pattern'
   | 'placeholder'
   | 'poster'
   | 'preload'
   | 'readonly'
   | 'rel'
   | 'required'
   | 'reversed'
   | 'rows'
   | 'rows'
   | 'rowspan'
   | 'sandbox'
   | 'scope'
   | 'selected'
   | 'shape'
   | 'size'
   | 'sizes'
   | 'span'
   | 'spellcheck'
   | 'src'
   | 'srcdoc'
   | 'srclang'
   | 'srcset'
   | 'start'
   | 'step'
   | 'tabindex'
   | 'target'
   | 'title'
   | 'translate'
   | 'type'
   | 'usemap'
   | 'value'
   | 'wrap'
   ;

CSSKEYWORD
   : 'align-content'
   | 'align-items'
   | 'align-self'
   | 'all'
   | 'animation'
   | 'animation-delay'
   | 'animation-direction'
   | 'animation-duration'
   | 'animation-fill-mode'
   | 'animation-iteration-count'
   | 'animation-name'
   | 'animation-play-state'
   | 'animation-timing-function'
   | 'backface-visibility'
   | 'background'
   | 'background-attachment'
   | 'background-blur'
   | 'background-clip'
   | 'background-color'
   | 'background-image'
   | 'background-origin'
   | 'background-position'
   | 'background-repeat'
   | 'background-size'
   | 'border'
   | 'border-bottom'
   | 'border-bottom-color'
   | 'border-bottom-left-radius'
   | 'border-bottom-right-radius'
   | 'border-bottom-style'
   | 'border-bottom-width'
   | 'border-collapse'
   | 'border-color'
   | 'border-image'
   | 'border-image-outset'
   | 'border-image-repeat'
   | 'border-image-slice'
   | 'border-image-source'
   | 'border-image-width'
   | 'border-left'
   | 'border-left-color'
   | 'border-left-style'
   | 'border-left-width'
   | 'border-radius'
   | 'border-right'
   | 'border-right-color'
   | 'border-right-style'
   | 'border-right-width'
   | 'border-spacing'
   | 'border-style'
   | 'border-top'
   | 'border-top-color'
   | 'border-top-left-radius'
   | 'border-top-right-radius'
   | 'border-top-style'
   | 'border-top-width'
   | 'border-width'
   | 'bottom'
   | 'box-decoration-break'
   | 'box-shadow'
   | 'box-sizing'
   | 'break-after'
   | 'break-before'
   | 'break-inside'
   | 'caption-side'
   | 'caret-color'
   | 'clear'
   | 'clip'
   | 'color'
   | 'column-count'
   | 'column-fill'
   | 'column-gap'
   | 'column-rule'
   | 'column-rule-color'
   | 'column-rule-style'
   | 'column-rule-width'
   | 'column-span'
   | 'column-width'
   | 'columns'
   | 'counter-increment'
   | 'counter-reset'
   | 'cursor'
   | 'direction'
   | 'display'
   | 'empty-cells'
   | 'filter'
   | 'flex'
   | 'flex-basis'
   | 'flex-direction'
   | 'flex-flow'
   | 'flex-grow'
   | 'flex-shrink'
   | 'flex-wrap'
   | 'float'
   | 'font-family'
   | 'font-size'
   | 'font-size-adjust'
   | 'font-stretch'
   | 'font-style'
   | 'font-variant'
   | 'font-variant-caps'
   | 'font-weight'
   | 'gap'
   | 'grid'
   | 'grid-area'
   | 'grid-auto-columns'
   | 'grid-auto-flow'
   | 'grid-auto-rows'
   | 'grid-column'
   | 'grid-column-end'
   | 'grid-column-gap'
   | 'grid-column-start'
   | 'grid-gap'
   | 'grid-row'
   | 'grid-row-end'
   | 'grid-row-gap'
   | 'grid-row-start'
   | 'grid-template'
   | 'grid-template-areas'
   | 'grid-template-columns'
   | 'grid-template-rows'
   | 'height'
   | 'ime-mode'
   | 'justify-content'
   | 'left'
   | 'letter-spacing'
   | 'line-break'
   | 'line-height'
   | 'list-style'
   | 'list-style-image'
   | 'list-style-position'
   | 'list-style-type'
   | 'margin'
   | 'margin-bottom'
   | 'margin-left'
   | 'margin-right'
   | 'margin-top'
   | 'marker-offset'
   | 'max-height'
   | 'max-width'
   | 'min-height'
   | 'min-width'
   | 'mix-blend-mode'
   | 'object-fit'
   | 'object-position'
   | 'opacity'
   | 'order'
   | 'orphans'
   | 'outline'
   | 'outline-color'
   | 'outline-offset'
   | 'outline-style'
   | 'outline-width'
   | 'overflow'
   | 'overflow-wrap'
   | 'overflow-x'
   | 'overflow-y'
   | 'padding'
   | 'padding-bottom'
   | 'padding-left'
   | 'padding-right'
   | 'padding-top'
   | 'page-break-after'
   | 'page-break-before'
   | 'page-break-inside'
   | 'perspective'
   | 'perspective-origin'
   | 'pointer-events'
   | 'position'
   | 'quotes'
   | 'resize'
   | 'right'
   | 'row-gap'
   | 'scroll-behavior'
   | 'speak'
   | 'table-layout'
   | 'tab-size'
   | 'text-align'
   | 'text-align-last'
   | 'text-decoration'
   | 'text-decoration-color'
   | 'text-decoration-line'
   | 'text-decoration-skip'
   | 'text-decoration-style'
   | 'text-indent'
   | 'text-justify'
   | 'text-overflow'
   | 'text-shadow'
   | 'text-transform'
   | 'text-underline-position'
   | 'top'
   | 'transform'
   | 'transform-origin'
   | 'transform-style'
   | 'transition'
   | 'transition-delay'
   | 'transition-duration'
   | 'transition-property'
   | 'transition-timing-function'
   | 'unicode-bidi'
   | 'vertical-align'
   | 'visibility'
   | 'white-space'
   | 'widows'
   | 'width'
   | 'will-change'
   | 'word-break'
   | 'word-spacing'
   | 'word-wrap'
   | 'writing-mode'
   | 'z-index'
   | '@import url(' //from css url 
   
   ;

CSSCOLOR
   : 'aliceblue'
   | 'antiquewhite'
   | 'aqua'
   | 'aquamarine'
   | 'azure'
   | 'beige'
   | 'bisque'
   | 'black'
   | 'blanchedalmond'
   | 'blue'
   | 'blueviolet'
   | 'brown'
   | 'burlywood'
   | 'cadetblue'
   | 'chartreuse'
   | 'chocolate'
   | 'coral'
   | 'cornflowerblue'
   | 'cornsilk'
   | 'crimson'
   | 'cyan'
   | 'darkblue'
   | 'darkcyan'
   | 'darkgoldenrod'
   | 'darkgray'
   | 'darkgreen'
   | 'darkkhaki'
   | 'darkmagenta'
   | 'darkolivegreen'
   | 'darkorange'
   | 'darkorchid'
   | 'darkred'
   | 'darksalmon'
   | 'darkseagreen'
   | 'darkslateblue'
   | 'darkslategray'
   | 'darkturquoise'
   | 'darkviolet'
   | 'deeppink'
   | 'deepskyblue'
   | 'dimgray'
   | 'dodgerblue'
   | 'firebrick'
   | 'floralwhite'
   | 'forestgreen'
   | 'fuchsia'
   | 'gainsboro'
   | 'ghostwhite'
   | 'gold'
   | 'goldenrod'
   | 'gray'
   | 'green'
   | 'greenyellow'
   | 'honeydew'
   | 'hotpink'
   | 'indianred'
   | 'indigo'
   | 'ivory'
   | 'khaki'
   | 'lavender'
   | 'lavenderblush'
   | 'lawngreen'
   | 'lemonchiffon'
   | 'lightblue'
   | 'lightcoral'
   | 'lightcyan'
   | 'lightgoldenrodyellow'
   | 'lightgray'
   | 'lightgreen'
   | 'lightpink'
   | 'lightsalmon'
   | 'lightseagreen'
   | 'lightskyblue'
   | 'lightslategray'
   | 'lightsteelblue'
   | 'lightyellow'
   | 'lime'
   | 'limegreen'
   | 'linen'
   | 'magenta'
   | 'maroon'
   | 'mediumaquamarine'
   | 'mediumblue'
   | 'mediumorchid'
   | 'mediumpurple'
   | 'mediumseagreen'
   | 'mediumslateblue'
   | 'mediumspringgreen'
   | 'mediumturquoise'
   | 'mediumvioletred'
   | 'midnightblue'
   | 'mintcream'
   | 'mistyrose'
   | 'moccasin'
   | 'navajowhite'
   | 'navy'
   | 'oldlace'
   | 'olive'
   | 'olivedrab'
   | 'orange'
   | 'orangered'
   | 'orchid'
   | 'palegoldenrod'
   | 'palegreen'
   | 'paleturquoise'
   | 'palevioletred'
   | 'papayawhip'
   | 'peachpuff'
   | 'peru'
   | 'pink'
   | 'plum'
   | 'powderblue'
   | 'purple'
   | 'rebeccapurple'
   | 'red'
   | 'rosybrown'
   | 'royalblue'
   | 'saddlebrown'
   | 'salmon'
   | 'sandybrown'
   | 'seagreen'
   | 'seashell'
   | 'sienna'
   | 'silver'
   | 'skyblue'
   | 'slateblue'
   | 'slategray'
   | 'snow'
   | 'springgreen'
   | 'steelblue'
   | 'tan'
   | 'teal'
   | 'thistle'
   | 'tomato'
   | 'turquoise'
   | 'violet'
   | 'wheat'
   | 'white'
   | 'whitesmoke'
   | 'yellow'
   | 'yellowgreen'
   ;
/* ÷÷÷ */
   
   
PHPSTARTENDS
   : '<?'
   | '?>'
   ;

Angle
   : 'deg'
   | 'rad'
   | 'grad'
   | 'turn'
   ;

Resolution
   : 'dpi'
   | 'dpcm'
   | 'dppx'
   ;

Freq
   : 'hz'
   | 'khz'
   | 'fr'
   ;

Time
   : 'ms'
   | 's'
   ;

Percentage
   : '%'
   ;

Import
   : '@import'
   ;

Include
   : '@include'
   ;

Use
   : '@use'
   ;

Require
   : '@require'
   ;

Charset
   : '@charset '
   ;

Mixin
   : '@mixin'
   ;

Function
   : '@function'
   ;

FontFace
   : '@font-face'
   ;

Forward
   : '@forward'
   ;

Content
   : '@content'
   ;

Keyframes
   : '@keyframes'
   ;

Return
   : '@return'
   ;

Media
   : '@media'
   ;

Extend
   : '@extend'
   ;

Warn
   : '@warn'
   ;

Error
   : '@error'
   ;

If
   : 'if'
   ;

AtIf
   : '@if'
   ;

AtFor
   : '@for'
   ;

AtElse
   : '@else'
   ;

AtWhile
   : '@while'
   ;

AtEach
   : '@each'
   ;

From
   : 'from'
   ;

To
   : 'to'
   ;

Through
   : 'through'
   ;

Only
   : 'only'
   ;

Not
   : 'not'
   ;

And
   : 'and'
   ;

Using
   : 'using'
   ;

As
   : 'as'
   ;

With
   : 'with'
   ;

Or
   : 'or'
   ;

In
   : 'in'
   ;

Default
   : '!default'
   ;

Important
   : '!important'
   ;

Lparen
   : '('
   ;

Rparen
   : ')'
   ;

Lbrack
   : '['
   ;

Rbrack
   : ']'
   ;

BlockStart
   : '{'
   ;

BlockEnd
   : '}'
   ;

fragment LETTER
   : [a-zA-Z]
   ;

fragment DIGIT
   : [0-9]
   ;

Dot
   : '.' (LETTER | DIGIT)*
   | '.' [a-zA-Z_]* '-' [a-zA-Z_]*
   ;

Comma
   : ','
   ;

Colon
   : ':'
   ;

Semi
   : ';'
   ;

Tilde
   : '~'
   ;

Under
   : '_'
   ;

Dollar
   : '$'
   ;

At
   : '@'
   ;

Amp
   : '&'
   ;

Hash
   : '#'
   ;

True
   : 'true'
   ;

False
   : 'false'
   ;

Plus
   : '+'
   ;

Div
   : '/'
   ;

Minus
   : '-'
   ;

Times
   : '*'
   ;

Eq
   : '='
   ;

NotEq
   : '!='
   ;

Greater
   : '>'
   ;

Less
   : '<'
   ;

Includes
   : '~='
   ;

DashMatch
   : '|='
   ;

Pipe
   : '|'
   ;

Cdo
   : '<!--'
   ;

Cdc
   : '-->'
   ;

PseudoNot
   : ':not('
   ;

Calc
   : 'calc('
   ;

Rotate
   : 'rotate('
   ;

Var
   : 'var('
   ;

Rgba
   : 'rgba('
   ;

Repeat
   : 'repeat('
   ;

PrefixMatch
   : '^='
   ;

SuffixMatch
   : '$='
   ;

SubstringMatch
   : '*='
   ;

VendorPrefix
   : '-moz-'
   | '-webkit-'
   | '-o-'
   ;

Variable
   : '--' (Interpolation | Nmstart) (Interpolation | Nmchar)*
   ;

fragment Interpolation
   : Hash BlockStart Dollar? Ident BlockEnd
   ;

Number
   : [0-9]+
   | [0-9]* '.' [0-9]+
   ;

String_
   : '"' (~ [\n\r\f\\"] | '\\' Newline | Escape)* '"'
   | '\'' (~ [\n\r\f\\'] | '\\' Newline | Escape)* '\''
   ;
   // Give Ident least priority so that more specific rules matches first
   
Ident
   : Nmstart Nmchar*
   ;

