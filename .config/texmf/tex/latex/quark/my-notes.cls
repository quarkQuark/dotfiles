\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{my-notes}

\LoadClass[reqno]{article}
\RequirePackage[margin=1in]{geometry}
\RequirePackage{amsmath, amssymb, amsthm}
\RequirePackage[most]{tcolorbox}
\RequirePackage{xcolor}

\RequirePackage{fontspec}
\setmainfont{ETbb}

\newtheorem{theorem}{Theorem}[subsection]
\newtheorem{corollary}{Corollary}[theorem]
\newtheorem{lemma}[theorem]{Lemma}
\newtheorem{proposition}[theorem]{Proposition}

\theoremstyle{definition} \newtheorem{definition}[theorem]{Definition}
\theoremstyle{remark} \newtheorem*{remark}{Remark}

% Indents proofs behind a coloured vertical bar.
\colorlet{colProof}{green!40!black}
\tcolorboxenvironment{proof}{
    blanker,breakable,left=3mm,top=4pt,bottom=4pt,
    before skip=10pt,
    after skip=10pt,
    borderline west={.5pt}{0pt}{colProof},
    coltext=colProof
}

% Formats theorems in a yellow box
\colorlet{colThm}{yellow!10!white}
\tcolorboxenvironment{theorem}{
    breakable,top=4pt,bottom=4pt,
    before skip=10pt,
    after skip=10pt,
    colback=colThm,
    boxrule = 0pt
}

% Formats propositions in an orange box
\colorlet{colProp}{orange!10!white}
\tcolorboxenvironment{proposition}{
    breakable,top=4pt,bottom=4pt,
    before skip=10pt,
    after skip=10pt,
    colback=colProp,
    boxrule = 0pt
}

% Indents examples behind a coloured vertical bar, with a title line.
\colorlet{colExampleTitle}{blue!60!black}
\colorlet{colExampleText}{blue!50!gray}
\newtcolorbox{example}[1][]{
    empty, % Empty default parameters
	title=Example: #1,
	attach boxed title to top left,
    minipage boxed title, % Ensures proper line breaking in longer titles
    % (boxed title style requires an overlay)
    boxed title style={empty,size=minimal,toprule=0pt,top=4pt,left=3mm,overlay={}},
    coltitle=colExampleTitle, fonttitle=\bfseries,
    coltext=colExampleText,
    %
    before=\par\medskip\noindent,parbox=false,boxsep=0pt,
    left=3mm,right=0mm,breakable,pad at break=0mm,
    % Use instead of parbox=true. This ensures parskip is inherited by box.
    before upper=\csname @totalleftmargin\endcsname0pt,
    %
    overlay unbroken={\draw[colExampleTitle,line width=.5pt]
        ([xshift=-0pt]title.north west) -- ([xshift=-0pt]frame.south west);
    },
    overlay first={\draw[colExampleTitle,line width=.5pt]
        ([xshift=-0pt]title.north west) -- ([xshift=-0pt]frame.south west);
    },
    overlay middle={\draw[colExampleTitle,line width=.5pt]
        ([xshift=-0pt]frame.north west) -- ([xshift=-0pt]frame.south west);
    },
    overlay last={\draw[colExampleTitle,line width=.5pt]
        ([xshift=-0pt]frame.north west) -- ([xshift=-0pt]frame.south west);
    }
}

\newcommand{\dd}{\,\mathrm{d}}
\newcommand{\eqtag}[1]{\stepcounter{equation}\tag{\theequation}\label{#1}}
