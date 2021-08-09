\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{my-notes}

\LoadClass[reqno]{scrartcl}
\RequirePackage[margin=1in]{geometry}
\RequirePackage{amsmath, amssymb, amsthm}
\RequirePackage[most]{tcolorbox}
\RequirePackage{xcolor}

\RequirePackage{fontspec}
\setmainfont{ETbb}

% Doom-one-light theme.
\definecolor{myRed}{HTML}{e45649}
\definecolor{myOrange}{HTML}{da8548}
\definecolor{myGreen}{HTML}{50a14f}
\definecolor{myBlue}{HTML}{4078f2}
\definecolor{myMagenta}{HTML}{a626a4}
\definecolor{myViolet}{HTML}{b751b6}
\definecolor{myDarkCyan}{HTML}{005478}

% Match doom-themes org-mode heading colours.
\addtokomafont{title}{\color{myMagenta}}
\addtokomafont{section}{\color{myRed}}
\addtokomafont{subsection}{\color{myOrange}}
\addtokomafont{subsubsection}{\color{myViolet}}

% Colours for elements without org-mode parallels.
\DeclareTextFontCommand{\textbf}{\bfseries\color{myDarkCyan}}
\colorlet{colProof}{myGreen}
\colorlet{colExampleTitle}{myBlue}
\colorlet{colExampleText}{myBlue}

% Doom-one-light colours don't really work for backgrounds.
\colorlet{colThm}{yellow!10!white}
\colorlet{colProp}{orange!10!white}
\colorlet{colDef}{cyan!5!white}

\newtheorem{theorem}{Theorem}[section]
\newtheorem{corollary}{Corollary}[theorem]
\newtheorem{lemma}[theorem]{Lemma}
\newtheorem{proposition}[theorem]{Proposition}

\theoremstyle{definition} \newtheorem{definition}[theorem]{Definition}
\theoremstyle{remark} \newtheorem*{remark}{Remark}

% Indents proofs behind a coloured vertical bar.
\tcolorboxenvironment{proof}{
    blanker,breakable,left=3mm,top=4pt,bottom=4pt,
    before skip=10pt,
    after skip=10pt,
    borderline west={.5pt}{0pt}{colProof},
    coltext=colProof
}

% Formats theorems in a yellow box
\tcolorboxenvironment{theorem}{
    breakable,top=4pt,bottom=4pt,
    before skip=10pt,
    after skip=10pt,
    colback=colThm,
    boxrule = 0pt
}

% Formats propositions in an orange box
\tcolorboxenvironment{proposition}{
    breakable,top=4pt,bottom=4pt,
    before skip=10pt,
    after skip=10pt,
    colback=colProp,
    boxrule = 0pt
}

% Formats definitions in a blue box
\tcolorboxenvironment{definition}{
    breakable,top=4pt,bottom=4pt,
    before skip=10pt,
    after skip=10pt,
    colback=colDef,
    boxrule = 0pt
}

% Indents examples behind a coloured vertical bar, with a title line.
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
