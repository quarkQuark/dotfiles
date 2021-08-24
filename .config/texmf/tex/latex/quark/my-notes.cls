\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{my-notes}

\LoadClass[reqno]{scrartcl}
\RequirePackage[margin=1in]{geometry}
\RequirePackage{amsmath, amssymb, amsthm}
\RequirePackage[most]{tcolorbox}
\RequirePackage{xcolor}

% My custom colours for headings and environments.
\RequirePackage{my-colours}

% Requires LuaTeX or XeTeX.
\RequirePackage{fontspec}
\setmainfont{ETbb}

% Match doom-themes org-mode heading colours.
\addtokomafont{title}{\color{myMagenta}}
\addtokomafont{section}{\color{myRed}}
\addtokomafont{subsection}{\color{myOrange}}
\addtokomafont{subsubsection}{\color{myViolet}}

\newtheorem{theorem}{Theorem}[section]
\newtheorem{corollary}{Corollary}[theorem]
\newtheorem{lemma}[theorem]{Lemma}
\newtheorem{proposition}[theorem]{Proposition}

\theoremstyle{definition} \newtheorem{definition}[theorem]{Definition}
\theoremstyle{remark} \newtheorem*{remark}{Remark}

\newcommand{\dd}{\,\mathrm{d}}
\newcommand{\eqtag}[1]{\stepcounter{equation}\tag{\theequation}\label{#1}}
