//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

const LONGNAMES_COUNT: usize = 1102;
const RAWLONGNAMES_COUNT: usize = 37;

const MBNOTSTRANGELETTERLIKECODEPOINTS_COUNT: usize = 296;
const MBPUNCTUATIONCODEPOINTS_COUNT: usize = 301;
const MBWHITESPACECODEPOINTS_COUNT: usize = 20;
const MBNEWLINECODEPOINTS_COUNT: usize = 6;
const MBUNINTERPRETABLECODEPOINTS_COUNT: usize = 4;

//
// All long name code points
//
pub const CODEPOINT_LONGNAME_RAWTAB: CodePoint = CodePoint::Char('\u{0009}');
pub const CODEPOINT_LONGNAME_NEWLINE: CodePoint = CodePoint::Char('\u{000a}');
pub const CODEPOINT_LONGNAME_RAWRETURN: CodePoint = CodePoint::Char('\u{000d}');
pub const CODEPOINT_LONGNAME_RAWESCAPE: CodePoint = CodePoint::Char('\u{001b}');
pub const CODEPOINT_LONGNAME_RAWSPACE: CodePoint = CodePoint::Char('\u{0020}');
pub const CODEPOINT_LONGNAME_RAWEXCLAMATION: CodePoint = CodePoint::Char('\u{0021}');
pub const CODEPOINT_LONGNAME_RAWDOUBLEQUOTE: CodePoint = CODEPOINT_STRINGMETA_DOUBLEQUOTE;
pub const CODEPOINT_LONGNAME_RAWNUMBERSIGN: CodePoint = CodePoint::Char('\u{0023}');
pub const CODEPOINT_LONGNAME_RAWDOLLAR: CodePoint = CodePoint::Char('\u{0024}');
pub const CODEPOINT_LONGNAME_RAWPERCENT: CodePoint = CodePoint::Char('\u{0025}');
pub const CODEPOINT_LONGNAME_RAWAMPERSAND: CodePoint = CodePoint::Char('\u{0026}');
pub const CODEPOINT_LONGNAME_RAWQUOTE: CodePoint = CodePoint::Char('\u{0027}');
pub const CODEPOINT_LONGNAME_RAWLEFTPARENTHESIS: CodePoint = CodePoint::Char('\u{0028}');
pub const CODEPOINT_LONGNAME_RAWRIGHTPARENTHESIS: CodePoint = CodePoint::Char('\u{0029}');
pub const CODEPOINT_LONGNAME_RAWSTAR: CodePoint = CodePoint::Char('\u{002a}');
pub const CODEPOINT_LONGNAME_RAWPLUS: CodePoint = CodePoint::Char('\u{002b}');
pub const CODEPOINT_LONGNAME_RAWCOMMA: CodePoint = CodePoint::Char('\u{002c}');
pub const CODEPOINT_LONGNAME_RAWDASH: CodePoint = CodePoint::Char('\u{002d}');
pub const CODEPOINT_LONGNAME_RAWDOT: CodePoint = CodePoint::Char('\u{002e}');
pub const CODEPOINT_LONGNAME_RAWSLASH: CodePoint = CodePoint::Char('\u{002f}');
pub const CODEPOINT_LONGNAME_RAWCOLON: CodePoint = CodePoint::Char('\u{003a}');
pub const CODEPOINT_LONGNAME_RAWSEMICOLON: CodePoint = CodePoint::Char('\u{003b}');
pub const CODEPOINT_LONGNAME_RAWLESS: CodePoint = CodePoint::Char('\u{003c}');
pub const CODEPOINT_LONGNAME_RAWEQUAL: CodePoint = CodePoint::Char('\u{003d}');
pub const CODEPOINT_LONGNAME_RAWGREATER: CodePoint = CodePoint::Char('\u{003e}');
pub const CODEPOINT_LONGNAME_RAWQUESTION: CodePoint = CodePoint::Char('\u{003f}');
pub const CODEPOINT_LONGNAME_RAWAT: CodePoint = CodePoint::Char('\u{0040}');
pub const CODEPOINT_LONGNAME_RAWLEFTBRACKET: CodePoint = CodePoint::Char('\u{005b}');
pub const CODEPOINT_LONGNAME_RAWBACKSLASH: CodePoint = CODEPOINT_STRINGMETA_BACKSLASH;
pub const CODEPOINT_LONGNAME_RAWRIGHTBRACKET: CodePoint = CodePoint::Char('\u{005d}');
pub const CODEPOINT_LONGNAME_RAWWEDGE: CodePoint = CodePoint::Char('\u{005e}');
pub const CODEPOINT_LONGNAME_RAWUNDERSCORE: CodePoint = CodePoint::Char('\u{005f}');
pub const CODEPOINT_LONGNAME_RAWBACKQUOTE: CodePoint = CodePoint::Char('\u{0060}');
pub const CODEPOINT_LONGNAME_RAWLEFTBRACE: CodePoint = CodePoint::Char('\u{007b}');
pub const CODEPOINT_LONGNAME_RAWVERTICALBAR: CodePoint = CodePoint::Char('\u{007c}');
pub const CODEPOINT_LONGNAME_RAWRIGHTBRACE: CodePoint = CodePoint::Char('\u{007d}');
pub const CODEPOINT_LONGNAME_RAWTILDE: CodePoint = CodePoint::Char('\u{007e}');
pub const CODEPOINT_LONGNAME_NONBREAKINGSPACE: CodePoint = CodePoint::Char('\u{00a0}');
pub const CODEPOINT_LONGNAME_DOWNEXCLAMATION: CodePoint = CodePoint::Char('\u{00a1}');
pub const CODEPOINT_LONGNAME_CENT: CodePoint = CodePoint::Char('\u{00a2}');
pub const CODEPOINT_LONGNAME_STERLING: CodePoint = CodePoint::Char('\u{00a3}');
pub const CODEPOINT_LONGNAME_CURRENCY: CodePoint = CodePoint::Char('\u{00a4}');
pub const CODEPOINT_LONGNAME_YEN: CodePoint = CodePoint::Char('\u{00a5}');
pub const CODEPOINT_LONGNAME_SECTION: CodePoint = CodePoint::Char('\u{00a7}');
pub const CODEPOINT_LONGNAME_DOUBLEDOT: CodePoint = CodePoint::Char('\u{00a8}');
pub const CODEPOINT_LONGNAME_COPYRIGHT: CodePoint = CodePoint::Char('\u{00a9}');
pub const CODEPOINT_LONGNAME_LEFTGUILLEMET: CodePoint = CodePoint::Char('\u{00ab}');
pub const CODEPOINT_LONGNAME_NOT: CodePoint = CodePoint::Char('\u{00ac}');
pub const CODEPOINT_LONGNAME_DISCRETIONARYHYPHEN: CodePoint = CodePoint::Char('\u{00ad}');
pub const CODEPOINT_LONGNAME_REGISTEREDTRADEMARK: CodePoint = CodePoint::Char('\u{00ae}');
pub const CODEPOINT_LONGNAME_DEGREE: CodePoint = CodePoint::Char('\u{00b0}');
pub const CODEPOINT_LONGNAME_PLUSMINUS: CodePoint = CodePoint::Char('\u{00b1}');
pub const CODEPOINT_LONGNAME_MICRO: CodePoint = CodePoint::Char('\u{00b5}');
pub const CODEPOINT_LONGNAME_PARAGRAPH: CodePoint = CodePoint::Char('\u{00b6}');
pub const CODEPOINT_LONGNAME_CENTERDOT: CodePoint = CodePoint::Char('\u{00b7}');
pub const CODEPOINT_LONGNAME_CEDILLA: CodePoint = CodePoint::Char('\u{00b8}');
pub const CODEPOINT_LONGNAME_RIGHTGUILLEMET: CodePoint = CodePoint::Char('\u{00bb}');
pub const CODEPOINT_LONGNAME_DOWNQUESTION: CodePoint = CodePoint::Char('\u{00bf}');
pub const CODEPOINT_LONGNAME_CAPITALAGRAVE: CodePoint = CodePoint::Char('\u{00c0}');
pub const CODEPOINT_LONGNAME_CAPITALAACUTE: CodePoint = CodePoint::Char('\u{00c1}');
pub const CODEPOINT_LONGNAME_CAPITALAHAT: CodePoint = CodePoint::Char('\u{00c2}');
pub const CODEPOINT_LONGNAME_CAPITALATILDE: CodePoint = CodePoint::Char('\u{00c3}');
pub const CODEPOINT_LONGNAME_CAPITALADOUBLEDOT: CodePoint = CodePoint::Char('\u{00c4}');
pub const CODEPOINT_LONGNAME_CAPITALARING: CodePoint = CodePoint::Char('\u{00c5}');
pub const CODEPOINT_LONGNAME_CAPITALAE: CodePoint = CodePoint::Char('\u{00c6}');
pub const CODEPOINT_LONGNAME_CAPITALCCEDILLA: CodePoint = CodePoint::Char('\u{00c7}');
pub const CODEPOINT_LONGNAME_CAPITALEGRAVE: CodePoint = CodePoint::Char('\u{00c8}');
pub const CODEPOINT_LONGNAME_CAPITALEACUTE: CodePoint = CodePoint::Char('\u{00c9}');
pub const CODEPOINT_LONGNAME_CAPITALEHAT: CodePoint = CodePoint::Char('\u{00ca}');
pub const CODEPOINT_LONGNAME_CAPITALEDOUBLEDOT: CodePoint = CodePoint::Char('\u{00cb}');
pub const CODEPOINT_LONGNAME_CAPITALIGRAVE: CodePoint = CodePoint::Char('\u{00cc}');
pub const CODEPOINT_LONGNAME_CAPITALIACUTE: CodePoint = CodePoint::Char('\u{00cd}');
pub const CODEPOINT_LONGNAME_CAPITALIHAT: CodePoint = CodePoint::Char('\u{00ce}');
pub const CODEPOINT_LONGNAME_CAPITALIDOUBLEDOT: CodePoint = CodePoint::Char('\u{00cf}');
pub const CODEPOINT_LONGNAME_CAPITALETH: CodePoint = CodePoint::Char('\u{00d0}');
pub const CODEPOINT_LONGNAME_CAPITALNTILDE: CodePoint = CodePoint::Char('\u{00d1}');
pub const CODEPOINT_LONGNAME_CAPITALOGRAVE: CodePoint = CodePoint::Char('\u{00d2}');
pub const CODEPOINT_LONGNAME_CAPITALOACUTE: CodePoint = CodePoint::Char('\u{00d3}');
pub const CODEPOINT_LONGNAME_CAPITALOHAT: CodePoint = CodePoint::Char('\u{00d4}');
pub const CODEPOINT_LONGNAME_CAPITALOTILDE: CodePoint = CodePoint::Char('\u{00d5}');
pub const CODEPOINT_LONGNAME_CAPITALODOUBLEDOT: CodePoint = CodePoint::Char('\u{00d6}');
pub const CODEPOINT_LONGNAME_TIMES: CodePoint = CodePoint::Char('\u{00d7}');
pub const CODEPOINT_LONGNAME_CAPITALOSLASH: CodePoint = CodePoint::Char('\u{00d8}');
pub const CODEPOINT_LONGNAME_CAPITALUGRAVE: CodePoint = CodePoint::Char('\u{00d9}');
pub const CODEPOINT_LONGNAME_CAPITALUACUTE: CodePoint = CodePoint::Char('\u{00da}');
pub const CODEPOINT_LONGNAME_CAPITALUHAT: CodePoint = CodePoint::Char('\u{00db}');
pub const CODEPOINT_LONGNAME_CAPITALUDOUBLEDOT: CodePoint = CodePoint::Char('\u{00dc}');
pub const CODEPOINT_LONGNAME_CAPITALYACUTE: CodePoint = CodePoint::Char('\u{00dd}');
pub const CODEPOINT_LONGNAME_CAPITALTHORN: CodePoint = CodePoint::Char('\u{00de}');
pub const CODEPOINT_LONGNAME_SZ: CodePoint = CodePoint::Char('\u{00df}');
pub const CODEPOINT_LONGNAME_AGRAVE: CodePoint = CodePoint::Char('\u{00e0}');
pub const CODEPOINT_LONGNAME_AACUTE: CodePoint = CodePoint::Char('\u{00e1}');
pub const CODEPOINT_LONGNAME_AHAT: CodePoint = CodePoint::Char('\u{00e2}');
pub const CODEPOINT_LONGNAME_ATILDE: CodePoint = CodePoint::Char('\u{00e3}');
pub const CODEPOINT_LONGNAME_ADOUBLEDOT: CodePoint = CodePoint::Char('\u{00e4}');
pub const CODEPOINT_LONGNAME_ARING: CodePoint = CodePoint::Char('\u{00e5}');
pub const CODEPOINT_LONGNAME_AE: CodePoint = CodePoint::Char('\u{00e6}');
pub const CODEPOINT_LONGNAME_CCEDILLA: CodePoint = CodePoint::Char('\u{00e7}');
pub const CODEPOINT_LONGNAME_EGRAVE: CodePoint = CodePoint::Char('\u{00e8}');
pub const CODEPOINT_LONGNAME_EACUTE: CodePoint = CodePoint::Char('\u{00e9}');
pub const CODEPOINT_LONGNAME_EHAT: CodePoint = CodePoint::Char('\u{00ea}');
pub const CODEPOINT_LONGNAME_EDOUBLEDOT: CodePoint = CodePoint::Char('\u{00eb}');
pub const CODEPOINT_LONGNAME_IGRAVE: CodePoint = CodePoint::Char('\u{00ec}');
pub const CODEPOINT_LONGNAME_IACUTE: CodePoint = CodePoint::Char('\u{00ed}');
pub const CODEPOINT_LONGNAME_IHAT: CodePoint = CodePoint::Char('\u{00ee}');
pub const CODEPOINT_LONGNAME_IDOUBLEDOT: CodePoint = CodePoint::Char('\u{00ef}');
pub const CODEPOINT_LONGNAME_ETH: CodePoint = CodePoint::Char('\u{00f0}');
pub const CODEPOINT_LONGNAME_NTILDE: CodePoint = CodePoint::Char('\u{00f1}');
pub const CODEPOINT_LONGNAME_OGRAVE: CodePoint = CodePoint::Char('\u{00f2}');
pub const CODEPOINT_LONGNAME_OACUTE: CodePoint = CodePoint::Char('\u{00f3}');
pub const CODEPOINT_LONGNAME_OHAT: CodePoint = CodePoint::Char('\u{00f4}');
pub const CODEPOINT_LONGNAME_OTILDE: CodePoint = CodePoint::Char('\u{00f5}');
pub const CODEPOINT_LONGNAME_ODOUBLEDOT: CodePoint = CodePoint::Char('\u{00f6}');
pub const CODEPOINT_LONGNAME_DIVIDE: CodePoint = CodePoint::Char('\u{00f7}');
pub const CODEPOINT_LONGNAME_OSLASH: CodePoint = CodePoint::Char('\u{00f8}');
pub const CODEPOINT_LONGNAME_UGRAVE: CodePoint = CodePoint::Char('\u{00f9}');
pub const CODEPOINT_LONGNAME_UACUTE: CodePoint = CodePoint::Char('\u{00fa}');
pub const CODEPOINT_LONGNAME_UHAT: CodePoint = CodePoint::Char('\u{00fb}');
pub const CODEPOINT_LONGNAME_UDOUBLEDOT: CodePoint = CodePoint::Char('\u{00fc}');
pub const CODEPOINT_LONGNAME_YACUTE: CodePoint = CodePoint::Char('\u{00fd}');
pub const CODEPOINT_LONGNAME_THORN: CodePoint = CodePoint::Char('\u{00fe}');
pub const CODEPOINT_LONGNAME_YDOUBLEDOT: CodePoint = CodePoint::Char('\u{00ff}');
pub const CODEPOINT_LONGNAME_CAPITALABAR: CodePoint = CodePoint::Char('\u{0100}');
pub const CODEPOINT_LONGNAME_ABAR: CodePoint = CodePoint::Char('\u{0101}');
pub const CODEPOINT_LONGNAME_CAPITALACUP: CodePoint = CodePoint::Char('\u{0102}');
pub const CODEPOINT_LONGNAME_ACUP: CodePoint = CodePoint::Char('\u{0103}');
pub const CODEPOINT_LONGNAME_CAPITALCACUTE: CodePoint = CodePoint::Char('\u{0106}');
pub const CODEPOINT_LONGNAME_CACUTE: CodePoint = CodePoint::Char('\u{0107}');
pub const CODEPOINT_LONGNAME_CAPITALCHACEK: CodePoint = CodePoint::Char('\u{010c}');
pub const CODEPOINT_LONGNAME_CHACEK: CodePoint = CodePoint::Char('\u{010d}');
pub const CODEPOINT_LONGNAME_CAPITALDHACEK: CodePoint = CodePoint::Char('\u{010e}');
pub const CODEPOINT_LONGNAME_DHACEK: CodePoint = CodePoint::Char('\u{010f}');
pub const CODEPOINT_LONGNAME_CAPITALEBAR: CodePoint = CodePoint::Char('\u{0112}');
pub const CODEPOINT_LONGNAME_EBAR: CodePoint = CodePoint::Char('\u{0113}');
pub const CODEPOINT_LONGNAME_CAPITALECUP: CodePoint = CodePoint::Char('\u{0114}');
pub const CODEPOINT_LONGNAME_ECUP: CodePoint = CodePoint::Char('\u{0115}');
pub const CODEPOINT_LONGNAME_CAPITALEHACEK: CodePoint = CodePoint::Char('\u{011a}');
pub const CODEPOINT_LONGNAME_EHACEK: CodePoint = CodePoint::Char('\u{011b}');
pub const CODEPOINT_LONGNAME_CAPITALICUP: CodePoint = CodePoint::Char('\u{012c}');
pub const CODEPOINT_LONGNAME_ICUP: CodePoint = CodePoint::Char('\u{012d}');
pub const CODEPOINT_LONGNAME_DOTLESSI: CodePoint = CodePoint::Char('\u{0131}');
pub const CODEPOINT_LONGNAME_CAPITALLSLASH: CodePoint = CodePoint::Char('\u{0141}');
pub const CODEPOINT_LONGNAME_LSLASH: CodePoint = CodePoint::Char('\u{0142}');
pub const CODEPOINT_LONGNAME_CAPITALNHACEK: CodePoint = CodePoint::Char('\u{0147}');
pub const CODEPOINT_LONGNAME_NHACEK: CodePoint = CodePoint::Char('\u{0148}');
pub const CODEPOINT_LONGNAME_CAPITALODOUBLEACUTE: CodePoint = CodePoint::Char('\u{0150}');
pub const CODEPOINT_LONGNAME_ODOUBLEACUTE: CodePoint = CodePoint::Char('\u{0151}');
pub const CODEPOINT_LONGNAME_CAPITALOE: CodePoint = CodePoint::Char('\u{0152}');
pub const CODEPOINT_LONGNAME_OE: CodePoint = CodePoint::Char('\u{0153}');
pub const CODEPOINT_LONGNAME_CAPITALRHACEK: CodePoint = CodePoint::Char('\u{0158}');
pub const CODEPOINT_LONGNAME_RHACEK: CodePoint = CodePoint::Char('\u{0159}');
pub const CODEPOINT_LONGNAME_CAPITALSHACEK: CodePoint = CodePoint::Char('\u{0160}');
pub const CODEPOINT_LONGNAME_SHACEK: CodePoint = CodePoint::Char('\u{0161}');
pub const CODEPOINT_LONGNAME_CAPITALTHACEK: CodePoint = CodePoint::Char('\u{0164}');
pub const CODEPOINT_LONGNAME_THACEK: CodePoint = CodePoint::Char('\u{0165}');
pub const CODEPOINT_LONGNAME_CAPITALURING: CodePoint = CodePoint::Char('\u{016e}');
pub const CODEPOINT_LONGNAME_URING: CodePoint = CodePoint::Char('\u{016f}');
pub const CODEPOINT_LONGNAME_CAPITALUDOUBLEACUTE: CodePoint = CodePoint::Char('\u{0170}');
pub const CODEPOINT_LONGNAME_UDOUBLEACUTE: CodePoint = CodePoint::Char('\u{0171}');
pub const CODEPOINT_LONGNAME_CAPITALZHACEK: CodePoint = CodePoint::Char('\u{017d}');
pub const CODEPOINT_LONGNAME_ZHACEK: CodePoint = CodePoint::Char('\u{017e}');
pub const CODEPOINT_LONGNAME_FLORIN: CodePoint = CodePoint::Char('\u{0192}');
pub const CODEPOINT_LONGNAME_HACEK: CodePoint = CodePoint::Char('\u{02c7}');
pub const CODEPOINT_LONGNAME_BREVE: CodePoint = CodePoint::Char('\u{02d8}');
pub const CODEPOINT_LONGNAME_CAPITALALPHA: CodePoint = CodePoint::Char('\u{0391}');
pub const CODEPOINT_LONGNAME_CAPITALBETA: CodePoint = CodePoint::Char('\u{0392}');
pub const CODEPOINT_LONGNAME_CAPITALGAMMA: CodePoint = CodePoint::Char('\u{0393}');
pub const CODEPOINT_LONGNAME_CAPITALDELTA: CodePoint = CodePoint::Char('\u{0394}');
pub const CODEPOINT_LONGNAME_CAPITALEPSILON: CodePoint = CodePoint::Char('\u{0395}');
pub const CODEPOINT_LONGNAME_CAPITALZETA: CodePoint = CodePoint::Char('\u{0396}');
pub const CODEPOINT_LONGNAME_CAPITALETA: CodePoint = CodePoint::Char('\u{0397}');
pub const CODEPOINT_LONGNAME_CAPITALTHETA: CodePoint = CodePoint::Char('\u{0398}');
pub const CODEPOINT_LONGNAME_CAPITALIOTA: CodePoint = CodePoint::Char('\u{0399}');
pub const CODEPOINT_LONGNAME_CAPITALKAPPA: CodePoint = CodePoint::Char('\u{039a}');
pub const CODEPOINT_LONGNAME_CAPITALLAMBDA: CodePoint = CodePoint::Char('\u{039b}');
pub const CODEPOINT_LONGNAME_CAPITALMU: CodePoint = CodePoint::Char('\u{039c}');
pub const CODEPOINT_LONGNAME_CAPITALNU: CodePoint = CodePoint::Char('\u{039d}');
pub const CODEPOINT_LONGNAME_CAPITALXI: CodePoint = CodePoint::Char('\u{039e}');
pub const CODEPOINT_LONGNAME_CAPITALOMICRON: CodePoint = CodePoint::Char('\u{039f}');
pub const CODEPOINT_LONGNAME_CAPITALPI: CodePoint = CodePoint::Char('\u{03a0}');
pub const CODEPOINT_LONGNAME_CAPITALRHO: CodePoint = CodePoint::Char('\u{03a1}');
pub const CODEPOINT_LONGNAME_CAPITALSIGMA: CodePoint = CodePoint::Char('\u{03a3}');
pub const CODEPOINT_LONGNAME_CAPITALTAU: CodePoint = CodePoint::Char('\u{03a4}');
pub const CODEPOINT_LONGNAME_CAPITALUPSILON: CodePoint = CodePoint::Char('\u{03a5}');
pub const CODEPOINT_LONGNAME_CAPITALPHI: CodePoint = CodePoint::Char('\u{03a6}');
pub const CODEPOINT_LONGNAME_CAPITALCHI: CodePoint = CodePoint::Char('\u{03a7}');
pub const CODEPOINT_LONGNAME_CAPITALPSI: CodePoint = CodePoint::Char('\u{03a8}');
pub const CODEPOINT_LONGNAME_CAPITALOMEGA: CodePoint = CodePoint::Char('\u{03a9}');
pub const CODEPOINT_LONGNAME_ALPHA: CodePoint = CodePoint::Char('\u{03b1}');
pub const CODEPOINT_LONGNAME_BETA: CodePoint = CodePoint::Char('\u{03b2}');
pub const CODEPOINT_LONGNAME_GAMMA: CodePoint = CodePoint::Char('\u{03b3}');
pub const CODEPOINT_LONGNAME_DELTA: CodePoint = CodePoint::Char('\u{03b4}');
pub const CODEPOINT_LONGNAME_CURLYEPSILON: CodePoint = CodePoint::Char('\u{03b5}');
pub const CODEPOINT_LONGNAME_ZETA: CodePoint = CodePoint::Char('\u{03b6}');
pub const CODEPOINT_LONGNAME_ETA: CodePoint = CodePoint::Char('\u{03b7}');
pub const CODEPOINT_LONGNAME_THETA: CodePoint = CodePoint::Char('\u{03b8}');
pub const CODEPOINT_LONGNAME_IOTA: CodePoint = CodePoint::Char('\u{03b9}');
pub const CODEPOINT_LONGNAME_KAPPA: CodePoint = CodePoint::Char('\u{03ba}');
pub const CODEPOINT_LONGNAME_LAMBDA: CodePoint = CodePoint::Char('\u{03bb}');
pub const CODEPOINT_LONGNAME_MU: CodePoint = CodePoint::Char('\u{03bc}');
pub const CODEPOINT_LONGNAME_NU: CodePoint = CodePoint::Char('\u{03bd}');
pub const CODEPOINT_LONGNAME_XI: CodePoint = CodePoint::Char('\u{03be}');
pub const CODEPOINT_LONGNAME_OMICRON: CodePoint = CodePoint::Char('\u{03bf}');
pub const CODEPOINT_LONGNAME_PI: CodePoint = CodePoint::Char('\u{03c0}');
pub const CODEPOINT_LONGNAME_RHO: CodePoint = CodePoint::Char('\u{03c1}');
pub const CODEPOINT_LONGNAME_FINALSIGMA: CodePoint = CodePoint::Char('\u{03c2}');
pub const CODEPOINT_LONGNAME_SIGMA: CodePoint = CodePoint::Char('\u{03c3}');
pub const CODEPOINT_LONGNAME_TAU: CodePoint = CodePoint::Char('\u{03c4}');
pub const CODEPOINT_LONGNAME_UPSILON: CodePoint = CodePoint::Char('\u{03c5}');
pub const CODEPOINT_LONGNAME_CURLYPHI: CodePoint = CodePoint::Char('\u{03c6}');
pub const CODEPOINT_LONGNAME_CHI: CodePoint = CodePoint::Char('\u{03c7}');
pub const CODEPOINT_LONGNAME_PSI: CodePoint = CodePoint::Char('\u{03c8}');
pub const CODEPOINT_LONGNAME_OMEGA: CodePoint = CodePoint::Char('\u{03c9}');
pub const CODEPOINT_LONGNAME_CURLYTHETA: CodePoint = CodePoint::Char('\u{03d1}');
pub const CODEPOINT_LONGNAME_CURLYCAPITALUPSILON: CodePoint = CodePoint::Char('\u{03d2}');
pub const CODEPOINT_LONGNAME_PHI: CodePoint = CodePoint::Char('\u{03d5}');
pub const CODEPOINT_LONGNAME_CURLYPI: CodePoint = CodePoint::Char('\u{03d6}');
pub const CODEPOINT_LONGNAME_CAPITALSTIGMA: CodePoint = CodePoint::Char('\u{03da}');
pub const CODEPOINT_LONGNAME_STIGMA: CodePoint = CodePoint::Char('\u{03db}');
pub const CODEPOINT_LONGNAME_CAPITALDIGAMMA: CodePoint = CodePoint::Char('\u{03dc}');
pub const CODEPOINT_LONGNAME_DIGAMMA: CodePoint = CodePoint::Char('\u{03dd}');
pub const CODEPOINT_LONGNAME_CAPITALKOPPA: CodePoint = CodePoint::Char('\u{03de}');
pub const CODEPOINT_LONGNAME_KOPPA: CodePoint = CodePoint::Char('\u{03df}');
pub const CODEPOINT_LONGNAME_CAPITALSAMPI: CodePoint = CodePoint::Char('\u{03e0}');
pub const CODEPOINT_LONGNAME_SAMPI: CodePoint = CodePoint::Char('\u{03e1}');
pub const CODEPOINT_LONGNAME_CURLYKAPPA: CodePoint = CodePoint::Char('\u{03f0}');
pub const CODEPOINT_LONGNAME_CURLYRHO: CodePoint = CodePoint::Char('\u{03f1}');
pub const CODEPOINT_LONGNAME_EPSILON: CodePoint = CodePoint::Char('\u{03f5}');
pub const CODEPOINT_LONGNAME_THICKSPACE: CodePoint = CodePoint::Char('\u{2005}');
pub const CODEPOINT_LONGNAME_THINSPACE: CodePoint = CodePoint::Char('\u{2009}');
pub const CODEPOINT_LONGNAME_VERYTHINSPACE: CodePoint = CodePoint::Char('\u{200a}');
pub const CODEPOINT_LONGNAME_HYPHEN: CodePoint = CodePoint::Char('\u{2010}');
pub const CODEPOINT_LONGNAME_DASH: CodePoint = CodePoint::Char('\u{2013}');
pub const CODEPOINT_LONGNAME_LONGDASH: CodePoint = CodePoint::Char('\u{2014}');
pub const CODEPOINT_LONGNAME_OPENCURLYQUOTE: CodePoint = CodePoint::Char('\u{2018}');
pub const CODEPOINT_LONGNAME_CLOSECURLYQUOTE: CodePoint = CodePoint::Char('\u{2019}');
pub const CODEPOINT_LONGNAME_OPENCURLYDOUBLEQUOTE: CodePoint = CodePoint::Char('\u{201c}');
pub const CODEPOINT_LONGNAME_CLOSECURLYDOUBLEQUOTE: CodePoint = CodePoint::Char('\u{201d}');
pub const CODEPOINT_LONGNAME_DAGGER: CodePoint = CodePoint::Char('\u{2020}');
pub const CODEPOINT_LONGNAME_DOUBLEDAGGER: CodePoint = CodePoint::Char('\u{2021}');
pub const CODEPOINT_LONGNAME_BULLET: CodePoint = CodePoint::Char('\u{2022}');
pub const CODEPOINT_LONGNAME_ELLIPSIS: CodePoint = CodePoint::Char('\u{2026}');
pub const CODEPOINT_LONGNAME_LINESEPARATOR: CodePoint = CodePoint::Char('\u{2028}');
pub const CODEPOINT_LONGNAME_PARAGRAPHSEPARATOR: CodePoint = CodePoint::Char('\u{2029}');
pub const CODEPOINT_LONGNAME_PRIME: CodePoint = CodePoint::Char('\u{2032}');
pub const CODEPOINT_LONGNAME_DOUBLEPRIME: CodePoint = CodePoint::Char('\u{2033}');
pub const CODEPOINT_LONGNAME_REVERSEPRIME: CodePoint = CodePoint::Char('\u{2035}');
pub const CODEPOINT_LONGNAME_REVERSEDOUBLEPRIME: CodePoint = CodePoint::Char('\u{2036}');
pub const CODEPOINT_LONGNAME_SKELETONINDICATOR: CodePoint = CodePoint::Char('\u{2043}');
pub const CODEPOINT_LONGNAME_MEDIUMSPACE: CodePoint = CodePoint::Char('\u{205f}');
pub const CODEPOINT_LONGNAME_NOBREAK: CodePoint = CodePoint::Char('\u{2060}');
pub const CODEPOINT_LONGNAME_INVISIBLETIMES: CodePoint = CodePoint::Char('\u{2062}');
pub const CODEPOINT_LONGNAME_EURO: CodePoint = CodePoint::Char('\u{20ac}');
pub const CODEPOINT_LONGNAME_RUPEE: CodePoint = CodePoint::Char('\u{20b9}');
pub const CODEPOINT_LONGNAME_SCRIPTG: CodePoint = CodePoint::Char('\u{210a}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALH: CodePoint = CodePoint::Char('\u{210b}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALH: CodePoint = CodePoint::Char('\u{210c}');
pub const CODEPOINT_LONGNAME_HBAR: CodePoint = CodePoint::Char('\u{210f}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALI: CodePoint = CodePoint::Char('\u{2110}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALI: CodePoint = CodePoint::Char('\u{2111}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALL: CodePoint = CodePoint::Char('\u{2112}');
pub const CODEPOINT_LONGNAME_SCRIPTL: CodePoint = CodePoint::Char('\u{2113}');
pub const CODEPOINT_LONGNAME_WEIERSTRASSP: CodePoint = CodePoint::Char('\u{2118}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALR: CodePoint = CodePoint::Char('\u{211b}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALR: CodePoint = CodePoint::Char('\u{211c}');
pub const CODEPOINT_LONGNAME_TRADEMARK: CodePoint = CodePoint::Char('\u{2122}');
pub const CODEPOINT_LONGNAME_MHO: CodePoint = CodePoint::Char('\u{2127}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALZ: CodePoint = CodePoint::Char('\u{2128}');
pub const CODEPOINT_LONGNAME_ANGSTROM: CodePoint = CodePoint::Char('\u{212b}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALB: CodePoint = CodePoint::Char('\u{212c}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALC: CodePoint = CodePoint::Char('\u{212d}');
pub const CODEPOINT_LONGNAME_SCRIPTE: CodePoint = CodePoint::Char('\u{212f}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALE: CodePoint = CodePoint::Char('\u{2130}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALF: CodePoint = CodePoint::Char('\u{2131}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALM: CodePoint = CodePoint::Char('\u{2133}');
pub const CODEPOINT_LONGNAME_SCRIPTO: CodePoint = CodePoint::Char('\u{2134}');
pub const CODEPOINT_LONGNAME_ALEPH: CodePoint = CodePoint::Char('\u{2135}');
pub const CODEPOINT_LONGNAME_BET: CodePoint = CodePoint::Char('\u{2136}');
pub const CODEPOINT_LONGNAME_GIMEL: CodePoint = CodePoint::Char('\u{2137}');
pub const CODEPOINT_LONGNAME_DALET: CodePoint = CodePoint::Char('\u{2138}');
pub const CODEPOINT_LONGNAME_LEFTARROW: CodePoint = CodePoint::Char('\u{2190}');
pub const CODEPOINT_LONGNAME_UPARROW: CodePoint = CodePoint::Char('\u{2191}');
pub const CODEPOINT_LONGNAME_RIGHTARROW: CodePoint = CodePoint::Char('\u{2192}');
pub const CODEPOINT_LONGNAME_DOWNARROW: CodePoint = CodePoint::Char('\u{2193}');
pub const CODEPOINT_LONGNAME_LEFTRIGHTARROW: CodePoint = CodePoint::Char('\u{2194}');
pub const CODEPOINT_LONGNAME_UPDOWNARROW: CodePoint = CodePoint::Char('\u{2195}');
pub const CODEPOINT_LONGNAME_UPPERLEFTARROW: CodePoint = CodePoint::Char('\u{2196}');
pub const CODEPOINT_LONGNAME_UPPERRIGHTARROW: CodePoint = CodePoint::Char('\u{2197}');
pub const CODEPOINT_LONGNAME_LOWERRIGHTARROW: CodePoint = CodePoint::Char('\u{2198}');
pub const CODEPOINT_LONGNAME_LOWERLEFTARROW: CodePoint = CodePoint::Char('\u{2199}');
pub const CODEPOINT_LONGNAME_LEFTTEEARROW: CodePoint = CodePoint::Char('\u{21a4}');
pub const CODEPOINT_LONGNAME_UPTEEARROW: CodePoint = CodePoint::Char('\u{21a5}');
pub const CODEPOINT_LONGNAME_RIGHTTEEARROW: CodePoint = CodePoint::Char('\u{21a6}');
pub const CODEPOINT_LONGNAME_DOWNTEEARROW: CodePoint = CodePoint::Char('\u{21a7}');
pub const CODEPOINT_LONGNAME_RETURNINDICATOR: CodePoint = CodePoint::Char('\u{21b5}');
pub const CODEPOINT_LONGNAME_LEFTVECTOR: CodePoint = CodePoint::Char('\u{21bc}');
pub const CODEPOINT_LONGNAME_DOWNLEFTVECTOR: CodePoint = CodePoint::Char('\u{21bd}');
pub const CODEPOINT_LONGNAME_RIGHTUPVECTOR: CodePoint = CodePoint::Char('\u{21be}');
pub const CODEPOINT_LONGNAME_LEFTUPVECTOR: CodePoint = CodePoint::Char('\u{21bf}');
pub const CODEPOINT_LONGNAME_RIGHTVECTOR: CodePoint = CodePoint::Char('\u{21c0}');
pub const CODEPOINT_LONGNAME_DOWNRIGHTVECTOR: CodePoint = CodePoint::Char('\u{21c1}');
pub const CODEPOINT_LONGNAME_RIGHTDOWNVECTOR: CodePoint = CodePoint::Char('\u{21c2}');
pub const CODEPOINT_LONGNAME_LEFTDOWNVECTOR: CodePoint = CodePoint::Char('\u{21c3}');
pub const CODEPOINT_LONGNAME_RIGHTARROWLEFTARROW: CodePoint = CodePoint::Char('\u{21c4}');
pub const CODEPOINT_LONGNAME_UPARROWDOWNARROW: CodePoint = CodePoint::Char('\u{21c5}');
pub const CODEPOINT_LONGNAME_LEFTARROWRIGHTARROW: CodePoint = CodePoint::Char('\u{21c6}');
pub const CODEPOINT_LONGNAME_REVERSEEQUILIBRIUM: CodePoint = CodePoint::Char('\u{21cb}');
pub const CODEPOINT_LONGNAME_EQUILIBRIUM: CodePoint = CodePoint::Char('\u{21cc}');
pub const CODEPOINT_LONGNAME_DOUBLELEFTARROW: CodePoint = CodePoint::Char('\u{21d0}');
pub const CODEPOINT_LONGNAME_DOUBLEUPARROW: CodePoint = CodePoint::Char('\u{21d1}');
pub const CODEPOINT_LONGNAME_DOUBLERIGHTARROW: CodePoint = CodePoint::Char('\u{21d2}');
pub const CODEPOINT_LONGNAME_DOUBLEDOWNARROW: CodePoint = CodePoint::Char('\u{21d3}');
pub const CODEPOINT_LONGNAME_DOUBLELEFTRIGHTARROW: CodePoint = CodePoint::Char('\u{21d4}');
pub const CODEPOINT_LONGNAME_DOUBLEUPDOWNARROW: CodePoint = CodePoint::Char('\u{21d5}');
pub const CODEPOINT_LONGNAME_LEFTARROWBAR: CodePoint = CodePoint::Char('\u{21e4}');
pub const CODEPOINT_LONGNAME_RIGHTARROWBAR: CodePoint = CodePoint::Char('\u{21e5}');
pub const CODEPOINT_LONGNAME_DOWNARROWUPARROW: CodePoint = CodePoint::Char('\u{21f5}');
pub const CODEPOINT_LONGNAME_FORALL: CodePoint = CodePoint::Char('\u{2200}');
pub const CODEPOINT_LONGNAME_PARTIALD: CodePoint = CodePoint::Char('\u{2202}');
pub const CODEPOINT_LONGNAME_EXISTS: CodePoint = CodePoint::Char('\u{2203}');
pub const CODEPOINT_LONGNAME_NOTEXISTS: CodePoint = CodePoint::Char('\u{2204}');
pub const CODEPOINT_LONGNAME_EMPTYSET: CodePoint = CodePoint::Char('\u{2205}');
pub const CODEPOINT_LONGNAME_DEL: CodePoint = CodePoint::Char('\u{2207}');
pub const CODEPOINT_LONGNAME_ELEMENT: CodePoint = CodePoint::Char('\u{2208}');
pub const CODEPOINT_LONGNAME_NOTELEMENT: CodePoint = CodePoint::Char('\u{2209}');
pub const CODEPOINT_LONGNAME_REVERSEELEMENT: CodePoint = CodePoint::Char('\u{220b}');
pub const CODEPOINT_LONGNAME_NOTREVERSEELEMENT: CodePoint = CodePoint::Char('\u{220c}');
pub const CODEPOINT_LONGNAME_SUCHTHAT: CodePoint = CodePoint::Char('\u{220d}');
pub const CODEPOINT_LONGNAME_PRODUCT: CodePoint = CodePoint::Char('\u{220f}');
pub const CODEPOINT_LONGNAME_COPRODUCT: CodePoint = CodePoint::Char('\u{2210}');
pub const CODEPOINT_LONGNAME_SUM: CodePoint = CodePoint::Char('\u{2211}');
pub const CODEPOINT_LONGNAME_MINUS: CodePoint = CodePoint::Char('\u{2212}');
pub const CODEPOINT_LONGNAME_MINUSPLUS: CodePoint = CodePoint::Char('\u{2213}');
pub const CODEPOINT_LONGNAME_DIVISIONSLASH: CodePoint = CodePoint::Char('\u{2215}');
pub const CODEPOINT_LONGNAME_BACKSLASH: CodePoint = CodePoint::Char('\u{2216}');
pub const CODEPOINT_LONGNAME_SMALLCIRCLE: CodePoint = CodePoint::Char('\u{2218}');
pub const CODEPOINT_LONGNAME_SQRT: CodePoint = CodePoint::Char('\u{221a}');
pub const CODEPOINT_LONGNAME_CUBEROOT: CodePoint = CodePoint::Char('\u{221b}');
pub const CODEPOINT_LONGNAME_PROPORTIONAL: CodePoint = CodePoint::Char('\u{221d}');
pub const CODEPOINT_LONGNAME_INFINITY: CodePoint = CodePoint::Char('\u{221e}');
pub const CODEPOINT_LONGNAME_RIGHTANGLE: CodePoint = CodePoint::Char('\u{221f}');
pub const CODEPOINT_LONGNAME_ANGLE: CodePoint = CodePoint::Char('\u{2220}');
pub const CODEPOINT_LONGNAME_MEASUREDANGLE: CodePoint = CodePoint::Char('\u{2221}');
pub const CODEPOINT_LONGNAME_SPHERICALANGLE: CodePoint = CodePoint::Char('\u{2222}');
pub const CODEPOINT_LONGNAME_DIVIDES: CodePoint = CodePoint::Char('\u{2223}');
pub const CODEPOINT_LONGNAME_DOUBLEVERTICALBAR: CodePoint = CodePoint::Char('\u{2225}');
pub const CODEPOINT_LONGNAME_NOTDOUBLEVERTICALBAR: CodePoint = CodePoint::Char('\u{2226}');
pub const CODEPOINT_LONGNAME_AND: CodePoint = CodePoint::Char('\u{2227}');
pub const CODEPOINT_LONGNAME_OR: CodePoint = CodePoint::Char('\u{2228}');
pub const CODEPOINT_LONGNAME_INTEGRAL: CodePoint = CodePoint::Char('\u{222b}');
pub const CODEPOINT_LONGNAME_CONTOURINTEGRAL: CodePoint = CodePoint::Char('\u{222e}');
pub const CODEPOINT_LONGNAME_DOUBLECONTOURINTEGRAL: CodePoint = CodePoint::Char('\u{222f}');
pub const CODEPOINT_LONGNAME_CLOCKWISECONTOURINTEGRAL: CodePoint = CodePoint::Char('\u{2232}');
pub const CODEPOINT_LONGNAME_COUNTERCLOCKWISECONTOURINTEGRAL: CodePoint = CodePoint::Char('\u{2233}');
pub const CODEPOINT_LONGNAME_THEREFORE: CodePoint = CodePoint::Char('\u{2234}');
pub const CODEPOINT_LONGNAME_BECAUSE: CodePoint = CodePoint::Char('\u{2235}');
pub const CODEPOINT_LONGNAME_COLON: CodePoint = CodePoint::Char('\u{2236}');
pub const CODEPOINT_LONGNAME_PROPORTION: CodePoint = CodePoint::Char('\u{2237}');
pub const CODEPOINT_LONGNAME_TILDE: CodePoint = CodePoint::Char('\u{223c}');
pub const CODEPOINT_LONGNAME_VERTICALTILDE: CodePoint = CodePoint::Char('\u{2240}');
pub const CODEPOINT_LONGNAME_NOTTILDE: CodePoint = CodePoint::Char('\u{2241}');
pub const CODEPOINT_LONGNAME_EQUALTILDE: CodePoint = CodePoint::Char('\u{2242}');
pub const CODEPOINT_LONGNAME_TILDEEQUAL: CodePoint = CodePoint::Char('\u{2243}');
pub const CODEPOINT_LONGNAME_NOTTILDEEQUAL: CodePoint = CodePoint::Char('\u{2244}');
pub const CODEPOINT_LONGNAME_TILDEFULLEQUAL: CodePoint = CodePoint::Char('\u{2245}');
pub const CODEPOINT_LONGNAME_NOTTILDEFULLEQUAL: CodePoint = CodePoint::Char('\u{2247}');
pub const CODEPOINT_LONGNAME_TILDETILDE: CodePoint = CodePoint::Char('\u{2248}');
pub const CODEPOINT_LONGNAME_NOTTILDETILDE: CodePoint = CodePoint::Char('\u{2249}');
pub const CODEPOINT_LONGNAME_CUPCAP: CodePoint = CodePoint::Char('\u{224d}');
pub const CODEPOINT_LONGNAME_HUMPDOWNHUMP: CodePoint = CodePoint::Char('\u{224e}');
pub const CODEPOINT_LONGNAME_HUMPEQUAL: CodePoint = CodePoint::Char('\u{224f}');
pub const CODEPOINT_LONGNAME_DOTEQUAL: CodePoint = CodePoint::Char('\u{2250}');
pub const CODEPOINT_LONGNAME_NOTEQUAL: CodePoint = CodePoint::Char('\u{2260}');
pub const CODEPOINT_LONGNAME_CONGRUENT: CodePoint = CodePoint::Char('\u{2261}');
pub const CODEPOINT_LONGNAME_NOTCONGRUENT: CodePoint = CodePoint::Char('\u{2262}');
pub const CODEPOINT_LONGNAME_LESSEQUAL: CodePoint = CodePoint::Char('\u{2264}');
pub const CODEPOINT_LONGNAME_GREATEREQUAL: CodePoint = CodePoint::Char('\u{2265}');
pub const CODEPOINT_LONGNAME_LESSFULLEQUAL: CodePoint = CodePoint::Char('\u{2266}');
pub const CODEPOINT_LONGNAME_GREATERFULLEQUAL: CodePoint = CodePoint::Char('\u{2267}');
pub const CODEPOINT_LONGNAME_NOTLESSFULLEQUAL: CodePoint = CodePoint::Char('\u{2268}');
pub const CODEPOINT_LONGNAME_NOTGREATERFULLEQUAL: CodePoint = CodePoint::Char('\u{2269}');
pub const CODEPOINT_LONGNAME_LESSLESS: CodePoint = CodePoint::Char('\u{226a}');
pub const CODEPOINT_LONGNAME_GREATERGREATER: CodePoint = CodePoint::Char('\u{226b}');
pub const CODEPOINT_LONGNAME_NOTCUPCAP: CodePoint = CodePoint::Char('\u{226d}');
pub const CODEPOINT_LONGNAME_NOTLESS: CodePoint = CodePoint::Char('\u{226e}');
pub const CODEPOINT_LONGNAME_NOTGREATER: CodePoint = CodePoint::Char('\u{226f}');
pub const CODEPOINT_LONGNAME_NOTLESSEQUAL: CodePoint = CodePoint::Char('\u{2270}');
pub const CODEPOINT_LONGNAME_NOTGREATEREQUAL: CodePoint = CodePoint::Char('\u{2271}');
pub const CODEPOINT_LONGNAME_LESSTILDE: CodePoint = CodePoint::Char('\u{2272}');
pub const CODEPOINT_LONGNAME_GREATERTILDE: CodePoint = CodePoint::Char('\u{2273}');
pub const CODEPOINT_LONGNAME_NOTLESSTILDE: CodePoint = CodePoint::Char('\u{2274}');
pub const CODEPOINT_LONGNAME_NOTGREATERTILDE: CodePoint = CodePoint::Char('\u{2275}');
pub const CODEPOINT_LONGNAME_LESSGREATER: CodePoint = CodePoint::Char('\u{2276}');
pub const CODEPOINT_LONGNAME_GREATERLESS: CodePoint = CodePoint::Char('\u{2277}');
pub const CODEPOINT_LONGNAME_NOTLESSGREATER: CodePoint = CodePoint::Char('\u{2278}');
pub const CODEPOINT_LONGNAME_NOTGREATERLESS: CodePoint = CodePoint::Char('\u{2279}');
pub const CODEPOINT_LONGNAME_PRECEDES: CodePoint = CodePoint::Char('\u{227a}');
pub const CODEPOINT_LONGNAME_SUCCEEDS: CodePoint = CodePoint::Char('\u{227b}');
pub const CODEPOINT_LONGNAME_PRECEDESSLANTEQUAL: CodePoint = CodePoint::Char('\u{227c}');
pub const CODEPOINT_LONGNAME_SUCCEEDSSLANTEQUAL: CodePoint = CodePoint::Char('\u{227d}');
pub const CODEPOINT_LONGNAME_PRECEDESTILDE: CodePoint = CodePoint::Char('\u{227e}');
pub const CODEPOINT_LONGNAME_SUCCEEDSTILDE: CodePoint = CodePoint::Char('\u{227f}');
pub const CODEPOINT_LONGNAME_NOTPRECEDES: CodePoint = CodePoint::Char('\u{2280}');
pub const CODEPOINT_LONGNAME_NOTSUCCEEDS: CodePoint = CodePoint::Char('\u{2281}');
pub const CODEPOINT_LONGNAME_SUBSET: CodePoint = CodePoint::Char('\u{2282}');
pub const CODEPOINT_LONGNAME_SUPERSET: CodePoint = CodePoint::Char('\u{2283}');
pub const CODEPOINT_LONGNAME_NOTSUBSET: CodePoint = CodePoint::Char('\u{2284}');
pub const CODEPOINT_LONGNAME_NOTSUPERSET: CodePoint = CodePoint::Char('\u{2285}');
pub const CODEPOINT_LONGNAME_SUBSETEQUAL: CodePoint = CodePoint::Char('\u{2286}');
pub const CODEPOINT_LONGNAME_SUPERSETEQUAL: CodePoint = CodePoint::Char('\u{2287}');
pub const CODEPOINT_LONGNAME_NOTSUBSETEQUAL: CodePoint = CodePoint::Char('\u{2288}');
pub const CODEPOINT_LONGNAME_NOTSUPERSETEQUAL: CodePoint = CodePoint::Char('\u{2289}');
pub const CODEPOINT_LONGNAME_UNIONPLUS: CodePoint = CodePoint::Char('\u{228e}');
pub const CODEPOINT_LONGNAME_SQUARESUBSET: CodePoint = CodePoint::Char('\u{228f}');
pub const CODEPOINT_LONGNAME_SQUARESUPERSET: CodePoint = CodePoint::Char('\u{2290}');
pub const CODEPOINT_LONGNAME_SQUARESUBSETEQUAL: CodePoint = CodePoint::Char('\u{2291}');
pub const CODEPOINT_LONGNAME_SQUARESUPERSETEQUAL: CodePoint = CodePoint::Char('\u{2292}');
pub const CODEPOINT_LONGNAME_SQUAREINTERSECTION: CodePoint = CodePoint::Char('\u{2293}');
pub const CODEPOINT_LONGNAME_SQUAREUNION: CodePoint = CodePoint::Char('\u{2294}');
pub const CODEPOINT_LONGNAME_CIRCLEPLUS: CodePoint = CodePoint::Char('\u{2295}');
pub const CODEPOINT_LONGNAME_CIRCLEMINUS: CodePoint = CodePoint::Char('\u{2296}');
pub const CODEPOINT_LONGNAME_CIRCLETIMES: CodePoint = CodePoint::Char('\u{2297}');
pub const CODEPOINT_LONGNAME_CIRCLEDOT: CodePoint = CodePoint::Char('\u{2299}');
pub const CODEPOINT_LONGNAME_RIGHTTEE: CodePoint = CodePoint::Char('\u{22a2}');
pub const CODEPOINT_LONGNAME_LEFTTEE: CodePoint = CodePoint::Char('\u{22a3}');
pub const CODEPOINT_LONGNAME_DOWNTEE: CodePoint = CodePoint::Char('\u{22a4}');
pub const CODEPOINT_LONGNAME_UPTEE: CodePoint = CodePoint::Char('\u{22a5}');
pub const CODEPOINT_LONGNAME_DOUBLERIGHTTEE: CodePoint = CodePoint::Char('\u{22a8}');
pub const CODEPOINT_LONGNAME_LEFTTRIANGLE: CodePoint = CodePoint::Char('\u{22b2}');
pub const CODEPOINT_LONGNAME_RIGHTTRIANGLE: CodePoint = CodePoint::Char('\u{22b3}');
pub const CODEPOINT_LONGNAME_LEFTTRIANGLEEQUAL: CodePoint = CodePoint::Char('\u{22b4}');
pub const CODEPOINT_LONGNAME_RIGHTTRIANGLEEQUAL: CodePoint = CodePoint::Char('\u{22b5}');
pub const CODEPOINT_LONGNAME_XOR: CodePoint = CodePoint::Char('\u{22bb}');
pub const CODEPOINT_LONGNAME_NAND: CodePoint = CodePoint::Char('\u{22bc}');
pub const CODEPOINT_LONGNAME_NOR: CodePoint = CodePoint::Char('\u{22bd}');
pub const CODEPOINT_LONGNAME_WEDGE: CodePoint = CodePoint::Char('\u{22c0}');
pub const CODEPOINT_LONGNAME_VEE: CodePoint = CodePoint::Char('\u{22c1}');
pub const CODEPOINT_LONGNAME_INTERSECTION: CodePoint = CodePoint::Char('\u{22c2}');
pub const CODEPOINT_LONGNAME_UNION: CodePoint = CodePoint::Char('\u{22c3}');
pub const CODEPOINT_LONGNAME_DIAMOND: CodePoint = CodePoint::Char('\u{22c4}');
pub const CODEPOINT_LONGNAME_STAR: CodePoint = CodePoint::Char('\u{22c6}');
pub const CODEPOINT_LONGNAME_LESSEQUALGREATER: CodePoint = CodePoint::Char('\u{22da}');
pub const CODEPOINT_LONGNAME_GREATEREQUALLESS: CodePoint = CodePoint::Char('\u{22db}');
pub const CODEPOINT_LONGNAME_NOTPRECEDESSLANTEQUAL: CodePoint = CodePoint::Char('\u{22e0}');
pub const CODEPOINT_LONGNAME_NOTSUCCEEDSSLANTEQUAL: CodePoint = CodePoint::Char('\u{22e1}');
pub const CODEPOINT_LONGNAME_NOTSQUARESUBSETEQUAL: CodePoint = CodePoint::Char('\u{22e2}');
pub const CODEPOINT_LONGNAME_NOTSQUARESUPERSETEQUAL: CodePoint = CodePoint::Char('\u{22e3}');
pub const CODEPOINT_LONGNAME_NOTPRECEDESTILDE: CodePoint = CodePoint::Char('\u{22e8}');
pub const CODEPOINT_LONGNAME_NOTSUCCEEDSTILDE: CodePoint = CodePoint::Char('\u{22e9}');
pub const CODEPOINT_LONGNAME_NOTLEFTTRIANGLE: CodePoint = CodePoint::Char('\u{22ea}');
pub const CODEPOINT_LONGNAME_NOTRIGHTTRIANGLE: CodePoint = CodePoint::Char('\u{22eb}');
pub const CODEPOINT_LONGNAME_NOTLEFTTRIANGLEEQUAL: CodePoint = CodePoint::Char('\u{22ec}');
pub const CODEPOINT_LONGNAME_NOTRIGHTTRIANGLEEQUAL: CodePoint = CodePoint::Char('\u{22ed}');
pub const CODEPOINT_LONGNAME_VERTICALELLIPSIS: CodePoint = CodePoint::Char('\u{22ee}');
pub const CODEPOINT_LONGNAME_CENTERELLIPSIS: CodePoint = CodePoint::Char('\u{22ef}');
pub const CODEPOINT_LONGNAME_ASCENDINGELLIPSIS: CodePoint = CodePoint::Char('\u{22f0}');
pub const CODEPOINT_LONGNAME_DESCENDINGELLIPSIS: CodePoint = CodePoint::Char('\u{22f1}');
pub const CODEPOINT_LONGNAME_DIAMETER: CodePoint = CodePoint::Char('\u{2300}');
pub const CODEPOINT_LONGNAME_LEFTCEILING: CodePoint = CodePoint::Char('\u{2308}');
pub const CODEPOINT_LONGNAME_RIGHTCEILING: CodePoint = CodePoint::Char('\u{2309}');
pub const CODEPOINT_LONGNAME_LEFTFLOOR: CodePoint = CodePoint::Char('\u{230a}');
pub const CODEPOINT_LONGNAME_RIGHTFLOOR: CodePoint = CodePoint::Char('\u{230b}');
pub const CODEPOINT_LONGNAME_CLOVERLEAF: CodePoint = CodePoint::Char('\u{2318}');
pub const CODEPOINT_LONGNAME_WATCHICON: CodePoint = CodePoint::Char('\u{231a}');
pub const CODEPOINT_LONGNAME_CAP: CodePoint = CodePoint::Char('\u{2322}');
pub const CODEPOINT_LONGNAME_CUP: CodePoint = CodePoint::Char('\u{2323}');
pub const CODEPOINT_LONGNAME_LEFTANGLEBRACKET: CodePoint = CodePoint::Char('\u{2329}');
pub const CODEPOINT_LONGNAME_RIGHTANGLEBRACKET: CodePoint = CodePoint::Char('\u{232a}');
pub const CODEPOINT_LONGNAME_OVERBRACKET: CodePoint = CodePoint::Char('\u{23b4}');
pub const CODEPOINT_LONGNAME_UNDERBRACKET: CodePoint = CodePoint::Char('\u{23b5}');
pub const CODEPOINT_LONGNAME_SPACEINDICATOR: CodePoint = CodePoint::Char('\u{2423}');
pub const CODEPOINT_LONGNAME_HORIZONTALLINE: CodePoint = CodePoint::Char('\u{2500}');
pub const CODEPOINT_LONGNAME_VERTICALLINE: CodePoint = CodePoint::Char('\u{2502}');
pub const CODEPOINT_LONGNAME_FILLEDSQUARE: CodePoint = CodePoint::Char('\u{25a0}');
pub const CODEPOINT_LONGNAME_EMPTYSQUARE: CodePoint = CodePoint::Char('\u{25a1}');
pub const CODEPOINT_LONGNAME_FILLEDVERYSMALLSQUARE: CodePoint = CodePoint::Char('\u{25aa}');
pub const CODEPOINT_LONGNAME_EMPTYVERYSMALLSQUARE: CodePoint = CodePoint::Char('\u{25ab}');
pub const CODEPOINT_LONGNAME_FILLEDRECTANGLE: CodePoint = CodePoint::Char('\u{25ae}');
pub const CODEPOINT_LONGNAME_EMPTYRECTANGLE: CodePoint = CodePoint::Char('\u{25af}');
pub const CODEPOINT_LONGNAME_FILLEDUPTRIANGLE: CodePoint = CodePoint::Char('\u{25b2}');
pub const CODEPOINT_LONGNAME_EMPTYUPTRIANGLE: CodePoint = CodePoint::Char('\u{25b3}');
pub const CODEPOINT_LONGNAME_UPPOINTER: CodePoint = CodePoint::Char('\u{25b4}');
pub const CODEPOINT_LONGNAME_FILLEDRIGHTTRIANGLE: CodePoint = CodePoint::Char('\u{25b6}');
pub const CODEPOINT_LONGNAME_RIGHTPOINTER: CodePoint = CodePoint::Char('\u{25b8}');
pub const CODEPOINT_LONGNAME_FILLEDDOWNTRIANGLE: CodePoint = CodePoint::Char('\u{25bc}');
pub const CODEPOINT_LONGNAME_EMPTYDOWNTRIANGLE: CodePoint = CodePoint::Char('\u{25bd}');
pub const CODEPOINT_LONGNAME_DOWNPOINTER: CodePoint = CodePoint::Char('\u{25be}');
pub const CODEPOINT_LONGNAME_FILLEDLEFTTRIANGLE: CodePoint = CodePoint::Char('\u{25c0}');
pub const CODEPOINT_LONGNAME_LEFTPOINTER: CodePoint = CodePoint::Char('\u{25c2}');
pub const CODEPOINT_LONGNAME_FILLEDDIAMOND: CodePoint = CodePoint::Char('\u{25c6}');
pub const CODEPOINT_LONGNAME_EMPTYDIAMOND: CodePoint = CodePoint::Char('\u{25c7}');
pub const CODEPOINT_LONGNAME_EMPTYCIRCLE: CodePoint = CodePoint::Char('\u{25cb}');
pub const CODEPOINT_LONGNAME_FILLEDCIRCLE: CodePoint = CodePoint::Char('\u{25cf}');
pub const CODEPOINT_LONGNAME_EMPTYSMALLCIRCLE: CodePoint = CodePoint::Char('\u{25e6}');
pub const CODEPOINT_LONGNAME_EMPTYSMALLSQUARE: CodePoint = CodePoint::Char('\u{25fb}');
pub const CODEPOINT_LONGNAME_FILLEDSMALLSQUARE: CodePoint = CodePoint::Char('\u{25fc}');
pub const CODEPOINT_LONGNAME_FIVEPOINTEDSTAR: CodePoint = CodePoint::Char('\u{2605}');
pub const CODEPOINT_LONGNAME_SUN: CodePoint = CodePoint::Char('\u{2609}');
pub const CODEPOINT_LONGNAME_CHECKMARKEDBOX: CodePoint = CodePoint::Char('\u{2611}');
pub const CODEPOINT_LONGNAME_CHECKEDBOX: CodePoint = CodePoint::Char('\u{2612}');
pub const CODEPOINT_LONGNAME_SADSMILEY: CodePoint = CodePoint::Char('\u{2639}');
pub const CODEPOINT_LONGNAME_HAPPYSMILEY: CodePoint = CodePoint::Char('\u{263a}');
pub const CODEPOINT_LONGNAME_MOON: CodePoint = CodePoint::Char('\u{263e}');
pub const CODEPOINT_LONGNAME_MERCURY: CodePoint = CodePoint::Char('\u{263f}');
pub const CODEPOINT_LONGNAME_VENUS: CodePoint = CodePoint::Char('\u{2640}');
pub const CODEPOINT_LONGNAME_MARS: CodePoint = CodePoint::Char('\u{2642}');
pub const CODEPOINT_LONGNAME_JUPITER: CodePoint = CodePoint::Char('\u{2643}');
pub const CODEPOINT_LONGNAME_SATURN: CodePoint = CodePoint::Char('\u{2644}');
pub const CODEPOINT_LONGNAME_NEPTUNE: CodePoint = CodePoint::Char('\u{2646}');
pub const CODEPOINT_LONGNAME_PLUTO: CodePoint = CodePoint::Char('\u{2647}');
pub const CODEPOINT_LONGNAME_ARIESSIGN: CodePoint = CodePoint::Char('\u{2648}');
pub const CODEPOINT_LONGNAME_TAURUSSIGN: CodePoint = CodePoint::Char('\u{2649}');
pub const CODEPOINT_LONGNAME_GEMINISIGN: CodePoint = CodePoint::Char('\u{264a}');
pub const CODEPOINT_LONGNAME_CANCERSIGN: CodePoint = CodePoint::Char('\u{264b}');
pub const CODEPOINT_LONGNAME_LEOSIGN: CodePoint = CodePoint::Char('\u{264c}');
pub const CODEPOINT_LONGNAME_VIRGOSIGN: CodePoint = CodePoint::Char('\u{264d}');
pub const CODEPOINT_LONGNAME_LIBRASIGN: CodePoint = CodePoint::Char('\u{264e}');
pub const CODEPOINT_LONGNAME_SCORPIOSIGN: CodePoint = CodePoint::Char('\u{264f}');
pub const CODEPOINT_LONGNAME_SAGITTARIUSSIGN: CodePoint = CodePoint::Char('\u{2650}');
pub const CODEPOINT_LONGNAME_CAPRICORNSIGN: CodePoint = CodePoint::Char('\u{2651}');
pub const CODEPOINT_LONGNAME_AQUARIUSSIGN: CodePoint = CodePoint::Char('\u{2652}');
pub const CODEPOINT_LONGNAME_PISCESSIGN: CodePoint = CodePoint::Char('\u{2653}');
pub const CODEPOINT_LONGNAME_WHITEKING: CodePoint = CodePoint::Char('\u{2654}');
pub const CODEPOINT_LONGNAME_WHITEQUEEN: CodePoint = CodePoint::Char('\u{2655}');
pub const CODEPOINT_LONGNAME_WHITEROOK: CodePoint = CodePoint::Char('\u{2656}');
pub const CODEPOINT_LONGNAME_WHITEBISHOP: CodePoint = CodePoint::Char('\u{2657}');
pub const CODEPOINT_LONGNAME_WHITEKNIGHT: CodePoint = CodePoint::Char('\u{2658}');
pub const CODEPOINT_LONGNAME_WHITEPAWN: CodePoint = CodePoint::Char('\u{2659}');
pub const CODEPOINT_LONGNAME_BLACKKING: CodePoint = CodePoint::Char('\u{265a}');
pub const CODEPOINT_LONGNAME_BLACKQUEEN: CodePoint = CodePoint::Char('\u{265b}');
pub const CODEPOINT_LONGNAME_BLACKROOK: CodePoint = CodePoint::Char('\u{265c}');
pub const CODEPOINT_LONGNAME_BLACKBISHOP: CodePoint = CodePoint::Char('\u{265d}');
pub const CODEPOINT_LONGNAME_BLACKKNIGHT: CodePoint = CodePoint::Char('\u{265e}');
pub const CODEPOINT_LONGNAME_BLACKPAWN: CodePoint = CodePoint::Char('\u{265f}');
pub const CODEPOINT_LONGNAME_SPADESUIT: CodePoint = CodePoint::Char('\u{2660}');
pub const CODEPOINT_LONGNAME_HEARTSUIT: CodePoint = CodePoint::Char('\u{2661}');
pub const CODEPOINT_LONGNAME_DIAMONDSUIT: CodePoint = CodePoint::Char('\u{2662}');
pub const CODEPOINT_LONGNAME_CLUBSUIT: CodePoint = CodePoint::Char('\u{2663}');
pub const CODEPOINT_LONGNAME_QUARTERNOTE: CodePoint = CodePoint::Char('\u{2669}');
pub const CODEPOINT_LONGNAME_EIGHTHNOTE: CodePoint = CodePoint::Char('\u{266a}');
pub const CODEPOINT_LONGNAME_BEAMEDEIGHTHNOTE: CodePoint = CodePoint::Char('\u{266b}');
pub const CODEPOINT_LONGNAME_BEAMEDSIXTEENTHNOTE: CodePoint = CodePoint::Char('\u{266c}');
pub const CODEPOINT_LONGNAME_FLAT: CodePoint = CodePoint::Char('\u{266d}');
pub const CODEPOINT_LONGNAME_NATURAL: CodePoint = CodePoint::Char('\u{266e}');
pub const CODEPOINT_LONGNAME_SHARP: CodePoint = CodePoint::Char('\u{266f}');
pub const CODEPOINT_LONGNAME_URANUS: CodePoint = CodePoint::Char('\u{26e2}');
pub const CODEPOINT_LONGNAME_CHECKMARK: CodePoint = CodePoint::Char('\u{2713}');
pub const CODEPOINT_LONGNAME_SIXPOINTEDSTAR: CodePoint = CodePoint::Char('\u{2736}');
pub const CODEPOINT_LONGNAME_PERPENDICULAR: CodePoint = CodePoint::Char('\u{27c2}');
pub const CODEPOINT_LONGNAME_LONGLEFTARROW: CodePoint = CodePoint::Char('\u{27f5}');
pub const CODEPOINT_LONGNAME_LONGRIGHTARROW: CodePoint = CodePoint::Char('\u{27f6}');
pub const CODEPOINT_LONGNAME_LONGLEFTRIGHTARROW: CodePoint = CodePoint::Char('\u{27f7}');
pub const CODEPOINT_LONGNAME_DOUBLELONGLEFTARROW: CodePoint = CodePoint::Char('\u{27f8}');
pub const CODEPOINT_LONGNAME_DOUBLELONGRIGHTARROW: CodePoint = CodePoint::Char('\u{27f9}');
pub const CODEPOINT_LONGNAME_DOUBLELONGLEFTRIGHTARROW: CodePoint = CodePoint::Char('\u{27fa}');
pub const CODEPOINT_LONGNAME_UPARROWBAR: CodePoint = CodePoint::Char('\u{2912}');
pub const CODEPOINT_LONGNAME_DOWNARROWBAR: CodePoint = CodePoint::Char('\u{2913}');
pub const CODEPOINT_LONGNAME_LEFTRIGHTVECTOR: CodePoint = CodePoint::Char('\u{294e}');
pub const CODEPOINT_LONGNAME_RIGHTUPDOWNVECTOR: CodePoint = CodePoint::Char('\u{294f}');
pub const CODEPOINT_LONGNAME_DOWNLEFTRIGHTVECTOR: CodePoint = CodePoint::Char('\u{2950}');
pub const CODEPOINT_LONGNAME_LEFTUPDOWNVECTOR: CodePoint = CodePoint::Char('\u{2951}');
pub const CODEPOINT_LONGNAME_LEFTVECTORBAR: CodePoint = CodePoint::Char('\u{2952}');
pub const CODEPOINT_LONGNAME_RIGHTVECTORBAR: CodePoint = CodePoint::Char('\u{2953}');
pub const CODEPOINT_LONGNAME_RIGHTUPVECTORBAR: CodePoint = CodePoint::Char('\u{2954}');
pub const CODEPOINT_LONGNAME_RIGHTDOWNVECTORBAR: CodePoint = CodePoint::Char('\u{2955}');
pub const CODEPOINT_LONGNAME_DOWNLEFTVECTORBAR: CodePoint = CodePoint::Char('\u{2956}');
pub const CODEPOINT_LONGNAME_DOWNRIGHTVECTORBAR: CodePoint = CodePoint::Char('\u{2957}');
pub const CODEPOINT_LONGNAME_LEFTUPVECTORBAR: CodePoint = CodePoint::Char('\u{2958}');
pub const CODEPOINT_LONGNAME_LEFTDOWNVECTORBAR: CodePoint = CodePoint::Char('\u{2959}');
pub const CODEPOINT_LONGNAME_LEFTTEEVECTOR: CodePoint = CodePoint::Char('\u{295a}');
pub const CODEPOINT_LONGNAME_RIGHTTEEVECTOR: CodePoint = CodePoint::Char('\u{295b}');
pub const CODEPOINT_LONGNAME_RIGHTUPTEEVECTOR: CodePoint = CodePoint::Char('\u{295c}');
pub const CODEPOINT_LONGNAME_RIGHTDOWNTEEVECTOR: CodePoint = CodePoint::Char('\u{295d}');
pub const CODEPOINT_LONGNAME_DOWNLEFTTEEVECTOR: CodePoint = CodePoint::Char('\u{295e}');
pub const CODEPOINT_LONGNAME_DOWNRIGHTTEEVECTOR: CodePoint = CodePoint::Char('\u{295f}');
pub const CODEPOINT_LONGNAME_LEFTUPTEEVECTOR: CodePoint = CodePoint::Char('\u{2960}');
pub const CODEPOINT_LONGNAME_LEFTDOWNTEEVECTOR: CodePoint = CodePoint::Char('\u{2961}');
pub const CODEPOINT_LONGNAME_UPEQUILIBRIUM: CodePoint = CodePoint::Char('\u{296e}');
pub const CODEPOINT_LONGNAME_REVERSEUPEQUILIBRIUM: CodePoint = CodePoint::Char('\u{296f}');
pub const CODEPOINT_LONGNAME_ROUNDIMPLIES: CodePoint = CodePoint::Char('\u{2970}');
pub const CODEPOINT_LONGNAME_LEFTTRIANGLEBAR: CodePoint = CodePoint::Char('\u{29cf}');
pub const CODEPOINT_LONGNAME_RIGHTTRIANGLEBAR: CodePoint = CodePoint::Char('\u{29d0}');
pub const CODEPOINT_LONGNAME_EQUIVALENT: CodePoint = CodePoint::Char('\u{29e6}');
pub const CODEPOINT_LONGNAME_LESSSLANTEQUAL: CodePoint = CodePoint::Char('\u{2a7d}');
pub const CODEPOINT_LONGNAME_GREATERSLANTEQUAL: CodePoint = CodePoint::Char('\u{2a7e}');
pub const CODEPOINT_LONGNAME_NESTEDLESSLESS: CodePoint = CodePoint::Char('\u{2aa1}');
pub const CODEPOINT_LONGNAME_NESTEDGREATERGREATER: CodePoint = CodePoint::Char('\u{2aa2}');
pub const CODEPOINT_LONGNAME_PRECEDESEQUAL: CodePoint = CodePoint::Char('\u{2aaf}');
pub const CODEPOINT_LONGNAME_SUCCEEDSEQUAL: CodePoint = CodePoint::Char('\u{2ab0}');
pub const CODEPOINT_LONGNAME_DOUBLELEFTTEE: CodePoint = CodePoint::Char('\u{2ae4}');
pub const CODEPOINT_LONGNAME_COMPATIBILITYKANJISPACE: CodePoint = CodePoint::Char('\u{3000}');
pub const CODEPOINT_LONGNAME_LEFTDOUBLEBRACKET: CodePoint = CodePoint::Char('\u{301a}');
pub const CODEPOINT_LONGNAME_RIGHTDOUBLEBRACKET: CodePoint = CodePoint::Char('\u{301b}');
pub const CODEPOINT_LONGNAME_LEFTASSOCIATION: CodePoint = CodePoint::Char('\u{f113}');
pub const CODEPOINT_LONGNAME_RIGHTASSOCIATION: CodePoint = CodePoint::Char('\u{f114}');
pub const CODEPOINT_LONGNAME_SHAH: CodePoint = CodePoint::Char('\u{f11d}');
pub const CODEPOINT_LONGNAME_WOLFRAMLANGUAGELOGO: CodePoint = CodePoint::Char('\u{f11e}');
pub const CODEPOINT_LONGNAME_WOLFRAMLANGUAGELOGOCIRCLE: CodePoint = CodePoint::Char('\u{f11f}');
pub const CODEPOINT_LONGNAME_TWOWAYRULE: CodePoint = CodePoint::Char('\u{f120}');
pub const CODEPOINT_LONGNAME_FREEFORMPROMPT: CodePoint = CodePoint::Char('\u{f351}');
pub const CODEPOINT_LONGNAME_WOLFRAMALPHAPROMPT: CodePoint = CodePoint::Char('\u{f352}');
pub const CODEPOINT_LONGNAME_INVISIBLESPACE: CodePoint = CodePoint::Char('\u{f360}');
pub const CODEPOINT_LONGNAME_PIECEWISE: CodePoint = CodePoint::Char('\u{f361}');
pub const CODEPOINT_LONGNAME_NEGATIVEVERYTHINSPACE: CodePoint = CodePoint::Char('\u{f380}');
pub const CODEPOINT_LONGNAME_NEGATIVETHINSPACE: CodePoint = CodePoint::Char('\u{f382}');
pub const CODEPOINT_LONGNAME_NEGATIVEMEDIUMSPACE: CodePoint = CodePoint::Char('\u{f383}');
pub const CODEPOINT_LONGNAME_NEGATIVETHICKSPACE: CodePoint = CodePoint::Char('\u{f384}');
pub const CODEPOINT_LONGNAME_IMPLICITPLUS: CodePoint = CodePoint::Char('\u{f39e}');
pub const CODEPOINT_LONGNAME_NULL: CodePoint = CodePoint::Char('\u{f3a0}');
pub const CODEPOINT_LONGNAME_COMPATIBILITYNOBREAK: CodePoint = CodePoint::Char('\u{f3a2}');
pub const CODEPOINT_LONGNAME_INDENTINGNEWLINE: CodePoint = CodePoint::Char('\u{f3a3}');
pub const CODEPOINT_LONGNAME_AUTOPLACEHOLDER: CodePoint = CodePoint::Char('\u{f3a4}');
pub const CODEPOINT_LONGNAME_AUTOLEFTMATCH: CodePoint = CodePoint::Char('\u{f3a8}');
pub const CODEPOINT_LONGNAME_AUTORIGHTMATCH: CodePoint = CodePoint::Char('\u{f3a9}');
pub const CODEPOINT_LONGNAME_AUTOSPACE: CodePoint = CodePoint::Char('\u{f3ad}');
pub const CODEPOINT_LONGNAME_AUTOOPERAND: CodePoint = CodePoint::Char('\u{f3ae}');
pub const CODEPOINT_LONGNAME_SYSTEMSMODELDELAY: CodePoint = CodePoint::Char('\u{f3af}');
pub const CODEPOINT_LONGNAME_CONTINUATION: CodePoint = CodePoint::Char('\u{f3b1}');
pub const CODEPOINT_LONGNAME_ROUNDSPACEINDICATOR: CodePoint = CodePoint::Char('\u{f3b2}');
pub const CODEPOINT_LONGNAME_INVISIBLEPREFIXSCRIPTBASE: CodePoint = CodePoint::Char('\u{f3b3}');
pub const CODEPOINT_LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE: CodePoint = CodePoint::Char('\u{f3b4}');
pub const CODEPOINT_LONGNAME_ENTITYSTART: CodePoint = CodePoint::Char('\u{f3b8}');
pub const CODEPOINT_LONGNAME_ENTITYEND: CodePoint = CodePoint::Char('\u{f3b9}');
pub const CODEPOINT_LONGNAME_SPANFROMLEFT: CodePoint = CodePoint::Char('\u{f3ba}');
pub const CODEPOINT_LONGNAME_SPANFROMABOVE: CodePoint = CodePoint::Char('\u{f3bb}');
pub const CODEPOINT_LONGNAME_SPANFROMBOTH: CodePoint = CodePoint::Char('\u{f3bc}');
pub const CODEPOINT_LONGNAME_PAGEBREAKABOVE: CodePoint = CodePoint::Char('\u{f3bd}');
pub const CODEPOINT_LONGNAME_PAGEBREAKBELOW: CodePoint = CodePoint::Char('\u{f3be}');
pub const CODEPOINT_LONGNAME_DISCRETIONARYPAGEBREAKABOVE: CodePoint = CodePoint::Char('\u{f3bf}');
pub const CODEPOINT_LONGNAME_DISCRETIONARYPAGEBREAKBELOW: CodePoint = CodePoint::Char('\u{f3c6}');
pub const CODEPOINT_LONGNAME_TRANSPOSE: CodePoint = CodePoint::Char('\u{f3c7}');
pub const CODEPOINT_LONGNAME_CONJUGATE: CodePoint = CodePoint::Char('\u{f3c8}');
pub const CODEPOINT_LONGNAME_CONJUGATETRANSPOSE: CodePoint = CodePoint::Char('\u{f3c9}');
pub const CODEPOINT_LONGNAME_STEPPERRIGHT: CodePoint = CodePoint::Char('\u{f3ca}');
pub const CODEPOINT_LONGNAME_STEPPERLEFT: CodePoint = CodePoint::Char('\u{f3cb}');
pub const CODEPOINT_LONGNAME_STEPPERUP: CodePoint = CodePoint::Char('\u{f3cc}');
pub const CODEPOINT_LONGNAME_STEPPERDOWN: CodePoint = CodePoint::Char('\u{f3cd}');
pub const CODEPOINT_LONGNAME_HERMITIANCONJUGATE: CodePoint = CodePoint::Char('\u{f3ce}');
pub const CODEPOINT_LONGNAME_VERTICALBAR: CodePoint = CodePoint::Char('\u{f3d0}');
pub const CODEPOINT_LONGNAME_NOTVERTICALBAR: CodePoint = CodePoint::Char('\u{f3d1}');
pub const CODEPOINT_LONGNAME_DISTRIBUTED: CodePoint = CodePoint::Char('\u{f3d2}');
pub const CODEPOINT_LONGNAME_CONDITIONED: CodePoint = CodePoint::Char('\u{f3d3}');
pub const CODEPOINT_LONGNAME_UNDIRECTEDEDGE: CodePoint = CodePoint::Char('\u{f3d4}');
pub const CODEPOINT_LONGNAME_DIRECTEDEDGE: CodePoint = CodePoint::Char('\u{f3d5}');
pub const CODEPOINT_LONGNAME_CONTINUEDFRACTIONK: CodePoint = CodePoint::Char('\u{f3d9}');
pub const CODEPOINT_LONGNAME_TENSORPRODUCT: CodePoint = CodePoint::Char('\u{f3da}');
pub const CODEPOINT_LONGNAME_TENSORWEDGE: CodePoint = CodePoint::Char('\u{f3db}');
pub const CODEPOINT_LONGNAME_PROBABILITYPR: CodePoint = CodePoint::Char('\u{f3dc}');
pub const CODEPOINT_LONGNAME_EXPECTATIONE: CodePoint = CodePoint::Char('\u{f3dd}');
pub const CODEPOINT_LONGNAME_PERMUTATIONPRODUCT: CodePoint = CodePoint::Char('\u{f3de}');
pub const CODEPOINT_LONGNAME_EARTH: CodePoint = CodePoint::Char('\u{f3df}');
pub const CODEPOINT_LONGNAME_NOTEQUALTILDE: CodePoint = CodePoint::Char('\u{f400}');
pub const CODEPOINT_LONGNAME_NOTHUMPEQUAL: CodePoint = CodePoint::Char('\u{f401}');
pub const CODEPOINT_LONGNAME_NOTHUMPDOWNHUMP: CodePoint = CodePoint::Char('\u{f402}');
pub const CODEPOINT_LONGNAME_NOTLEFTTRIANGLEBAR: CodePoint = CodePoint::Char('\u{f412}');
pub const CODEPOINT_LONGNAME_NOTRIGHTTRIANGLEBAR: CodePoint = CodePoint::Char('\u{f413}');
pub const CODEPOINT_LONGNAME_NOTLESSLESS: CodePoint = CodePoint::Char('\u{f422}');
pub const CODEPOINT_LONGNAME_NOTNESTEDLESSLESS: CodePoint = CodePoint::Char('\u{f423}');
pub const CODEPOINT_LONGNAME_NOTLESSSLANTEQUAL: CodePoint = CodePoint::Char('\u{f424}');
pub const CODEPOINT_LONGNAME_NOTGREATERGREATER: CodePoint = CodePoint::Char('\u{f427}');
pub const CODEPOINT_LONGNAME_NOTNESTEDGREATERGREATER: CodePoint = CodePoint::Char('\u{f428}');
pub const CODEPOINT_LONGNAME_NOTGREATERSLANTEQUAL: CodePoint = CodePoint::Char('\u{f429}');
pub const CODEPOINT_LONGNAME_NOTPRECEDESEQUAL: CodePoint = CodePoint::Char('\u{f42b}');
pub const CODEPOINT_LONGNAME_NOTSUCCEEDSEQUAL: CodePoint = CodePoint::Char('\u{f42d}');
pub const CODEPOINT_LONGNAME_NOTSQUARESUBSET: CodePoint = CodePoint::Char('\u{f42e}');
pub const CODEPOINT_LONGNAME_NOTSQUARESUPERSET: CodePoint = CodePoint::Char('\u{f42f}');
pub const CODEPOINT_LONGNAME_EQUAL: CodePoint = CodePoint::Char('\u{f431}');
pub const CODEPOINT_LONGNAME_VERTICALSEPARATOR: CodePoint = CodePoint::Char('\u{f432}');
pub const CODEPOINT_LONGNAME_VECTORGREATER: CodePoint = CodePoint::Char('\u{f434}');
pub const CODEPOINT_LONGNAME_VECTORGREATEREQUAL: CodePoint = CodePoint::Char('\u{f435}');
pub const CODEPOINT_LONGNAME_VECTORLESS: CodePoint = CodePoint::Char('\u{f436}');
pub const CODEPOINT_LONGNAME_VECTORLESSEQUAL: CodePoint = CodePoint::Char('\u{f437}');
pub const CODEPOINT_LONGNAME_LIMIT: CodePoint = CodePoint::Char('\u{f438}');
pub const CODEPOINT_LONGNAME_MAXLIMIT: CodePoint = CodePoint::Char('\u{f439}');
pub const CODEPOINT_LONGNAME_MINLIMIT: CodePoint = CodePoint::Char('\u{f43a}');
pub const CODEPOINT_LONGNAME_CROSS: CodePoint = CodePoint::Char('\u{f4a0}');
pub const CODEPOINT_LONGNAME_FUNCTION: CodePoint = CodePoint::Char('\u{f4a1}');
pub const CODEPOINT_LONGNAME_XNOR: CodePoint = CodePoint::Char('\u{f4a2}');
pub const CODEPOINT_LONGNAME_DISCRETESHIFT: CodePoint = CodePoint::Char('\u{f4a3}');
pub const CODEPOINT_LONGNAME_DIFFERENCEDELTA: CodePoint = CodePoint::Char('\u{f4a4}');
pub const CODEPOINT_LONGNAME_DISCRETERATIO: CodePoint = CodePoint::Char('\u{f4a5}');
pub const CODEPOINT_LONGNAME_INLINEPART: CodePoint = CodePoint::Char('\u{f51e}');
pub const CODEPOINT_LONGNAME_RULEDELAYED: CodePoint = CodePoint::Char('\u{f51f}');
pub const CODEPOINT_LONGNAME_SQUARE: CodePoint = CodePoint::Char('\u{f520}');
pub const CODEPOINT_LONGNAME_RULE: CodePoint = CodePoint::Char('\u{f522}');
pub const CODEPOINT_LONGNAME_IMPLIES: CodePoint = CodePoint::Char('\u{f523}');
pub const CODEPOINT_LONGNAME_SHORTRIGHTARROW: CodePoint = CodePoint::Char('\u{f525}');
pub const CODEPOINT_LONGNAME_SHORTLEFTARROW: CodePoint = CodePoint::Char('\u{f526}');
pub const CODEPOINT_LONGNAME_SELECTIONPLACEHOLDER: CodePoint = CodePoint::Char('\u{f527}');
pub const CODEPOINT_LONGNAME_PLACEHOLDER: CodePoint = CodePoint::Char('\u{f528}');
pub const CODEPOINT_LONGNAME_SHORTUPARROW: CodePoint = CodePoint::Char('\u{f52a}');
pub const CODEPOINT_LONGNAME_SHORTDOWNARROW: CodePoint = CodePoint::Char('\u{f52b}');
pub const CODEPOINT_LONGNAME_APPLICATION: CodePoint = CodePoint::Char('\u{f530}');
pub const CODEPOINT_LONGNAME_LEFTBRACKETINGBAR: CodePoint = CodePoint::Char('\u{f603}');
pub const CODEPOINT_LONGNAME_RIGHTBRACKETINGBAR: CodePoint = CodePoint::Char('\u{f604}');
pub const CODEPOINT_LONGNAME_LEFTDOUBLEBRACKETINGBAR: CodePoint = CodePoint::Char('\u{f605}');
pub const CODEPOINT_LONGNAME_RIGHTDOUBLEBRACKETINGBAR: CodePoint = CodePoint::Char('\u{f606}');
pub const CODEPOINT_LONGNAME_SCRIPTA: CodePoint = CodePoint::Char('\u{f6b2}');
pub const CODEPOINT_LONGNAME_SCRIPTB: CodePoint = CodePoint::Char('\u{f6b3}');
pub const CODEPOINT_LONGNAME_SCRIPTC: CodePoint = CodePoint::Char('\u{f6b4}');
pub const CODEPOINT_LONGNAME_SCRIPTD: CodePoint = CodePoint::Char('\u{f6b5}');
pub const CODEPOINT_LONGNAME_SCRIPTF: CodePoint = CodePoint::Char('\u{f6b7}');
pub const CODEPOINT_LONGNAME_SCRIPTH: CodePoint = CodePoint::Char('\u{f6b9}');
pub const CODEPOINT_LONGNAME_SCRIPTI: CodePoint = CodePoint::Char('\u{f6ba}');
pub const CODEPOINT_LONGNAME_SCRIPTJ: CodePoint = CodePoint::Char('\u{f6bb}');
pub const CODEPOINT_LONGNAME_SCRIPTK: CodePoint = CodePoint::Char('\u{f6bc}');
pub const CODEPOINT_LONGNAME_SCRIPTM: CodePoint = CodePoint::Char('\u{f6be}');
pub const CODEPOINT_LONGNAME_SCRIPTN: CodePoint = CodePoint::Char('\u{f6bf}');
pub const CODEPOINT_LONGNAME_SCRIPTP: CodePoint = CodePoint::Char('\u{f6c1}');
pub const CODEPOINT_LONGNAME_SCRIPTQ: CodePoint = CodePoint::Char('\u{f6c2}');
pub const CODEPOINT_LONGNAME_SCRIPTR: CodePoint = CodePoint::Char('\u{f6c3}');
pub const CODEPOINT_LONGNAME_SCRIPTS: CodePoint = CodePoint::Char('\u{f6c4}');
pub const CODEPOINT_LONGNAME_SCRIPTT: CodePoint = CodePoint::Char('\u{f6c5}');
pub const CODEPOINT_LONGNAME_SCRIPTU: CodePoint = CodePoint::Char('\u{f6c6}');
pub const CODEPOINT_LONGNAME_SCRIPTV: CodePoint = CodePoint::Char('\u{f6c7}');
pub const CODEPOINT_LONGNAME_SCRIPTW: CodePoint = CodePoint::Char('\u{f6c8}');
pub const CODEPOINT_LONGNAME_SCRIPTX: CodePoint = CodePoint::Char('\u{f6c9}');
pub const CODEPOINT_LONGNAME_SCRIPTY: CodePoint = CodePoint::Char('\u{f6ca}');
pub const CODEPOINT_LONGNAME_SCRIPTZ: CodePoint = CodePoint::Char('\u{f6cb}');
pub const CODEPOINT_LONGNAME_GOTHICA: CodePoint = CodePoint::Char('\u{f6cc}');
pub const CODEPOINT_LONGNAME_GOTHICB: CodePoint = CodePoint::Char('\u{f6cd}');
pub const CODEPOINT_LONGNAME_GOTHICC: CodePoint = CodePoint::Char('\u{f6ce}');
pub const CODEPOINT_LONGNAME_GOTHICD: CodePoint = CodePoint::Char('\u{f6cf}');
pub const CODEPOINT_LONGNAME_GOTHICE: CodePoint = CodePoint::Char('\u{f6d0}');
pub const CODEPOINT_LONGNAME_GOTHICF: CodePoint = CodePoint::Char('\u{f6d1}');
pub const CODEPOINT_LONGNAME_GOTHICG: CodePoint = CodePoint::Char('\u{f6d2}');
pub const CODEPOINT_LONGNAME_GOTHICH: CodePoint = CodePoint::Char('\u{f6d3}');
pub const CODEPOINT_LONGNAME_GOTHICI: CodePoint = CodePoint::Char('\u{f6d4}');
pub const CODEPOINT_LONGNAME_GOTHICJ: CodePoint = CodePoint::Char('\u{f6d5}');
pub const CODEPOINT_LONGNAME_GOTHICK: CodePoint = CodePoint::Char('\u{f6d6}');
pub const CODEPOINT_LONGNAME_GOTHICL: CodePoint = CodePoint::Char('\u{f6d7}');
pub const CODEPOINT_LONGNAME_GOTHICM: CodePoint = CodePoint::Char('\u{f6d8}');
pub const CODEPOINT_LONGNAME_GOTHICN: CodePoint = CodePoint::Char('\u{f6d9}');
pub const CODEPOINT_LONGNAME_GOTHICO: CodePoint = CodePoint::Char('\u{f6da}');
pub const CODEPOINT_LONGNAME_GOTHICP: CodePoint = CodePoint::Char('\u{f6db}');
pub const CODEPOINT_LONGNAME_GOTHICQ: CodePoint = CodePoint::Char('\u{f6dc}');
pub const CODEPOINT_LONGNAME_GOTHICR: CodePoint = CodePoint::Char('\u{f6dd}');
pub const CODEPOINT_LONGNAME_GOTHICS: CodePoint = CodePoint::Char('\u{f6de}');
pub const CODEPOINT_LONGNAME_GOTHICT: CodePoint = CodePoint::Char('\u{f6df}');
pub const CODEPOINT_LONGNAME_GOTHICU: CodePoint = CodePoint::Char('\u{f6e0}');
pub const CODEPOINT_LONGNAME_GOTHICV: CodePoint = CodePoint::Char('\u{f6e1}');
pub const CODEPOINT_LONGNAME_GOTHICW: CodePoint = CodePoint::Char('\u{f6e2}');
pub const CODEPOINT_LONGNAME_GOTHICX: CodePoint = CodePoint::Char('\u{f6e3}');
pub const CODEPOINT_LONGNAME_GOTHICY: CodePoint = CodePoint::Char('\u{f6e4}');
pub const CODEPOINT_LONGNAME_GOTHICZ: CodePoint = CodePoint::Char('\u{f6e5}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKA: CodePoint = CodePoint::Char('\u{f6e6}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKB: CodePoint = CodePoint::Char('\u{f6e7}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKC: CodePoint = CodePoint::Char('\u{f6e8}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKD: CodePoint = CodePoint::Char('\u{f6e9}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKE: CodePoint = CodePoint::Char('\u{f6ea}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKF: CodePoint = CodePoint::Char('\u{f6eb}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKG: CodePoint = CodePoint::Char('\u{f6ec}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKH: CodePoint = CodePoint::Char('\u{f6ed}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKI: CodePoint = CodePoint::Char('\u{f6ee}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKJ: CodePoint = CodePoint::Char('\u{f6ef}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKK: CodePoint = CodePoint::Char('\u{f6f0}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKL: CodePoint = CodePoint::Char('\u{f6f1}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKM: CodePoint = CodePoint::Char('\u{f6f2}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKN: CodePoint = CodePoint::Char('\u{f6f3}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKO: CodePoint = CodePoint::Char('\u{f6f4}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKP: CodePoint = CodePoint::Char('\u{f6f5}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKQ: CodePoint = CodePoint::Char('\u{f6f6}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKR: CodePoint = CodePoint::Char('\u{f6f7}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKS: CodePoint = CodePoint::Char('\u{f6f8}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKT: CodePoint = CodePoint::Char('\u{f6f9}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKU: CodePoint = CodePoint::Char('\u{f6fa}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKV: CodePoint = CodePoint::Char('\u{f6fb}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKW: CodePoint = CodePoint::Char('\u{f6fc}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKX: CodePoint = CodePoint::Char('\u{f6fd}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKY: CodePoint = CodePoint::Char('\u{f6fe}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKZ: CodePoint = CodePoint::Char('\u{f6ff}');
pub const CODEPOINT_LONGNAME_DOTLESSJ: CodePoint = CodePoint::Char('\u{f700}');
pub const CODEPOINT_LONGNAME_WOLF: CodePoint = CodePoint::Char('\u{f720}');
pub const CODEPOINT_LONGNAME_FREAKEDSMILEY: CodePoint = CodePoint::Char('\u{f721}');
pub const CODEPOINT_LONGNAME_NEUTRALSMILEY: CodePoint = CodePoint::Char('\u{f722}');
pub const CODEPOINT_LONGNAME_LIGHTBULB: CodePoint = CodePoint::Char('\u{f723}');
pub const CODEPOINT_LONGNAME_NUMBERSIGN: CodePoint = CodePoint::Char('\u{f724}');
pub const CODEPOINT_LONGNAME_WARNINGSIGN: CodePoint = CodePoint::Char('\u{f725}');
pub const CODEPOINT_LONGNAME_VILLA: CodePoint = CodePoint::Char('\u{f727}');
pub const CODEPOINT_LONGNAME_AKUZ: CodePoint = CodePoint::Char('\u{f728}');
pub const CODEPOINT_LONGNAME_ANDY: CodePoint = CodePoint::Char('\u{f729}');
pub const CODEPOINT_LONGNAME_SPOOKY: CodePoint = CodePoint::Char('\u{f72a}');
pub const CODEPOINT_LONGNAME_SCRIPTDOTLESSI: CodePoint = CodePoint::Char('\u{f730}');
pub const CODEPOINT_LONGNAME_SCRIPTDOTLESSJ: CodePoint = CodePoint::Char('\u{f731}');
pub const CODEPOINT_LONGNAME_DOUBLEDPI: CodePoint = CodePoint::Char('\u{f749}');
pub const CODEPOINT_LONGNAME_DOUBLEDGAMMA: CodePoint = CodePoint::Char('\u{f74a}');
pub const CODEPOINT_LONGNAME_CAPITALDIFFERENTIALD: CodePoint = CodePoint::Char('\u{f74b}');
pub const CODEPOINT_LONGNAME_DIFFERENTIALD: CodePoint = CodePoint::Char('\u{f74c}');
pub const CODEPOINT_LONGNAME_EXPONENTIALE: CodePoint = CodePoint::Char('\u{f74d}');
pub const CODEPOINT_LONGNAME_IMAGINARYI: CodePoint = CodePoint::Char('\u{f74e}');
pub const CODEPOINT_LONGNAME_IMAGINARYJ: CodePoint = CodePoint::Char('\u{f74f}');
pub const CODEPOINT_LONGNAME_FILLEDSMALLCIRCLE: CodePoint = CodePoint::Char('\u{f750}');
pub const CODEPOINT_LONGNAME_DOTTEDSQUARE: CodePoint = CodePoint::Char('\u{f751}');
pub const CODEPOINT_LONGNAME_GRAYSQUARE: CodePoint = CodePoint::Char('\u{f752}');
pub const CODEPOINT_LONGNAME_GRAYCIRCLE: CodePoint = CodePoint::Char('\u{f753}');
pub const CODEPOINT_LONGNAME_LETTERSPACE: CodePoint = CodePoint::Char('\u{f754}');
pub const CODEPOINT_LONGNAME_DOWNBREVE: CodePoint = CodePoint::Char('\u{f755}');
pub const CODEPOINT_LONGNAME_KERNELICON: CodePoint = CodePoint::Char('\u{f756}');
pub const CODEPOINT_LONGNAME_MATHEMATICAICON: CodePoint = CodePoint::Char('\u{f757}');
pub const CODEPOINT_LONGNAME_TRIPLEDOT: CodePoint = CodePoint::Char('\u{f758}');
pub const CODEPOINT_LONGNAME_SYSTEMENTERKEY: CodePoint = CodePoint::Char('\u{f75f}');
pub const CODEPOINT_LONGNAME_ALIGNMENTMARKER: CodePoint = CodePoint::Char('\u{f760}');
pub const CODEPOINT_LONGNAME_LEFTSKELETON: CodePoint = CodePoint::Char('\u{f761}');
pub const CODEPOINT_LONGNAME_RIGHTSKELETON: CodePoint = CodePoint::Char('\u{f762}');
pub const CODEPOINT_LONGNAME_CONTROLKEY: CodePoint = CodePoint::Char('\u{f763}');
pub const CODEPOINT_LONGNAME_ALIASDELIMITER: CodePoint = CodePoint::Char('\u{f764}');
pub const CODEPOINT_LONGNAME_INVISIBLECOMMA: CodePoint = CodePoint::Char('\u{f765}');
pub const CODEPOINT_LONGNAME_RETURNKEY: CodePoint = CodePoint::Char('\u{f766}');
pub const CODEPOINT_LONGNAME_ERRORINDICATOR: CodePoint = CodePoint::Char('\u{f767}');
pub const CODEPOINT_LONGNAME_ALIASINDICATOR: CodePoint = CodePoint::Char('\u{f768}');
pub const CODEPOINT_LONGNAME_ESCAPEKEY: CodePoint = CodePoint::Char('\u{f769}');
pub const CODEPOINT_LONGNAME_COMMANDKEY: CodePoint = CodePoint::Char('\u{f76a}');
pub const CODEPOINT_LONGNAME_LEFTMODIFIED: CodePoint = CodePoint::Char('\u{f76b}');
pub const CODEPOINT_LONGNAME_RIGHTMODIFIED: CodePoint = CodePoint::Char('\u{f76c}');
pub const CODEPOINT_LONGNAME_INVISIBLEAPPLICATION: CodePoint = CodePoint::Char('\u{f76d}');
pub const CODEPOINT_LONGNAME_DISCRETIONARYLINESEPARATOR: CodePoint = CodePoint::Char('\u{f76e}');
pub const CODEPOINT_LONGNAME_DISCRETIONARYPARAGRAPHSEPARATOR: CodePoint = CodePoint::Char('\u{f76f}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALA: CodePoint = CodePoint::Char('\u{f770}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALC: CodePoint = CodePoint::Char('\u{f772}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALD: CodePoint = CodePoint::Char('\u{f773}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALG: CodePoint = CodePoint::Char('\u{f776}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALJ: CodePoint = CodePoint::Char('\u{f779}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALK: CodePoint = CodePoint::Char('\u{f77a}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALN: CodePoint = CodePoint::Char('\u{f77d}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALO: CodePoint = CodePoint::Char('\u{f77e}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALP: CodePoint = CodePoint::Char('\u{f77f}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALQ: CodePoint = CodePoint::Char('\u{f780}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALS: CodePoint = CodePoint::Char('\u{f782}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALT: CodePoint = CodePoint::Char('\u{f783}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALU: CodePoint = CodePoint::Char('\u{f784}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALV: CodePoint = CodePoint::Char('\u{f785}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALW: CodePoint = CodePoint::Char('\u{f786}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALX: CodePoint = CodePoint::Char('\u{f787}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALY: CodePoint = CodePoint::Char('\u{f788}');
pub const CODEPOINT_LONGNAME_SCRIPTCAPITALZ: CodePoint = CodePoint::Char('\u{f789}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALA: CodePoint = CodePoint::Char('\u{f78a}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALB: CodePoint = CodePoint::Char('\u{f78b}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALD: CodePoint = CodePoint::Char('\u{f78d}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALE: CodePoint = CodePoint::Char('\u{f78e}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALF: CodePoint = CodePoint::Char('\u{f78f}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALG: CodePoint = CodePoint::Char('\u{f790}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALJ: CodePoint = CodePoint::Char('\u{f793}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALK: CodePoint = CodePoint::Char('\u{f794}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALL: CodePoint = CodePoint::Char('\u{f795}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALM: CodePoint = CodePoint::Char('\u{f796}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALN: CodePoint = CodePoint::Char('\u{f797}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALO: CodePoint = CodePoint::Char('\u{f798}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALP: CodePoint = CodePoint::Char('\u{f799}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALQ: CodePoint = CodePoint::Char('\u{f79a}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALS: CodePoint = CodePoint::Char('\u{f79c}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALT: CodePoint = CodePoint::Char('\u{f79d}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALU: CodePoint = CodePoint::Char('\u{f79e}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALV: CodePoint = CodePoint::Char('\u{f79f}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALW: CodePoint = CodePoint::Char('\u{f7a0}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALX: CodePoint = CodePoint::Char('\u{f7a1}');
pub const CODEPOINT_LONGNAME_GOTHICCAPITALY: CodePoint = CodePoint::Char('\u{f7a2}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALA: CodePoint = CodePoint::Char('\u{f7a4}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALB: CodePoint = CodePoint::Char('\u{f7a5}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALC: CodePoint = CodePoint::Char('\u{f7a6}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALD: CodePoint = CodePoint::Char('\u{f7a7}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALE: CodePoint = CodePoint::Char('\u{f7a8}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALF: CodePoint = CodePoint::Char('\u{f7a9}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALG: CodePoint = CodePoint::Char('\u{f7aa}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALH: CodePoint = CodePoint::Char('\u{f7ab}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALI: CodePoint = CodePoint::Char('\u{f7ac}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALJ: CodePoint = CodePoint::Char('\u{f7ad}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALK: CodePoint = CodePoint::Char('\u{f7ae}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALL: CodePoint = CodePoint::Char('\u{f7af}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALM: CodePoint = CodePoint::Char('\u{f7b0}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALN: CodePoint = CodePoint::Char('\u{f7b1}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALO: CodePoint = CodePoint::Char('\u{f7b2}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALP: CodePoint = CodePoint::Char('\u{f7b3}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALQ: CodePoint = CodePoint::Char('\u{f7b4}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALR: CodePoint = CodePoint::Char('\u{f7b5}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALS: CodePoint = CodePoint::Char('\u{f7b6}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALT: CodePoint = CodePoint::Char('\u{f7b7}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALU: CodePoint = CodePoint::Char('\u{f7b8}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALV: CodePoint = CodePoint::Char('\u{f7b9}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALW: CodePoint = CodePoint::Char('\u{f7ba}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALX: CodePoint = CodePoint::Char('\u{f7bb}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALY: CodePoint = CodePoint::Char('\u{f7bc}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALZ: CodePoint = CodePoint::Char('\u{f7bd}');
pub const CODEPOINT_LONGNAME_TABKEY: CodePoint = CodePoint::Char('\u{f7be}');
pub const CODEPOINT_LONGNAME_SPACEKEY: CodePoint = CodePoint::Char('\u{f7bf}');
pub const CODEPOINT_LONGNAME_DELETEKEY: CodePoint = CodePoint::Char('\u{f7d0}');
pub const CODEPOINT_LONGNAME_ALTKEY: CodePoint = CodePoint::Char('\u{f7d1}');
pub const CODEPOINT_LONGNAME_OPTIONKEY: CodePoint = CodePoint::Char('\u{f7d2}');
pub const CODEPOINT_LONGNAME_KEYBAR: CodePoint = CodePoint::Char('\u{f7d3}');
pub const CODEPOINT_LONGNAME_ENTERKEY: CodePoint = CodePoint::Char('\u{f7d4}');
pub const CODEPOINT_LONGNAME_SHIFTKEY: CodePoint = CodePoint::Char('\u{f7d5}');
pub const CODEPOINT_LONGNAME_MOD1KEY: CodePoint = CodePoint::Char('\u{f7d6}');
pub const CODEPOINT_LONGNAME_MOD2KEY: CodePoint = CodePoint::Char('\u{f7d7}');
pub const CODEPOINT_LONGNAME_LONGEQUAL: CodePoint = CodePoint::Char('\u{f7d9}');
pub const CODEPOINT_LONGNAME_CONSTANTC: CodePoint = CodePoint::Char('\u{f7da}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKZERO: CodePoint = CodePoint::Char('\u{f7db}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKONE: CodePoint = CodePoint::Char('\u{f7dc}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKTWO: CodePoint = CodePoint::Char('\u{f7dd}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKTHREE: CodePoint = CodePoint::Char('\u{f7de}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKFOUR: CodePoint = CodePoint::Char('\u{f7df}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKFIVE: CodePoint = CodePoint::Char('\u{f7e0}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKSIX: CodePoint = CodePoint::Char('\u{f7e1}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKSEVEN: CodePoint = CodePoint::Char('\u{f7e2}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKEIGHT: CodePoint = CodePoint::Char('\u{f7e3}');
pub const CODEPOINT_LONGNAME_DOUBLESTRUCKNINE: CodePoint = CodePoint::Char('\u{f7e4}');
pub const CODEPOINT_LONGNAME_GOTHICZERO: CodePoint = CodePoint::Char('\u{f7e5}');
pub const CODEPOINT_LONGNAME_GOTHICONE: CodePoint = CodePoint::Char('\u{f7e6}');
pub const CODEPOINT_LONGNAME_GOTHICTWO: CodePoint = CodePoint::Char('\u{f7e7}');
pub const CODEPOINT_LONGNAME_GOTHICTHREE: CodePoint = CodePoint::Char('\u{f7e8}');
pub const CODEPOINT_LONGNAME_GOTHICFOUR: CodePoint = CodePoint::Char('\u{f7e9}');
pub const CODEPOINT_LONGNAME_GOTHICFIVE: CodePoint = CodePoint::Char('\u{f7ea}');
pub const CODEPOINT_LONGNAME_GOTHICSIX: CodePoint = CodePoint::Char('\u{f7eb}');
pub const CODEPOINT_LONGNAME_GOTHICSEVEN: CodePoint = CodePoint::Char('\u{f7ec}');
pub const CODEPOINT_LONGNAME_GOTHICEIGHT: CodePoint = CodePoint::Char('\u{f7ed}');
pub const CODEPOINT_LONGNAME_GOTHICNINE: CodePoint = CodePoint::Char('\u{f7ef}');
pub const CODEPOINT_LONGNAME_SCRIPTZERO: CodePoint = CodePoint::Char('\u{f7f0}');
pub const CODEPOINT_LONGNAME_SCRIPTONE: CodePoint = CodePoint::Char('\u{f7f1}');
pub const CODEPOINT_LONGNAME_SCRIPTTWO: CodePoint = CodePoint::Char('\u{f7f2}');
pub const CODEPOINT_LONGNAME_SCRIPTTHREE: CodePoint = CodePoint::Char('\u{f7f3}');
pub const CODEPOINT_LONGNAME_SCRIPTFOUR: CodePoint = CodePoint::Char('\u{f7f4}');
pub const CODEPOINT_LONGNAME_SCRIPTFIVE: CodePoint = CodePoint::Char('\u{f7f5}');
pub const CODEPOINT_LONGNAME_SCRIPTSIX: CodePoint = CodePoint::Char('\u{f7f6}');
pub const CODEPOINT_LONGNAME_SCRIPTSEVEN: CodePoint = CodePoint::Char('\u{f7f7}');
pub const CODEPOINT_LONGNAME_SCRIPTEIGHT: CodePoint = CodePoint::Char('\u{f7f8}');
pub const CODEPOINT_LONGNAME_SCRIPTNINE: CodePoint = CodePoint::Char('\u{f7f9}');
pub const CODEPOINT_LONGNAME_FIRSTPAGE: CodePoint = CodePoint::Char('\u{f7fa}');
pub const CODEPOINT_LONGNAME_LASTPAGE: CodePoint = CodePoint::Char('\u{f7fb}');
pub const CODEPOINT_LONGNAME_NUMBERCOMMA: CodePoint = CodePoint::Char('\u{f7fc}');
pub const CODEPOINT_LONGNAME_FORMALA: CodePoint = CodePoint::Char('\u{f800}');
pub const CODEPOINT_LONGNAME_FORMALB: CodePoint = CodePoint::Char('\u{f801}');
pub const CODEPOINT_LONGNAME_FORMALC: CodePoint = CodePoint::Char('\u{f802}');
pub const CODEPOINT_LONGNAME_FORMALD: CodePoint = CodePoint::Char('\u{f803}');
pub const CODEPOINT_LONGNAME_FORMALE: CodePoint = CodePoint::Char('\u{f804}');
pub const CODEPOINT_LONGNAME_FORMALF: CodePoint = CodePoint::Char('\u{f805}');
pub const CODEPOINT_LONGNAME_FORMALG: CodePoint = CodePoint::Char('\u{f806}');
pub const CODEPOINT_LONGNAME_FORMALH: CodePoint = CodePoint::Char('\u{f807}');
pub const CODEPOINT_LONGNAME_FORMALI: CodePoint = CodePoint::Char('\u{f808}');
pub const CODEPOINT_LONGNAME_FORMALJ: CodePoint = CodePoint::Char('\u{f809}');
pub const CODEPOINT_LONGNAME_FORMALK: CodePoint = CodePoint::Char('\u{f80a}');
pub const CODEPOINT_LONGNAME_FORMALL: CodePoint = CodePoint::Char('\u{f80b}');
pub const CODEPOINT_LONGNAME_FORMALM: CodePoint = CodePoint::Char('\u{f80c}');
pub const CODEPOINT_LONGNAME_FORMALN: CodePoint = CodePoint::Char('\u{f80d}');
pub const CODEPOINT_LONGNAME_FORMALO: CodePoint = CodePoint::Char('\u{f80e}');
pub const CODEPOINT_LONGNAME_FORMALP: CodePoint = CodePoint::Char('\u{f80f}');
pub const CODEPOINT_LONGNAME_FORMALQ: CodePoint = CodePoint::Char('\u{f810}');
pub const CODEPOINT_LONGNAME_FORMALR: CodePoint = CodePoint::Char('\u{f811}');
pub const CODEPOINT_LONGNAME_FORMALS: CodePoint = CodePoint::Char('\u{f812}');
pub const CODEPOINT_LONGNAME_FORMALT: CodePoint = CodePoint::Char('\u{f813}');
pub const CODEPOINT_LONGNAME_FORMALU: CodePoint = CodePoint::Char('\u{f814}');
pub const CODEPOINT_LONGNAME_FORMALV: CodePoint = CodePoint::Char('\u{f815}');
pub const CODEPOINT_LONGNAME_FORMALW: CodePoint = CodePoint::Char('\u{f816}');
pub const CODEPOINT_LONGNAME_FORMALX: CodePoint = CodePoint::Char('\u{f817}');
pub const CODEPOINT_LONGNAME_FORMALY: CodePoint = CodePoint::Char('\u{f818}');
pub const CODEPOINT_LONGNAME_FORMALZ: CodePoint = CodePoint::Char('\u{f819}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALA: CodePoint = CodePoint::Char('\u{f81a}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALB: CodePoint = CodePoint::Char('\u{f81b}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALC: CodePoint = CodePoint::Char('\u{f81c}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALD: CodePoint = CodePoint::Char('\u{f81d}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALE: CodePoint = CodePoint::Char('\u{f81e}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALF: CodePoint = CodePoint::Char('\u{f81f}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALG: CodePoint = CodePoint::Char('\u{f820}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALH: CodePoint = CodePoint::Char('\u{f821}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALI: CodePoint = CodePoint::Char('\u{f822}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALJ: CodePoint = CodePoint::Char('\u{f823}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALK: CodePoint = CodePoint::Char('\u{f824}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALL: CodePoint = CodePoint::Char('\u{f825}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALM: CodePoint = CodePoint::Char('\u{f826}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALN: CodePoint = CodePoint::Char('\u{f827}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALO: CodePoint = CodePoint::Char('\u{f828}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALP: CodePoint = CodePoint::Char('\u{f829}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALQ: CodePoint = CodePoint::Char('\u{f82a}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALR: CodePoint = CodePoint::Char('\u{f82b}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALS: CodePoint = CodePoint::Char('\u{f82c}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALT: CodePoint = CodePoint::Char('\u{f82d}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALU: CodePoint = CodePoint::Char('\u{f82e}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALV: CodePoint = CodePoint::Char('\u{f82f}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALW: CodePoint = CodePoint::Char('\u{f830}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALX: CodePoint = CodePoint::Char('\u{f831}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALY: CodePoint = CodePoint::Char('\u{f832}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALZ: CodePoint = CodePoint::Char('\u{f833}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALALPHA: CodePoint = CodePoint::Char('\u{f834}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALBETA: CodePoint = CodePoint::Char('\u{f835}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALGAMMA: CodePoint = CodePoint::Char('\u{f836}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALDELTA: CodePoint = CodePoint::Char('\u{f837}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALEPSILON: CodePoint = CodePoint::Char('\u{f838}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALZETA: CodePoint = CodePoint::Char('\u{f839}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALETA: CodePoint = CodePoint::Char('\u{f83a}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALTHETA: CodePoint = CodePoint::Char('\u{f83b}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALIOTA: CodePoint = CodePoint::Char('\u{f83c}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALKAPPA: CodePoint = CodePoint::Char('\u{f83d}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALLAMBDA: CodePoint = CodePoint::Char('\u{f83e}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALMU: CodePoint = CodePoint::Char('\u{f83f}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALNU: CodePoint = CodePoint::Char('\u{f840}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALXI: CodePoint = CodePoint::Char('\u{f841}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALOMICRON: CodePoint = CodePoint::Char('\u{f842}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALPI: CodePoint = CodePoint::Char('\u{f843}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALRHO: CodePoint = CodePoint::Char('\u{f844}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALSIGMA: CodePoint = CodePoint::Char('\u{f846}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALTAU: CodePoint = CodePoint::Char('\u{f847}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALUPSILON: CodePoint = CodePoint::Char('\u{f848}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALPHI: CodePoint = CodePoint::Char('\u{f849}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALCHI: CodePoint = CodePoint::Char('\u{f84a}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALPSI: CodePoint = CodePoint::Char('\u{f84b}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALOMEGA: CodePoint = CodePoint::Char('\u{f84c}');
pub const CODEPOINT_LONGNAME_FORMALALPHA: CodePoint = CodePoint::Char('\u{f854}');
pub const CODEPOINT_LONGNAME_FORMALBETA: CodePoint = CodePoint::Char('\u{f855}');
pub const CODEPOINT_LONGNAME_FORMALGAMMA: CodePoint = CodePoint::Char('\u{f856}');
pub const CODEPOINT_LONGNAME_FORMALDELTA: CodePoint = CodePoint::Char('\u{f857}');
pub const CODEPOINT_LONGNAME_FORMALCURLYEPSILON: CodePoint = CodePoint::Char('\u{f858}');
pub const CODEPOINT_LONGNAME_FORMALZETA: CodePoint = CodePoint::Char('\u{f859}');
pub const CODEPOINT_LONGNAME_FORMALETA: CodePoint = CodePoint::Char('\u{f85a}');
pub const CODEPOINT_LONGNAME_FORMALTHETA: CodePoint = CodePoint::Char('\u{f85b}');
pub const CODEPOINT_LONGNAME_FORMALIOTA: CodePoint = CodePoint::Char('\u{f85c}');
pub const CODEPOINT_LONGNAME_FORMALKAPPA: CodePoint = CodePoint::Char('\u{f85d}');
pub const CODEPOINT_LONGNAME_FORMALLAMBDA: CodePoint = CodePoint::Char('\u{f85e}');
pub const CODEPOINT_LONGNAME_FORMALMU: CodePoint = CodePoint::Char('\u{f85f}');
pub const CODEPOINT_LONGNAME_FORMALNU: CodePoint = CodePoint::Char('\u{f860}');
pub const CODEPOINT_LONGNAME_FORMALXI: CodePoint = CodePoint::Char('\u{f861}');
pub const CODEPOINT_LONGNAME_FORMALOMICRON: CodePoint = CodePoint::Char('\u{f862}');
pub const CODEPOINT_LONGNAME_FORMALPI: CodePoint = CodePoint::Char('\u{f863}');
pub const CODEPOINT_LONGNAME_FORMALRHO: CodePoint = CodePoint::Char('\u{f864}');
pub const CODEPOINT_LONGNAME_FORMALFINALSIGMA: CodePoint = CodePoint::Char('\u{f865}');
pub const CODEPOINT_LONGNAME_FORMALSIGMA: CodePoint = CodePoint::Char('\u{f866}');
pub const CODEPOINT_LONGNAME_FORMALTAU: CodePoint = CodePoint::Char('\u{f867}');
pub const CODEPOINT_LONGNAME_FORMALUPSILON: CodePoint = CodePoint::Char('\u{f868}');
pub const CODEPOINT_LONGNAME_FORMALCURLYPHI: CodePoint = CodePoint::Char('\u{f869}');
pub const CODEPOINT_LONGNAME_FORMALCHI: CodePoint = CodePoint::Char('\u{f86a}');
pub const CODEPOINT_LONGNAME_FORMALPSI: CodePoint = CodePoint::Char('\u{f86b}');
pub const CODEPOINT_LONGNAME_FORMALOMEGA: CodePoint = CodePoint::Char('\u{f86c}');
pub const CODEPOINT_LONGNAME_FORMALCURLYTHETA: CodePoint = CodePoint::Char('\u{f874}');
pub const CODEPOINT_LONGNAME_FORMALCURLYCAPITALUPSILON: CodePoint = CodePoint::Char('\u{f875}');
pub const CODEPOINT_LONGNAME_FORMALPHI: CodePoint = CodePoint::Char('\u{f878}');
pub const CODEPOINT_LONGNAME_FORMALCURLYPI: CodePoint = CodePoint::Char('\u{f879}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALSTIGMA: CodePoint = CodePoint::Char('\u{f87d}');
pub const CODEPOINT_LONGNAME_FORMALSTIGMA: CodePoint = CodePoint::Char('\u{f87e}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALDIGAMMA: CodePoint = CodePoint::Char('\u{f87f}');
pub const CODEPOINT_LONGNAME_FORMALDIGAMMA: CodePoint = CodePoint::Char('\u{f880}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALKOPPA: CodePoint = CodePoint::Char('\u{f881}');
pub const CODEPOINT_LONGNAME_FORMALKOPPA: CodePoint = CodePoint::Char('\u{f882}');
pub const CODEPOINT_LONGNAME_FORMALCAPITALSAMPI: CodePoint = CodePoint::Char('\u{f883}');
pub const CODEPOINT_LONGNAME_FORMALSAMPI: CodePoint = CodePoint::Char('\u{f884}');
pub const CODEPOINT_LONGNAME_FORMALCURLYKAPPA: CodePoint = CodePoint::Char('\u{f885}');
pub const CODEPOINT_LONGNAME_FORMALCURLYRHO: CodePoint = CodePoint::Char('\u{f886}');
pub const CODEPOINT_LONGNAME_FORMALEPSILON: CodePoint = CodePoint::Char('\u{f88a}');
pub const CODEPOINT_LONGNAME_FILIGATURE: CodePoint = CodePoint::Char('\u{fb01}');
pub const CODEPOINT_LONGNAME_FLLIGATURE: CodePoint = CodePoint::Char('\u{fb02}');
pub const CODEPOINT_LONGNAME_OVERPARENTHESIS: CodePoint = CodePoint::Char('\u{fe35}');
pub const CODEPOINT_LONGNAME_UNDERPARENTHESIS: CodePoint = CodePoint::Char('\u{fe36}');
pub const CODEPOINT_LONGNAME_OVERBRACE: CodePoint = CodePoint::Char('\u{fe37}');
pub const CODEPOINT_LONGNAME_UNDERBRACE: CodePoint = CodePoint::Char('\u{fe38}');
pub const CODEPOINT_LONGNAME_UNKNOWNGLYPH: CodePoint = CodePoint::Char('\u{fffd}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTA: CodePoint = CodePoint::Char('\u{0fa000}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTB: CodePoint = CodePoint::Char('\u{0fa001}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTC: CodePoint = CodePoint::Char('\u{0fa002}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTD: CodePoint = CodePoint::Char('\u{0fa003}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTE: CodePoint = CodePoint::Char('\u{0fa004}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTF: CodePoint = CodePoint::Char('\u{0fa005}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTG: CodePoint = CodePoint::Char('\u{0fa006}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTH: CodePoint = CodePoint::Char('\u{0fa007}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTI: CodePoint = CodePoint::Char('\u{0fa008}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTJ: CodePoint = CodePoint::Char('\u{0fa009}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTK: CodePoint = CodePoint::Char('\u{0fa00a}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTL: CodePoint = CodePoint::Char('\u{0fa00b}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTM: CodePoint = CodePoint::Char('\u{0fa00c}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTN: CodePoint = CodePoint::Char('\u{0fa00d}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTO: CodePoint = CodePoint::Char('\u{0fa00e}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTP: CodePoint = CodePoint::Char('\u{0fa00f}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTQ: CodePoint = CodePoint::Char('\u{0fa010}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTR: CodePoint = CodePoint::Char('\u{0fa011}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTS: CodePoint = CodePoint::Char('\u{0fa012}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTT: CodePoint = CodePoint::Char('\u{0fa013}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTU: CodePoint = CodePoint::Char('\u{0fa014}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTV: CodePoint = CodePoint::Char('\u{0fa015}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTW: CodePoint = CodePoint::Char('\u{0fa016}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTX: CodePoint = CodePoint::Char('\u{0fa017}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTY: CodePoint = CodePoint::Char('\u{0fa018}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTZ: CodePoint = CodePoint::Char('\u{0fa019}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALA: CodePoint = CodePoint::Char('\u{0fa01a}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALB: CodePoint = CodePoint::Char('\u{0fa01b}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALC: CodePoint = CodePoint::Char('\u{0fa01c}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALD: CodePoint = CodePoint::Char('\u{0fa01d}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALE: CodePoint = CodePoint::Char('\u{0fa01e}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALF: CodePoint = CodePoint::Char('\u{0fa01f}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALG: CodePoint = CodePoint::Char('\u{0fa020}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALH: CodePoint = CodePoint::Char('\u{0fa021}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALI: CodePoint = CodePoint::Char('\u{0fa022}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALJ: CodePoint = CodePoint::Char('\u{0fa023}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALK: CodePoint = CodePoint::Char('\u{0fa024}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALL: CodePoint = CodePoint::Char('\u{0fa025}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALM: CodePoint = CodePoint::Char('\u{0fa026}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALN: CodePoint = CodePoint::Char('\u{0fa027}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALO: CodePoint = CodePoint::Char('\u{0fa028}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALP: CodePoint = CodePoint::Char('\u{0fa029}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALQ: CodePoint = CodePoint::Char('\u{0fa02a}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALR: CodePoint = CodePoint::Char('\u{0fa02b}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALS: CodePoint = CodePoint::Char('\u{0fa02c}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALT: CodePoint = CodePoint::Char('\u{0fa02d}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALU: CodePoint = CodePoint::Char('\u{0fa02e}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALV: CodePoint = CodePoint::Char('\u{0fa02f}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALW: CodePoint = CodePoint::Char('\u{0fa030}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALX: CodePoint = CodePoint::Char('\u{0fa031}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALY: CodePoint = CodePoint::Char('\u{0fa032}');
pub const CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALZ: CodePoint = CodePoint::Char('\u{0fa033}');


//
// AUTO GENERATED FILE
// DO NOT MODIFY
//

use std::collections::HashMap;

use once_cell::sync::Lazy;

use crate::{
	code_point::*,
	token::TokenKind::{self, *},
};


//
//
//
pub const LONGNAME_TO_CODE_POINT_MAP__NAMES: [&str; LONGNAMES_COUNT] = [
"AAcute", "ABar", "ACup", "ADoubleDot", "AE", "AGrave", "AHat", "ARing", "ATilde", "Akuz", "Aleph", "AliasDelimiter", 
"AliasIndicator", "AlignmentMarker", "Alpha", "AltKey", "And", "Andy", "Angle", "Angstrom", "Application", 
"AquariusSign", "AriesSign", "AscendingEllipsis", "AutoLeftMatch", "AutoOperand", "AutoPlaceholder", "AutoRightMatch", 
"AutoSpace", "Backslash", "BeamedEighthNote", "BeamedSixteenthNote", "Because", "Bet", "Beta", "BlackBishop", 
"BlackKing", "BlackKnight", "BlackPawn", "BlackQueen", "BlackRook", "Breve", "Bullet", "CAcute", "CCedilla", "CHacek", 
"COMPATIBILITYKanjiSpace", "COMPATIBILITYNoBreak", "CancerSign", "Cap", "CapitalAAcute", "CapitalABar", "CapitalACup", 
"CapitalADoubleDot", "CapitalAE", "CapitalAGrave", "CapitalAHat", "CapitalARing", "CapitalATilde", "CapitalAlpha", 
"CapitalBeta", "CapitalCAcute", "CapitalCCedilla", "CapitalCHacek", "CapitalChi", "CapitalDHacek", "CapitalDelta", 
"CapitalDifferentialD", "CapitalDigamma", "CapitalEAcute", "CapitalEBar", "CapitalECup", "CapitalEDoubleDot", 
"CapitalEGrave", "CapitalEHacek", "CapitalEHat", "CapitalEpsilon", "CapitalEta", "CapitalEth", "CapitalGamma", 
"CapitalIAcute", "CapitalICup", "CapitalIDoubleDot", "CapitalIGrave", "CapitalIHat", "CapitalIota", "CapitalKappa", 
"CapitalKoppa", "CapitalLSlash", "CapitalLambda", "CapitalMu", "CapitalNHacek", "CapitalNTilde", "CapitalNu", 
"CapitalOAcute", "CapitalODoubleAcute", "CapitalODoubleDot", "CapitalOE", "CapitalOGrave", "CapitalOHat", 
"CapitalOSlash", "CapitalOTilde", "CapitalOmega", "CapitalOmicron", "CapitalPhi", "CapitalPi", "CapitalPsi", 
"CapitalRHacek", "CapitalRho", "CapitalSHacek", "CapitalSampi", "CapitalSigma", "CapitalStigma", "CapitalTHacek", 
"CapitalTau", "CapitalTheta", "CapitalThorn", "CapitalUAcute", "CapitalUDoubleAcute", "CapitalUDoubleDot", 
"CapitalUGrave", "CapitalUHat", "CapitalURing", "CapitalUpsilon", "CapitalXi", "CapitalYAcute", "CapitalZHacek", 
"CapitalZeta", "CapricornSign", "Cedilla", "Cent", "CenterDot", "CenterEllipsis", "CheckedBox", "Checkmark", 
"CheckmarkedBox", "Chi", "CircleDot", "CircleMinus", "CirclePlus", "CircleTimes", "ClockwiseContourIntegral", 
"CloseCurlyDoubleQuote", "CloseCurlyQuote", "CloverLeaf", "ClubSuit", "Colon", "CommandKey", "Conditioned", "Congruent",
 "Conjugate", "ConjugateTranspose", "ConstantC", "Continuation", "ContinuedFractionK", "ContourIntegral", "ControlKey", 
"Coproduct", "Copyright", "CounterClockwiseContourIntegral", "Cross", "CubeRoot", "Cup", "CupCap", "CurlyCapitalUpsilon"
, "CurlyEpsilon", "CurlyKappa", "CurlyPhi", "CurlyPi", "CurlyRho", "CurlyTheta", "Currency", "DHacek", "Dagger", "Dalet"
, "Dash", "Degree", "Del", "DeleteKey", "Delta", "DescendingEllipsis", "Diameter", "Diamond", "DiamondSuit", 
"DifferenceDelta", "DifferentialD", "Digamma", "DirectedEdge", "DiscreteRatio", "DiscreteShift", "DiscretionaryHyphen", 
"DiscretionaryLineSeparator", "DiscretionaryPageBreakAbove", "DiscretionaryPageBreakBelow", 
"DiscretionaryParagraphSeparator", "Distributed", "Divide", "Divides", "DivisionSlash", "DotEqual", "DotlessI", 
"DotlessJ", "DottedSquare", "DoubleContourIntegral", "DoubleDagger", "DoubleDot", "DoubleDownArrow", "DoubleLeftArrow", 
"DoubleLeftRightArrow", "DoubleLeftTee", "DoubleLongLeftArrow", "DoubleLongLeftRightArrow", "DoubleLongRightArrow", 
"DoublePrime", "DoubleRightArrow", "DoubleRightTee", "DoubleStruckA", "DoubleStruckB", "DoubleStruckC", 
"DoubleStruckCapitalA", "DoubleStruckCapitalB", "DoubleStruckCapitalC", "DoubleStruckCapitalD", "DoubleStruckCapitalE", 
"DoubleStruckCapitalF", "DoubleStruckCapitalG", "DoubleStruckCapitalH", "DoubleStruckCapitalI", "DoubleStruckCapitalJ", 
"DoubleStruckCapitalK", "DoubleStruckCapitalL", "DoubleStruckCapitalM", "DoubleStruckCapitalN", "DoubleStruckCapitalO", 
"DoubleStruckCapitalP", "DoubleStruckCapitalQ", "DoubleStruckCapitalR", "DoubleStruckCapitalS", "DoubleStruckCapitalT", 
"DoubleStruckCapitalU", "DoubleStruckCapitalV", "DoubleStruckCapitalW", "DoubleStruckCapitalX", "DoubleStruckCapitalY", 
"DoubleStruckCapitalZ", "DoubleStruckD", "DoubleStruckE", "DoubleStruckEight", "DoubleStruckF", "DoubleStruckFive", 
"DoubleStruckFour", "DoubleStruckG", "DoubleStruckH", "DoubleStruckI", "DoubleStruckJ", "DoubleStruckK", "DoubleStruckL"
, "DoubleStruckM", "DoubleStruckN", "DoubleStruckNine", "DoubleStruckO", "DoubleStruckOne", "DoubleStruckP", 
"DoubleStruckQ", "DoubleStruckR", "DoubleStruckS", "DoubleStruckSeven", "DoubleStruckSix", "DoubleStruckT", 
"DoubleStruckThree", "DoubleStruckTwo", "DoubleStruckU", "DoubleStruckV", "DoubleStruckW", "DoubleStruckX", 
"DoubleStruckY", "DoubleStruckZ", "DoubleStruckZero", "DoubleUpArrow", "DoubleUpDownArrow", "DoubleVerticalBar", 
"DoubledGamma", "DoubledPi", "DownArrow", "DownArrowBar", "DownArrowUpArrow", "DownBreve", "DownExclamation", 
"DownLeftRightVector", "DownLeftTeeVector", "DownLeftVector", "DownLeftVectorBar", "DownPointer", "DownQuestion", 
"DownRightTeeVector", "DownRightVector", "DownRightVectorBar", "DownTee", "DownTeeArrow", "EAcute", "EBar", "ECup", 
"EDoubleDot", "EGrave", "EHacek", "EHat", "Earth", "EighthNote", "Element", "Ellipsis", "EmptyCircle", "EmptyDiamond", 
"EmptyDownTriangle", "EmptyRectangle", "EmptySet", "EmptySmallCircle", "EmptySmallSquare", "EmptySquare", 
"EmptyUpTriangle", "EmptyVerySmallSquare", "EnterKey", "EntityEnd", "EntityStart", "Epsilon", "Equal", "EqualTilde", 
"Equilibrium", "Equivalent", "ErrorIndicator", "EscapeKey", "Eta", "Eth", "Euro", "Exists", "ExpectationE", 
"ExponentialE", "FiLigature", "FilledCircle", "FilledDiamond", "FilledDownTriangle", "FilledLeftTriangle", 
"FilledRectangle", "FilledRightTriangle", "FilledSmallCircle", "FilledSmallSquare", "FilledSquare", "FilledUpTriangle", 
"FilledVerySmallSquare", "FinalSigma", "FirstPage", "FivePointedStar", "FlLigature", "Flat", "Florin", "ForAll", 
"FormalA", "FormalAlpha", "FormalB", "FormalBeta", "FormalC", "FormalCapitalA", "FormalCapitalAlpha", "FormalCapitalB", 
"FormalCapitalBeta", "FormalCapitalC", "FormalCapitalChi", "FormalCapitalD", "FormalCapitalDelta", 
"FormalCapitalDigamma", "FormalCapitalE", "FormalCapitalEpsilon", "FormalCapitalEta", "FormalCapitalF", "FormalCapitalG"
, "FormalCapitalGamma", "FormalCapitalH", "FormalCapitalI", "FormalCapitalIota", "FormalCapitalJ", "FormalCapitalK", 
"FormalCapitalKappa", "FormalCapitalKoppa", "FormalCapitalL", "FormalCapitalLambda", "FormalCapitalM", "FormalCapitalMu"
, "FormalCapitalN", "FormalCapitalNu", "FormalCapitalO", "FormalCapitalOmega", "FormalCapitalOmicron", "FormalCapitalP",
 "FormalCapitalPhi", "FormalCapitalPi", "FormalCapitalPsi", "FormalCapitalQ", "FormalCapitalR", "FormalCapitalRho", 
"FormalCapitalS", "FormalCapitalSampi", "FormalCapitalSigma", "FormalCapitalStigma", "FormalCapitalT", 
"FormalCapitalTau", "FormalCapitalTheta", "FormalCapitalU", "FormalCapitalUpsilon", "FormalCapitalV", "FormalCapitalW", 
"FormalCapitalX", "FormalCapitalXi", "FormalCapitalY", "FormalCapitalZ", "FormalCapitalZeta", "FormalChi", 
"FormalCurlyCapitalUpsilon", "FormalCurlyEpsilon", "FormalCurlyKappa", "FormalCurlyPhi", "FormalCurlyPi", 
"FormalCurlyRho", "FormalCurlyTheta", "FormalD", "FormalDelta", "FormalDigamma", "FormalE", "FormalEpsilon", "FormalEta"
, "FormalF", "FormalFinalSigma", "FormalG", "FormalGamma", "FormalH", "FormalI", "FormalIota", "FormalJ", "FormalK", 
"FormalKappa", "FormalKoppa", "FormalL", "FormalLambda", "FormalM", "FormalMu", "FormalN", "FormalNu", "FormalO", 
"FormalOmega", "FormalOmicron", "FormalP", "FormalPhi", "FormalPi", "FormalPsi", "FormalQ", "FormalR", "FormalRho", 
"FormalS", "FormalSampi", "FormalScriptA", "FormalScriptB", "FormalScriptC", "FormalScriptCapitalA", 
"FormalScriptCapitalB", "FormalScriptCapitalC", "FormalScriptCapitalD", "FormalScriptCapitalE", "FormalScriptCapitalF", 
"FormalScriptCapitalG", "FormalScriptCapitalH", "FormalScriptCapitalI", "FormalScriptCapitalJ", "FormalScriptCapitalK", 
"FormalScriptCapitalL", "FormalScriptCapitalM", "FormalScriptCapitalN", "FormalScriptCapitalO", "FormalScriptCapitalP", 
"FormalScriptCapitalQ", "FormalScriptCapitalR", "FormalScriptCapitalS", "FormalScriptCapitalT", "FormalScriptCapitalU", 
"FormalScriptCapitalV", "FormalScriptCapitalW", "FormalScriptCapitalX", "FormalScriptCapitalY", "FormalScriptCapitalZ", 
"FormalScriptD", "FormalScriptE", "FormalScriptF", "FormalScriptG", "FormalScriptH", "FormalScriptI", "FormalScriptJ", 
"FormalScriptK", "FormalScriptL", "FormalScriptM", "FormalScriptN", "FormalScriptO", "FormalScriptP", "FormalScriptQ", 
"FormalScriptR", "FormalScriptS", "FormalScriptT", "FormalScriptU", "FormalScriptV", "FormalScriptW", "FormalScriptX", 
"FormalScriptY", "FormalScriptZ", "FormalSigma", "FormalStigma", "FormalT", "FormalTau", "FormalTheta", "FormalU", 
"FormalUpsilon", "FormalV", "FormalW", "FormalX", "FormalXi", "FormalY", "FormalZ", "FormalZeta", "FreakedSmiley", 
"FreeformPrompt", "Function", "Gamma", "GeminiSign", "Gimel", "GothicA", "GothicB", "GothicC", "GothicCapitalA", 
"GothicCapitalB", "GothicCapitalC", "GothicCapitalD", "GothicCapitalE", "GothicCapitalF", "GothicCapitalG", 
"GothicCapitalH", "GothicCapitalI", "GothicCapitalJ", "GothicCapitalK", "GothicCapitalL", "GothicCapitalM", 
"GothicCapitalN", "GothicCapitalO", "GothicCapitalP", "GothicCapitalQ", "GothicCapitalR", "GothicCapitalS", 
"GothicCapitalT", "GothicCapitalU", "GothicCapitalV", "GothicCapitalW", "GothicCapitalX", "GothicCapitalY", 
"GothicCapitalZ", "GothicD", "GothicE", "GothicEight", "GothicF", "GothicFive", "GothicFour", "GothicG", "GothicH", 
"GothicI", "GothicJ", "GothicK", "GothicL", "GothicM", "GothicN", "GothicNine", "GothicO", "GothicOne", "GothicP", 
"GothicQ", "GothicR", "GothicS", "GothicSeven", "GothicSix", "GothicT", "GothicThree", "GothicTwo", "GothicU", "GothicV"
, "GothicW", "GothicX", "GothicY", "GothicZ", "GothicZero", "GrayCircle", "GraySquare", "GreaterEqual", 
"GreaterEqualLess", "GreaterFullEqual", "GreaterGreater", "GreaterLess", "GreaterSlantEqual", "GreaterTilde", "HBar", 
"Hacek", "HappySmiley", "HeartSuit", "HermitianConjugate", "HorizontalLine", "HumpDownHump", "HumpEqual", "Hyphen", 
"IAcute", "ICup", "IDoubleDot", "IGrave", "IHat", "ImaginaryI", "ImaginaryJ", "ImplicitPlus", "Implies", 
"IndentingNewLine", "Infinity", "InlinePart", "Integral", "Intersection", "InvisibleApplication", "InvisibleComma", 
"InvisiblePostfixScriptBase", "InvisiblePrefixScriptBase", "InvisibleSpace", "InvisibleTimes", "Iota", "Jupiter", 
"Kappa", "KernelIcon", "KeyBar", "Koppa", "LSlash", "Lambda", "LastPage", "LeftAngleBracket", "LeftArrow", 
"LeftArrowBar", "LeftArrowRightArrow", "LeftAssociation", "LeftBracketingBar", "LeftCeiling", "LeftDoubleBracket", 
"LeftDoubleBracketingBar", "LeftDownTeeVector", "LeftDownVector", "LeftDownVectorBar", "LeftFloor", "LeftGuillemet", 
"LeftModified", "LeftPointer", "LeftRightArrow", "LeftRightVector", "LeftSkeleton", "LeftTee", "LeftTeeArrow", 
"LeftTeeVector", "LeftTriangle", "LeftTriangleBar", "LeftTriangleEqual", "LeftUpDownVector", "LeftUpTeeVector", 
"LeftUpVector", "LeftUpVectorBar", "LeftVector", "LeftVectorBar", "LeoSign", "LessEqual", "LessEqualGreater", 
"LessFullEqual", "LessGreater", "LessLess", "LessSlantEqual", "LessTilde", "LetterSpace", "LibraSign", "LightBulb", 
"Limit", "LineSeparator", "LongDash", "LongEqual", "LongLeftArrow", "LongLeftRightArrow", "LongRightArrow", 
"LowerLeftArrow", "LowerRightArrow", "Mars", "MathematicaIcon", "MaxLimit", "MeasuredAngle", "MediumSpace", "Mercury", 
"Mho", "Micro", "MinLimit", "Minus", "MinusPlus", "Mod1Key", "Mod2Key", "Moon", "Mu", "NHacek", "NTilde", "Nand", 
"Natural", "NegativeMediumSpace", "NegativeThickSpace", "NegativeThinSpace", "NegativeVeryThinSpace", "Neptune", 
"NestedGreaterGreater", "NestedLessLess", "NeutralSmiley", "NewLine", "NoBreak", "NonBreakingSpace", "Nor", "Not", 
"NotCongruent", "NotCupCap", "NotDoubleVerticalBar", "NotElement", "NotEqual", "NotEqualTilde", "NotExists", 
"NotGreater", "NotGreaterEqual", "NotGreaterFullEqual", "NotGreaterGreater", "NotGreaterLess", "NotGreaterSlantEqual", 
"NotGreaterTilde", "NotHumpDownHump", "NotHumpEqual", "NotLeftTriangle", "NotLeftTriangleBar", "NotLeftTriangleEqual", 
"NotLess", "NotLessEqual", "NotLessFullEqual", "NotLessGreater", "NotLessLess", "NotLessSlantEqual", "NotLessTilde", 
"NotNestedGreaterGreater", "NotNestedLessLess", "NotPrecedes", "NotPrecedesEqual", "NotPrecedesSlantEqual", 
"NotPrecedesTilde", "NotReverseElement", "NotRightTriangle", "NotRightTriangleBar", "NotRightTriangleEqual", 
"NotSquareSubset", "NotSquareSubsetEqual", "NotSquareSuperset", "NotSquareSupersetEqual", "NotSubset", "NotSubsetEqual",
 "NotSucceeds", "NotSucceedsEqual", "NotSucceedsSlantEqual", "NotSucceedsTilde", "NotSuperset", "NotSupersetEqual", 
"NotTilde", "NotTildeEqual", "NotTildeFullEqual", "NotTildeTilde", "NotVerticalBar", "Nu", "Null", "NumberComma", 
"NumberSign", "OAcute", "ODoubleAcute", "ODoubleDot", "OE", "OGrave", "OHat", "OSlash", "OTilde", "Omega", "Omicron", 
"OpenCurlyDoubleQuote", "OpenCurlyQuote", "OptionKey", "Or", "OverBrace", "OverBracket", "OverParenthesis", 
"PageBreakAbove", "PageBreakBelow", "Paragraph", "ParagraphSeparator", "PartialD", "PermutationProduct", "Perpendicular"
, "Phi", "Pi", "Piecewise", "PiscesSign", "Placeholder", "PlusMinus", "Pluto", "Precedes", "PrecedesEqual", 
"PrecedesSlantEqual", "PrecedesTilde", "Prime", "ProbabilityPr", "Product", "Proportion", "Proportional", "Psi", 
"QuarterNote", "RHacek", "RawAmpersand", "RawAt", "RawBackquote", "RawBackslash", "RawColon", "RawComma", "RawDash", 
"RawDollar", "RawDot", "RawDoubleQuote", "RawEqual", "RawEscape", "RawExclamation", "RawGreater", "RawLeftBrace", 
"RawLeftBracket", "RawLeftParenthesis", "RawLess", "RawNumberSign", "RawPercent", "RawPlus", "RawQuestion", "RawQuote", 
"RawReturn", "RawRightBrace", "RawRightBracket", "RawRightParenthesis", "RawSemicolon", "RawSlash", "RawSpace", 
"RawStar", "RawTab", "RawTilde", "RawUnderscore", "RawVerticalBar", "RawWedge", "RegisteredTrademark", "ReturnIndicator"
, "ReturnKey", "ReverseDoublePrime", "ReverseElement", "ReverseEquilibrium", "ReversePrime", "ReverseUpEquilibrium", 
"Rho", "RightAngle", "RightAngleBracket", "RightArrow", "RightArrowBar", "RightArrowLeftArrow", "RightAssociation", 
"RightBracketingBar", "RightCeiling", "RightDoubleBracket", "RightDoubleBracketingBar", "RightDownTeeVector", 
"RightDownVector", "RightDownVectorBar", "RightFloor", "RightGuillemet", "RightModified", "RightPointer", 
"RightSkeleton", "RightTee", "RightTeeArrow", "RightTeeVector", "RightTriangle", "RightTriangleBar", 
"RightTriangleEqual", "RightUpDownVector", "RightUpTeeVector", "RightUpVector", "RightUpVectorBar", "RightVector", 
"RightVectorBar", "RoundImplies", "RoundSpaceIndicator", "Rule", "RuleDelayed", "Rupee", "SHacek", "SZ", "SadSmiley", 
"SagittariusSign", "Sampi", "Saturn", "ScorpioSign", "ScriptA", "ScriptB", "ScriptC", "ScriptCapitalA", "ScriptCapitalB"
, "ScriptCapitalC", "ScriptCapitalD", "ScriptCapitalE", "ScriptCapitalF", "ScriptCapitalG", "ScriptCapitalH", 
"ScriptCapitalI", "ScriptCapitalJ", "ScriptCapitalK", "ScriptCapitalL", "ScriptCapitalM", "ScriptCapitalN", 
"ScriptCapitalO", "ScriptCapitalP", "ScriptCapitalQ", "ScriptCapitalR", "ScriptCapitalS", "ScriptCapitalT", 
"ScriptCapitalU", "ScriptCapitalV", "ScriptCapitalW", "ScriptCapitalX", "ScriptCapitalY", "ScriptCapitalZ", "ScriptD", 
"ScriptDotlessI", "ScriptDotlessJ", "ScriptE", "ScriptEight", "ScriptF", "ScriptFive", "ScriptFour", "ScriptG", 
"ScriptH", "ScriptI", "ScriptJ", "ScriptK", "ScriptL", "ScriptM", "ScriptN", "ScriptNine", "ScriptO", "ScriptOne", 
"ScriptP", "ScriptQ", "ScriptR", "ScriptS", "ScriptSeven", "ScriptSix", "ScriptT", "ScriptThree", "ScriptTwo", "ScriptU"
, "ScriptV", "ScriptW", "ScriptX", "ScriptY", "ScriptZ", "ScriptZero", "Section", "SelectionPlaceholder", "Shah", 
"Sharp", "ShiftKey", "ShortDownArrow", "ShortLeftArrow", "ShortRightArrow", "ShortUpArrow", "Sigma", "SixPointedStar", 
"SkeletonIndicator", "SmallCircle", "SpaceIndicator", "SpaceKey", "SpadeSuit", "SpanFromAbove", "SpanFromBoth", 
"SpanFromLeft", "SphericalAngle", "Spooky", "Sqrt", "Square", "SquareIntersection", "SquareSubset", "SquareSubsetEqual",
 "SquareSuperset", "SquareSupersetEqual", "SquareUnion", "Star", "StepperDown", "StepperLeft", "StepperRight", 
"StepperUp", "Sterling", "Stigma", "Subset", "SubsetEqual", "Succeeds", "SucceedsEqual", "SucceedsSlantEqual", 
"SucceedsTilde", "SuchThat", "Sum", "Sun", "Superset", "SupersetEqual", "SystemEnterKey", "SystemsModelDelay", "THacek",
 "TabKey", "Tau", "TaurusSign", "TensorProduct", "TensorWedge", "Therefore", "Theta", "ThickSpace", "ThinSpace", "Thorn"
, "Tilde", "TildeEqual", "TildeFullEqual", "TildeTilde", "Times", "Trademark", "Transpose", "TripleDot", "TwoWayRule", 
"UAcute", "UDoubleAcute", "UDoubleDot", "UGrave", "UHat", "URing", "UnderBrace", "UnderBracket", "UnderParenthesis", 
"UndirectedEdge", "Union", "UnionPlus", "UnknownGlyph", "UpArrow", "UpArrowBar", "UpArrowDownArrow", "UpDownArrow", 
"UpEquilibrium", "UpPointer", "UpTee", "UpTeeArrow", "UpperLeftArrow", "UpperRightArrow", "Upsilon", "Uranus", 
"VectorGreater", "VectorGreaterEqual", "VectorLess", "VectorLessEqual", "Vee", "Venus", "VerticalBar", 
"VerticalEllipsis", "VerticalLine", "VerticalSeparator", "VerticalTilde", "VeryThinSpace", "Villa", "VirgoSign", 
"WarningSign", "WatchIcon", "Wedge", "WeierstrassP", "WhiteBishop", "WhiteKing", "WhiteKnight", "WhitePawn", 
"WhiteQueen", "WhiteRook", "Wolf", "WolframAlphaPrompt", "WolframLanguageLogo", "WolframLanguageLogoCircle", "Xi", 
"Xnor", "Xor", "YAcute", "YDoubleDot", "Yen", "ZHacek", "Zeta", 
];

//
//
//
pub const LONGNAME_TO_CODE_POINT_MAP__POINTS: [CodePoint; LONGNAMES_COUNT] = [
CODEPOINT_LONGNAME_AACUTE, CODEPOINT_LONGNAME_ABAR, CODEPOINT_LONGNAME_ACUP, CODEPOINT_LONGNAME_ADOUBLEDOT, 
CODEPOINT_LONGNAME_AE, CODEPOINT_LONGNAME_AGRAVE, CODEPOINT_LONGNAME_AHAT, CODEPOINT_LONGNAME_ARING, 
CODEPOINT_LONGNAME_ATILDE, CODEPOINT_LONGNAME_AKUZ, CODEPOINT_LONGNAME_ALEPH, CODEPOINT_LONGNAME_ALIASDELIMITER, 
CODEPOINT_LONGNAME_ALIASINDICATOR, CODEPOINT_LONGNAME_ALIGNMENTMARKER, CODEPOINT_LONGNAME_ALPHA, 
CODEPOINT_LONGNAME_ALTKEY, CODEPOINT_LONGNAME_AND, CODEPOINT_LONGNAME_ANDY, CODEPOINT_LONGNAME_ANGLE, 
CODEPOINT_LONGNAME_ANGSTROM, CODEPOINT_LONGNAME_APPLICATION, CODEPOINT_LONGNAME_AQUARIUSSIGN, 
CODEPOINT_LONGNAME_ARIESSIGN, CODEPOINT_LONGNAME_ASCENDINGELLIPSIS, CODEPOINT_LONGNAME_AUTOLEFTMATCH, 
CODEPOINT_LONGNAME_AUTOOPERAND, CODEPOINT_LONGNAME_AUTOPLACEHOLDER, CODEPOINT_LONGNAME_AUTORIGHTMATCH, 
CODEPOINT_LONGNAME_AUTOSPACE, CODEPOINT_LONGNAME_BACKSLASH, CODEPOINT_LONGNAME_BEAMEDEIGHTHNOTE, 
CODEPOINT_LONGNAME_BEAMEDSIXTEENTHNOTE, CODEPOINT_LONGNAME_BECAUSE, CODEPOINT_LONGNAME_BET, CODEPOINT_LONGNAME_BETA, 
CODEPOINT_LONGNAME_BLACKBISHOP, CODEPOINT_LONGNAME_BLACKKING, CODEPOINT_LONGNAME_BLACKKNIGHT, 
CODEPOINT_LONGNAME_BLACKPAWN, CODEPOINT_LONGNAME_BLACKQUEEN, CODEPOINT_LONGNAME_BLACKROOK, CODEPOINT_LONGNAME_BREVE, 
CODEPOINT_LONGNAME_BULLET, CODEPOINT_LONGNAME_CACUTE, CODEPOINT_LONGNAME_CCEDILLA, CODEPOINT_LONGNAME_CHACEK, 
CODEPOINT_LONGNAME_COMPATIBILITYKANJISPACE, CODEPOINT_LONGNAME_COMPATIBILITYNOBREAK, CODEPOINT_LONGNAME_CANCERSIGN, 
CODEPOINT_LONGNAME_CAP, CODEPOINT_LONGNAME_CAPITALAACUTE, CODEPOINT_LONGNAME_CAPITALABAR, CODEPOINT_LONGNAME_CAPITALACUP
, CODEPOINT_LONGNAME_CAPITALADOUBLEDOT, CODEPOINT_LONGNAME_CAPITALAE, CODEPOINT_LONGNAME_CAPITALAGRAVE, 
CODEPOINT_LONGNAME_CAPITALAHAT, CODEPOINT_LONGNAME_CAPITALARING, CODEPOINT_LONGNAME_CAPITALATILDE, 
CODEPOINT_LONGNAME_CAPITALALPHA, CODEPOINT_LONGNAME_CAPITALBETA, CODEPOINT_LONGNAME_CAPITALCACUTE, 
CODEPOINT_LONGNAME_CAPITALCCEDILLA, CODEPOINT_LONGNAME_CAPITALCHACEK, CODEPOINT_LONGNAME_CAPITALCHI, 
CODEPOINT_LONGNAME_CAPITALDHACEK, CODEPOINT_LONGNAME_CAPITALDELTA, CODEPOINT_LONGNAME_CAPITALDIFFERENTIALD, 
CODEPOINT_LONGNAME_CAPITALDIGAMMA, CODEPOINT_LONGNAME_CAPITALEACUTE, CODEPOINT_LONGNAME_CAPITALEBAR, 
CODEPOINT_LONGNAME_CAPITALECUP, CODEPOINT_LONGNAME_CAPITALEDOUBLEDOT, CODEPOINT_LONGNAME_CAPITALEGRAVE, 
CODEPOINT_LONGNAME_CAPITALEHACEK, CODEPOINT_LONGNAME_CAPITALEHAT, CODEPOINT_LONGNAME_CAPITALEPSILON, 
CODEPOINT_LONGNAME_CAPITALETA, CODEPOINT_LONGNAME_CAPITALETH, CODEPOINT_LONGNAME_CAPITALGAMMA, 
CODEPOINT_LONGNAME_CAPITALIACUTE, CODEPOINT_LONGNAME_CAPITALICUP, CODEPOINT_LONGNAME_CAPITALIDOUBLEDOT, 
CODEPOINT_LONGNAME_CAPITALIGRAVE, CODEPOINT_LONGNAME_CAPITALIHAT, CODEPOINT_LONGNAME_CAPITALIOTA, 
CODEPOINT_LONGNAME_CAPITALKAPPA, CODEPOINT_LONGNAME_CAPITALKOPPA, CODEPOINT_LONGNAME_CAPITALLSLASH, 
CODEPOINT_LONGNAME_CAPITALLAMBDA, CODEPOINT_LONGNAME_CAPITALMU, CODEPOINT_LONGNAME_CAPITALNHACEK, 
CODEPOINT_LONGNAME_CAPITALNTILDE, CODEPOINT_LONGNAME_CAPITALNU, CODEPOINT_LONGNAME_CAPITALOACUTE, 
CODEPOINT_LONGNAME_CAPITALODOUBLEACUTE, CODEPOINT_LONGNAME_CAPITALODOUBLEDOT, CODEPOINT_LONGNAME_CAPITALOE, 
CODEPOINT_LONGNAME_CAPITALOGRAVE, CODEPOINT_LONGNAME_CAPITALOHAT, CODEPOINT_LONGNAME_CAPITALOSLASH, 
CODEPOINT_LONGNAME_CAPITALOTILDE, CODEPOINT_LONGNAME_CAPITALOMEGA, CODEPOINT_LONGNAME_CAPITALOMICRON, 
CODEPOINT_LONGNAME_CAPITALPHI, CODEPOINT_LONGNAME_CAPITALPI, CODEPOINT_LONGNAME_CAPITALPSI, 
CODEPOINT_LONGNAME_CAPITALRHACEK, CODEPOINT_LONGNAME_CAPITALRHO, CODEPOINT_LONGNAME_CAPITALSHACEK, 
CODEPOINT_LONGNAME_CAPITALSAMPI, CODEPOINT_LONGNAME_CAPITALSIGMA, CODEPOINT_LONGNAME_CAPITALSTIGMA, 
CODEPOINT_LONGNAME_CAPITALTHACEK, CODEPOINT_LONGNAME_CAPITALTAU, CODEPOINT_LONGNAME_CAPITALTHETA, 
CODEPOINT_LONGNAME_CAPITALTHORN, CODEPOINT_LONGNAME_CAPITALUACUTE, CODEPOINT_LONGNAME_CAPITALUDOUBLEACUTE, 
CODEPOINT_LONGNAME_CAPITALUDOUBLEDOT, CODEPOINT_LONGNAME_CAPITALUGRAVE, CODEPOINT_LONGNAME_CAPITALUHAT, 
CODEPOINT_LONGNAME_CAPITALURING, CODEPOINT_LONGNAME_CAPITALUPSILON, CODEPOINT_LONGNAME_CAPITALXI, 
CODEPOINT_LONGNAME_CAPITALYACUTE, CODEPOINT_LONGNAME_CAPITALZHACEK, CODEPOINT_LONGNAME_CAPITALZETA, 
CODEPOINT_LONGNAME_CAPRICORNSIGN, CODEPOINT_LONGNAME_CEDILLA, CODEPOINT_LONGNAME_CENT, CODEPOINT_LONGNAME_CENTERDOT, 
CODEPOINT_LONGNAME_CENTERELLIPSIS, CODEPOINT_LONGNAME_CHECKEDBOX, CODEPOINT_LONGNAME_CHECKMARK, 
CODEPOINT_LONGNAME_CHECKMARKEDBOX, CODEPOINT_LONGNAME_CHI, CODEPOINT_LONGNAME_CIRCLEDOT, CODEPOINT_LONGNAME_CIRCLEMINUS,
 CODEPOINT_LONGNAME_CIRCLEPLUS, CODEPOINT_LONGNAME_CIRCLETIMES, CODEPOINT_LONGNAME_CLOCKWISECONTOURINTEGRAL, 
CODEPOINT_LONGNAME_CLOSECURLYDOUBLEQUOTE, CODEPOINT_LONGNAME_CLOSECURLYQUOTE, CODEPOINT_LONGNAME_CLOVERLEAF, 
CODEPOINT_LONGNAME_CLUBSUIT, CODEPOINT_LONGNAME_COLON, CODEPOINT_LONGNAME_COMMANDKEY, CODEPOINT_LONGNAME_CONDITIONED, 
CODEPOINT_LONGNAME_CONGRUENT, CODEPOINT_LONGNAME_CONJUGATE, CODEPOINT_LONGNAME_CONJUGATETRANSPOSE, 
CODEPOINT_LONGNAME_CONSTANTC, CODEPOINT_LONGNAME_CONTINUATION, CODEPOINT_LONGNAME_CONTINUEDFRACTIONK, 
CODEPOINT_LONGNAME_CONTOURINTEGRAL, CODEPOINT_LONGNAME_CONTROLKEY, CODEPOINT_LONGNAME_COPRODUCT, 
CODEPOINT_LONGNAME_COPYRIGHT, CODEPOINT_LONGNAME_COUNTERCLOCKWISECONTOURINTEGRAL, CODEPOINT_LONGNAME_CROSS, 
CODEPOINT_LONGNAME_CUBEROOT, CODEPOINT_LONGNAME_CUP, CODEPOINT_LONGNAME_CUPCAP, CODEPOINT_LONGNAME_CURLYCAPITALUPSILON, 
CODEPOINT_LONGNAME_CURLYEPSILON, CODEPOINT_LONGNAME_CURLYKAPPA, CODEPOINT_LONGNAME_CURLYPHI, CODEPOINT_LONGNAME_CURLYPI,
 CODEPOINT_LONGNAME_CURLYRHO, CODEPOINT_LONGNAME_CURLYTHETA, CODEPOINT_LONGNAME_CURRENCY, CODEPOINT_LONGNAME_DHACEK, 
CODEPOINT_LONGNAME_DAGGER, CODEPOINT_LONGNAME_DALET, CODEPOINT_LONGNAME_DASH, CODEPOINT_LONGNAME_DEGREE, 
CODEPOINT_LONGNAME_DEL, CODEPOINT_LONGNAME_DELETEKEY, CODEPOINT_LONGNAME_DELTA, CODEPOINT_LONGNAME_DESCENDINGELLIPSIS, 
CODEPOINT_LONGNAME_DIAMETER, CODEPOINT_LONGNAME_DIAMOND, CODEPOINT_LONGNAME_DIAMONDSUIT, 
CODEPOINT_LONGNAME_DIFFERENCEDELTA, CODEPOINT_LONGNAME_DIFFERENTIALD, CODEPOINT_LONGNAME_DIGAMMA, 
CODEPOINT_LONGNAME_DIRECTEDEDGE, CODEPOINT_LONGNAME_DISCRETERATIO, CODEPOINT_LONGNAME_DISCRETESHIFT, 
CODEPOINT_LONGNAME_DISCRETIONARYHYPHEN, CODEPOINT_LONGNAME_DISCRETIONARYLINESEPARATOR, 
CODEPOINT_LONGNAME_DISCRETIONARYPAGEBREAKABOVE, CODEPOINT_LONGNAME_DISCRETIONARYPAGEBREAKBELOW, 
CODEPOINT_LONGNAME_DISCRETIONARYPARAGRAPHSEPARATOR, CODEPOINT_LONGNAME_DISTRIBUTED, CODEPOINT_LONGNAME_DIVIDE, 
CODEPOINT_LONGNAME_DIVIDES, CODEPOINT_LONGNAME_DIVISIONSLASH, CODEPOINT_LONGNAME_DOTEQUAL, CODEPOINT_LONGNAME_DOTLESSI, 
CODEPOINT_LONGNAME_DOTLESSJ, CODEPOINT_LONGNAME_DOTTEDSQUARE, CODEPOINT_LONGNAME_DOUBLECONTOURINTEGRAL, 
CODEPOINT_LONGNAME_DOUBLEDAGGER, CODEPOINT_LONGNAME_DOUBLEDOT, CODEPOINT_LONGNAME_DOUBLEDOWNARROW, 
CODEPOINT_LONGNAME_DOUBLELEFTARROW, CODEPOINT_LONGNAME_DOUBLELEFTRIGHTARROW, CODEPOINT_LONGNAME_DOUBLELEFTTEE, 
CODEPOINT_LONGNAME_DOUBLELONGLEFTARROW, CODEPOINT_LONGNAME_DOUBLELONGLEFTRIGHTARROW, 
CODEPOINT_LONGNAME_DOUBLELONGRIGHTARROW, CODEPOINT_LONGNAME_DOUBLEPRIME, CODEPOINT_LONGNAME_DOUBLERIGHTARROW, 
CODEPOINT_LONGNAME_DOUBLERIGHTTEE, CODEPOINT_LONGNAME_DOUBLESTRUCKA, CODEPOINT_LONGNAME_DOUBLESTRUCKB, 
CODEPOINT_LONGNAME_DOUBLESTRUCKC, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALA, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALB, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALC, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALD, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALE, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALF, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALG, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALH, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALI, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALJ, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALK, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALL, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALM, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALN, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALO, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALP, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALQ, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALR, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALS, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALT, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALU, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALV, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALW, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALX, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALY, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALZ, CODEPOINT_LONGNAME_DOUBLESTRUCKD, 
CODEPOINT_LONGNAME_DOUBLESTRUCKE, CODEPOINT_LONGNAME_DOUBLESTRUCKEIGHT, CODEPOINT_LONGNAME_DOUBLESTRUCKF, 
CODEPOINT_LONGNAME_DOUBLESTRUCKFIVE, CODEPOINT_LONGNAME_DOUBLESTRUCKFOUR, CODEPOINT_LONGNAME_DOUBLESTRUCKG, 
CODEPOINT_LONGNAME_DOUBLESTRUCKH, CODEPOINT_LONGNAME_DOUBLESTRUCKI, CODEPOINT_LONGNAME_DOUBLESTRUCKJ, 
CODEPOINT_LONGNAME_DOUBLESTRUCKK, CODEPOINT_LONGNAME_DOUBLESTRUCKL, CODEPOINT_LONGNAME_DOUBLESTRUCKM, 
CODEPOINT_LONGNAME_DOUBLESTRUCKN, CODEPOINT_LONGNAME_DOUBLESTRUCKNINE, CODEPOINT_LONGNAME_DOUBLESTRUCKO, 
CODEPOINT_LONGNAME_DOUBLESTRUCKONE, CODEPOINT_LONGNAME_DOUBLESTRUCKP, CODEPOINT_LONGNAME_DOUBLESTRUCKQ, 
CODEPOINT_LONGNAME_DOUBLESTRUCKR, CODEPOINT_LONGNAME_DOUBLESTRUCKS, CODEPOINT_LONGNAME_DOUBLESTRUCKSEVEN, 
CODEPOINT_LONGNAME_DOUBLESTRUCKSIX, CODEPOINT_LONGNAME_DOUBLESTRUCKT, CODEPOINT_LONGNAME_DOUBLESTRUCKTHREE, 
CODEPOINT_LONGNAME_DOUBLESTRUCKTWO, CODEPOINT_LONGNAME_DOUBLESTRUCKU, CODEPOINT_LONGNAME_DOUBLESTRUCKV, 
CODEPOINT_LONGNAME_DOUBLESTRUCKW, CODEPOINT_LONGNAME_DOUBLESTRUCKX, CODEPOINT_LONGNAME_DOUBLESTRUCKY, 
CODEPOINT_LONGNAME_DOUBLESTRUCKZ, CODEPOINT_LONGNAME_DOUBLESTRUCKZERO, CODEPOINT_LONGNAME_DOUBLEUPARROW, 
CODEPOINT_LONGNAME_DOUBLEUPDOWNARROW, CODEPOINT_LONGNAME_DOUBLEVERTICALBAR, CODEPOINT_LONGNAME_DOUBLEDGAMMA, 
CODEPOINT_LONGNAME_DOUBLEDPI, CODEPOINT_LONGNAME_DOWNARROW, CODEPOINT_LONGNAME_DOWNARROWBAR, 
CODEPOINT_LONGNAME_DOWNARROWUPARROW, CODEPOINT_LONGNAME_DOWNBREVE, CODEPOINT_LONGNAME_DOWNEXCLAMATION, 
CODEPOINT_LONGNAME_DOWNLEFTRIGHTVECTOR, CODEPOINT_LONGNAME_DOWNLEFTTEEVECTOR, CODEPOINT_LONGNAME_DOWNLEFTVECTOR, 
CODEPOINT_LONGNAME_DOWNLEFTVECTORBAR, CODEPOINT_LONGNAME_DOWNPOINTER, CODEPOINT_LONGNAME_DOWNQUESTION, 
CODEPOINT_LONGNAME_DOWNRIGHTTEEVECTOR, CODEPOINT_LONGNAME_DOWNRIGHTVECTOR, CODEPOINT_LONGNAME_DOWNRIGHTVECTORBAR, 
CODEPOINT_LONGNAME_DOWNTEE, CODEPOINT_LONGNAME_DOWNTEEARROW, CODEPOINT_LONGNAME_EACUTE, CODEPOINT_LONGNAME_EBAR, 
CODEPOINT_LONGNAME_ECUP, CODEPOINT_LONGNAME_EDOUBLEDOT, CODEPOINT_LONGNAME_EGRAVE, CODEPOINT_LONGNAME_EHACEK, 
CODEPOINT_LONGNAME_EHAT, CODEPOINT_LONGNAME_EARTH, CODEPOINT_LONGNAME_EIGHTHNOTE, CODEPOINT_LONGNAME_ELEMENT, 
CODEPOINT_LONGNAME_ELLIPSIS, CODEPOINT_LONGNAME_EMPTYCIRCLE, CODEPOINT_LONGNAME_EMPTYDIAMOND, 
CODEPOINT_LONGNAME_EMPTYDOWNTRIANGLE, CODEPOINT_LONGNAME_EMPTYRECTANGLE, CODEPOINT_LONGNAME_EMPTYSET, 
CODEPOINT_LONGNAME_EMPTYSMALLCIRCLE, CODEPOINT_LONGNAME_EMPTYSMALLSQUARE, CODEPOINT_LONGNAME_EMPTYSQUARE, 
CODEPOINT_LONGNAME_EMPTYUPTRIANGLE, CODEPOINT_LONGNAME_EMPTYVERYSMALLSQUARE, CODEPOINT_LONGNAME_ENTERKEY, 
CODEPOINT_LONGNAME_ENTITYEND, CODEPOINT_LONGNAME_ENTITYSTART, CODEPOINT_LONGNAME_EPSILON, CODEPOINT_LONGNAME_EQUAL, 
CODEPOINT_LONGNAME_EQUALTILDE, CODEPOINT_LONGNAME_EQUILIBRIUM, CODEPOINT_LONGNAME_EQUIVALENT, 
CODEPOINT_LONGNAME_ERRORINDICATOR, CODEPOINT_LONGNAME_ESCAPEKEY, CODEPOINT_LONGNAME_ETA, CODEPOINT_LONGNAME_ETH, 
CODEPOINT_LONGNAME_EURO, CODEPOINT_LONGNAME_EXISTS, CODEPOINT_LONGNAME_EXPECTATIONE, CODEPOINT_LONGNAME_EXPONENTIALE, 
CODEPOINT_LONGNAME_FILIGATURE, CODEPOINT_LONGNAME_FILLEDCIRCLE, CODEPOINT_LONGNAME_FILLEDDIAMOND, 
CODEPOINT_LONGNAME_FILLEDDOWNTRIANGLE, CODEPOINT_LONGNAME_FILLEDLEFTTRIANGLE, CODEPOINT_LONGNAME_FILLEDRECTANGLE, 
CODEPOINT_LONGNAME_FILLEDRIGHTTRIANGLE, CODEPOINT_LONGNAME_FILLEDSMALLCIRCLE, CODEPOINT_LONGNAME_FILLEDSMALLSQUARE, 
CODEPOINT_LONGNAME_FILLEDSQUARE, CODEPOINT_LONGNAME_FILLEDUPTRIANGLE, CODEPOINT_LONGNAME_FILLEDVERYSMALLSQUARE, 
CODEPOINT_LONGNAME_FINALSIGMA, CODEPOINT_LONGNAME_FIRSTPAGE, CODEPOINT_LONGNAME_FIVEPOINTEDSTAR, 
CODEPOINT_LONGNAME_FLLIGATURE, CODEPOINT_LONGNAME_FLAT, CODEPOINT_LONGNAME_FLORIN, CODEPOINT_LONGNAME_FORALL, 
CODEPOINT_LONGNAME_FORMALA, CODEPOINT_LONGNAME_FORMALALPHA, CODEPOINT_LONGNAME_FORMALB, CODEPOINT_LONGNAME_FORMALBETA, 
CODEPOINT_LONGNAME_FORMALC, CODEPOINT_LONGNAME_FORMALCAPITALA, CODEPOINT_LONGNAME_FORMALCAPITALALPHA, 
CODEPOINT_LONGNAME_FORMALCAPITALB, CODEPOINT_LONGNAME_FORMALCAPITALBETA, CODEPOINT_LONGNAME_FORMALCAPITALC, 
CODEPOINT_LONGNAME_FORMALCAPITALCHI, CODEPOINT_LONGNAME_FORMALCAPITALD, CODEPOINT_LONGNAME_FORMALCAPITALDELTA, 
CODEPOINT_LONGNAME_FORMALCAPITALDIGAMMA, CODEPOINT_LONGNAME_FORMALCAPITALE, CODEPOINT_LONGNAME_FORMALCAPITALEPSILON, 
CODEPOINT_LONGNAME_FORMALCAPITALETA, CODEPOINT_LONGNAME_FORMALCAPITALF, CODEPOINT_LONGNAME_FORMALCAPITALG, 
CODEPOINT_LONGNAME_FORMALCAPITALGAMMA, CODEPOINT_LONGNAME_FORMALCAPITALH, CODEPOINT_LONGNAME_FORMALCAPITALI, 
CODEPOINT_LONGNAME_FORMALCAPITALIOTA, CODEPOINT_LONGNAME_FORMALCAPITALJ, CODEPOINT_LONGNAME_FORMALCAPITALK, 
CODEPOINT_LONGNAME_FORMALCAPITALKAPPA, CODEPOINT_LONGNAME_FORMALCAPITALKOPPA, CODEPOINT_LONGNAME_FORMALCAPITALL, 
CODEPOINT_LONGNAME_FORMALCAPITALLAMBDA, CODEPOINT_LONGNAME_FORMALCAPITALM, CODEPOINT_LONGNAME_FORMALCAPITALMU, 
CODEPOINT_LONGNAME_FORMALCAPITALN, CODEPOINT_LONGNAME_FORMALCAPITALNU, CODEPOINT_LONGNAME_FORMALCAPITALO, 
CODEPOINT_LONGNAME_FORMALCAPITALOMEGA, CODEPOINT_LONGNAME_FORMALCAPITALOMICRON, CODEPOINT_LONGNAME_FORMALCAPITALP, 
CODEPOINT_LONGNAME_FORMALCAPITALPHI, CODEPOINT_LONGNAME_FORMALCAPITALPI, CODEPOINT_LONGNAME_FORMALCAPITALPSI, 
CODEPOINT_LONGNAME_FORMALCAPITALQ, CODEPOINT_LONGNAME_FORMALCAPITALR, CODEPOINT_LONGNAME_FORMALCAPITALRHO, 
CODEPOINT_LONGNAME_FORMALCAPITALS, CODEPOINT_LONGNAME_FORMALCAPITALSAMPI, CODEPOINT_LONGNAME_FORMALCAPITALSIGMA, 
CODEPOINT_LONGNAME_FORMALCAPITALSTIGMA, CODEPOINT_LONGNAME_FORMALCAPITALT, CODEPOINT_LONGNAME_FORMALCAPITALTAU, 
CODEPOINT_LONGNAME_FORMALCAPITALTHETA, CODEPOINT_LONGNAME_FORMALCAPITALU, CODEPOINT_LONGNAME_FORMALCAPITALUPSILON, 
CODEPOINT_LONGNAME_FORMALCAPITALV, CODEPOINT_LONGNAME_FORMALCAPITALW, CODEPOINT_LONGNAME_FORMALCAPITALX, 
CODEPOINT_LONGNAME_FORMALCAPITALXI, CODEPOINT_LONGNAME_FORMALCAPITALY, CODEPOINT_LONGNAME_FORMALCAPITALZ, 
CODEPOINT_LONGNAME_FORMALCAPITALZETA, CODEPOINT_LONGNAME_FORMALCHI, CODEPOINT_LONGNAME_FORMALCURLYCAPITALUPSILON, 
CODEPOINT_LONGNAME_FORMALCURLYEPSILON, CODEPOINT_LONGNAME_FORMALCURLYKAPPA, CODEPOINT_LONGNAME_FORMALCURLYPHI, 
CODEPOINT_LONGNAME_FORMALCURLYPI, CODEPOINT_LONGNAME_FORMALCURLYRHO, CODEPOINT_LONGNAME_FORMALCURLYTHETA, 
CODEPOINT_LONGNAME_FORMALD, CODEPOINT_LONGNAME_FORMALDELTA, CODEPOINT_LONGNAME_FORMALDIGAMMA, CODEPOINT_LONGNAME_FORMALE
, CODEPOINT_LONGNAME_FORMALEPSILON, CODEPOINT_LONGNAME_FORMALETA, CODEPOINT_LONGNAME_FORMALF, 
CODEPOINT_LONGNAME_FORMALFINALSIGMA, CODEPOINT_LONGNAME_FORMALG, CODEPOINT_LONGNAME_FORMALGAMMA, 
CODEPOINT_LONGNAME_FORMALH, CODEPOINT_LONGNAME_FORMALI, CODEPOINT_LONGNAME_FORMALIOTA, CODEPOINT_LONGNAME_FORMALJ, 
CODEPOINT_LONGNAME_FORMALK, CODEPOINT_LONGNAME_FORMALKAPPA, CODEPOINT_LONGNAME_FORMALKOPPA, CODEPOINT_LONGNAME_FORMALL, 
CODEPOINT_LONGNAME_FORMALLAMBDA, CODEPOINT_LONGNAME_FORMALM, CODEPOINT_LONGNAME_FORMALMU, CODEPOINT_LONGNAME_FORMALN, 
CODEPOINT_LONGNAME_FORMALNU, CODEPOINT_LONGNAME_FORMALO, CODEPOINT_LONGNAME_FORMALOMEGA, 
CODEPOINT_LONGNAME_FORMALOMICRON, CODEPOINT_LONGNAME_FORMALP, CODEPOINT_LONGNAME_FORMALPHI, CODEPOINT_LONGNAME_FORMALPI,
 CODEPOINT_LONGNAME_FORMALPSI, CODEPOINT_LONGNAME_FORMALQ, CODEPOINT_LONGNAME_FORMALR, CODEPOINT_LONGNAME_FORMALRHO, 
CODEPOINT_LONGNAME_FORMALS, CODEPOINT_LONGNAME_FORMALSAMPI, CODEPOINT_LONGNAME_FORMALSCRIPTA, 
CODEPOINT_LONGNAME_FORMALSCRIPTB, CODEPOINT_LONGNAME_FORMALSCRIPTC, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALA, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALB, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALC, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALD, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALE, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALF, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALG, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALH, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALI, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALJ, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALK, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALL, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALM, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALN, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALO, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALP, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALQ, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALR, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALS, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALT, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALU, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALV, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALW, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALX, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALY, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALZ, CODEPOINT_LONGNAME_FORMALSCRIPTD, CODEPOINT_LONGNAME_FORMALSCRIPTE, 
CODEPOINT_LONGNAME_FORMALSCRIPTF, CODEPOINT_LONGNAME_FORMALSCRIPTG, CODEPOINT_LONGNAME_FORMALSCRIPTH, 
CODEPOINT_LONGNAME_FORMALSCRIPTI, CODEPOINT_LONGNAME_FORMALSCRIPTJ, CODEPOINT_LONGNAME_FORMALSCRIPTK, 
CODEPOINT_LONGNAME_FORMALSCRIPTL, CODEPOINT_LONGNAME_FORMALSCRIPTM, CODEPOINT_LONGNAME_FORMALSCRIPTN, 
CODEPOINT_LONGNAME_FORMALSCRIPTO, CODEPOINT_LONGNAME_FORMALSCRIPTP, CODEPOINT_LONGNAME_FORMALSCRIPTQ, 
CODEPOINT_LONGNAME_FORMALSCRIPTR, CODEPOINT_LONGNAME_FORMALSCRIPTS, CODEPOINT_LONGNAME_FORMALSCRIPTT, 
CODEPOINT_LONGNAME_FORMALSCRIPTU, CODEPOINT_LONGNAME_FORMALSCRIPTV, CODEPOINT_LONGNAME_FORMALSCRIPTW, 
CODEPOINT_LONGNAME_FORMALSCRIPTX, CODEPOINT_LONGNAME_FORMALSCRIPTY, CODEPOINT_LONGNAME_FORMALSCRIPTZ, 
CODEPOINT_LONGNAME_FORMALSIGMA, CODEPOINT_LONGNAME_FORMALSTIGMA, CODEPOINT_LONGNAME_FORMALT, 
CODEPOINT_LONGNAME_FORMALTAU, CODEPOINT_LONGNAME_FORMALTHETA, CODEPOINT_LONGNAME_FORMALU, 
CODEPOINT_LONGNAME_FORMALUPSILON, CODEPOINT_LONGNAME_FORMALV, CODEPOINT_LONGNAME_FORMALW, CODEPOINT_LONGNAME_FORMALX, 
CODEPOINT_LONGNAME_FORMALXI, CODEPOINT_LONGNAME_FORMALY, CODEPOINT_LONGNAME_FORMALZ, CODEPOINT_LONGNAME_FORMALZETA, 
CODEPOINT_LONGNAME_FREAKEDSMILEY, CODEPOINT_LONGNAME_FREEFORMPROMPT, CODEPOINT_LONGNAME_FUNCTION, 
CODEPOINT_LONGNAME_GAMMA, CODEPOINT_LONGNAME_GEMINISIGN, CODEPOINT_LONGNAME_GIMEL, CODEPOINT_LONGNAME_GOTHICA, 
CODEPOINT_LONGNAME_GOTHICB, CODEPOINT_LONGNAME_GOTHICC, CODEPOINT_LONGNAME_GOTHICCAPITALA, 
CODEPOINT_LONGNAME_GOTHICCAPITALB, CODEPOINT_LONGNAME_GOTHICCAPITALC, CODEPOINT_LONGNAME_GOTHICCAPITALD, 
CODEPOINT_LONGNAME_GOTHICCAPITALE, CODEPOINT_LONGNAME_GOTHICCAPITALF, CODEPOINT_LONGNAME_GOTHICCAPITALG, 
CODEPOINT_LONGNAME_GOTHICCAPITALH, CODEPOINT_LONGNAME_GOTHICCAPITALI, CODEPOINT_LONGNAME_GOTHICCAPITALJ, 
CODEPOINT_LONGNAME_GOTHICCAPITALK, CODEPOINT_LONGNAME_GOTHICCAPITALL, CODEPOINT_LONGNAME_GOTHICCAPITALM, 
CODEPOINT_LONGNAME_GOTHICCAPITALN, CODEPOINT_LONGNAME_GOTHICCAPITALO, CODEPOINT_LONGNAME_GOTHICCAPITALP, 
CODEPOINT_LONGNAME_GOTHICCAPITALQ, CODEPOINT_LONGNAME_GOTHICCAPITALR, CODEPOINT_LONGNAME_GOTHICCAPITALS, 
CODEPOINT_LONGNAME_GOTHICCAPITALT, CODEPOINT_LONGNAME_GOTHICCAPITALU, CODEPOINT_LONGNAME_GOTHICCAPITALV, 
CODEPOINT_LONGNAME_GOTHICCAPITALW, CODEPOINT_LONGNAME_GOTHICCAPITALX, CODEPOINT_LONGNAME_GOTHICCAPITALY, 
CODEPOINT_LONGNAME_GOTHICCAPITALZ, CODEPOINT_LONGNAME_GOTHICD, CODEPOINT_LONGNAME_GOTHICE, 
CODEPOINT_LONGNAME_GOTHICEIGHT, CODEPOINT_LONGNAME_GOTHICF, CODEPOINT_LONGNAME_GOTHICFIVE, CODEPOINT_LONGNAME_GOTHICFOUR
, CODEPOINT_LONGNAME_GOTHICG, CODEPOINT_LONGNAME_GOTHICH, CODEPOINT_LONGNAME_GOTHICI, CODEPOINT_LONGNAME_GOTHICJ, 
CODEPOINT_LONGNAME_GOTHICK, CODEPOINT_LONGNAME_GOTHICL, CODEPOINT_LONGNAME_GOTHICM, CODEPOINT_LONGNAME_GOTHICN, 
CODEPOINT_LONGNAME_GOTHICNINE, CODEPOINT_LONGNAME_GOTHICO, CODEPOINT_LONGNAME_GOTHICONE, CODEPOINT_LONGNAME_GOTHICP, 
CODEPOINT_LONGNAME_GOTHICQ, CODEPOINT_LONGNAME_GOTHICR, CODEPOINT_LONGNAME_GOTHICS, CODEPOINT_LONGNAME_GOTHICSEVEN, 
CODEPOINT_LONGNAME_GOTHICSIX, CODEPOINT_LONGNAME_GOTHICT, CODEPOINT_LONGNAME_GOTHICTHREE, CODEPOINT_LONGNAME_GOTHICTWO, 
CODEPOINT_LONGNAME_GOTHICU, CODEPOINT_LONGNAME_GOTHICV, CODEPOINT_LONGNAME_GOTHICW, CODEPOINT_LONGNAME_GOTHICX, 
CODEPOINT_LONGNAME_GOTHICY, CODEPOINT_LONGNAME_GOTHICZ, CODEPOINT_LONGNAME_GOTHICZERO, CODEPOINT_LONGNAME_GRAYCIRCLE, 
CODEPOINT_LONGNAME_GRAYSQUARE, CODEPOINT_LONGNAME_GREATEREQUAL, CODEPOINT_LONGNAME_GREATEREQUALLESS, 
CODEPOINT_LONGNAME_GREATERFULLEQUAL, CODEPOINT_LONGNAME_GREATERGREATER, CODEPOINT_LONGNAME_GREATERLESS, 
CODEPOINT_LONGNAME_GREATERSLANTEQUAL, CODEPOINT_LONGNAME_GREATERTILDE, CODEPOINT_LONGNAME_HBAR, CODEPOINT_LONGNAME_HACEK
, CODEPOINT_LONGNAME_HAPPYSMILEY, CODEPOINT_LONGNAME_HEARTSUIT, CODEPOINT_LONGNAME_HERMITIANCONJUGATE, 
CODEPOINT_LONGNAME_HORIZONTALLINE, CODEPOINT_LONGNAME_HUMPDOWNHUMP, CODEPOINT_LONGNAME_HUMPEQUAL, 
CODEPOINT_LONGNAME_HYPHEN, CODEPOINT_LONGNAME_IACUTE, CODEPOINT_LONGNAME_ICUP, CODEPOINT_LONGNAME_IDOUBLEDOT, 
CODEPOINT_LONGNAME_IGRAVE, CODEPOINT_LONGNAME_IHAT, CODEPOINT_LONGNAME_IMAGINARYI, CODEPOINT_LONGNAME_IMAGINARYJ, 
CODEPOINT_LONGNAME_IMPLICITPLUS, CODEPOINT_LONGNAME_IMPLIES, CODEPOINT_LONGNAME_INDENTINGNEWLINE, 
CODEPOINT_LONGNAME_INFINITY, CODEPOINT_LONGNAME_INLINEPART, CODEPOINT_LONGNAME_INTEGRAL, CODEPOINT_LONGNAME_INTERSECTION
, CODEPOINT_LONGNAME_INVISIBLEAPPLICATION, CODEPOINT_LONGNAME_INVISIBLECOMMA, 
CODEPOINT_LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE, CODEPOINT_LONGNAME_INVISIBLEPREFIXSCRIPTBASE, 
CODEPOINT_LONGNAME_INVISIBLESPACE, CODEPOINT_LONGNAME_INVISIBLETIMES, CODEPOINT_LONGNAME_IOTA, 
CODEPOINT_LONGNAME_JUPITER, CODEPOINT_LONGNAME_KAPPA, CODEPOINT_LONGNAME_KERNELICON, CODEPOINT_LONGNAME_KEYBAR, 
CODEPOINT_LONGNAME_KOPPA, CODEPOINT_LONGNAME_LSLASH, CODEPOINT_LONGNAME_LAMBDA, CODEPOINT_LONGNAME_LASTPAGE, 
CODEPOINT_LONGNAME_LEFTANGLEBRACKET, CODEPOINT_LONGNAME_LEFTARROW, CODEPOINT_LONGNAME_LEFTARROWBAR, 
CODEPOINT_LONGNAME_LEFTARROWRIGHTARROW, CODEPOINT_LONGNAME_LEFTASSOCIATION, CODEPOINT_LONGNAME_LEFTBRACKETINGBAR, 
CODEPOINT_LONGNAME_LEFTCEILING, CODEPOINT_LONGNAME_LEFTDOUBLEBRACKET, CODEPOINT_LONGNAME_LEFTDOUBLEBRACKETINGBAR, 
CODEPOINT_LONGNAME_LEFTDOWNTEEVECTOR, CODEPOINT_LONGNAME_LEFTDOWNVECTOR, CODEPOINT_LONGNAME_LEFTDOWNVECTORBAR, 
CODEPOINT_LONGNAME_LEFTFLOOR, CODEPOINT_LONGNAME_LEFTGUILLEMET, CODEPOINT_LONGNAME_LEFTMODIFIED, 
CODEPOINT_LONGNAME_LEFTPOINTER, CODEPOINT_LONGNAME_LEFTRIGHTARROW, CODEPOINT_LONGNAME_LEFTRIGHTVECTOR, 
CODEPOINT_LONGNAME_LEFTSKELETON, CODEPOINT_LONGNAME_LEFTTEE, CODEPOINT_LONGNAME_LEFTTEEARROW, 
CODEPOINT_LONGNAME_LEFTTEEVECTOR, CODEPOINT_LONGNAME_LEFTTRIANGLE, CODEPOINT_LONGNAME_LEFTTRIANGLEBAR, 
CODEPOINT_LONGNAME_LEFTTRIANGLEEQUAL, CODEPOINT_LONGNAME_LEFTUPDOWNVECTOR, CODEPOINT_LONGNAME_LEFTUPTEEVECTOR, 
CODEPOINT_LONGNAME_LEFTUPVECTOR, CODEPOINT_LONGNAME_LEFTUPVECTORBAR, CODEPOINT_LONGNAME_LEFTVECTOR, 
CODEPOINT_LONGNAME_LEFTVECTORBAR, CODEPOINT_LONGNAME_LEOSIGN, CODEPOINT_LONGNAME_LESSEQUAL, 
CODEPOINT_LONGNAME_LESSEQUALGREATER, CODEPOINT_LONGNAME_LESSFULLEQUAL, CODEPOINT_LONGNAME_LESSGREATER, 
CODEPOINT_LONGNAME_LESSLESS, CODEPOINT_LONGNAME_LESSSLANTEQUAL, CODEPOINT_LONGNAME_LESSTILDE, 
CODEPOINT_LONGNAME_LETTERSPACE, CODEPOINT_LONGNAME_LIBRASIGN, CODEPOINT_LONGNAME_LIGHTBULB, CODEPOINT_LONGNAME_LIMIT, 
CODEPOINT_LONGNAME_LINESEPARATOR, CODEPOINT_LONGNAME_LONGDASH, CODEPOINT_LONGNAME_LONGEQUAL, 
CODEPOINT_LONGNAME_LONGLEFTARROW, CODEPOINT_LONGNAME_LONGLEFTRIGHTARROW, CODEPOINT_LONGNAME_LONGRIGHTARROW, 
CODEPOINT_LONGNAME_LOWERLEFTARROW, CODEPOINT_LONGNAME_LOWERRIGHTARROW, CODEPOINT_LONGNAME_MARS, 
CODEPOINT_LONGNAME_MATHEMATICAICON, CODEPOINT_LONGNAME_MAXLIMIT, CODEPOINT_LONGNAME_MEASUREDANGLE, 
CODEPOINT_LONGNAME_MEDIUMSPACE, CODEPOINT_LONGNAME_MERCURY, CODEPOINT_LONGNAME_MHO, CODEPOINT_LONGNAME_MICRO, 
CODEPOINT_LONGNAME_MINLIMIT, CODEPOINT_LONGNAME_MINUS, CODEPOINT_LONGNAME_MINUSPLUS, CODEPOINT_LONGNAME_MOD1KEY, 
CODEPOINT_LONGNAME_MOD2KEY, CODEPOINT_LONGNAME_MOON, CODEPOINT_LONGNAME_MU, CODEPOINT_LONGNAME_NHACEK, 
CODEPOINT_LONGNAME_NTILDE, CODEPOINT_LONGNAME_NAND, CODEPOINT_LONGNAME_NATURAL, CODEPOINT_LONGNAME_NEGATIVEMEDIUMSPACE, 
CODEPOINT_LONGNAME_NEGATIVETHICKSPACE, CODEPOINT_LONGNAME_NEGATIVETHINSPACE, CODEPOINT_LONGNAME_NEGATIVEVERYTHINSPACE, 
CODEPOINT_LONGNAME_NEPTUNE, CODEPOINT_LONGNAME_NESTEDGREATERGREATER, CODEPOINT_LONGNAME_NESTEDLESSLESS, 
CODEPOINT_LONGNAME_NEUTRALSMILEY, CODEPOINT_LONGNAME_NEWLINE, CODEPOINT_LONGNAME_NOBREAK, 
CODEPOINT_LONGNAME_NONBREAKINGSPACE, CODEPOINT_LONGNAME_NOR, CODEPOINT_LONGNAME_NOT, CODEPOINT_LONGNAME_NOTCONGRUENT, 
CODEPOINT_LONGNAME_NOTCUPCAP, CODEPOINT_LONGNAME_NOTDOUBLEVERTICALBAR, CODEPOINT_LONGNAME_NOTELEMENT, 
CODEPOINT_LONGNAME_NOTEQUAL, CODEPOINT_LONGNAME_NOTEQUALTILDE, CODEPOINT_LONGNAME_NOTEXISTS, 
CODEPOINT_LONGNAME_NOTGREATER, CODEPOINT_LONGNAME_NOTGREATEREQUAL, CODEPOINT_LONGNAME_NOTGREATERFULLEQUAL, 
CODEPOINT_LONGNAME_NOTGREATERGREATER, CODEPOINT_LONGNAME_NOTGREATERLESS, CODEPOINT_LONGNAME_NOTGREATERSLANTEQUAL, 
CODEPOINT_LONGNAME_NOTGREATERTILDE, CODEPOINT_LONGNAME_NOTHUMPDOWNHUMP, CODEPOINT_LONGNAME_NOTHUMPEQUAL, 
CODEPOINT_LONGNAME_NOTLEFTTRIANGLE, CODEPOINT_LONGNAME_NOTLEFTTRIANGLEBAR, CODEPOINT_LONGNAME_NOTLEFTTRIANGLEEQUAL, 
CODEPOINT_LONGNAME_NOTLESS, CODEPOINT_LONGNAME_NOTLESSEQUAL, CODEPOINT_LONGNAME_NOTLESSFULLEQUAL, 
CODEPOINT_LONGNAME_NOTLESSGREATER, CODEPOINT_LONGNAME_NOTLESSLESS, CODEPOINT_LONGNAME_NOTLESSSLANTEQUAL, 
CODEPOINT_LONGNAME_NOTLESSTILDE, CODEPOINT_LONGNAME_NOTNESTEDGREATERGREATER, CODEPOINT_LONGNAME_NOTNESTEDLESSLESS, 
CODEPOINT_LONGNAME_NOTPRECEDES, CODEPOINT_LONGNAME_NOTPRECEDESEQUAL, CODEPOINT_LONGNAME_NOTPRECEDESSLANTEQUAL, 
CODEPOINT_LONGNAME_NOTPRECEDESTILDE, CODEPOINT_LONGNAME_NOTREVERSEELEMENT, CODEPOINT_LONGNAME_NOTRIGHTTRIANGLE, 
CODEPOINT_LONGNAME_NOTRIGHTTRIANGLEBAR, CODEPOINT_LONGNAME_NOTRIGHTTRIANGLEEQUAL, CODEPOINT_LONGNAME_NOTSQUARESUBSET, 
CODEPOINT_LONGNAME_NOTSQUARESUBSETEQUAL, CODEPOINT_LONGNAME_NOTSQUARESUPERSET, CODEPOINT_LONGNAME_NOTSQUARESUPERSETEQUAL
, CODEPOINT_LONGNAME_NOTSUBSET, CODEPOINT_LONGNAME_NOTSUBSETEQUAL, CODEPOINT_LONGNAME_NOTSUCCEEDS, 
CODEPOINT_LONGNAME_NOTSUCCEEDSEQUAL, CODEPOINT_LONGNAME_NOTSUCCEEDSSLANTEQUAL, CODEPOINT_LONGNAME_NOTSUCCEEDSTILDE, 
CODEPOINT_LONGNAME_NOTSUPERSET, CODEPOINT_LONGNAME_NOTSUPERSETEQUAL, CODEPOINT_LONGNAME_NOTTILDE, 
CODEPOINT_LONGNAME_NOTTILDEEQUAL, CODEPOINT_LONGNAME_NOTTILDEFULLEQUAL, CODEPOINT_LONGNAME_NOTTILDETILDE, 
CODEPOINT_LONGNAME_NOTVERTICALBAR, CODEPOINT_LONGNAME_NU, CODEPOINT_LONGNAME_NULL, CODEPOINT_LONGNAME_NUMBERCOMMA, 
CODEPOINT_LONGNAME_NUMBERSIGN, CODEPOINT_LONGNAME_OACUTE, CODEPOINT_LONGNAME_ODOUBLEACUTE, CODEPOINT_LONGNAME_ODOUBLEDOT
, CODEPOINT_LONGNAME_OE, CODEPOINT_LONGNAME_OGRAVE, CODEPOINT_LONGNAME_OHAT, CODEPOINT_LONGNAME_OSLASH, 
CODEPOINT_LONGNAME_OTILDE, CODEPOINT_LONGNAME_OMEGA, CODEPOINT_LONGNAME_OMICRON, CODEPOINT_LONGNAME_OPENCURLYDOUBLEQUOTE
, CODEPOINT_LONGNAME_OPENCURLYQUOTE, CODEPOINT_LONGNAME_OPTIONKEY, CODEPOINT_LONGNAME_OR, CODEPOINT_LONGNAME_OVERBRACE, 
CODEPOINT_LONGNAME_OVERBRACKET, CODEPOINT_LONGNAME_OVERPARENTHESIS, CODEPOINT_LONGNAME_PAGEBREAKABOVE, 
CODEPOINT_LONGNAME_PAGEBREAKBELOW, CODEPOINT_LONGNAME_PARAGRAPH, CODEPOINT_LONGNAME_PARAGRAPHSEPARATOR, 
CODEPOINT_LONGNAME_PARTIALD, CODEPOINT_LONGNAME_PERMUTATIONPRODUCT, CODEPOINT_LONGNAME_PERPENDICULAR, 
CODEPOINT_LONGNAME_PHI, CODEPOINT_LONGNAME_PI, CODEPOINT_LONGNAME_PIECEWISE, CODEPOINT_LONGNAME_PISCESSIGN, 
CODEPOINT_LONGNAME_PLACEHOLDER, CODEPOINT_LONGNAME_PLUSMINUS, CODEPOINT_LONGNAME_PLUTO, CODEPOINT_LONGNAME_PRECEDES, 
CODEPOINT_LONGNAME_PRECEDESEQUAL, CODEPOINT_LONGNAME_PRECEDESSLANTEQUAL, CODEPOINT_LONGNAME_PRECEDESTILDE, 
CODEPOINT_LONGNAME_PRIME, CODEPOINT_LONGNAME_PROBABILITYPR, CODEPOINT_LONGNAME_PRODUCT, CODEPOINT_LONGNAME_PROPORTION, 
CODEPOINT_LONGNAME_PROPORTIONAL, CODEPOINT_LONGNAME_PSI, CODEPOINT_LONGNAME_QUARTERNOTE, CODEPOINT_LONGNAME_RHACEK, 
CODEPOINT_LONGNAME_RAWAMPERSAND, CODEPOINT_LONGNAME_RAWAT, CODEPOINT_LONGNAME_RAWBACKQUOTE, 
CODEPOINT_LONGNAME_RAWBACKSLASH, CODEPOINT_LONGNAME_RAWCOLON, CODEPOINT_LONGNAME_RAWCOMMA, CODEPOINT_LONGNAME_RAWDASH, 
CODEPOINT_LONGNAME_RAWDOLLAR, CODEPOINT_LONGNAME_RAWDOT, CODEPOINT_LONGNAME_RAWDOUBLEQUOTE, CODEPOINT_LONGNAME_RAWEQUAL,
 CODEPOINT_LONGNAME_RAWESCAPE, CODEPOINT_LONGNAME_RAWEXCLAMATION, CODEPOINT_LONGNAME_RAWGREATER, 
CODEPOINT_LONGNAME_RAWLEFTBRACE, CODEPOINT_LONGNAME_RAWLEFTBRACKET, CODEPOINT_LONGNAME_RAWLEFTPARENTHESIS, 
CODEPOINT_LONGNAME_RAWLESS, CODEPOINT_LONGNAME_RAWNUMBERSIGN, CODEPOINT_LONGNAME_RAWPERCENT, CODEPOINT_LONGNAME_RAWPLUS,
 CODEPOINT_LONGNAME_RAWQUESTION, CODEPOINT_LONGNAME_RAWQUOTE, CODEPOINT_LONGNAME_RAWRETURN, 
CODEPOINT_LONGNAME_RAWRIGHTBRACE, CODEPOINT_LONGNAME_RAWRIGHTBRACKET, CODEPOINT_LONGNAME_RAWRIGHTPARENTHESIS, 
CODEPOINT_LONGNAME_RAWSEMICOLON, CODEPOINT_LONGNAME_RAWSLASH, CODEPOINT_LONGNAME_RAWSPACE, CODEPOINT_LONGNAME_RAWSTAR, 
CODEPOINT_LONGNAME_RAWTAB, CODEPOINT_LONGNAME_RAWTILDE, CODEPOINT_LONGNAME_RAWUNDERSCORE, 
CODEPOINT_LONGNAME_RAWVERTICALBAR, CODEPOINT_LONGNAME_RAWWEDGE, CODEPOINT_LONGNAME_REGISTEREDTRADEMARK, 
CODEPOINT_LONGNAME_RETURNINDICATOR, CODEPOINT_LONGNAME_RETURNKEY, CODEPOINT_LONGNAME_REVERSEDOUBLEPRIME, 
CODEPOINT_LONGNAME_REVERSEELEMENT, CODEPOINT_LONGNAME_REVERSEEQUILIBRIUM, CODEPOINT_LONGNAME_REVERSEPRIME, 
CODEPOINT_LONGNAME_REVERSEUPEQUILIBRIUM, CODEPOINT_LONGNAME_RHO, CODEPOINT_LONGNAME_RIGHTANGLE, 
CODEPOINT_LONGNAME_RIGHTANGLEBRACKET, CODEPOINT_LONGNAME_RIGHTARROW, CODEPOINT_LONGNAME_RIGHTARROWBAR, 
CODEPOINT_LONGNAME_RIGHTARROWLEFTARROW, CODEPOINT_LONGNAME_RIGHTASSOCIATION, CODEPOINT_LONGNAME_RIGHTBRACKETINGBAR, 
CODEPOINT_LONGNAME_RIGHTCEILING, CODEPOINT_LONGNAME_RIGHTDOUBLEBRACKET, CODEPOINT_LONGNAME_RIGHTDOUBLEBRACKETINGBAR, 
CODEPOINT_LONGNAME_RIGHTDOWNTEEVECTOR, CODEPOINT_LONGNAME_RIGHTDOWNVECTOR, CODEPOINT_LONGNAME_RIGHTDOWNVECTORBAR, 
CODEPOINT_LONGNAME_RIGHTFLOOR, CODEPOINT_LONGNAME_RIGHTGUILLEMET, CODEPOINT_LONGNAME_RIGHTMODIFIED, 
CODEPOINT_LONGNAME_RIGHTPOINTER, CODEPOINT_LONGNAME_RIGHTSKELETON, CODEPOINT_LONGNAME_RIGHTTEE, 
CODEPOINT_LONGNAME_RIGHTTEEARROW, CODEPOINT_LONGNAME_RIGHTTEEVECTOR, CODEPOINT_LONGNAME_RIGHTTRIANGLE, 
CODEPOINT_LONGNAME_RIGHTTRIANGLEBAR, CODEPOINT_LONGNAME_RIGHTTRIANGLEEQUAL, CODEPOINT_LONGNAME_RIGHTUPDOWNVECTOR, 
CODEPOINT_LONGNAME_RIGHTUPTEEVECTOR, CODEPOINT_LONGNAME_RIGHTUPVECTOR, CODEPOINT_LONGNAME_RIGHTUPVECTORBAR, 
CODEPOINT_LONGNAME_RIGHTVECTOR, CODEPOINT_LONGNAME_RIGHTVECTORBAR, CODEPOINT_LONGNAME_ROUNDIMPLIES, 
CODEPOINT_LONGNAME_ROUNDSPACEINDICATOR, CODEPOINT_LONGNAME_RULE, CODEPOINT_LONGNAME_RULEDELAYED, 
CODEPOINT_LONGNAME_RUPEE, CODEPOINT_LONGNAME_SHACEK, CODEPOINT_LONGNAME_SZ, CODEPOINT_LONGNAME_SADSMILEY, 
CODEPOINT_LONGNAME_SAGITTARIUSSIGN, CODEPOINT_LONGNAME_SAMPI, CODEPOINT_LONGNAME_SATURN, CODEPOINT_LONGNAME_SCORPIOSIGN,
 CODEPOINT_LONGNAME_SCRIPTA, CODEPOINT_LONGNAME_SCRIPTB, CODEPOINT_LONGNAME_SCRIPTC, CODEPOINT_LONGNAME_SCRIPTCAPITALA, 
CODEPOINT_LONGNAME_SCRIPTCAPITALB, CODEPOINT_LONGNAME_SCRIPTCAPITALC, CODEPOINT_LONGNAME_SCRIPTCAPITALD, 
CODEPOINT_LONGNAME_SCRIPTCAPITALE, CODEPOINT_LONGNAME_SCRIPTCAPITALF, CODEPOINT_LONGNAME_SCRIPTCAPITALG, 
CODEPOINT_LONGNAME_SCRIPTCAPITALH, CODEPOINT_LONGNAME_SCRIPTCAPITALI, CODEPOINT_LONGNAME_SCRIPTCAPITALJ, 
CODEPOINT_LONGNAME_SCRIPTCAPITALK, CODEPOINT_LONGNAME_SCRIPTCAPITALL, CODEPOINT_LONGNAME_SCRIPTCAPITALM, 
CODEPOINT_LONGNAME_SCRIPTCAPITALN, CODEPOINT_LONGNAME_SCRIPTCAPITALO, CODEPOINT_LONGNAME_SCRIPTCAPITALP, 
CODEPOINT_LONGNAME_SCRIPTCAPITALQ, CODEPOINT_LONGNAME_SCRIPTCAPITALR, CODEPOINT_LONGNAME_SCRIPTCAPITALS, 
CODEPOINT_LONGNAME_SCRIPTCAPITALT, CODEPOINT_LONGNAME_SCRIPTCAPITALU, CODEPOINT_LONGNAME_SCRIPTCAPITALV, 
CODEPOINT_LONGNAME_SCRIPTCAPITALW, CODEPOINT_LONGNAME_SCRIPTCAPITALX, CODEPOINT_LONGNAME_SCRIPTCAPITALY, 
CODEPOINT_LONGNAME_SCRIPTCAPITALZ, CODEPOINT_LONGNAME_SCRIPTD, CODEPOINT_LONGNAME_SCRIPTDOTLESSI, 
CODEPOINT_LONGNAME_SCRIPTDOTLESSJ, CODEPOINT_LONGNAME_SCRIPTE, CODEPOINT_LONGNAME_SCRIPTEIGHT, 
CODEPOINT_LONGNAME_SCRIPTF, CODEPOINT_LONGNAME_SCRIPTFIVE, CODEPOINT_LONGNAME_SCRIPTFOUR, CODEPOINT_LONGNAME_SCRIPTG, 
CODEPOINT_LONGNAME_SCRIPTH, CODEPOINT_LONGNAME_SCRIPTI, CODEPOINT_LONGNAME_SCRIPTJ, CODEPOINT_LONGNAME_SCRIPTK, 
CODEPOINT_LONGNAME_SCRIPTL, CODEPOINT_LONGNAME_SCRIPTM, CODEPOINT_LONGNAME_SCRIPTN, CODEPOINT_LONGNAME_SCRIPTNINE, 
CODEPOINT_LONGNAME_SCRIPTO, CODEPOINT_LONGNAME_SCRIPTONE, CODEPOINT_LONGNAME_SCRIPTP, CODEPOINT_LONGNAME_SCRIPTQ, 
CODEPOINT_LONGNAME_SCRIPTR, CODEPOINT_LONGNAME_SCRIPTS, CODEPOINT_LONGNAME_SCRIPTSEVEN, CODEPOINT_LONGNAME_SCRIPTSIX, 
CODEPOINT_LONGNAME_SCRIPTT, CODEPOINT_LONGNAME_SCRIPTTHREE, CODEPOINT_LONGNAME_SCRIPTTWO, CODEPOINT_LONGNAME_SCRIPTU, 
CODEPOINT_LONGNAME_SCRIPTV, CODEPOINT_LONGNAME_SCRIPTW, CODEPOINT_LONGNAME_SCRIPTX, CODEPOINT_LONGNAME_SCRIPTY, 
CODEPOINT_LONGNAME_SCRIPTZ, CODEPOINT_LONGNAME_SCRIPTZERO, CODEPOINT_LONGNAME_SECTION, 
CODEPOINT_LONGNAME_SELECTIONPLACEHOLDER, CODEPOINT_LONGNAME_SHAH, CODEPOINT_LONGNAME_SHARP, CODEPOINT_LONGNAME_SHIFTKEY,
 CODEPOINT_LONGNAME_SHORTDOWNARROW, CODEPOINT_LONGNAME_SHORTLEFTARROW, CODEPOINT_LONGNAME_SHORTRIGHTARROW, 
CODEPOINT_LONGNAME_SHORTUPARROW, CODEPOINT_LONGNAME_SIGMA, CODEPOINT_LONGNAME_SIXPOINTEDSTAR, 
CODEPOINT_LONGNAME_SKELETONINDICATOR, CODEPOINT_LONGNAME_SMALLCIRCLE, CODEPOINT_LONGNAME_SPACEINDICATOR, 
CODEPOINT_LONGNAME_SPACEKEY, CODEPOINT_LONGNAME_SPADESUIT, CODEPOINT_LONGNAME_SPANFROMABOVE, 
CODEPOINT_LONGNAME_SPANFROMBOTH, CODEPOINT_LONGNAME_SPANFROMLEFT, CODEPOINT_LONGNAME_SPHERICALANGLE, 
CODEPOINT_LONGNAME_SPOOKY, CODEPOINT_LONGNAME_SQRT, CODEPOINT_LONGNAME_SQUARE, CODEPOINT_LONGNAME_SQUAREINTERSECTION, 
CODEPOINT_LONGNAME_SQUARESUBSET, CODEPOINT_LONGNAME_SQUARESUBSETEQUAL, CODEPOINT_LONGNAME_SQUARESUPERSET, 
CODEPOINT_LONGNAME_SQUARESUPERSETEQUAL, CODEPOINT_LONGNAME_SQUAREUNION, CODEPOINT_LONGNAME_STAR, 
CODEPOINT_LONGNAME_STEPPERDOWN, CODEPOINT_LONGNAME_STEPPERLEFT, CODEPOINT_LONGNAME_STEPPERRIGHT, 
CODEPOINT_LONGNAME_STEPPERUP, CODEPOINT_LONGNAME_STERLING, CODEPOINT_LONGNAME_STIGMA, CODEPOINT_LONGNAME_SUBSET, 
CODEPOINT_LONGNAME_SUBSETEQUAL, CODEPOINT_LONGNAME_SUCCEEDS, CODEPOINT_LONGNAME_SUCCEEDSEQUAL, 
CODEPOINT_LONGNAME_SUCCEEDSSLANTEQUAL, CODEPOINT_LONGNAME_SUCCEEDSTILDE, CODEPOINT_LONGNAME_SUCHTHAT, 
CODEPOINT_LONGNAME_SUM, CODEPOINT_LONGNAME_SUN, CODEPOINT_LONGNAME_SUPERSET, CODEPOINT_LONGNAME_SUPERSETEQUAL, 
CODEPOINT_LONGNAME_SYSTEMENTERKEY, CODEPOINT_LONGNAME_SYSTEMSMODELDELAY, CODEPOINT_LONGNAME_THACEK, 
CODEPOINT_LONGNAME_TABKEY, CODEPOINT_LONGNAME_TAU, CODEPOINT_LONGNAME_TAURUSSIGN, CODEPOINT_LONGNAME_TENSORPRODUCT, 
CODEPOINT_LONGNAME_TENSORWEDGE, CODEPOINT_LONGNAME_THEREFORE, CODEPOINT_LONGNAME_THETA, CODEPOINT_LONGNAME_THICKSPACE, 
CODEPOINT_LONGNAME_THINSPACE, CODEPOINT_LONGNAME_THORN, CODEPOINT_LONGNAME_TILDE, CODEPOINT_LONGNAME_TILDEEQUAL, 
CODEPOINT_LONGNAME_TILDEFULLEQUAL, CODEPOINT_LONGNAME_TILDETILDE, CODEPOINT_LONGNAME_TIMES, CODEPOINT_LONGNAME_TRADEMARK
, CODEPOINT_LONGNAME_TRANSPOSE, CODEPOINT_LONGNAME_TRIPLEDOT, CODEPOINT_LONGNAME_TWOWAYRULE, CODEPOINT_LONGNAME_UACUTE, 
CODEPOINT_LONGNAME_UDOUBLEACUTE, CODEPOINT_LONGNAME_UDOUBLEDOT, CODEPOINT_LONGNAME_UGRAVE, CODEPOINT_LONGNAME_UHAT, 
CODEPOINT_LONGNAME_URING, CODEPOINT_LONGNAME_UNDERBRACE, CODEPOINT_LONGNAME_UNDERBRACKET, 
CODEPOINT_LONGNAME_UNDERPARENTHESIS, CODEPOINT_LONGNAME_UNDIRECTEDEDGE, CODEPOINT_LONGNAME_UNION, 
CODEPOINT_LONGNAME_UNIONPLUS, CODEPOINT_LONGNAME_UNKNOWNGLYPH, CODEPOINT_LONGNAME_UPARROW, CODEPOINT_LONGNAME_UPARROWBAR
, CODEPOINT_LONGNAME_UPARROWDOWNARROW, CODEPOINT_LONGNAME_UPDOWNARROW, CODEPOINT_LONGNAME_UPEQUILIBRIUM, 
CODEPOINT_LONGNAME_UPPOINTER, CODEPOINT_LONGNAME_UPTEE, CODEPOINT_LONGNAME_UPTEEARROW, CODEPOINT_LONGNAME_UPPERLEFTARROW
, CODEPOINT_LONGNAME_UPPERRIGHTARROW, CODEPOINT_LONGNAME_UPSILON, CODEPOINT_LONGNAME_URANUS, 
CODEPOINT_LONGNAME_VECTORGREATER, CODEPOINT_LONGNAME_VECTORGREATEREQUAL, CODEPOINT_LONGNAME_VECTORLESS, 
CODEPOINT_LONGNAME_VECTORLESSEQUAL, CODEPOINT_LONGNAME_VEE, CODEPOINT_LONGNAME_VENUS, CODEPOINT_LONGNAME_VERTICALBAR, 
CODEPOINT_LONGNAME_VERTICALELLIPSIS, CODEPOINT_LONGNAME_VERTICALLINE, CODEPOINT_LONGNAME_VERTICALSEPARATOR, 
CODEPOINT_LONGNAME_VERTICALTILDE, CODEPOINT_LONGNAME_VERYTHINSPACE, CODEPOINT_LONGNAME_VILLA, 
CODEPOINT_LONGNAME_VIRGOSIGN, CODEPOINT_LONGNAME_WARNINGSIGN, CODEPOINT_LONGNAME_WATCHICON, CODEPOINT_LONGNAME_WEDGE, 
CODEPOINT_LONGNAME_WEIERSTRASSP, CODEPOINT_LONGNAME_WHITEBISHOP, CODEPOINT_LONGNAME_WHITEKING, 
CODEPOINT_LONGNAME_WHITEKNIGHT, CODEPOINT_LONGNAME_WHITEPAWN, CODEPOINT_LONGNAME_WHITEQUEEN, 
CODEPOINT_LONGNAME_WHITEROOK, CODEPOINT_LONGNAME_WOLF, CODEPOINT_LONGNAME_WOLFRAMALPHAPROMPT, 
CODEPOINT_LONGNAME_WOLFRAMLANGUAGELOGO, CODEPOINT_LONGNAME_WOLFRAMLANGUAGELOGOCIRCLE, CODEPOINT_LONGNAME_XI, 
CODEPOINT_LONGNAME_XNOR, CODEPOINT_LONGNAME_XOR, CODEPOINT_LONGNAME_YACUTE, CODEPOINT_LONGNAME_YDOUBLEDOT, 
CODEPOINT_LONGNAME_YEN, CODEPOINT_LONGNAME_ZHACEK, CODEPOINT_LONGNAME_ZETA, 
];

//
//
//
pub const CODE_POINT_TO_LONGNAME_MAP__POINTS: [CodePoint; LONGNAMES_COUNT] = [
CODEPOINT_LONGNAME_RAWTAB, CODEPOINT_LONGNAME_NEWLINE, CODEPOINT_LONGNAME_RAWRETURN, CODEPOINT_LONGNAME_RAWESCAPE, 
CODEPOINT_LONGNAME_RAWSPACE, CODEPOINT_LONGNAME_RAWEXCLAMATION, CODEPOINT_LONGNAME_RAWDOUBLEQUOTE, 
CODEPOINT_LONGNAME_RAWNUMBERSIGN, CODEPOINT_LONGNAME_RAWDOLLAR, CODEPOINT_LONGNAME_RAWPERCENT, 
CODEPOINT_LONGNAME_RAWAMPERSAND, CODEPOINT_LONGNAME_RAWQUOTE, CODEPOINT_LONGNAME_RAWLEFTPARENTHESIS, 
CODEPOINT_LONGNAME_RAWRIGHTPARENTHESIS, CODEPOINT_LONGNAME_RAWSTAR, CODEPOINT_LONGNAME_RAWPLUS, 
CODEPOINT_LONGNAME_RAWCOMMA, CODEPOINT_LONGNAME_RAWDASH, CODEPOINT_LONGNAME_RAWDOT, CODEPOINT_LONGNAME_RAWSLASH, 
CODEPOINT_LONGNAME_RAWCOLON, CODEPOINT_LONGNAME_RAWSEMICOLON, CODEPOINT_LONGNAME_RAWLESS, CODEPOINT_LONGNAME_RAWEQUAL, 
CODEPOINT_LONGNAME_RAWGREATER, CODEPOINT_LONGNAME_RAWQUESTION, CODEPOINT_LONGNAME_RAWAT, 
CODEPOINT_LONGNAME_RAWLEFTBRACKET, CODEPOINT_LONGNAME_RAWBACKSLASH, CODEPOINT_LONGNAME_RAWRIGHTBRACKET, 
CODEPOINT_LONGNAME_RAWWEDGE, CODEPOINT_LONGNAME_RAWUNDERSCORE, CODEPOINT_LONGNAME_RAWBACKQUOTE, 
CODEPOINT_LONGNAME_RAWLEFTBRACE, CODEPOINT_LONGNAME_RAWVERTICALBAR, CODEPOINT_LONGNAME_RAWRIGHTBRACE, 
CODEPOINT_LONGNAME_RAWTILDE, CODEPOINT_LONGNAME_NONBREAKINGSPACE, CODEPOINT_LONGNAME_DOWNEXCLAMATION, 
CODEPOINT_LONGNAME_CENT, CODEPOINT_LONGNAME_STERLING, CODEPOINT_LONGNAME_CURRENCY, CODEPOINT_LONGNAME_YEN, 
CODEPOINT_LONGNAME_SECTION, CODEPOINT_LONGNAME_DOUBLEDOT, CODEPOINT_LONGNAME_COPYRIGHT, CODEPOINT_LONGNAME_LEFTGUILLEMET
, CODEPOINT_LONGNAME_NOT, CODEPOINT_LONGNAME_DISCRETIONARYHYPHEN, CODEPOINT_LONGNAME_REGISTEREDTRADEMARK, 
CODEPOINT_LONGNAME_DEGREE, CODEPOINT_LONGNAME_PLUSMINUS, CODEPOINT_LONGNAME_MICRO, CODEPOINT_LONGNAME_PARAGRAPH, 
CODEPOINT_LONGNAME_CENTERDOT, CODEPOINT_LONGNAME_CEDILLA, CODEPOINT_LONGNAME_RIGHTGUILLEMET, 
CODEPOINT_LONGNAME_DOWNQUESTION, CODEPOINT_LONGNAME_CAPITALAGRAVE, CODEPOINT_LONGNAME_CAPITALAACUTE, 
CODEPOINT_LONGNAME_CAPITALAHAT, CODEPOINT_LONGNAME_CAPITALATILDE, CODEPOINT_LONGNAME_CAPITALADOUBLEDOT, 
CODEPOINT_LONGNAME_CAPITALARING, CODEPOINT_LONGNAME_CAPITALAE, CODEPOINT_LONGNAME_CAPITALCCEDILLA, 
CODEPOINT_LONGNAME_CAPITALEGRAVE, CODEPOINT_LONGNAME_CAPITALEACUTE, CODEPOINT_LONGNAME_CAPITALEHAT, 
CODEPOINT_LONGNAME_CAPITALEDOUBLEDOT, CODEPOINT_LONGNAME_CAPITALIGRAVE, CODEPOINT_LONGNAME_CAPITALIACUTE, 
CODEPOINT_LONGNAME_CAPITALIHAT, CODEPOINT_LONGNAME_CAPITALIDOUBLEDOT, CODEPOINT_LONGNAME_CAPITALETH, 
CODEPOINT_LONGNAME_CAPITALNTILDE, CODEPOINT_LONGNAME_CAPITALOGRAVE, CODEPOINT_LONGNAME_CAPITALOACUTE, 
CODEPOINT_LONGNAME_CAPITALOHAT, CODEPOINT_LONGNAME_CAPITALOTILDE, CODEPOINT_LONGNAME_CAPITALODOUBLEDOT, 
CODEPOINT_LONGNAME_TIMES, CODEPOINT_LONGNAME_CAPITALOSLASH, CODEPOINT_LONGNAME_CAPITALUGRAVE, 
CODEPOINT_LONGNAME_CAPITALUACUTE, CODEPOINT_LONGNAME_CAPITALUHAT, CODEPOINT_LONGNAME_CAPITALUDOUBLEDOT, 
CODEPOINT_LONGNAME_CAPITALYACUTE, CODEPOINT_LONGNAME_CAPITALTHORN, CODEPOINT_LONGNAME_SZ, CODEPOINT_LONGNAME_AGRAVE, 
CODEPOINT_LONGNAME_AACUTE, CODEPOINT_LONGNAME_AHAT, CODEPOINT_LONGNAME_ATILDE, CODEPOINT_LONGNAME_ADOUBLEDOT, 
CODEPOINT_LONGNAME_ARING, CODEPOINT_LONGNAME_AE, CODEPOINT_LONGNAME_CCEDILLA, CODEPOINT_LONGNAME_EGRAVE, 
CODEPOINT_LONGNAME_EACUTE, CODEPOINT_LONGNAME_EHAT, CODEPOINT_LONGNAME_EDOUBLEDOT, CODEPOINT_LONGNAME_IGRAVE, 
CODEPOINT_LONGNAME_IACUTE, CODEPOINT_LONGNAME_IHAT, CODEPOINT_LONGNAME_IDOUBLEDOT, CODEPOINT_LONGNAME_ETH, 
CODEPOINT_LONGNAME_NTILDE, CODEPOINT_LONGNAME_OGRAVE, CODEPOINT_LONGNAME_OACUTE, CODEPOINT_LONGNAME_OHAT, 
CODEPOINT_LONGNAME_OTILDE, CODEPOINT_LONGNAME_ODOUBLEDOT, CODEPOINT_LONGNAME_DIVIDE, CODEPOINT_LONGNAME_OSLASH, 
CODEPOINT_LONGNAME_UGRAVE, CODEPOINT_LONGNAME_UACUTE, CODEPOINT_LONGNAME_UHAT, CODEPOINT_LONGNAME_UDOUBLEDOT, 
CODEPOINT_LONGNAME_YACUTE, CODEPOINT_LONGNAME_THORN, CODEPOINT_LONGNAME_YDOUBLEDOT, CODEPOINT_LONGNAME_CAPITALABAR, 
CODEPOINT_LONGNAME_ABAR, CODEPOINT_LONGNAME_CAPITALACUP, CODEPOINT_LONGNAME_ACUP, CODEPOINT_LONGNAME_CAPITALCACUTE, 
CODEPOINT_LONGNAME_CACUTE, CODEPOINT_LONGNAME_CAPITALCHACEK, CODEPOINT_LONGNAME_CHACEK, CODEPOINT_LONGNAME_CAPITALDHACEK
, CODEPOINT_LONGNAME_DHACEK, CODEPOINT_LONGNAME_CAPITALEBAR, CODEPOINT_LONGNAME_EBAR, CODEPOINT_LONGNAME_CAPITALECUP, 
CODEPOINT_LONGNAME_ECUP, CODEPOINT_LONGNAME_CAPITALEHACEK, CODEPOINT_LONGNAME_EHACEK, CODEPOINT_LONGNAME_CAPITALICUP, 
CODEPOINT_LONGNAME_ICUP, CODEPOINT_LONGNAME_DOTLESSI, CODEPOINT_LONGNAME_CAPITALLSLASH, CODEPOINT_LONGNAME_LSLASH, 
CODEPOINT_LONGNAME_CAPITALNHACEK, CODEPOINT_LONGNAME_NHACEK, CODEPOINT_LONGNAME_CAPITALODOUBLEACUTE, 
CODEPOINT_LONGNAME_ODOUBLEACUTE, CODEPOINT_LONGNAME_CAPITALOE, CODEPOINT_LONGNAME_OE, CODEPOINT_LONGNAME_CAPITALRHACEK, 
CODEPOINT_LONGNAME_RHACEK, CODEPOINT_LONGNAME_CAPITALSHACEK, CODEPOINT_LONGNAME_SHACEK, CODEPOINT_LONGNAME_CAPITALTHACEK
, CODEPOINT_LONGNAME_THACEK, CODEPOINT_LONGNAME_CAPITALURING, CODEPOINT_LONGNAME_URING, 
CODEPOINT_LONGNAME_CAPITALUDOUBLEACUTE, CODEPOINT_LONGNAME_UDOUBLEACUTE, CODEPOINT_LONGNAME_CAPITALZHACEK, 
CODEPOINT_LONGNAME_ZHACEK, CODEPOINT_LONGNAME_FLORIN, CODEPOINT_LONGNAME_HACEK, CODEPOINT_LONGNAME_BREVE, 
CODEPOINT_LONGNAME_CAPITALALPHA, CODEPOINT_LONGNAME_CAPITALBETA, CODEPOINT_LONGNAME_CAPITALGAMMA, 
CODEPOINT_LONGNAME_CAPITALDELTA, CODEPOINT_LONGNAME_CAPITALEPSILON, CODEPOINT_LONGNAME_CAPITALZETA, 
CODEPOINT_LONGNAME_CAPITALETA, CODEPOINT_LONGNAME_CAPITALTHETA, CODEPOINT_LONGNAME_CAPITALIOTA, 
CODEPOINT_LONGNAME_CAPITALKAPPA, CODEPOINT_LONGNAME_CAPITALLAMBDA, CODEPOINT_LONGNAME_CAPITALMU, 
CODEPOINT_LONGNAME_CAPITALNU, CODEPOINT_LONGNAME_CAPITALXI, CODEPOINT_LONGNAME_CAPITALOMICRON, 
CODEPOINT_LONGNAME_CAPITALPI, CODEPOINT_LONGNAME_CAPITALRHO, CODEPOINT_LONGNAME_CAPITALSIGMA, 
CODEPOINT_LONGNAME_CAPITALTAU, CODEPOINT_LONGNAME_CAPITALUPSILON, CODEPOINT_LONGNAME_CAPITALPHI, 
CODEPOINT_LONGNAME_CAPITALCHI, CODEPOINT_LONGNAME_CAPITALPSI, CODEPOINT_LONGNAME_CAPITALOMEGA, CODEPOINT_LONGNAME_ALPHA,
 CODEPOINT_LONGNAME_BETA, CODEPOINT_LONGNAME_GAMMA, CODEPOINT_LONGNAME_DELTA, CODEPOINT_LONGNAME_CURLYEPSILON, 
CODEPOINT_LONGNAME_ZETA, CODEPOINT_LONGNAME_ETA, CODEPOINT_LONGNAME_THETA, CODEPOINT_LONGNAME_IOTA, 
CODEPOINT_LONGNAME_KAPPA, CODEPOINT_LONGNAME_LAMBDA, CODEPOINT_LONGNAME_MU, CODEPOINT_LONGNAME_NU, CODEPOINT_LONGNAME_XI
, CODEPOINT_LONGNAME_OMICRON, CODEPOINT_LONGNAME_PI, CODEPOINT_LONGNAME_RHO, CODEPOINT_LONGNAME_FINALSIGMA, 
CODEPOINT_LONGNAME_SIGMA, CODEPOINT_LONGNAME_TAU, CODEPOINT_LONGNAME_UPSILON, CODEPOINT_LONGNAME_CURLYPHI, 
CODEPOINT_LONGNAME_CHI, CODEPOINT_LONGNAME_PSI, CODEPOINT_LONGNAME_OMEGA, CODEPOINT_LONGNAME_CURLYTHETA, 
CODEPOINT_LONGNAME_CURLYCAPITALUPSILON, CODEPOINT_LONGNAME_PHI, CODEPOINT_LONGNAME_CURLYPI, 
CODEPOINT_LONGNAME_CAPITALSTIGMA, CODEPOINT_LONGNAME_STIGMA, CODEPOINT_LONGNAME_CAPITALDIGAMMA, 
CODEPOINT_LONGNAME_DIGAMMA, CODEPOINT_LONGNAME_CAPITALKOPPA, CODEPOINT_LONGNAME_KOPPA, CODEPOINT_LONGNAME_CAPITALSAMPI, 
CODEPOINT_LONGNAME_SAMPI, CODEPOINT_LONGNAME_CURLYKAPPA, CODEPOINT_LONGNAME_CURLYRHO, CODEPOINT_LONGNAME_EPSILON, 
CODEPOINT_LONGNAME_THICKSPACE, CODEPOINT_LONGNAME_THINSPACE, CODEPOINT_LONGNAME_VERYTHINSPACE, CODEPOINT_LONGNAME_HYPHEN
, CODEPOINT_LONGNAME_DASH, CODEPOINT_LONGNAME_LONGDASH, CODEPOINT_LONGNAME_OPENCURLYQUOTE, 
CODEPOINT_LONGNAME_CLOSECURLYQUOTE, CODEPOINT_LONGNAME_OPENCURLYDOUBLEQUOTE, CODEPOINT_LONGNAME_CLOSECURLYDOUBLEQUOTE, 
CODEPOINT_LONGNAME_DAGGER, CODEPOINT_LONGNAME_DOUBLEDAGGER, CODEPOINT_LONGNAME_BULLET, CODEPOINT_LONGNAME_ELLIPSIS, 
CODEPOINT_LONGNAME_LINESEPARATOR, CODEPOINT_LONGNAME_PARAGRAPHSEPARATOR, CODEPOINT_LONGNAME_PRIME, 
CODEPOINT_LONGNAME_DOUBLEPRIME, CODEPOINT_LONGNAME_REVERSEPRIME, CODEPOINT_LONGNAME_REVERSEDOUBLEPRIME, 
CODEPOINT_LONGNAME_SKELETONINDICATOR, CODEPOINT_LONGNAME_MEDIUMSPACE, CODEPOINT_LONGNAME_NOBREAK, 
CODEPOINT_LONGNAME_INVISIBLETIMES, CODEPOINT_LONGNAME_EURO, CODEPOINT_LONGNAME_RUPEE, CODEPOINT_LONGNAME_SCRIPTG, 
CODEPOINT_LONGNAME_SCRIPTCAPITALH, CODEPOINT_LONGNAME_GOTHICCAPITALH, CODEPOINT_LONGNAME_HBAR, 
CODEPOINT_LONGNAME_SCRIPTCAPITALI, CODEPOINT_LONGNAME_GOTHICCAPITALI, CODEPOINT_LONGNAME_SCRIPTCAPITALL, 
CODEPOINT_LONGNAME_SCRIPTL, CODEPOINT_LONGNAME_WEIERSTRASSP, CODEPOINT_LONGNAME_SCRIPTCAPITALR, 
CODEPOINT_LONGNAME_GOTHICCAPITALR, CODEPOINT_LONGNAME_TRADEMARK, CODEPOINT_LONGNAME_MHO, 
CODEPOINT_LONGNAME_GOTHICCAPITALZ, CODEPOINT_LONGNAME_ANGSTROM, CODEPOINT_LONGNAME_SCRIPTCAPITALB, 
CODEPOINT_LONGNAME_GOTHICCAPITALC, CODEPOINT_LONGNAME_SCRIPTE, CODEPOINT_LONGNAME_SCRIPTCAPITALE, 
CODEPOINT_LONGNAME_SCRIPTCAPITALF, CODEPOINT_LONGNAME_SCRIPTCAPITALM, CODEPOINT_LONGNAME_SCRIPTO, 
CODEPOINT_LONGNAME_ALEPH, CODEPOINT_LONGNAME_BET, CODEPOINT_LONGNAME_GIMEL, CODEPOINT_LONGNAME_DALET, 
CODEPOINT_LONGNAME_LEFTARROW, CODEPOINT_LONGNAME_UPARROW, CODEPOINT_LONGNAME_RIGHTARROW, CODEPOINT_LONGNAME_DOWNARROW, 
CODEPOINT_LONGNAME_LEFTRIGHTARROW, CODEPOINT_LONGNAME_UPDOWNARROW, CODEPOINT_LONGNAME_UPPERLEFTARROW, 
CODEPOINT_LONGNAME_UPPERRIGHTARROW, CODEPOINT_LONGNAME_LOWERRIGHTARROW, CODEPOINT_LONGNAME_LOWERLEFTARROW, 
CODEPOINT_LONGNAME_LEFTTEEARROW, CODEPOINT_LONGNAME_UPTEEARROW, CODEPOINT_LONGNAME_RIGHTTEEARROW, 
CODEPOINT_LONGNAME_DOWNTEEARROW, CODEPOINT_LONGNAME_RETURNINDICATOR, CODEPOINT_LONGNAME_LEFTVECTOR, 
CODEPOINT_LONGNAME_DOWNLEFTVECTOR, CODEPOINT_LONGNAME_RIGHTUPVECTOR, CODEPOINT_LONGNAME_LEFTUPVECTOR, 
CODEPOINT_LONGNAME_RIGHTVECTOR, CODEPOINT_LONGNAME_DOWNRIGHTVECTOR, CODEPOINT_LONGNAME_RIGHTDOWNVECTOR, 
CODEPOINT_LONGNAME_LEFTDOWNVECTOR, CODEPOINT_LONGNAME_RIGHTARROWLEFTARROW, CODEPOINT_LONGNAME_UPARROWDOWNARROW, 
CODEPOINT_LONGNAME_LEFTARROWRIGHTARROW, CODEPOINT_LONGNAME_REVERSEEQUILIBRIUM, CODEPOINT_LONGNAME_EQUILIBRIUM, 
CODEPOINT_LONGNAME_DOUBLELEFTARROW, CODEPOINT_LONGNAME_DOUBLEUPARROW, CODEPOINT_LONGNAME_DOUBLERIGHTARROW, 
CODEPOINT_LONGNAME_DOUBLEDOWNARROW, CODEPOINT_LONGNAME_DOUBLELEFTRIGHTARROW, CODEPOINT_LONGNAME_DOUBLEUPDOWNARROW, 
CODEPOINT_LONGNAME_LEFTARROWBAR, CODEPOINT_LONGNAME_RIGHTARROWBAR, CODEPOINT_LONGNAME_DOWNARROWUPARROW, 
CODEPOINT_LONGNAME_FORALL, CODEPOINT_LONGNAME_PARTIALD, CODEPOINT_LONGNAME_EXISTS, CODEPOINT_LONGNAME_NOTEXISTS, 
CODEPOINT_LONGNAME_EMPTYSET, CODEPOINT_LONGNAME_DEL, CODEPOINT_LONGNAME_ELEMENT, CODEPOINT_LONGNAME_NOTELEMENT, 
CODEPOINT_LONGNAME_REVERSEELEMENT, CODEPOINT_LONGNAME_NOTREVERSEELEMENT, CODEPOINT_LONGNAME_SUCHTHAT, 
CODEPOINT_LONGNAME_PRODUCT, CODEPOINT_LONGNAME_COPRODUCT, CODEPOINT_LONGNAME_SUM, CODEPOINT_LONGNAME_MINUS, 
CODEPOINT_LONGNAME_MINUSPLUS, CODEPOINT_LONGNAME_DIVISIONSLASH, CODEPOINT_LONGNAME_BACKSLASH, 
CODEPOINT_LONGNAME_SMALLCIRCLE, CODEPOINT_LONGNAME_SQRT, CODEPOINT_LONGNAME_CUBEROOT, CODEPOINT_LONGNAME_PROPORTIONAL, 
CODEPOINT_LONGNAME_INFINITY, CODEPOINT_LONGNAME_RIGHTANGLE, CODEPOINT_LONGNAME_ANGLE, CODEPOINT_LONGNAME_MEASUREDANGLE, 
CODEPOINT_LONGNAME_SPHERICALANGLE, CODEPOINT_LONGNAME_DIVIDES, CODEPOINT_LONGNAME_DOUBLEVERTICALBAR, 
CODEPOINT_LONGNAME_NOTDOUBLEVERTICALBAR, CODEPOINT_LONGNAME_AND, CODEPOINT_LONGNAME_OR, CODEPOINT_LONGNAME_INTEGRAL, 
CODEPOINT_LONGNAME_CONTOURINTEGRAL, CODEPOINT_LONGNAME_DOUBLECONTOURINTEGRAL, 
CODEPOINT_LONGNAME_CLOCKWISECONTOURINTEGRAL, CODEPOINT_LONGNAME_COUNTERCLOCKWISECONTOURINTEGRAL, 
CODEPOINT_LONGNAME_THEREFORE, CODEPOINT_LONGNAME_BECAUSE, CODEPOINT_LONGNAME_COLON, CODEPOINT_LONGNAME_PROPORTION, 
CODEPOINT_LONGNAME_TILDE, CODEPOINT_LONGNAME_VERTICALTILDE, CODEPOINT_LONGNAME_NOTTILDE, CODEPOINT_LONGNAME_EQUALTILDE, 
CODEPOINT_LONGNAME_TILDEEQUAL, CODEPOINT_LONGNAME_NOTTILDEEQUAL, CODEPOINT_LONGNAME_TILDEFULLEQUAL, 
CODEPOINT_LONGNAME_NOTTILDEFULLEQUAL, CODEPOINT_LONGNAME_TILDETILDE, CODEPOINT_LONGNAME_NOTTILDETILDE, 
CODEPOINT_LONGNAME_CUPCAP, CODEPOINT_LONGNAME_HUMPDOWNHUMP, CODEPOINT_LONGNAME_HUMPEQUAL, CODEPOINT_LONGNAME_DOTEQUAL, 
CODEPOINT_LONGNAME_NOTEQUAL, CODEPOINT_LONGNAME_CONGRUENT, CODEPOINT_LONGNAME_NOTCONGRUENT, CODEPOINT_LONGNAME_LESSEQUAL
, CODEPOINT_LONGNAME_GREATEREQUAL, CODEPOINT_LONGNAME_LESSFULLEQUAL, CODEPOINT_LONGNAME_GREATERFULLEQUAL, 
CODEPOINT_LONGNAME_NOTLESSFULLEQUAL, CODEPOINT_LONGNAME_NOTGREATERFULLEQUAL, CODEPOINT_LONGNAME_LESSLESS, 
CODEPOINT_LONGNAME_GREATERGREATER, CODEPOINT_LONGNAME_NOTCUPCAP, CODEPOINT_LONGNAME_NOTLESS, 
CODEPOINT_LONGNAME_NOTGREATER, CODEPOINT_LONGNAME_NOTLESSEQUAL, CODEPOINT_LONGNAME_NOTGREATEREQUAL, 
CODEPOINT_LONGNAME_LESSTILDE, CODEPOINT_LONGNAME_GREATERTILDE, CODEPOINT_LONGNAME_NOTLESSTILDE, 
CODEPOINT_LONGNAME_NOTGREATERTILDE, CODEPOINT_LONGNAME_LESSGREATER, CODEPOINT_LONGNAME_GREATERLESS, 
CODEPOINT_LONGNAME_NOTLESSGREATER, CODEPOINT_LONGNAME_NOTGREATERLESS, CODEPOINT_LONGNAME_PRECEDES, 
CODEPOINT_LONGNAME_SUCCEEDS, CODEPOINT_LONGNAME_PRECEDESSLANTEQUAL, CODEPOINT_LONGNAME_SUCCEEDSSLANTEQUAL, 
CODEPOINT_LONGNAME_PRECEDESTILDE, CODEPOINT_LONGNAME_SUCCEEDSTILDE, CODEPOINT_LONGNAME_NOTPRECEDES, 
CODEPOINT_LONGNAME_NOTSUCCEEDS, CODEPOINT_LONGNAME_SUBSET, CODEPOINT_LONGNAME_SUPERSET, CODEPOINT_LONGNAME_NOTSUBSET, 
CODEPOINT_LONGNAME_NOTSUPERSET, CODEPOINT_LONGNAME_SUBSETEQUAL, CODEPOINT_LONGNAME_SUPERSETEQUAL, 
CODEPOINT_LONGNAME_NOTSUBSETEQUAL, CODEPOINT_LONGNAME_NOTSUPERSETEQUAL, CODEPOINT_LONGNAME_UNIONPLUS, 
CODEPOINT_LONGNAME_SQUARESUBSET, CODEPOINT_LONGNAME_SQUARESUPERSET, CODEPOINT_LONGNAME_SQUARESUBSETEQUAL, 
CODEPOINT_LONGNAME_SQUARESUPERSETEQUAL, CODEPOINT_LONGNAME_SQUAREINTERSECTION, CODEPOINT_LONGNAME_SQUAREUNION, 
CODEPOINT_LONGNAME_CIRCLEPLUS, CODEPOINT_LONGNAME_CIRCLEMINUS, CODEPOINT_LONGNAME_CIRCLETIMES, 
CODEPOINT_LONGNAME_CIRCLEDOT, CODEPOINT_LONGNAME_RIGHTTEE, CODEPOINT_LONGNAME_LEFTTEE, CODEPOINT_LONGNAME_DOWNTEE, 
CODEPOINT_LONGNAME_UPTEE, CODEPOINT_LONGNAME_DOUBLERIGHTTEE, CODEPOINT_LONGNAME_LEFTTRIANGLE, 
CODEPOINT_LONGNAME_RIGHTTRIANGLE, CODEPOINT_LONGNAME_LEFTTRIANGLEEQUAL, CODEPOINT_LONGNAME_RIGHTTRIANGLEEQUAL, 
CODEPOINT_LONGNAME_XOR, CODEPOINT_LONGNAME_NAND, CODEPOINT_LONGNAME_NOR, CODEPOINT_LONGNAME_WEDGE, 
CODEPOINT_LONGNAME_VEE, CODEPOINT_LONGNAME_INTERSECTION, CODEPOINT_LONGNAME_UNION, CODEPOINT_LONGNAME_DIAMOND, 
CODEPOINT_LONGNAME_STAR, CODEPOINT_LONGNAME_LESSEQUALGREATER, CODEPOINT_LONGNAME_GREATEREQUALLESS, 
CODEPOINT_LONGNAME_NOTPRECEDESSLANTEQUAL, CODEPOINT_LONGNAME_NOTSUCCEEDSSLANTEQUAL, 
CODEPOINT_LONGNAME_NOTSQUARESUBSETEQUAL, CODEPOINT_LONGNAME_NOTSQUARESUPERSETEQUAL, CODEPOINT_LONGNAME_NOTPRECEDESTILDE,
 CODEPOINT_LONGNAME_NOTSUCCEEDSTILDE, CODEPOINT_LONGNAME_NOTLEFTTRIANGLE, CODEPOINT_LONGNAME_NOTRIGHTTRIANGLE, 
CODEPOINT_LONGNAME_NOTLEFTTRIANGLEEQUAL, CODEPOINT_LONGNAME_NOTRIGHTTRIANGLEEQUAL, CODEPOINT_LONGNAME_VERTICALELLIPSIS, 
CODEPOINT_LONGNAME_CENTERELLIPSIS, CODEPOINT_LONGNAME_ASCENDINGELLIPSIS, CODEPOINT_LONGNAME_DESCENDINGELLIPSIS, 
CODEPOINT_LONGNAME_DIAMETER, CODEPOINT_LONGNAME_LEFTCEILING, CODEPOINT_LONGNAME_RIGHTCEILING, 
CODEPOINT_LONGNAME_LEFTFLOOR, CODEPOINT_LONGNAME_RIGHTFLOOR, CODEPOINT_LONGNAME_CLOVERLEAF, CODEPOINT_LONGNAME_WATCHICON
, CODEPOINT_LONGNAME_CAP, CODEPOINT_LONGNAME_CUP, CODEPOINT_LONGNAME_LEFTANGLEBRACKET, 
CODEPOINT_LONGNAME_RIGHTANGLEBRACKET, CODEPOINT_LONGNAME_OVERBRACKET, CODEPOINT_LONGNAME_UNDERBRACKET, 
CODEPOINT_LONGNAME_SPACEINDICATOR, CODEPOINT_LONGNAME_HORIZONTALLINE, CODEPOINT_LONGNAME_VERTICALLINE, 
CODEPOINT_LONGNAME_FILLEDSQUARE, CODEPOINT_LONGNAME_EMPTYSQUARE, CODEPOINT_LONGNAME_FILLEDVERYSMALLSQUARE, 
CODEPOINT_LONGNAME_EMPTYVERYSMALLSQUARE, CODEPOINT_LONGNAME_FILLEDRECTANGLE, CODEPOINT_LONGNAME_EMPTYRECTANGLE, 
CODEPOINT_LONGNAME_FILLEDUPTRIANGLE, CODEPOINT_LONGNAME_EMPTYUPTRIANGLE, CODEPOINT_LONGNAME_UPPOINTER, 
CODEPOINT_LONGNAME_FILLEDRIGHTTRIANGLE, CODEPOINT_LONGNAME_RIGHTPOINTER, CODEPOINT_LONGNAME_FILLEDDOWNTRIANGLE, 
CODEPOINT_LONGNAME_EMPTYDOWNTRIANGLE, CODEPOINT_LONGNAME_DOWNPOINTER, CODEPOINT_LONGNAME_FILLEDLEFTTRIANGLE, 
CODEPOINT_LONGNAME_LEFTPOINTER, CODEPOINT_LONGNAME_FILLEDDIAMOND, CODEPOINT_LONGNAME_EMPTYDIAMOND, 
CODEPOINT_LONGNAME_EMPTYCIRCLE, CODEPOINT_LONGNAME_FILLEDCIRCLE, CODEPOINT_LONGNAME_EMPTYSMALLCIRCLE, 
CODEPOINT_LONGNAME_EMPTYSMALLSQUARE, CODEPOINT_LONGNAME_FILLEDSMALLSQUARE, CODEPOINT_LONGNAME_FIVEPOINTEDSTAR, 
CODEPOINT_LONGNAME_SUN, CODEPOINT_LONGNAME_CHECKMARKEDBOX, CODEPOINT_LONGNAME_CHECKEDBOX, CODEPOINT_LONGNAME_SADSMILEY, 
CODEPOINT_LONGNAME_HAPPYSMILEY, CODEPOINT_LONGNAME_MOON, CODEPOINT_LONGNAME_MERCURY, CODEPOINT_LONGNAME_VENUS, 
CODEPOINT_LONGNAME_MARS, CODEPOINT_LONGNAME_JUPITER, CODEPOINT_LONGNAME_SATURN, CODEPOINT_LONGNAME_NEPTUNE, 
CODEPOINT_LONGNAME_PLUTO, CODEPOINT_LONGNAME_ARIESSIGN, CODEPOINT_LONGNAME_TAURUSSIGN, CODEPOINT_LONGNAME_GEMINISIGN, 
CODEPOINT_LONGNAME_CANCERSIGN, CODEPOINT_LONGNAME_LEOSIGN, CODEPOINT_LONGNAME_VIRGOSIGN, CODEPOINT_LONGNAME_LIBRASIGN, 
CODEPOINT_LONGNAME_SCORPIOSIGN, CODEPOINT_LONGNAME_SAGITTARIUSSIGN, CODEPOINT_LONGNAME_CAPRICORNSIGN, 
CODEPOINT_LONGNAME_AQUARIUSSIGN, CODEPOINT_LONGNAME_PISCESSIGN, CODEPOINT_LONGNAME_WHITEKING, 
CODEPOINT_LONGNAME_WHITEQUEEN, CODEPOINT_LONGNAME_WHITEROOK, CODEPOINT_LONGNAME_WHITEBISHOP, 
CODEPOINT_LONGNAME_WHITEKNIGHT, CODEPOINT_LONGNAME_WHITEPAWN, CODEPOINT_LONGNAME_BLACKKING, 
CODEPOINT_LONGNAME_BLACKQUEEN, CODEPOINT_LONGNAME_BLACKROOK, CODEPOINT_LONGNAME_BLACKBISHOP, 
CODEPOINT_LONGNAME_BLACKKNIGHT, CODEPOINT_LONGNAME_BLACKPAWN, CODEPOINT_LONGNAME_SPADESUIT, CODEPOINT_LONGNAME_HEARTSUIT
, CODEPOINT_LONGNAME_DIAMONDSUIT, CODEPOINT_LONGNAME_CLUBSUIT, CODEPOINT_LONGNAME_QUARTERNOTE, 
CODEPOINT_LONGNAME_EIGHTHNOTE, CODEPOINT_LONGNAME_BEAMEDEIGHTHNOTE, CODEPOINT_LONGNAME_BEAMEDSIXTEENTHNOTE, 
CODEPOINT_LONGNAME_FLAT, CODEPOINT_LONGNAME_NATURAL, CODEPOINT_LONGNAME_SHARP, CODEPOINT_LONGNAME_URANUS, 
CODEPOINT_LONGNAME_CHECKMARK, CODEPOINT_LONGNAME_SIXPOINTEDSTAR, CODEPOINT_LONGNAME_PERPENDICULAR, 
CODEPOINT_LONGNAME_LONGLEFTARROW, CODEPOINT_LONGNAME_LONGRIGHTARROW, CODEPOINT_LONGNAME_LONGLEFTRIGHTARROW, 
CODEPOINT_LONGNAME_DOUBLELONGLEFTARROW, CODEPOINT_LONGNAME_DOUBLELONGRIGHTARROW, 
CODEPOINT_LONGNAME_DOUBLELONGLEFTRIGHTARROW, CODEPOINT_LONGNAME_UPARROWBAR, CODEPOINT_LONGNAME_DOWNARROWBAR, 
CODEPOINT_LONGNAME_LEFTRIGHTVECTOR, CODEPOINT_LONGNAME_RIGHTUPDOWNVECTOR, CODEPOINT_LONGNAME_DOWNLEFTRIGHTVECTOR, 
CODEPOINT_LONGNAME_LEFTUPDOWNVECTOR, CODEPOINT_LONGNAME_LEFTVECTORBAR, CODEPOINT_LONGNAME_RIGHTVECTORBAR, 
CODEPOINT_LONGNAME_RIGHTUPVECTORBAR, CODEPOINT_LONGNAME_RIGHTDOWNVECTORBAR, CODEPOINT_LONGNAME_DOWNLEFTVECTORBAR, 
CODEPOINT_LONGNAME_DOWNRIGHTVECTORBAR, CODEPOINT_LONGNAME_LEFTUPVECTORBAR, CODEPOINT_LONGNAME_LEFTDOWNVECTORBAR, 
CODEPOINT_LONGNAME_LEFTTEEVECTOR, CODEPOINT_LONGNAME_RIGHTTEEVECTOR, CODEPOINT_LONGNAME_RIGHTUPTEEVECTOR, 
CODEPOINT_LONGNAME_RIGHTDOWNTEEVECTOR, CODEPOINT_LONGNAME_DOWNLEFTTEEVECTOR, CODEPOINT_LONGNAME_DOWNRIGHTTEEVECTOR, 
CODEPOINT_LONGNAME_LEFTUPTEEVECTOR, CODEPOINT_LONGNAME_LEFTDOWNTEEVECTOR, CODEPOINT_LONGNAME_UPEQUILIBRIUM, 
CODEPOINT_LONGNAME_REVERSEUPEQUILIBRIUM, CODEPOINT_LONGNAME_ROUNDIMPLIES, CODEPOINT_LONGNAME_LEFTTRIANGLEBAR, 
CODEPOINT_LONGNAME_RIGHTTRIANGLEBAR, CODEPOINT_LONGNAME_EQUIVALENT, CODEPOINT_LONGNAME_LESSSLANTEQUAL, 
CODEPOINT_LONGNAME_GREATERSLANTEQUAL, CODEPOINT_LONGNAME_NESTEDLESSLESS, CODEPOINT_LONGNAME_NESTEDGREATERGREATER, 
CODEPOINT_LONGNAME_PRECEDESEQUAL, CODEPOINT_LONGNAME_SUCCEEDSEQUAL, CODEPOINT_LONGNAME_DOUBLELEFTTEE, 
CODEPOINT_LONGNAME_COMPATIBILITYKANJISPACE, CODEPOINT_LONGNAME_LEFTDOUBLEBRACKET, CODEPOINT_LONGNAME_RIGHTDOUBLEBRACKET,
 CODEPOINT_LONGNAME_LEFTASSOCIATION, CODEPOINT_LONGNAME_RIGHTASSOCIATION, CODEPOINT_LONGNAME_SHAH, 
CODEPOINT_LONGNAME_WOLFRAMLANGUAGELOGO, CODEPOINT_LONGNAME_WOLFRAMLANGUAGELOGOCIRCLE, CODEPOINT_LONGNAME_TWOWAYRULE, 
CODEPOINT_LONGNAME_FREEFORMPROMPT, CODEPOINT_LONGNAME_WOLFRAMALPHAPROMPT, CODEPOINT_LONGNAME_INVISIBLESPACE, 
CODEPOINT_LONGNAME_PIECEWISE, CODEPOINT_LONGNAME_NEGATIVEVERYTHINSPACE, CODEPOINT_LONGNAME_NEGATIVETHINSPACE, 
CODEPOINT_LONGNAME_NEGATIVEMEDIUMSPACE, CODEPOINT_LONGNAME_NEGATIVETHICKSPACE, CODEPOINT_LONGNAME_IMPLICITPLUS, 
CODEPOINT_LONGNAME_NULL, CODEPOINT_LONGNAME_COMPATIBILITYNOBREAK, CODEPOINT_LONGNAME_INDENTINGNEWLINE, 
CODEPOINT_LONGNAME_AUTOPLACEHOLDER, CODEPOINT_LONGNAME_AUTOLEFTMATCH, CODEPOINT_LONGNAME_AUTORIGHTMATCH, 
CODEPOINT_LONGNAME_AUTOSPACE, CODEPOINT_LONGNAME_AUTOOPERAND, CODEPOINT_LONGNAME_SYSTEMSMODELDELAY, 
CODEPOINT_LONGNAME_CONTINUATION, CODEPOINT_LONGNAME_ROUNDSPACEINDICATOR, CODEPOINT_LONGNAME_INVISIBLEPREFIXSCRIPTBASE, 
CODEPOINT_LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE, CODEPOINT_LONGNAME_ENTITYSTART, CODEPOINT_LONGNAME_ENTITYEND, 
CODEPOINT_LONGNAME_SPANFROMLEFT, CODEPOINT_LONGNAME_SPANFROMABOVE, CODEPOINT_LONGNAME_SPANFROMBOTH, 
CODEPOINT_LONGNAME_PAGEBREAKABOVE, CODEPOINT_LONGNAME_PAGEBREAKBELOW, CODEPOINT_LONGNAME_DISCRETIONARYPAGEBREAKABOVE, 
CODEPOINT_LONGNAME_DISCRETIONARYPAGEBREAKBELOW, CODEPOINT_LONGNAME_TRANSPOSE, CODEPOINT_LONGNAME_CONJUGATE, 
CODEPOINT_LONGNAME_CONJUGATETRANSPOSE, CODEPOINT_LONGNAME_STEPPERRIGHT, CODEPOINT_LONGNAME_STEPPERLEFT, 
CODEPOINT_LONGNAME_STEPPERUP, CODEPOINT_LONGNAME_STEPPERDOWN, CODEPOINT_LONGNAME_HERMITIANCONJUGATE, 
CODEPOINT_LONGNAME_VERTICALBAR, CODEPOINT_LONGNAME_NOTVERTICALBAR, CODEPOINT_LONGNAME_DISTRIBUTED, 
CODEPOINT_LONGNAME_CONDITIONED, CODEPOINT_LONGNAME_UNDIRECTEDEDGE, CODEPOINT_LONGNAME_DIRECTEDEDGE, 
CODEPOINT_LONGNAME_CONTINUEDFRACTIONK, CODEPOINT_LONGNAME_TENSORPRODUCT, CODEPOINT_LONGNAME_TENSORWEDGE, 
CODEPOINT_LONGNAME_PROBABILITYPR, CODEPOINT_LONGNAME_EXPECTATIONE, CODEPOINT_LONGNAME_PERMUTATIONPRODUCT, 
CODEPOINT_LONGNAME_EARTH, CODEPOINT_LONGNAME_NOTEQUALTILDE, CODEPOINT_LONGNAME_NOTHUMPEQUAL, 
CODEPOINT_LONGNAME_NOTHUMPDOWNHUMP, CODEPOINT_LONGNAME_NOTLEFTTRIANGLEBAR, CODEPOINT_LONGNAME_NOTRIGHTTRIANGLEBAR, 
CODEPOINT_LONGNAME_NOTLESSLESS, CODEPOINT_LONGNAME_NOTNESTEDLESSLESS, CODEPOINT_LONGNAME_NOTLESSSLANTEQUAL, 
CODEPOINT_LONGNAME_NOTGREATERGREATER, CODEPOINT_LONGNAME_NOTNESTEDGREATERGREATER, 
CODEPOINT_LONGNAME_NOTGREATERSLANTEQUAL, CODEPOINT_LONGNAME_NOTPRECEDESEQUAL, CODEPOINT_LONGNAME_NOTSUCCEEDSEQUAL, 
CODEPOINT_LONGNAME_NOTSQUARESUBSET, CODEPOINT_LONGNAME_NOTSQUARESUPERSET, CODEPOINT_LONGNAME_EQUAL, 
CODEPOINT_LONGNAME_VERTICALSEPARATOR, CODEPOINT_LONGNAME_VECTORGREATER, CODEPOINT_LONGNAME_VECTORGREATEREQUAL, 
CODEPOINT_LONGNAME_VECTORLESS, CODEPOINT_LONGNAME_VECTORLESSEQUAL, CODEPOINT_LONGNAME_LIMIT, CODEPOINT_LONGNAME_MAXLIMIT
, CODEPOINT_LONGNAME_MINLIMIT, CODEPOINT_LONGNAME_CROSS, CODEPOINT_LONGNAME_FUNCTION, CODEPOINT_LONGNAME_XNOR, 
CODEPOINT_LONGNAME_DISCRETESHIFT, CODEPOINT_LONGNAME_DIFFERENCEDELTA, CODEPOINT_LONGNAME_DISCRETERATIO, 
CODEPOINT_LONGNAME_INLINEPART, CODEPOINT_LONGNAME_RULEDELAYED, CODEPOINT_LONGNAME_SQUARE, CODEPOINT_LONGNAME_RULE, 
CODEPOINT_LONGNAME_IMPLIES, CODEPOINT_LONGNAME_SHORTRIGHTARROW, CODEPOINT_LONGNAME_SHORTLEFTARROW, 
CODEPOINT_LONGNAME_SELECTIONPLACEHOLDER, CODEPOINT_LONGNAME_PLACEHOLDER, CODEPOINT_LONGNAME_SHORTUPARROW, 
CODEPOINT_LONGNAME_SHORTDOWNARROW, CODEPOINT_LONGNAME_APPLICATION, CODEPOINT_LONGNAME_LEFTBRACKETINGBAR, 
CODEPOINT_LONGNAME_RIGHTBRACKETINGBAR, CODEPOINT_LONGNAME_LEFTDOUBLEBRACKETINGBAR, 
CODEPOINT_LONGNAME_RIGHTDOUBLEBRACKETINGBAR, CODEPOINT_LONGNAME_SCRIPTA, CODEPOINT_LONGNAME_SCRIPTB, 
CODEPOINT_LONGNAME_SCRIPTC, CODEPOINT_LONGNAME_SCRIPTD, CODEPOINT_LONGNAME_SCRIPTF, CODEPOINT_LONGNAME_SCRIPTH, 
CODEPOINT_LONGNAME_SCRIPTI, CODEPOINT_LONGNAME_SCRIPTJ, CODEPOINT_LONGNAME_SCRIPTK, CODEPOINT_LONGNAME_SCRIPTM, 
CODEPOINT_LONGNAME_SCRIPTN, CODEPOINT_LONGNAME_SCRIPTP, CODEPOINT_LONGNAME_SCRIPTQ, CODEPOINT_LONGNAME_SCRIPTR, 
CODEPOINT_LONGNAME_SCRIPTS, CODEPOINT_LONGNAME_SCRIPTT, CODEPOINT_LONGNAME_SCRIPTU, CODEPOINT_LONGNAME_SCRIPTV, 
CODEPOINT_LONGNAME_SCRIPTW, CODEPOINT_LONGNAME_SCRIPTX, CODEPOINT_LONGNAME_SCRIPTY, CODEPOINT_LONGNAME_SCRIPTZ, 
CODEPOINT_LONGNAME_GOTHICA, CODEPOINT_LONGNAME_GOTHICB, CODEPOINT_LONGNAME_GOTHICC, CODEPOINT_LONGNAME_GOTHICD, 
CODEPOINT_LONGNAME_GOTHICE, CODEPOINT_LONGNAME_GOTHICF, CODEPOINT_LONGNAME_GOTHICG, CODEPOINT_LONGNAME_GOTHICH, 
CODEPOINT_LONGNAME_GOTHICI, CODEPOINT_LONGNAME_GOTHICJ, CODEPOINT_LONGNAME_GOTHICK, CODEPOINT_LONGNAME_GOTHICL, 
CODEPOINT_LONGNAME_GOTHICM, CODEPOINT_LONGNAME_GOTHICN, CODEPOINT_LONGNAME_GOTHICO, CODEPOINT_LONGNAME_GOTHICP, 
CODEPOINT_LONGNAME_GOTHICQ, CODEPOINT_LONGNAME_GOTHICR, CODEPOINT_LONGNAME_GOTHICS, CODEPOINT_LONGNAME_GOTHICT, 
CODEPOINT_LONGNAME_GOTHICU, CODEPOINT_LONGNAME_GOTHICV, CODEPOINT_LONGNAME_GOTHICW, CODEPOINT_LONGNAME_GOTHICX, 
CODEPOINT_LONGNAME_GOTHICY, CODEPOINT_LONGNAME_GOTHICZ, CODEPOINT_LONGNAME_DOUBLESTRUCKA, 
CODEPOINT_LONGNAME_DOUBLESTRUCKB, CODEPOINT_LONGNAME_DOUBLESTRUCKC, CODEPOINT_LONGNAME_DOUBLESTRUCKD, 
CODEPOINT_LONGNAME_DOUBLESTRUCKE, CODEPOINT_LONGNAME_DOUBLESTRUCKF, CODEPOINT_LONGNAME_DOUBLESTRUCKG, 
CODEPOINT_LONGNAME_DOUBLESTRUCKH, CODEPOINT_LONGNAME_DOUBLESTRUCKI, CODEPOINT_LONGNAME_DOUBLESTRUCKJ, 
CODEPOINT_LONGNAME_DOUBLESTRUCKK, CODEPOINT_LONGNAME_DOUBLESTRUCKL, CODEPOINT_LONGNAME_DOUBLESTRUCKM, 
CODEPOINT_LONGNAME_DOUBLESTRUCKN, CODEPOINT_LONGNAME_DOUBLESTRUCKO, CODEPOINT_LONGNAME_DOUBLESTRUCKP, 
CODEPOINT_LONGNAME_DOUBLESTRUCKQ, CODEPOINT_LONGNAME_DOUBLESTRUCKR, CODEPOINT_LONGNAME_DOUBLESTRUCKS, 
CODEPOINT_LONGNAME_DOUBLESTRUCKT, CODEPOINT_LONGNAME_DOUBLESTRUCKU, CODEPOINT_LONGNAME_DOUBLESTRUCKV, 
CODEPOINT_LONGNAME_DOUBLESTRUCKW, CODEPOINT_LONGNAME_DOUBLESTRUCKX, CODEPOINT_LONGNAME_DOUBLESTRUCKY, 
CODEPOINT_LONGNAME_DOUBLESTRUCKZ, CODEPOINT_LONGNAME_DOTLESSJ, CODEPOINT_LONGNAME_WOLF, CODEPOINT_LONGNAME_FREAKEDSMILEY
, CODEPOINT_LONGNAME_NEUTRALSMILEY, CODEPOINT_LONGNAME_LIGHTBULB, CODEPOINT_LONGNAME_NUMBERSIGN, 
CODEPOINT_LONGNAME_WARNINGSIGN, CODEPOINT_LONGNAME_VILLA, CODEPOINT_LONGNAME_AKUZ, CODEPOINT_LONGNAME_ANDY, 
CODEPOINT_LONGNAME_SPOOKY, CODEPOINT_LONGNAME_SCRIPTDOTLESSI, CODEPOINT_LONGNAME_SCRIPTDOTLESSJ, 
CODEPOINT_LONGNAME_DOUBLEDPI, CODEPOINT_LONGNAME_DOUBLEDGAMMA, CODEPOINT_LONGNAME_CAPITALDIFFERENTIALD, 
CODEPOINT_LONGNAME_DIFFERENTIALD, CODEPOINT_LONGNAME_EXPONENTIALE, CODEPOINT_LONGNAME_IMAGINARYI, 
CODEPOINT_LONGNAME_IMAGINARYJ, CODEPOINT_LONGNAME_FILLEDSMALLCIRCLE, CODEPOINT_LONGNAME_DOTTEDSQUARE, 
CODEPOINT_LONGNAME_GRAYSQUARE, CODEPOINT_LONGNAME_GRAYCIRCLE, CODEPOINT_LONGNAME_LETTERSPACE, 
CODEPOINT_LONGNAME_DOWNBREVE, CODEPOINT_LONGNAME_KERNELICON, CODEPOINT_LONGNAME_MATHEMATICAICON, 
CODEPOINT_LONGNAME_TRIPLEDOT, CODEPOINT_LONGNAME_SYSTEMENTERKEY, CODEPOINT_LONGNAME_ALIGNMENTMARKER, 
CODEPOINT_LONGNAME_LEFTSKELETON, CODEPOINT_LONGNAME_RIGHTSKELETON, CODEPOINT_LONGNAME_CONTROLKEY, 
CODEPOINT_LONGNAME_ALIASDELIMITER, CODEPOINT_LONGNAME_INVISIBLECOMMA, CODEPOINT_LONGNAME_RETURNKEY, 
CODEPOINT_LONGNAME_ERRORINDICATOR, CODEPOINT_LONGNAME_ALIASINDICATOR, CODEPOINT_LONGNAME_ESCAPEKEY, 
CODEPOINT_LONGNAME_COMMANDKEY, CODEPOINT_LONGNAME_LEFTMODIFIED, CODEPOINT_LONGNAME_RIGHTMODIFIED, 
CODEPOINT_LONGNAME_INVISIBLEAPPLICATION, CODEPOINT_LONGNAME_DISCRETIONARYLINESEPARATOR, 
CODEPOINT_LONGNAME_DISCRETIONARYPARAGRAPHSEPARATOR, CODEPOINT_LONGNAME_SCRIPTCAPITALA, CODEPOINT_LONGNAME_SCRIPTCAPITALC
, CODEPOINT_LONGNAME_SCRIPTCAPITALD, CODEPOINT_LONGNAME_SCRIPTCAPITALG, CODEPOINT_LONGNAME_SCRIPTCAPITALJ, 
CODEPOINT_LONGNAME_SCRIPTCAPITALK, CODEPOINT_LONGNAME_SCRIPTCAPITALN, CODEPOINT_LONGNAME_SCRIPTCAPITALO, 
CODEPOINT_LONGNAME_SCRIPTCAPITALP, CODEPOINT_LONGNAME_SCRIPTCAPITALQ, CODEPOINT_LONGNAME_SCRIPTCAPITALS, 
CODEPOINT_LONGNAME_SCRIPTCAPITALT, CODEPOINT_LONGNAME_SCRIPTCAPITALU, CODEPOINT_LONGNAME_SCRIPTCAPITALV, 
CODEPOINT_LONGNAME_SCRIPTCAPITALW, CODEPOINT_LONGNAME_SCRIPTCAPITALX, CODEPOINT_LONGNAME_SCRIPTCAPITALY, 
CODEPOINT_LONGNAME_SCRIPTCAPITALZ, CODEPOINT_LONGNAME_GOTHICCAPITALA, CODEPOINT_LONGNAME_GOTHICCAPITALB, 
CODEPOINT_LONGNAME_GOTHICCAPITALD, CODEPOINT_LONGNAME_GOTHICCAPITALE, CODEPOINT_LONGNAME_GOTHICCAPITALF, 
CODEPOINT_LONGNAME_GOTHICCAPITALG, CODEPOINT_LONGNAME_GOTHICCAPITALJ, CODEPOINT_LONGNAME_GOTHICCAPITALK, 
CODEPOINT_LONGNAME_GOTHICCAPITALL, CODEPOINT_LONGNAME_GOTHICCAPITALM, CODEPOINT_LONGNAME_GOTHICCAPITALN, 
CODEPOINT_LONGNAME_GOTHICCAPITALO, CODEPOINT_LONGNAME_GOTHICCAPITALP, CODEPOINT_LONGNAME_GOTHICCAPITALQ, 
CODEPOINT_LONGNAME_GOTHICCAPITALS, CODEPOINT_LONGNAME_GOTHICCAPITALT, CODEPOINT_LONGNAME_GOTHICCAPITALU, 
CODEPOINT_LONGNAME_GOTHICCAPITALV, CODEPOINT_LONGNAME_GOTHICCAPITALW, CODEPOINT_LONGNAME_GOTHICCAPITALX, 
CODEPOINT_LONGNAME_GOTHICCAPITALY, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALA, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALB, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALC, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALD, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALE, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALF, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALG, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALH, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALI, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALJ, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALK, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALL, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALM, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALN, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALO, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALP, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALQ, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALR, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALS, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALT, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALU, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALV, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALW, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALX, 
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALY, CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALZ, CODEPOINT_LONGNAME_TABKEY, 
CODEPOINT_LONGNAME_SPACEKEY, CODEPOINT_LONGNAME_DELETEKEY, CODEPOINT_LONGNAME_ALTKEY, CODEPOINT_LONGNAME_OPTIONKEY, 
CODEPOINT_LONGNAME_KEYBAR, CODEPOINT_LONGNAME_ENTERKEY, CODEPOINT_LONGNAME_SHIFTKEY, CODEPOINT_LONGNAME_MOD1KEY, 
CODEPOINT_LONGNAME_MOD2KEY, CODEPOINT_LONGNAME_LONGEQUAL, CODEPOINT_LONGNAME_CONSTANTC, 
CODEPOINT_LONGNAME_DOUBLESTRUCKZERO, CODEPOINT_LONGNAME_DOUBLESTRUCKONE, CODEPOINT_LONGNAME_DOUBLESTRUCKTWO, 
CODEPOINT_LONGNAME_DOUBLESTRUCKTHREE, CODEPOINT_LONGNAME_DOUBLESTRUCKFOUR, CODEPOINT_LONGNAME_DOUBLESTRUCKFIVE, 
CODEPOINT_LONGNAME_DOUBLESTRUCKSIX, CODEPOINT_LONGNAME_DOUBLESTRUCKSEVEN, CODEPOINT_LONGNAME_DOUBLESTRUCKEIGHT, 
CODEPOINT_LONGNAME_DOUBLESTRUCKNINE, CODEPOINT_LONGNAME_GOTHICZERO, CODEPOINT_LONGNAME_GOTHICONE, 
CODEPOINT_LONGNAME_GOTHICTWO, CODEPOINT_LONGNAME_GOTHICTHREE, CODEPOINT_LONGNAME_GOTHICFOUR, 
CODEPOINT_LONGNAME_GOTHICFIVE, CODEPOINT_LONGNAME_GOTHICSIX, CODEPOINT_LONGNAME_GOTHICSEVEN, 
CODEPOINT_LONGNAME_GOTHICEIGHT, CODEPOINT_LONGNAME_GOTHICNINE, CODEPOINT_LONGNAME_SCRIPTZERO, 
CODEPOINT_LONGNAME_SCRIPTONE, CODEPOINT_LONGNAME_SCRIPTTWO, CODEPOINT_LONGNAME_SCRIPTTHREE, 
CODEPOINT_LONGNAME_SCRIPTFOUR, CODEPOINT_LONGNAME_SCRIPTFIVE, CODEPOINT_LONGNAME_SCRIPTSIX, 
CODEPOINT_LONGNAME_SCRIPTSEVEN, CODEPOINT_LONGNAME_SCRIPTEIGHT, CODEPOINT_LONGNAME_SCRIPTNINE, 
CODEPOINT_LONGNAME_FIRSTPAGE, CODEPOINT_LONGNAME_LASTPAGE, CODEPOINT_LONGNAME_NUMBERCOMMA, CODEPOINT_LONGNAME_FORMALA, 
CODEPOINT_LONGNAME_FORMALB, CODEPOINT_LONGNAME_FORMALC, CODEPOINT_LONGNAME_FORMALD, CODEPOINT_LONGNAME_FORMALE, 
CODEPOINT_LONGNAME_FORMALF, CODEPOINT_LONGNAME_FORMALG, CODEPOINT_LONGNAME_FORMALH, CODEPOINT_LONGNAME_FORMALI, 
CODEPOINT_LONGNAME_FORMALJ, CODEPOINT_LONGNAME_FORMALK, CODEPOINT_LONGNAME_FORMALL, CODEPOINT_LONGNAME_FORMALM, 
CODEPOINT_LONGNAME_FORMALN, CODEPOINT_LONGNAME_FORMALO, CODEPOINT_LONGNAME_FORMALP, CODEPOINT_LONGNAME_FORMALQ, 
CODEPOINT_LONGNAME_FORMALR, CODEPOINT_LONGNAME_FORMALS, CODEPOINT_LONGNAME_FORMALT, CODEPOINT_LONGNAME_FORMALU, 
CODEPOINT_LONGNAME_FORMALV, CODEPOINT_LONGNAME_FORMALW, CODEPOINT_LONGNAME_FORMALX, CODEPOINT_LONGNAME_FORMALY, 
CODEPOINT_LONGNAME_FORMALZ, CODEPOINT_LONGNAME_FORMALCAPITALA, CODEPOINT_LONGNAME_FORMALCAPITALB, 
CODEPOINT_LONGNAME_FORMALCAPITALC, CODEPOINT_LONGNAME_FORMALCAPITALD, CODEPOINT_LONGNAME_FORMALCAPITALE, 
CODEPOINT_LONGNAME_FORMALCAPITALF, CODEPOINT_LONGNAME_FORMALCAPITALG, CODEPOINT_LONGNAME_FORMALCAPITALH, 
CODEPOINT_LONGNAME_FORMALCAPITALI, CODEPOINT_LONGNAME_FORMALCAPITALJ, CODEPOINT_LONGNAME_FORMALCAPITALK, 
CODEPOINT_LONGNAME_FORMALCAPITALL, CODEPOINT_LONGNAME_FORMALCAPITALM, CODEPOINT_LONGNAME_FORMALCAPITALN, 
CODEPOINT_LONGNAME_FORMALCAPITALO, CODEPOINT_LONGNAME_FORMALCAPITALP, CODEPOINT_LONGNAME_FORMALCAPITALQ, 
CODEPOINT_LONGNAME_FORMALCAPITALR, CODEPOINT_LONGNAME_FORMALCAPITALS, CODEPOINT_LONGNAME_FORMALCAPITALT, 
CODEPOINT_LONGNAME_FORMALCAPITALU, CODEPOINT_LONGNAME_FORMALCAPITALV, CODEPOINT_LONGNAME_FORMALCAPITALW, 
CODEPOINT_LONGNAME_FORMALCAPITALX, CODEPOINT_LONGNAME_FORMALCAPITALY, CODEPOINT_LONGNAME_FORMALCAPITALZ, 
CODEPOINT_LONGNAME_FORMALCAPITALALPHA, CODEPOINT_LONGNAME_FORMALCAPITALBETA, CODEPOINT_LONGNAME_FORMALCAPITALGAMMA, 
CODEPOINT_LONGNAME_FORMALCAPITALDELTA, CODEPOINT_LONGNAME_FORMALCAPITALEPSILON, CODEPOINT_LONGNAME_FORMALCAPITALZETA, 
CODEPOINT_LONGNAME_FORMALCAPITALETA, CODEPOINT_LONGNAME_FORMALCAPITALTHETA, CODEPOINT_LONGNAME_FORMALCAPITALIOTA, 
CODEPOINT_LONGNAME_FORMALCAPITALKAPPA, CODEPOINT_LONGNAME_FORMALCAPITALLAMBDA, CODEPOINT_LONGNAME_FORMALCAPITALMU, 
CODEPOINT_LONGNAME_FORMALCAPITALNU, CODEPOINT_LONGNAME_FORMALCAPITALXI, CODEPOINT_LONGNAME_FORMALCAPITALOMICRON, 
CODEPOINT_LONGNAME_FORMALCAPITALPI, CODEPOINT_LONGNAME_FORMALCAPITALRHO, CODEPOINT_LONGNAME_FORMALCAPITALSIGMA, 
CODEPOINT_LONGNAME_FORMALCAPITALTAU, CODEPOINT_LONGNAME_FORMALCAPITALUPSILON, CODEPOINT_LONGNAME_FORMALCAPITALPHI, 
CODEPOINT_LONGNAME_FORMALCAPITALCHI, CODEPOINT_LONGNAME_FORMALCAPITALPSI, CODEPOINT_LONGNAME_FORMALCAPITALOMEGA, 
CODEPOINT_LONGNAME_FORMALALPHA, CODEPOINT_LONGNAME_FORMALBETA, CODEPOINT_LONGNAME_FORMALGAMMA, 
CODEPOINT_LONGNAME_FORMALDELTA, CODEPOINT_LONGNAME_FORMALCURLYEPSILON, CODEPOINT_LONGNAME_FORMALZETA, 
CODEPOINT_LONGNAME_FORMALETA, CODEPOINT_LONGNAME_FORMALTHETA, CODEPOINT_LONGNAME_FORMALIOTA, 
CODEPOINT_LONGNAME_FORMALKAPPA, CODEPOINT_LONGNAME_FORMALLAMBDA, CODEPOINT_LONGNAME_FORMALMU, 
CODEPOINT_LONGNAME_FORMALNU, CODEPOINT_LONGNAME_FORMALXI, CODEPOINT_LONGNAME_FORMALOMICRON, CODEPOINT_LONGNAME_FORMALPI,
 CODEPOINT_LONGNAME_FORMALRHO, CODEPOINT_LONGNAME_FORMALFINALSIGMA, CODEPOINT_LONGNAME_FORMALSIGMA, 
CODEPOINT_LONGNAME_FORMALTAU, CODEPOINT_LONGNAME_FORMALUPSILON, CODEPOINT_LONGNAME_FORMALCURLYPHI, 
CODEPOINT_LONGNAME_FORMALCHI, CODEPOINT_LONGNAME_FORMALPSI, CODEPOINT_LONGNAME_FORMALOMEGA, 
CODEPOINT_LONGNAME_FORMALCURLYTHETA, CODEPOINT_LONGNAME_FORMALCURLYCAPITALUPSILON, CODEPOINT_LONGNAME_FORMALPHI, 
CODEPOINT_LONGNAME_FORMALCURLYPI, CODEPOINT_LONGNAME_FORMALCAPITALSTIGMA, CODEPOINT_LONGNAME_FORMALSTIGMA, 
CODEPOINT_LONGNAME_FORMALCAPITALDIGAMMA, CODEPOINT_LONGNAME_FORMALDIGAMMA, CODEPOINT_LONGNAME_FORMALCAPITALKOPPA, 
CODEPOINT_LONGNAME_FORMALKOPPA, CODEPOINT_LONGNAME_FORMALCAPITALSAMPI, CODEPOINT_LONGNAME_FORMALSAMPI, 
CODEPOINT_LONGNAME_FORMALCURLYKAPPA, CODEPOINT_LONGNAME_FORMALCURLYRHO, CODEPOINT_LONGNAME_FORMALEPSILON, 
CODEPOINT_LONGNAME_FILIGATURE, CODEPOINT_LONGNAME_FLLIGATURE, CODEPOINT_LONGNAME_OVERPARENTHESIS, 
CODEPOINT_LONGNAME_UNDERPARENTHESIS, CODEPOINT_LONGNAME_OVERBRACE, CODEPOINT_LONGNAME_UNDERBRACE, 
CODEPOINT_LONGNAME_UNKNOWNGLYPH, CODEPOINT_LONGNAME_FORMALSCRIPTA, CODEPOINT_LONGNAME_FORMALSCRIPTB, 
CODEPOINT_LONGNAME_FORMALSCRIPTC, CODEPOINT_LONGNAME_FORMALSCRIPTD, CODEPOINT_LONGNAME_FORMALSCRIPTE, 
CODEPOINT_LONGNAME_FORMALSCRIPTF, CODEPOINT_LONGNAME_FORMALSCRIPTG, CODEPOINT_LONGNAME_FORMALSCRIPTH, 
CODEPOINT_LONGNAME_FORMALSCRIPTI, CODEPOINT_LONGNAME_FORMALSCRIPTJ, CODEPOINT_LONGNAME_FORMALSCRIPTK, 
CODEPOINT_LONGNAME_FORMALSCRIPTL, CODEPOINT_LONGNAME_FORMALSCRIPTM, CODEPOINT_LONGNAME_FORMALSCRIPTN, 
CODEPOINT_LONGNAME_FORMALSCRIPTO, CODEPOINT_LONGNAME_FORMALSCRIPTP, CODEPOINT_LONGNAME_FORMALSCRIPTQ, 
CODEPOINT_LONGNAME_FORMALSCRIPTR, CODEPOINT_LONGNAME_FORMALSCRIPTS, CODEPOINT_LONGNAME_FORMALSCRIPTT, 
CODEPOINT_LONGNAME_FORMALSCRIPTU, CODEPOINT_LONGNAME_FORMALSCRIPTV, CODEPOINT_LONGNAME_FORMALSCRIPTW, 
CODEPOINT_LONGNAME_FORMALSCRIPTX, CODEPOINT_LONGNAME_FORMALSCRIPTY, CODEPOINT_LONGNAME_FORMALSCRIPTZ, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALA, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALB, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALC, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALD, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALE, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALF, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALG, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALH, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALI, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALJ, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALK, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALL, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALM, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALN, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALO, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALP, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALQ, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALR, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALS, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALT, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALU, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALV, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALW, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALX, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALY, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALZ, 
];

//
//
//
pub const CODE_POINT_TO_LONGNAME_MAP__NAMES: [&str; LONGNAMES_COUNT] = [
"RawTab", "NewLine", "RawReturn", "RawEscape", "RawSpace", "RawExclamation", "RawDoubleQuote", "RawNumberSign", 
"RawDollar", "RawPercent", "RawAmpersand", "RawQuote", "RawLeftParenthesis", "RawRightParenthesis", "RawStar", "RawPlus"
, "RawComma", "RawDash", "RawDot", "RawSlash", "RawColon", "RawSemicolon", "RawLess", "RawEqual", "RawGreater", 
"RawQuestion", "RawAt", "RawLeftBracket", "RawBackslash", "RawRightBracket", "RawWedge", "RawUnderscore", "RawBackquote"
, "RawLeftBrace", "RawVerticalBar", "RawRightBrace", "RawTilde", "NonBreakingSpace", "DownExclamation", "Cent", 
"Sterling", "Currency", "Yen", "Section", "DoubleDot", "Copyright", "LeftGuillemet", "Not", "DiscretionaryHyphen", 
"RegisteredTrademark", "Degree", "PlusMinus", "Micro", "Paragraph", "CenterDot", "Cedilla", "RightGuillemet", 
"DownQuestion", "CapitalAGrave", "CapitalAAcute", "CapitalAHat", "CapitalATilde", "CapitalADoubleDot", "CapitalARing", 
"CapitalAE", "CapitalCCedilla", "CapitalEGrave", "CapitalEAcute", "CapitalEHat", "CapitalEDoubleDot", "CapitalIGrave", 
"CapitalIAcute", "CapitalIHat", "CapitalIDoubleDot", "CapitalEth", "CapitalNTilde", "CapitalOGrave", "CapitalOAcute", 
"CapitalOHat", "CapitalOTilde", "CapitalODoubleDot", "Times", "CapitalOSlash", "CapitalUGrave", "CapitalUAcute", 
"CapitalUHat", "CapitalUDoubleDot", "CapitalYAcute", "CapitalThorn", "SZ", "AGrave", "AAcute", "AHat", "ATilde", 
"ADoubleDot", "ARing", "AE", "CCedilla", "EGrave", "EAcute", "EHat", "EDoubleDot", "IGrave", "IAcute", "IHat", 
"IDoubleDot", "Eth", "NTilde", "OGrave", "OAcute", "OHat", "OTilde", "ODoubleDot", "Divide", "OSlash", "UGrave", 
"UAcute", "UHat", "UDoubleDot", "YAcute", "Thorn", "YDoubleDot", "CapitalABar", "ABar", "CapitalACup", "ACup", 
"CapitalCAcute", "CAcute", "CapitalCHacek", "CHacek", "CapitalDHacek", "DHacek", "CapitalEBar", "EBar", "CapitalECup", 
"ECup", "CapitalEHacek", "EHacek", "CapitalICup", "ICup", "DotlessI", "CapitalLSlash", "LSlash", "CapitalNHacek", 
"NHacek", "CapitalODoubleAcute", "ODoubleAcute", "CapitalOE", "OE", "CapitalRHacek", "RHacek", "CapitalSHacek", "SHacek"
, "CapitalTHacek", "THacek", "CapitalURing", "URing", "CapitalUDoubleAcute", "UDoubleAcute", "CapitalZHacek", "ZHacek", 
"Florin", "Hacek", "Breve", "CapitalAlpha", "CapitalBeta", "CapitalGamma", "CapitalDelta", "CapitalEpsilon", 
"CapitalZeta", "CapitalEta", "CapitalTheta", "CapitalIota", "CapitalKappa", "CapitalLambda", "CapitalMu", "CapitalNu", 
"CapitalXi", "CapitalOmicron", "CapitalPi", "CapitalRho", "CapitalSigma", "CapitalTau", "CapitalUpsilon", "CapitalPhi", 
"CapitalChi", "CapitalPsi", "CapitalOmega", "Alpha", "Beta", "Gamma", "Delta", "CurlyEpsilon", "Zeta", "Eta", "Theta", 
"Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi", "Rho", "FinalSigma", "Sigma", "Tau", "Upsilon", "CurlyPhi"
, "Chi", "Psi", "Omega", "CurlyTheta", "CurlyCapitalUpsilon", "Phi", "CurlyPi", "CapitalStigma", "Stigma", 
"CapitalDigamma", "Digamma", "CapitalKoppa", "Koppa", "CapitalSampi", "Sampi", "CurlyKappa", "CurlyRho", "Epsilon", 
"ThickSpace", "ThinSpace", "VeryThinSpace", "Hyphen", "Dash", "LongDash", "OpenCurlyQuote", "CloseCurlyQuote", 
"OpenCurlyDoubleQuote", "CloseCurlyDoubleQuote", "Dagger", "DoubleDagger", "Bullet", "Ellipsis", "LineSeparator", 
"ParagraphSeparator", "Prime", "DoublePrime", "ReversePrime", "ReverseDoublePrime", "SkeletonIndicator", "MediumSpace", 
"NoBreak", "InvisibleTimes", "Euro", "Rupee", "ScriptG", "ScriptCapitalH", "GothicCapitalH", "HBar", "ScriptCapitalI", 
"GothicCapitalI", "ScriptCapitalL", "ScriptL", "WeierstrassP", "ScriptCapitalR", "GothicCapitalR", "Trademark", "Mho", 
"GothicCapitalZ", "Angstrom", "ScriptCapitalB", "GothicCapitalC", "ScriptE", "ScriptCapitalE", "ScriptCapitalF", 
"ScriptCapitalM", "ScriptO", "Aleph", "Bet", "Gimel", "Dalet", "LeftArrow", "UpArrow", "RightArrow", "DownArrow", 
"LeftRightArrow", "UpDownArrow", "UpperLeftArrow", "UpperRightArrow", "LowerRightArrow", "LowerLeftArrow", 
"LeftTeeArrow", "UpTeeArrow", "RightTeeArrow", "DownTeeArrow", "ReturnIndicator", "LeftVector", "DownLeftVector", 
"RightUpVector", "LeftUpVector", "RightVector", "DownRightVector", "RightDownVector", "LeftDownVector", 
"RightArrowLeftArrow", "UpArrowDownArrow", "LeftArrowRightArrow", "ReverseEquilibrium", "Equilibrium", "DoubleLeftArrow"
, "DoubleUpArrow", "DoubleRightArrow", "DoubleDownArrow", "DoubleLeftRightArrow", "DoubleUpDownArrow", "LeftArrowBar", 
"RightArrowBar", "DownArrowUpArrow", "ForAll", "PartialD", "Exists", "NotExists", "EmptySet", "Del", "Element", 
"NotElement", "ReverseElement", "NotReverseElement", "SuchThat", "Product", "Coproduct", "Sum", "Minus", "MinusPlus", 
"DivisionSlash", "Backslash", "SmallCircle", "Sqrt", "CubeRoot", "Proportional", "Infinity", "RightAngle", "Angle", 
"MeasuredAngle", "SphericalAngle", "Divides", "DoubleVerticalBar", "NotDoubleVerticalBar", "And", "Or", "Integral", 
"ContourIntegral", "DoubleContourIntegral", "ClockwiseContourIntegral", "CounterClockwiseContourIntegral", "Therefore", 
"Because", "Colon", "Proportion", "Tilde", "VerticalTilde", "NotTilde", "EqualTilde", "TildeEqual", "NotTildeEqual", 
"TildeFullEqual", "NotTildeFullEqual", "TildeTilde", "NotTildeTilde", "CupCap", "HumpDownHump", "HumpEqual", "DotEqual",
 "NotEqual", "Congruent", "NotCongruent", "LessEqual", "GreaterEqual", "LessFullEqual", "GreaterFullEqual", 
"NotLessFullEqual", "NotGreaterFullEqual", "LessLess", "GreaterGreater", "NotCupCap", "NotLess", "NotGreater", 
"NotLessEqual", "NotGreaterEqual", "LessTilde", "GreaterTilde", "NotLessTilde", "NotGreaterTilde", "LessGreater", 
"GreaterLess", "NotLessGreater", "NotGreaterLess", "Precedes", "Succeeds", "PrecedesSlantEqual", "SucceedsSlantEqual", 
"PrecedesTilde", "SucceedsTilde", "NotPrecedes", "NotSucceeds", "Subset", "Superset", "NotSubset", "NotSuperset", 
"SubsetEqual", "SupersetEqual", "NotSubsetEqual", "NotSupersetEqual", "UnionPlus", "SquareSubset", "SquareSuperset", 
"SquareSubsetEqual", "SquareSupersetEqual", "SquareIntersection", "SquareUnion", "CirclePlus", "CircleMinus", 
"CircleTimes", "CircleDot", "RightTee", "LeftTee", "DownTee", "UpTee", "DoubleRightTee", "LeftTriangle", "RightTriangle"
, "LeftTriangleEqual", "RightTriangleEqual", "Xor", "Nand", "Nor", "Wedge", "Vee", "Intersection", "Union", "Diamond", 
"Star", "LessEqualGreater", "GreaterEqualLess", "NotPrecedesSlantEqual", "NotSucceedsSlantEqual", "NotSquareSubsetEqual"
, "NotSquareSupersetEqual", "NotPrecedesTilde", "NotSucceedsTilde", "NotLeftTriangle", "NotRightTriangle", 
"NotLeftTriangleEqual", "NotRightTriangleEqual", "VerticalEllipsis", "CenterEllipsis", "AscendingEllipsis", 
"DescendingEllipsis", "Diameter", "LeftCeiling", "RightCeiling", "LeftFloor", "RightFloor", "CloverLeaf", "WatchIcon", 
"Cap", "Cup", "LeftAngleBracket", "RightAngleBracket", "OverBracket", "UnderBracket", "SpaceIndicator", "HorizontalLine"
, "VerticalLine", "FilledSquare", "EmptySquare", "FilledVerySmallSquare", "EmptyVerySmallSquare", "FilledRectangle", 
"EmptyRectangle", "FilledUpTriangle", "EmptyUpTriangle", "UpPointer", "FilledRightTriangle", "RightPointer", 
"FilledDownTriangle", "EmptyDownTriangle", "DownPointer", "FilledLeftTriangle", "LeftPointer", "FilledDiamond", 
"EmptyDiamond", "EmptyCircle", "FilledCircle", "EmptySmallCircle", "EmptySmallSquare", "FilledSmallSquare", 
"FivePointedStar", "Sun", "CheckmarkedBox", "CheckedBox", "SadSmiley", "HappySmiley", "Moon", "Mercury", "Venus", "Mars"
, "Jupiter", "Saturn", "Neptune", "Pluto", "AriesSign", "TaurusSign", "GeminiSign", "CancerSign", "LeoSign", "VirgoSign"
, "LibraSign", "ScorpioSign", "SagittariusSign", "CapricornSign", "AquariusSign", "PiscesSign", "WhiteKing", 
"WhiteQueen", "WhiteRook", "WhiteBishop", "WhiteKnight", "WhitePawn", "BlackKing", "BlackQueen", "BlackRook", 
"BlackBishop", "BlackKnight", "BlackPawn", "SpadeSuit", "HeartSuit", "DiamondSuit", "ClubSuit", "QuarterNote", 
"EighthNote", "BeamedEighthNote", "BeamedSixteenthNote", "Flat", "Natural", "Sharp", "Uranus", "Checkmark", 
"SixPointedStar", "Perpendicular", "LongLeftArrow", "LongRightArrow", "LongLeftRightArrow", "DoubleLongLeftArrow", 
"DoubleLongRightArrow", "DoubleLongLeftRightArrow", "UpArrowBar", "DownArrowBar", "LeftRightVector", "RightUpDownVector"
, "DownLeftRightVector", "LeftUpDownVector", "LeftVectorBar", "RightVectorBar", "RightUpVectorBar", "RightDownVectorBar"
, "DownLeftVectorBar", "DownRightVectorBar", "LeftUpVectorBar", "LeftDownVectorBar", "LeftTeeVector", "RightTeeVector", 
"RightUpTeeVector", "RightDownTeeVector", "DownLeftTeeVector", "DownRightTeeVector", "LeftUpTeeVector", 
"LeftDownTeeVector", "UpEquilibrium", "ReverseUpEquilibrium", "RoundImplies", "LeftTriangleBar", "RightTriangleBar", 
"Equivalent", "LessSlantEqual", "GreaterSlantEqual", "NestedLessLess", "NestedGreaterGreater", "PrecedesEqual", 
"SucceedsEqual", "DoubleLeftTee", "COMPATIBILITYKanjiSpace", "LeftDoubleBracket", "RightDoubleBracket", 
"LeftAssociation", "RightAssociation", "Shah", "WolframLanguageLogo", "WolframLanguageLogoCircle", "TwoWayRule", 
"FreeformPrompt", "WolframAlphaPrompt", "InvisibleSpace", "Piecewise", "NegativeVeryThinSpace", "NegativeThinSpace", 
"NegativeMediumSpace", "NegativeThickSpace", "ImplicitPlus", "Null", "COMPATIBILITYNoBreak", "IndentingNewLine", 
"AutoPlaceholder", "AutoLeftMatch", "AutoRightMatch", "AutoSpace", "AutoOperand", "SystemsModelDelay", "Continuation", 
"RoundSpaceIndicator", "InvisiblePrefixScriptBase", "InvisiblePostfixScriptBase", "EntityStart", "EntityEnd", 
"SpanFromLeft", "SpanFromAbove", "SpanFromBoth", "PageBreakAbove", "PageBreakBelow", "DiscretionaryPageBreakAbove", 
"DiscretionaryPageBreakBelow", "Transpose", "Conjugate", "ConjugateTranspose", "StepperRight", "StepperLeft", 
"StepperUp", "StepperDown", "HermitianConjugate", "VerticalBar", "NotVerticalBar", "Distributed", "Conditioned", 
"UndirectedEdge", "DirectedEdge", "ContinuedFractionK", "TensorProduct", "TensorWedge", "ProbabilityPr", "ExpectationE",
 "PermutationProduct", "Earth", "NotEqualTilde", "NotHumpEqual", "NotHumpDownHump", "NotLeftTriangleBar", 
"NotRightTriangleBar", "NotLessLess", "NotNestedLessLess", "NotLessSlantEqual", "NotGreaterGreater", 
"NotNestedGreaterGreater", "NotGreaterSlantEqual", "NotPrecedesEqual", "NotSucceedsEqual", "NotSquareSubset", 
"NotSquareSuperset", "Equal", "VerticalSeparator", "VectorGreater", "VectorGreaterEqual", "VectorLess", 
"VectorLessEqual", "Limit", "MaxLimit", "MinLimit", "Cross", "Function", "Xnor", "DiscreteShift", "DifferenceDelta", 
"DiscreteRatio", "InlinePart", "RuleDelayed", "Square", "Rule", "Implies", "ShortRightArrow", "ShortLeftArrow", 
"SelectionPlaceholder", "Placeholder", "ShortUpArrow", "ShortDownArrow", "Application", "LeftBracketingBar", 
"RightBracketingBar", "LeftDoubleBracketingBar", "RightDoubleBracketingBar", "ScriptA", "ScriptB", "ScriptC", "ScriptD",
 "ScriptF", "ScriptH", "ScriptI", "ScriptJ", "ScriptK", "ScriptM", "ScriptN", "ScriptP", "ScriptQ", "ScriptR", "ScriptS"
, "ScriptT", "ScriptU", "ScriptV", "ScriptW", "ScriptX", "ScriptY", "ScriptZ", "GothicA", "GothicB", "GothicC", 
"GothicD", "GothicE", "GothicF", "GothicG", "GothicH", "GothicI", "GothicJ", "GothicK", "GothicL", "GothicM", "GothicN",
 "GothicO", "GothicP", "GothicQ", "GothicR", "GothicS", "GothicT", "GothicU", "GothicV", "GothicW", "GothicX", "GothicY"
, "GothicZ", "DoubleStruckA", "DoubleStruckB", "DoubleStruckC", "DoubleStruckD", "DoubleStruckE", "DoubleStruckF", 
"DoubleStruckG", "DoubleStruckH", "DoubleStruckI", "DoubleStruckJ", "DoubleStruckK", "DoubleStruckL", "DoubleStruckM", 
"DoubleStruckN", "DoubleStruckO", "DoubleStruckP", "DoubleStruckQ", "DoubleStruckR", "DoubleStruckS", "DoubleStruckT", 
"DoubleStruckU", "DoubleStruckV", "DoubleStruckW", "DoubleStruckX", "DoubleStruckY", "DoubleStruckZ", "DotlessJ", "Wolf"
, "FreakedSmiley", "NeutralSmiley", "LightBulb", "NumberSign", "WarningSign", "Villa", "Akuz", "Andy", "Spooky", 
"ScriptDotlessI", "ScriptDotlessJ", "DoubledPi", "DoubledGamma", "CapitalDifferentialD", "DifferentialD", "ExponentialE"
, "ImaginaryI", "ImaginaryJ", "FilledSmallCircle", "DottedSquare", "GraySquare", "GrayCircle", "LetterSpace", 
"DownBreve", "KernelIcon", "MathematicaIcon", "TripleDot", "SystemEnterKey", "AlignmentMarker", "LeftSkeleton", 
"RightSkeleton", "ControlKey", "AliasDelimiter", "InvisibleComma", "ReturnKey", "ErrorIndicator", "AliasIndicator", 
"EscapeKey", "CommandKey", "LeftModified", "RightModified", "InvisibleApplication", "DiscretionaryLineSeparator", 
"DiscretionaryParagraphSeparator", "ScriptCapitalA", "ScriptCapitalC", "ScriptCapitalD", "ScriptCapitalG", 
"ScriptCapitalJ", "ScriptCapitalK", "ScriptCapitalN", "ScriptCapitalO", "ScriptCapitalP", "ScriptCapitalQ", 
"ScriptCapitalS", "ScriptCapitalT", "ScriptCapitalU", "ScriptCapitalV", "ScriptCapitalW", "ScriptCapitalX", 
"ScriptCapitalY", "ScriptCapitalZ", "GothicCapitalA", "GothicCapitalB", "GothicCapitalD", "GothicCapitalE", 
"GothicCapitalF", "GothicCapitalG", "GothicCapitalJ", "GothicCapitalK", "GothicCapitalL", "GothicCapitalM", 
"GothicCapitalN", "GothicCapitalO", "GothicCapitalP", "GothicCapitalQ", "GothicCapitalS", "GothicCapitalT", 
"GothicCapitalU", "GothicCapitalV", "GothicCapitalW", "GothicCapitalX", "GothicCapitalY", "DoubleStruckCapitalA", 
"DoubleStruckCapitalB", "DoubleStruckCapitalC", "DoubleStruckCapitalD", "DoubleStruckCapitalE", "DoubleStruckCapitalF", 
"DoubleStruckCapitalG", "DoubleStruckCapitalH", "DoubleStruckCapitalI", "DoubleStruckCapitalJ", "DoubleStruckCapitalK", 
"DoubleStruckCapitalL", "DoubleStruckCapitalM", "DoubleStruckCapitalN", "DoubleStruckCapitalO", "DoubleStruckCapitalP", 
"DoubleStruckCapitalQ", "DoubleStruckCapitalR", "DoubleStruckCapitalS", "DoubleStruckCapitalT", "DoubleStruckCapitalU", 
"DoubleStruckCapitalV", "DoubleStruckCapitalW", "DoubleStruckCapitalX", "DoubleStruckCapitalY", "DoubleStruckCapitalZ", 
"TabKey", "SpaceKey", "DeleteKey", "AltKey", "OptionKey", "KeyBar", "EnterKey", "ShiftKey", "Mod1Key", "Mod2Key", 
"LongEqual", "ConstantC", "DoubleStruckZero", "DoubleStruckOne", "DoubleStruckTwo", "DoubleStruckThree", 
"DoubleStruckFour", "DoubleStruckFive", "DoubleStruckSix", "DoubleStruckSeven", "DoubleStruckEight", "DoubleStruckNine",
 "GothicZero", "GothicOne", "GothicTwo", "GothicThree", "GothicFour", "GothicFive", "GothicSix", "GothicSeven", 
"GothicEight", "GothicNine", "ScriptZero", "ScriptOne", "ScriptTwo", "ScriptThree", "ScriptFour", "ScriptFive", 
"ScriptSix", "ScriptSeven", "ScriptEight", "ScriptNine", "FirstPage", "LastPage", "NumberComma", "FormalA", "FormalB", 
"FormalC", "FormalD", "FormalE", "FormalF", "FormalG", "FormalH", "FormalI", "FormalJ", "FormalK", "FormalL", "FormalM",
 "FormalN", "FormalO", "FormalP", "FormalQ", "FormalR", "FormalS", "FormalT", "FormalU", "FormalV", "FormalW", "FormalX"
, "FormalY", "FormalZ", "FormalCapitalA", "FormalCapitalB", "FormalCapitalC", "FormalCapitalD", "FormalCapitalE", 
"FormalCapitalF", "FormalCapitalG", "FormalCapitalH", "FormalCapitalI", "FormalCapitalJ", "FormalCapitalK", 
"FormalCapitalL", "FormalCapitalM", "FormalCapitalN", "FormalCapitalO", "FormalCapitalP", "FormalCapitalQ", 
"FormalCapitalR", "FormalCapitalS", "FormalCapitalT", "FormalCapitalU", "FormalCapitalV", "FormalCapitalW", 
"FormalCapitalX", "FormalCapitalY", "FormalCapitalZ", "FormalCapitalAlpha", "FormalCapitalBeta", "FormalCapitalGamma", 
"FormalCapitalDelta", "FormalCapitalEpsilon", "FormalCapitalZeta", "FormalCapitalEta", "FormalCapitalTheta", 
"FormalCapitalIota", "FormalCapitalKappa", "FormalCapitalLambda", "FormalCapitalMu", "FormalCapitalNu", 
"FormalCapitalXi", "FormalCapitalOmicron", "FormalCapitalPi", "FormalCapitalRho", "FormalCapitalSigma", 
"FormalCapitalTau", "FormalCapitalUpsilon", "FormalCapitalPhi", "FormalCapitalChi", "FormalCapitalPsi", 
"FormalCapitalOmega", "FormalAlpha", "FormalBeta", "FormalGamma", "FormalDelta", "FormalCurlyEpsilon", "FormalZeta", 
"FormalEta", "FormalTheta", "FormalIota", "FormalKappa", "FormalLambda", "FormalMu", "FormalNu", "FormalXi", 
"FormalOmicron", "FormalPi", "FormalRho", "FormalFinalSigma", "FormalSigma", "FormalTau", "FormalUpsilon", 
"FormalCurlyPhi", "FormalChi", "FormalPsi", "FormalOmega", "FormalCurlyTheta", "FormalCurlyCapitalUpsilon", "FormalPhi",
 "FormalCurlyPi", "FormalCapitalStigma", "FormalStigma", "FormalCapitalDigamma", "FormalDigamma", "FormalCapitalKoppa", 
"FormalKoppa", "FormalCapitalSampi", "FormalSampi", "FormalCurlyKappa", "FormalCurlyRho", "FormalEpsilon", "FiLigature",
 "FlLigature", "OverParenthesis", "UnderParenthesis", "OverBrace", "UnderBrace", "UnknownGlyph", "FormalScriptA", 
"FormalScriptB", "FormalScriptC", "FormalScriptD", "FormalScriptE", "FormalScriptF", "FormalScriptG", "FormalScriptH", 
"FormalScriptI", "FormalScriptJ", "FormalScriptK", "FormalScriptL", "FormalScriptM", "FormalScriptN", "FormalScriptO", 
"FormalScriptP", "FormalScriptQ", "FormalScriptR", "FormalScriptS", "FormalScriptT", "FormalScriptU", "FormalScriptV", 
"FormalScriptW", "FormalScriptX", "FormalScriptY", "FormalScriptZ", "FormalScriptCapitalA", "FormalScriptCapitalB", 
"FormalScriptCapitalC", "FormalScriptCapitalD", "FormalScriptCapitalE", "FormalScriptCapitalF", "FormalScriptCapitalG", 
"FormalScriptCapitalH", "FormalScriptCapitalI", "FormalScriptCapitalJ", "FormalScriptCapitalK", "FormalScriptCapitalL", 
"FormalScriptCapitalM", "FormalScriptCapitalN", "FormalScriptCapitalO", "FormalScriptCapitalP", "FormalScriptCapitalQ", 
"FormalScriptCapitalR", "FormalScriptCapitalS", "FormalScriptCapitalT", "FormalScriptCapitalU", "FormalScriptCapitalV", 
"FormalScriptCapitalW", "FormalScriptCapitalX", "FormalScriptCapitalY", "FormalScriptCapitalZ", 
];

//
//
//
pub const RAW_SET: [&str; RAWLONGNAMES_COUNT] = [
"NewLine", "RawAmpersand", "RawAt", "RawBackquote", "RawBackslash", "RawColon", "RawComma", "RawDash", "RawDollar", 
"RawDot", "RawDoubleQuote", "RawEqual", "RawEscape", "RawExclamation", "RawGreater", "RawLeftBrace", "RawLeftBracket", 
"RawLeftParenthesis", "RawLess", "RawNumberSign", "RawPercent", "RawPlus", "RawQuestion", "RawQuote", "RawReturn", 
"RawRightBrace", "RawRightBracket", "RawRightParenthesis", "RawSemicolon", "RawSlash", "RawSpace", "RawStar", "RawTab", 
"RawTilde", "RawUnderscore", "RawVerticalBar", "RawWedge", 
];

//
//
//
pub const MB_NOT_STRAGE_LETTERLIKE_CODE_POINTS: [CodePoint; MBNOTSTRANGELETTERLIKECODEPOINTS_COUNT] = [
CODEPOINT_LONGNAME_DEGREE, CODEPOINT_LONGNAME_CAPITALALPHA, CODEPOINT_LONGNAME_CAPITALBETA, 
CODEPOINT_LONGNAME_CAPITALGAMMA, CODEPOINT_LONGNAME_CAPITALDELTA, CODEPOINT_LONGNAME_CAPITALEPSILON, 
CODEPOINT_LONGNAME_CAPITALZETA, CODEPOINT_LONGNAME_CAPITALETA, CODEPOINT_LONGNAME_CAPITALTHETA, 
CODEPOINT_LONGNAME_CAPITALIOTA, CODEPOINT_LONGNAME_CAPITALKAPPA, CODEPOINT_LONGNAME_CAPITALLAMBDA, 
CODEPOINT_LONGNAME_CAPITALMU, CODEPOINT_LONGNAME_CAPITALNU, CODEPOINT_LONGNAME_CAPITALXI, 
CODEPOINT_LONGNAME_CAPITALOMICRON, CODEPOINT_LONGNAME_CAPITALPI, CODEPOINT_LONGNAME_CAPITALRHO, 
CODEPOINT_LONGNAME_CAPITALSIGMA, CODEPOINT_LONGNAME_CAPITALTAU, CODEPOINT_LONGNAME_CAPITALUPSILON, 
CODEPOINT_LONGNAME_CAPITALPHI, CODEPOINT_LONGNAME_CAPITALCHI, CODEPOINT_LONGNAME_CAPITALPSI, 
CODEPOINT_LONGNAME_CAPITALOMEGA, CODEPOINT_LONGNAME_ALPHA, CODEPOINT_LONGNAME_BETA, CODEPOINT_LONGNAME_GAMMA, 
CODEPOINT_LONGNAME_DELTA, CODEPOINT_LONGNAME_CURLYEPSILON, CODEPOINT_LONGNAME_ZETA, CODEPOINT_LONGNAME_ETA, 
CODEPOINT_LONGNAME_THETA, CODEPOINT_LONGNAME_IOTA, CODEPOINT_LONGNAME_KAPPA, CODEPOINT_LONGNAME_LAMBDA, 
CODEPOINT_LONGNAME_MU, CODEPOINT_LONGNAME_NU, CODEPOINT_LONGNAME_XI, CODEPOINT_LONGNAME_OMICRON, CODEPOINT_LONGNAME_PI, 
CODEPOINT_LONGNAME_RHO, CODEPOINT_LONGNAME_FINALSIGMA, CODEPOINT_LONGNAME_SIGMA, CODEPOINT_LONGNAME_TAU, 
CODEPOINT_LONGNAME_UPSILON, CODEPOINT_LONGNAME_CURLYPHI, CODEPOINT_LONGNAME_CHI, CODEPOINT_LONGNAME_PSI, 
CODEPOINT_LONGNAME_OMEGA, CODEPOINT_LONGNAME_CURLYTHETA, CODEPOINT_LONGNAME_CURLYCAPITALUPSILON, CODEPOINT_LONGNAME_PHI,
 CODEPOINT_LONGNAME_CURLYPI, CODEPOINT_LONGNAME_CAPITALSTIGMA, CODEPOINT_LONGNAME_STIGMA, 
CODEPOINT_LONGNAME_CAPITALDIGAMMA, CODEPOINT_LONGNAME_DIGAMMA, CODEPOINT_LONGNAME_CAPITALKOPPA, CODEPOINT_LONGNAME_KOPPA
, CODEPOINT_LONGNAME_CAPITALSAMPI, CODEPOINT_LONGNAME_SAMPI, CODEPOINT_LONGNAME_CURLYKAPPA, CODEPOINT_LONGNAME_CURLYRHO,
 CODEPOINT_LONGNAME_EPSILON, CODEPOINT_LONGNAME_DAGGER, CODEPOINT_LONGNAME_PRIME, CODEPOINT_LONGNAME_DOUBLEPRIME, 
CODEPOINT_LONGNAME_SCRIPTG, CODEPOINT_LONGNAME_SCRIPTCAPITALH, CODEPOINT_LONGNAME_GOTHICCAPITALH, 
CODEPOINT_LONGNAME_HBAR, CODEPOINT_LONGNAME_SCRIPTCAPITALI, CODEPOINT_LONGNAME_GOTHICCAPITALI, 
CODEPOINT_LONGNAME_SCRIPTCAPITALL, CODEPOINT_LONGNAME_SCRIPTL, CODEPOINT_LONGNAME_SCRIPTCAPITALR, 
CODEPOINT_LONGNAME_SCRIPTCAPITALB, CODEPOINT_LONGNAME_SCRIPTE, CODEPOINT_LONGNAME_SCRIPTCAPITALE, 
CODEPOINT_LONGNAME_SCRIPTCAPITALF, CODEPOINT_LONGNAME_SCRIPTCAPITALM, CODEPOINT_LONGNAME_SCRIPTO, 
CODEPOINT_LONGNAME_INFINITY, CODEPOINT_LONGNAME_SYSTEMSMODELDELAY, CODEPOINT_LONGNAME_SCRIPTA, 
CODEPOINT_LONGNAME_SCRIPTB, CODEPOINT_LONGNAME_SCRIPTC, CODEPOINT_LONGNAME_SCRIPTD, CODEPOINT_LONGNAME_SCRIPTF, 
CODEPOINT_LONGNAME_SCRIPTH, CODEPOINT_LONGNAME_SCRIPTI, CODEPOINT_LONGNAME_SCRIPTJ, CODEPOINT_LONGNAME_SCRIPTK, 
CODEPOINT_LONGNAME_SCRIPTM, CODEPOINT_LONGNAME_SCRIPTN, CODEPOINT_LONGNAME_SCRIPTP, CODEPOINT_LONGNAME_SCRIPTQ, 
CODEPOINT_LONGNAME_SCRIPTR, CODEPOINT_LONGNAME_SCRIPTS, CODEPOINT_LONGNAME_SCRIPTT, CODEPOINT_LONGNAME_SCRIPTU, 
CODEPOINT_LONGNAME_SCRIPTV, CODEPOINT_LONGNAME_SCRIPTW, CODEPOINT_LONGNAME_SCRIPTX, CODEPOINT_LONGNAME_SCRIPTY, 
CODEPOINT_LONGNAME_SCRIPTZ, CODEPOINT_LONGNAME_EXPONENTIALE, CODEPOINT_LONGNAME_IMAGINARYI, 
CODEPOINT_LONGNAME_IMAGINARYJ, CODEPOINT_LONGNAME_SCRIPTCAPITALA, CODEPOINT_LONGNAME_SCRIPTCAPITALC, 
CODEPOINT_LONGNAME_SCRIPTCAPITALD, CODEPOINT_LONGNAME_SCRIPTCAPITALG, CODEPOINT_LONGNAME_SCRIPTCAPITALJ, 
CODEPOINT_LONGNAME_SCRIPTCAPITALK, CODEPOINT_LONGNAME_SCRIPTCAPITALN, CODEPOINT_LONGNAME_SCRIPTCAPITALO, 
CODEPOINT_LONGNAME_SCRIPTCAPITALP, CODEPOINT_LONGNAME_SCRIPTCAPITALQ, CODEPOINT_LONGNAME_SCRIPTCAPITALS, 
CODEPOINT_LONGNAME_SCRIPTCAPITALT, CODEPOINT_LONGNAME_SCRIPTCAPITALU, CODEPOINT_LONGNAME_SCRIPTCAPITALV, 
CODEPOINT_LONGNAME_SCRIPTCAPITALW, CODEPOINT_LONGNAME_SCRIPTCAPITALX, CODEPOINT_LONGNAME_SCRIPTCAPITALY, 
CODEPOINT_LONGNAME_SCRIPTCAPITALZ, CODEPOINT_LONGNAME_FORMALA, CODEPOINT_LONGNAME_FORMALB, CODEPOINT_LONGNAME_FORMALC, 
CODEPOINT_LONGNAME_FORMALD, CODEPOINT_LONGNAME_FORMALE, CODEPOINT_LONGNAME_FORMALF, CODEPOINT_LONGNAME_FORMALG, 
CODEPOINT_LONGNAME_FORMALH, CODEPOINT_LONGNAME_FORMALI, CODEPOINT_LONGNAME_FORMALJ, CODEPOINT_LONGNAME_FORMALK, 
CODEPOINT_LONGNAME_FORMALL, CODEPOINT_LONGNAME_FORMALM, CODEPOINT_LONGNAME_FORMALN, CODEPOINT_LONGNAME_FORMALO, 
CODEPOINT_LONGNAME_FORMALP, CODEPOINT_LONGNAME_FORMALQ, CODEPOINT_LONGNAME_FORMALR, CODEPOINT_LONGNAME_FORMALS, 
CODEPOINT_LONGNAME_FORMALT, CODEPOINT_LONGNAME_FORMALU, CODEPOINT_LONGNAME_FORMALV, CODEPOINT_LONGNAME_FORMALW, 
CODEPOINT_LONGNAME_FORMALX, CODEPOINT_LONGNAME_FORMALY, CODEPOINT_LONGNAME_FORMALZ, CODEPOINT_LONGNAME_FORMALCAPITALA, 
CODEPOINT_LONGNAME_FORMALCAPITALB, CODEPOINT_LONGNAME_FORMALCAPITALC, CODEPOINT_LONGNAME_FORMALCAPITALD, 
CODEPOINT_LONGNAME_FORMALCAPITALE, CODEPOINT_LONGNAME_FORMALCAPITALF, CODEPOINT_LONGNAME_FORMALCAPITALG, 
CODEPOINT_LONGNAME_FORMALCAPITALH, CODEPOINT_LONGNAME_FORMALCAPITALI, CODEPOINT_LONGNAME_FORMALCAPITALJ, 
CODEPOINT_LONGNAME_FORMALCAPITALK, CODEPOINT_LONGNAME_FORMALCAPITALL, CODEPOINT_LONGNAME_FORMALCAPITALM, 
CODEPOINT_LONGNAME_FORMALCAPITALN, CODEPOINT_LONGNAME_FORMALCAPITALO, CODEPOINT_LONGNAME_FORMALCAPITALP, 
CODEPOINT_LONGNAME_FORMALCAPITALQ, CODEPOINT_LONGNAME_FORMALCAPITALR, CODEPOINT_LONGNAME_FORMALCAPITALS, 
CODEPOINT_LONGNAME_FORMALCAPITALT, CODEPOINT_LONGNAME_FORMALCAPITALU, CODEPOINT_LONGNAME_FORMALCAPITALV, 
CODEPOINT_LONGNAME_FORMALCAPITALW, CODEPOINT_LONGNAME_FORMALCAPITALX, CODEPOINT_LONGNAME_FORMALCAPITALY, 
CODEPOINT_LONGNAME_FORMALCAPITALZ, CODEPOINT_LONGNAME_FORMALCAPITALALPHA, CODEPOINT_LONGNAME_FORMALCAPITALBETA, 
CODEPOINT_LONGNAME_FORMALCAPITALGAMMA, CODEPOINT_LONGNAME_FORMALCAPITALDELTA, CODEPOINT_LONGNAME_FORMALCAPITALEPSILON, 
CODEPOINT_LONGNAME_FORMALCAPITALZETA, CODEPOINT_LONGNAME_FORMALCAPITALETA, CODEPOINT_LONGNAME_FORMALCAPITALTHETA, 
CODEPOINT_LONGNAME_FORMALCAPITALIOTA, CODEPOINT_LONGNAME_FORMALCAPITALKAPPA, CODEPOINT_LONGNAME_FORMALCAPITALLAMBDA, 
CODEPOINT_LONGNAME_FORMALCAPITALMU, CODEPOINT_LONGNAME_FORMALCAPITALNU, CODEPOINT_LONGNAME_FORMALCAPITALXI, 
CODEPOINT_LONGNAME_FORMALCAPITALOMICRON, CODEPOINT_LONGNAME_FORMALCAPITALPI, CODEPOINT_LONGNAME_FORMALCAPITALRHO, 
CODEPOINT_LONGNAME_FORMALCAPITALSIGMA, CODEPOINT_LONGNAME_FORMALCAPITALTAU, CODEPOINT_LONGNAME_FORMALCAPITALUPSILON, 
CODEPOINT_LONGNAME_FORMALCAPITALPHI, CODEPOINT_LONGNAME_FORMALCAPITALCHI, CODEPOINT_LONGNAME_FORMALCAPITALPSI, 
CODEPOINT_LONGNAME_FORMALCAPITALOMEGA, CODEPOINT_LONGNAME_FORMALALPHA, CODEPOINT_LONGNAME_FORMALBETA, 
CODEPOINT_LONGNAME_FORMALGAMMA, CODEPOINT_LONGNAME_FORMALDELTA, CODEPOINT_LONGNAME_FORMALCURLYEPSILON, 
CODEPOINT_LONGNAME_FORMALZETA, CODEPOINT_LONGNAME_FORMALETA, CODEPOINT_LONGNAME_FORMALTHETA, 
CODEPOINT_LONGNAME_FORMALIOTA, CODEPOINT_LONGNAME_FORMALKAPPA, CODEPOINT_LONGNAME_FORMALLAMBDA, 
CODEPOINT_LONGNAME_FORMALMU, CODEPOINT_LONGNAME_FORMALNU, CODEPOINT_LONGNAME_FORMALXI, CODEPOINT_LONGNAME_FORMALOMICRON,
 CODEPOINT_LONGNAME_FORMALPI, CODEPOINT_LONGNAME_FORMALRHO, CODEPOINT_LONGNAME_FORMALFINALSIGMA, 
CODEPOINT_LONGNAME_FORMALSIGMA, CODEPOINT_LONGNAME_FORMALTAU, CODEPOINT_LONGNAME_FORMALUPSILON, 
CODEPOINT_LONGNAME_FORMALCURLYPHI, CODEPOINT_LONGNAME_FORMALCHI, CODEPOINT_LONGNAME_FORMALPSI, 
CODEPOINT_LONGNAME_FORMALOMEGA, CODEPOINT_LONGNAME_FORMALCURLYTHETA, CODEPOINT_LONGNAME_FORMALCURLYCAPITALUPSILON, 
CODEPOINT_LONGNAME_FORMALPHI, CODEPOINT_LONGNAME_FORMALCURLYPI, CODEPOINT_LONGNAME_FORMALCAPITALSTIGMA, 
CODEPOINT_LONGNAME_FORMALSTIGMA, CODEPOINT_LONGNAME_FORMALCAPITALDIGAMMA, CODEPOINT_LONGNAME_FORMALDIGAMMA, 
CODEPOINT_LONGNAME_FORMALCAPITALKOPPA, CODEPOINT_LONGNAME_FORMALKOPPA, CODEPOINT_LONGNAME_FORMALCAPITALSAMPI, 
CODEPOINT_LONGNAME_FORMALSAMPI, CODEPOINT_LONGNAME_FORMALCURLYKAPPA, CODEPOINT_LONGNAME_FORMALCURLYRHO, 
CODEPOINT_LONGNAME_FORMALEPSILON, CODEPOINT_LONGNAME_FORMALSCRIPTA, CODEPOINT_LONGNAME_FORMALSCRIPTB, 
CODEPOINT_LONGNAME_FORMALSCRIPTC, CODEPOINT_LONGNAME_FORMALSCRIPTD, CODEPOINT_LONGNAME_FORMALSCRIPTE, 
CODEPOINT_LONGNAME_FORMALSCRIPTF, CODEPOINT_LONGNAME_FORMALSCRIPTG, CODEPOINT_LONGNAME_FORMALSCRIPTH, 
CODEPOINT_LONGNAME_FORMALSCRIPTI, CODEPOINT_LONGNAME_FORMALSCRIPTJ, CODEPOINT_LONGNAME_FORMALSCRIPTK, 
CODEPOINT_LONGNAME_FORMALSCRIPTL, CODEPOINT_LONGNAME_FORMALSCRIPTM, CODEPOINT_LONGNAME_FORMALSCRIPTN, 
CODEPOINT_LONGNAME_FORMALSCRIPTO, CODEPOINT_LONGNAME_FORMALSCRIPTP, CODEPOINT_LONGNAME_FORMALSCRIPTQ, 
CODEPOINT_LONGNAME_FORMALSCRIPTR, CODEPOINT_LONGNAME_FORMALSCRIPTS, CODEPOINT_LONGNAME_FORMALSCRIPTT, 
CODEPOINT_LONGNAME_FORMALSCRIPTU, CODEPOINT_LONGNAME_FORMALSCRIPTV, CODEPOINT_LONGNAME_FORMALSCRIPTW, 
CODEPOINT_LONGNAME_FORMALSCRIPTX, CODEPOINT_LONGNAME_FORMALSCRIPTY, CODEPOINT_LONGNAME_FORMALSCRIPTZ, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALA, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALB, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALC, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALD, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALE, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALF, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALG, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALH, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALI, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALJ, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALK, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALL, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALM, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALN, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALO, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALP, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALQ, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALR, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALS, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALT, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALU, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALV, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALW, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALX, 
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALY, CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALZ, 
];

//
//
//
pub static ASCII_REPLACEMENTS_MAP: Lazy<HashMap<CodePoint, &[&str]>> = Lazy::new(|| HashMap::from_iter([
(CODEPOINT_LONGNAME_NONBREAKINGSPACE, [" ", ].as_slice()), (CODEPOINT_LONGNAME_CENT, ["c", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLEDOT, ["..", ].as_slice()), (CODEPOINT_LONGNAME_COPYRIGHT, ["(c)", ].as_slice()), (
CODEPOINT_LONGNAME_LEFTGUILLEMET, ["<<", "\"", ].as_slice()), (CODEPOINT_LONGNAME_NOT, ["!", ].as_slice()), (
CODEPOINT_LONGNAME_DISCRETIONARYHYPHEN, ["-", ].as_slice()), (CODEPOINT_LONGNAME_REGISTEREDTRADEMARK, ["(R)", 
].as_slice()), (CODEPOINT_LONGNAME_DEGREE, ["Degree", ].as_slice()), (CODEPOINT_LONGNAME_PLUSMINUS, ["+-", ].as_slice())
, (CODEPOINT_LONGNAME_MICRO, ["u", ].as_slice()), (CODEPOINT_LONGNAME_CENTERDOT, [".", ].as_slice()), (
CODEPOINT_LONGNAME_CEDILLA, [",", ].as_slice()), (CODEPOINT_LONGNAME_RIGHTGUILLEMET, [">>", "\"", ].as_slice()), (
CODEPOINT_LONGNAME_CAPITALAGRAVE, ["A`", "A", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALAACUTE, ["A'", "A", ].as_slice()
), (CODEPOINT_LONGNAME_CAPITALAHAT, ["A^", "A", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALATILDE, ["A~", "A", 
].as_slice()), (CODEPOINT_LONGNAME_CAPITALADOUBLEDOT, ["A\"", "AE", "A", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALARING
, ["Ao", "A", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALAE, ["AE", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALCCEDILLA, [
"C,", "C", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALEGRAVE, ["E`", "E", ].as_slice()), (
CODEPOINT_LONGNAME_CAPITALEACUTE, ["E'", "E", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALEHAT, ["E^", "E", ].as_slice())
, (CODEPOINT_LONGNAME_CAPITALEDOUBLEDOT, ["E\"", "E", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALIGRAVE, ["I`", "I", 
].as_slice()), (CODEPOINT_LONGNAME_CAPITALIACUTE, ["I'", "I", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALIHAT, ["I^", "I"
, ].as_slice()), (CODEPOINT_LONGNAME_CAPITALIDOUBLEDOT, ["I\"", "I", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALETH, [
"Eth", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALNTILDE, ["N~", "N", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALOGRAVE, [
"O`", "O", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALOACUTE, ["O'", "O", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALOHAT
, ["O^", "O", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALOTILDE, ["O~", "O", ].as_slice()), (
CODEPOINT_LONGNAME_CAPITALODOUBLEDOT, ["O\"", "OE", "O", ].as_slice()), (CODEPOINT_LONGNAME_TIMES, ["*", "x", 
].as_slice()), (CODEPOINT_LONGNAME_CAPITALOSLASH, ["O/", "O", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALUGRAVE, ["U`", 
"U", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALUACUTE, ["U'", "U", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALUHAT, ["U^"
, "U", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALUDOUBLEDOT, ["U\"", "UE", "U", ].as_slice()), (
CODEPOINT_LONGNAME_CAPITALYACUTE, ["Y'", "Y", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALTHORN, ["Th", ].as_slice()), (
CODEPOINT_LONGNAME_SZ, ["sz", ].as_slice()), (CODEPOINT_LONGNAME_AGRAVE, ["a`", "a", ].as_slice()), (
CODEPOINT_LONGNAME_AACUTE, ["a'", "a", ].as_slice()), (CODEPOINT_LONGNAME_AHAT, ["a^", "a", ].as_slice()), (
CODEPOINT_LONGNAME_ATILDE, ["a~", "a", ].as_slice()), (CODEPOINT_LONGNAME_ADOUBLEDOT, ["a\"", "ae", "a", ].as_slice()), 
(CODEPOINT_LONGNAME_ARING, ["ao", "a", ].as_slice()), (CODEPOINT_LONGNAME_AE, ["ae", "a", ].as_slice()), (
CODEPOINT_LONGNAME_CCEDILLA, ["c,", "c", ].as_slice()), (CODEPOINT_LONGNAME_EGRAVE, ["e`", "e", ].as_slice()), (
CODEPOINT_LONGNAME_EACUTE, ["e'", "e", ].as_slice()), (CODEPOINT_LONGNAME_EHAT, ["e^", "e", ].as_slice()), (
CODEPOINT_LONGNAME_EDOUBLEDOT, ["e\"", "e", ].as_slice()), (CODEPOINT_LONGNAME_IGRAVE, ["i`", "i", ].as_slice()), (
CODEPOINT_LONGNAME_IACUTE, ["i'", "i", ].as_slice()), (CODEPOINT_LONGNAME_IHAT, ["i^", "i", ].as_slice()), (
CODEPOINT_LONGNAME_IDOUBLEDOT, ["i\"", "i", ].as_slice()), (CODEPOINT_LONGNAME_ETH, ["eth", ].as_slice()), (
CODEPOINT_LONGNAME_NTILDE, ["n~", "n", ].as_slice()), (CODEPOINT_LONGNAME_OGRAVE, ["o`", "o", ].as_slice()), (
CODEPOINT_LONGNAME_OACUTE, ["o'", "o", ].as_slice()), (CODEPOINT_LONGNAME_OHAT, ["o^", "o", ].as_slice()), (
CODEPOINT_LONGNAME_OTILDE, ["o~", "o", ].as_slice()), (CODEPOINT_LONGNAME_ODOUBLEDOT, ["o\"", "oe", "o", ].as_slice()), 
(CODEPOINT_LONGNAME_DIVIDE, ["/", ].as_slice()), (CODEPOINT_LONGNAME_OSLASH, ["o/", "o", ].as_slice()), (
CODEPOINT_LONGNAME_UGRAVE, ["u`", "u", ].as_slice()), (CODEPOINT_LONGNAME_UACUTE, ["u'", "u", ].as_slice()), (
CODEPOINT_LONGNAME_UHAT, ["u^", "u", ].as_slice()), (CODEPOINT_LONGNAME_UDOUBLEDOT, ["u\"", "ue", "u", ].as_slice()), (
CODEPOINT_LONGNAME_YACUTE, ["y'", "y", ].as_slice()), (CODEPOINT_LONGNAME_THORN, ["th", ].as_slice()), (
CODEPOINT_LONGNAME_YDOUBLEDOT, ["y\"", "y", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALABAR, ["A-", "A", ].as_slice()), (
CODEPOINT_LONGNAME_ABAR, ["a-", "a", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALACUP, ["Au", "A", ].as_slice()), (
CODEPOINT_LONGNAME_ACUP, ["au", "a", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALCACUTE, ["C'", "C", ].as_slice()), (
CODEPOINT_LONGNAME_CACUTE, ["c'", "c", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALCHACEK, ["Cv", "C", ].as_slice()), (
CODEPOINT_LONGNAME_CHACEK, ["cv", "c", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALDHACEK, ["Dv", "D", ].as_slice()), (
CODEPOINT_LONGNAME_DHACEK, ["dv", "d", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALEBAR, ["E-", "E", ].as_slice()), (
CODEPOINT_LONGNAME_EBAR, ["e-", "e", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALECUP, ["Eu", "E", ].as_slice()), (
CODEPOINT_LONGNAME_ECUP, ["eu", "e", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALEHACEK, ["Ev", "E", ].as_slice()), (
CODEPOINT_LONGNAME_EHACEK, ["ev", "e", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALICUP, ["Iu", "I", ].as_slice()), (
CODEPOINT_LONGNAME_ICUP, ["iu", "i", ].as_slice()), (CODEPOINT_LONGNAME_DOTLESSI, ["i", ].as_slice()), (
CODEPOINT_LONGNAME_CAPITALLSLASH, ["L/", "L", ].as_slice()), (CODEPOINT_LONGNAME_LSLASH, ["l/", "l", ].as_slice()), (
CODEPOINT_LONGNAME_CAPITALNHACEK, ["Nv", "N", ].as_slice()), (CODEPOINT_LONGNAME_NHACEK, ["nv", "n", ].as_slice()), (
CODEPOINT_LONGNAME_CAPITALODOUBLEACUTE, ["O''", "O", ].as_slice()), (CODEPOINT_LONGNAME_ODOUBLEACUTE, ["o''", "o", 
].as_slice()), (CODEPOINT_LONGNAME_CAPITALOE, ["OE", ].as_slice()), (CODEPOINT_LONGNAME_OE, ["oe", ].as_slice()), (
CODEPOINT_LONGNAME_CAPITALRHACEK, ["Rv", "R", ].as_slice()), (CODEPOINT_LONGNAME_RHACEK, ["rv", "r", ].as_slice()), (
CODEPOINT_LONGNAME_CAPITALSHACEK, ["Sv", "S", ].as_slice()), (CODEPOINT_LONGNAME_SHACEK, ["sv", "s", ].as_slice()), (
CODEPOINT_LONGNAME_CAPITALTHACEK, ["Tv", "T", ].as_slice()), (CODEPOINT_LONGNAME_THACEK, ["tv", "t", ].as_slice()), (
CODEPOINT_LONGNAME_CAPITALURING, ["Uo", "U", ].as_slice()), (CODEPOINT_LONGNAME_URING, ["uo", "u", ].as_slice()), (
CODEPOINT_LONGNAME_CAPITALUDOUBLEACUTE, ["U''", "U", ].as_slice()), (CODEPOINT_LONGNAME_UDOUBLEACUTE, ["u''", "u", 
].as_slice()), (CODEPOINT_LONGNAME_CAPITALZHACEK, ["Zv", "Z", ].as_slice()), (CODEPOINT_LONGNAME_ZHACEK, ["zv", "z", 
].as_slice()), (CODEPOINT_LONGNAME_FLORIN, ["f", ].as_slice()), (CODEPOINT_LONGNAME_HACEK, ["v", ].as_slice()), (
CODEPOINT_LONGNAME_MU, ["u", ].as_slice()), (CODEPOINT_LONGNAME_THICKSPACE, [" ", ].as_slice()), (
CODEPOINT_LONGNAME_THINSPACE, [" ", ].as_slice()), (CODEPOINT_LONGNAME_VERYTHINSPACE, [" ", ].as_slice()), (
CODEPOINT_LONGNAME_HYPHEN, ["-", ].as_slice()), (CODEPOINT_LONGNAME_DASH, ["-", ].as_slice()), (
CODEPOINT_LONGNAME_LONGDASH, ["--", "-", ].as_slice()), (CODEPOINT_LONGNAME_OPENCURLYQUOTE, ["'", ].as_slice()), (
CODEPOINT_LONGNAME_CLOSECURLYQUOTE, ["'", ].as_slice()), (CODEPOINT_LONGNAME_OPENCURLYDOUBLEQUOTE, ["\"", ].as_slice())
, (CODEPOINT_LONGNAME_CLOSECURLYDOUBLEQUOTE, ["\"", ].as_slice()), (CODEPOINT_LONGNAME_ELLIPSIS, ["...", ].as_slice()), 
(CODEPOINT_LONGNAME_LINESEPARATOR, ["\n", ].as_slice()), (CODEPOINT_LONGNAME_PARAGRAPHSEPARATOR, ["\n\n", ].as_slice())
, (CODEPOINT_LONGNAME_PRIME, ["'", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLEPRIME, ["''", "\"", ].as_slice()), (
CODEPOINT_LONGNAME_REVERSEPRIME, ["`", ].as_slice()), (CODEPOINT_LONGNAME_REVERSEDOUBLEPRIME, ["``", "\"", ].as_slice())
, (CODEPOINT_LONGNAME_SKELETONINDICATOR, ["-", ].as_slice()), (CODEPOINT_LONGNAME_MEDIUMSPACE, [" ", ].as_slice()), (
CODEPOINT_LONGNAME_INVISIBLETIMES, ["*", "x", "", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTG, ["g", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTCAPITALH, ["H", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICCAPITALH, ["H", ].as_slice()), (
CODEPOINT_LONGNAME_HBAR, ["h", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTCAPITALI, ["I", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICCAPITALI, ["I", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTCAPITALL, ["L", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTL, ["l", ].as_slice()), (CODEPOINT_LONGNAME_WEIERSTRASSP, ["p", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTCAPITALR, ["R", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICCAPITALR, ["R", ].as_slice()), (
CODEPOINT_LONGNAME_TRADEMARK, ["(TM)", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICCAPITALZ, ["Z", ].as_slice()), (
CODEPOINT_LONGNAME_ANGSTROM, ["A", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTCAPITALB, ["B", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICCAPITALC, ["C", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTE, ["e", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTCAPITALE, ["E", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTCAPITALF, ["F", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTCAPITALM, ["M", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTO, ["o", ].as_slice()), (
CODEPOINT_LONGNAME_LEFTARROW, ["<-", ].as_slice()), (CODEPOINT_LONGNAME_RIGHTARROW, ["->", ].as_slice()), (
CODEPOINT_LONGNAME_LEFTRIGHTARROW, ["<->", ].as_slice()), (CODEPOINT_LONGNAME_LEFTTEEARROW, ["<-|", ].as_slice()), (
CODEPOINT_LONGNAME_RIGHTTEEARROW, ["|->", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLELEFTARROW, ["<=", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLERIGHTARROW, ["=>", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLELEFTRIGHTARROW, ["<=>", 
].as_slice()), (CODEPOINT_LONGNAME_LEFTARROWBAR, ["|<-", ].as_slice()), (CODEPOINT_LONGNAME_RIGHTARROWBAR, ["->|", 
].as_slice()), (CODEPOINT_LONGNAME_MINUS, ["-", ].as_slice()), (CODEPOINT_LONGNAME_MINUSPLUS, ["-+", ].as_slice()), (
CODEPOINT_LONGNAME_DIVISIONSLASH, ["/", ].as_slice()), (CODEPOINT_LONGNAME_BACKSLASH, ["\\", ].as_slice()), (
CODEPOINT_LONGNAME_SMALLCIRCLE, ["()", ].as_slice()), (CODEPOINT_LONGNAME_INFINITY, ["Infinity", ].as_slice()), (
CODEPOINT_LONGNAME_DIVIDES, ["|", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLEVERTICALBAR, ["||", ].as_slice()), (
CODEPOINT_LONGNAME_NOTDOUBLEVERTICALBAR, ["!||", ].as_slice()), (CODEPOINT_LONGNAME_AND, ["&&", ].as_slice()), (
CODEPOINT_LONGNAME_OR, ["||", ].as_slice()), (CODEPOINT_LONGNAME_COLON, [":", ].as_slice()), (CODEPOINT_LONGNAME_TILDE, 
["~", ].as_slice()), (CODEPOINT_LONGNAME_NOTTILDE, ["!~", ].as_slice()), (CODEPOINT_LONGNAME_EQUALTILDE, ["=~", 
].as_slice()), (CODEPOINT_LONGNAME_TILDEEQUAL, ["~=", ].as_slice()), (CODEPOINT_LONGNAME_NOTTILDEEQUAL, ["!~=", 
].as_slice()), (CODEPOINT_LONGNAME_TILDEFULLEQUAL, ["~==", ].as_slice()), (CODEPOINT_LONGNAME_NOTTILDEFULLEQUAL, ["!~=="
, ].as_slice()), (CODEPOINT_LONGNAME_TILDETILDE, ["~~", ].as_slice()), (CODEPOINT_LONGNAME_NOTTILDETILDE, ["!~~", 
].as_slice()), (CODEPOINT_LONGNAME_NOTEQUAL, ["!=", ].as_slice()), (CODEPOINT_LONGNAME_LESSEQUAL, ["<=", ].as_slice()), 
(CODEPOINT_LONGNAME_GREATEREQUAL, [">=", ].as_slice()), (CODEPOINT_LONGNAME_LESSFULLEQUAL, ["<=", ].as_slice()), (
CODEPOINT_LONGNAME_GREATERFULLEQUAL, [">=", ].as_slice()), (CODEPOINT_LONGNAME_NOTLESSFULLEQUAL, ["!<==", ].as_slice())
, (CODEPOINT_LONGNAME_NOTGREATERFULLEQUAL, ["!>==", ].as_slice()), (CODEPOINT_LONGNAME_LESSLESS, ["<<", ].as_slice()), (
CODEPOINT_LONGNAME_GREATERGREATER, [">>", ].as_slice()), (CODEPOINT_LONGNAME_NOTLESS, ["!<", ].as_slice()), (
CODEPOINT_LONGNAME_NOTGREATER, ["!>", ].as_slice()), (CODEPOINT_LONGNAME_NOTLESSEQUAL, ["!<=", ].as_slice()), (
CODEPOINT_LONGNAME_NOTGREATEREQUAL, ["!>=", ].as_slice()), (CODEPOINT_LONGNAME_LESSTILDE, ["<~", ].as_slice()), (
CODEPOINT_LONGNAME_GREATERTILDE, [">~", ].as_slice()), (CODEPOINT_LONGNAME_NOTLESSTILDE, ["!<~", ].as_slice()), (
CODEPOINT_LONGNAME_NOTGREATERTILDE, ["!>~", ].as_slice()), (CODEPOINT_LONGNAME_LESSGREATER, ["<>", ].as_slice()), (
CODEPOINT_LONGNAME_GREATERLESS, ["><", ].as_slice()), (CODEPOINT_LONGNAME_NOTLESSGREATER, ["!<>", ].as_slice()), (
CODEPOINT_LONGNAME_NOTGREATERLESS, ["!><", ].as_slice()), (CODEPOINT_LONGNAME_CIRCLEPLUS, ["(+)", ].as_slice()), (
CODEPOINT_LONGNAME_CIRCLEMINUS, ["(-)", ].as_slice()), (CODEPOINT_LONGNAME_CIRCLETIMES, ["(x)", ].as_slice()), (
CODEPOINT_LONGNAME_CIRCLEDOT, ["(.)", ].as_slice()), (CODEPOINT_LONGNAME_RIGHTTEE, ["|-", ].as_slice()), (
CODEPOINT_LONGNAME_LEFTTEE, ["-|", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLERIGHTTEE, ["|=", ].as_slice()), (
CODEPOINT_LONGNAME_LEFTTRIANGLE, ["<|", ].as_slice()), (CODEPOINT_LONGNAME_RIGHTTRIANGLE, ["|>", ].as_slice()), (
CODEPOINT_LONGNAME_LEFTTRIANGLEEQUAL, ["<|=", ].as_slice()), (CODEPOINT_LONGNAME_RIGHTTRIANGLEEQUAL, ["|>=", 
].as_slice()), (CODEPOINT_LONGNAME_WEDGE, ["^", ].as_slice()), (CODEPOINT_LONGNAME_DIAMOND, ["<>", ].as_slice()), (
CODEPOINT_LONGNAME_STAR, ["*", ].as_slice()), (CODEPOINT_LONGNAME_LESSEQUALGREATER, ["<=>", ].as_slice()), (
CODEPOINT_LONGNAME_GREATEREQUALLESS, [">=<", ].as_slice()), (CODEPOINT_LONGNAME_NOTLEFTTRIANGLE, ["!<|", ].as_slice()), 
(CODEPOINT_LONGNAME_NOTRIGHTTRIANGLE, ["!|>", ].as_slice()), (CODEPOINT_LONGNAME_NOTLEFTTRIANGLEEQUAL, ["!<|=", 
].as_slice()), (CODEPOINT_LONGNAME_NOTRIGHTTRIANGLEEQUAL, ["!|>=", ].as_slice()), (CODEPOINT_LONGNAME_CENTERELLIPSIS, [
"...", ].as_slice()), (CODEPOINT_LONGNAME_LEFTANGLEBRACKET, ["<", ].as_slice()), (CODEPOINT_LONGNAME_RIGHTANGLEBRACKET, 
[">", ].as_slice()), (CODEPOINT_LONGNAME_SPACEINDICATOR, [" ", "_", ].as_slice()), (CODEPOINT_LONGNAME_SADSMILEY, [":-("
, ].as_slice()), (CODEPOINT_LONGNAME_HAPPYSMILEY, [":-)", ].as_slice()), (CODEPOINT_LONGNAME_SHARP, ["#", ].as_slice())
, (CODEPOINT_LONGNAME_LONGLEFTARROW, ["<--", ].as_slice()), (CODEPOINT_LONGNAME_LONGRIGHTARROW, ["-->", ].as_slice()), (
CODEPOINT_LONGNAME_LONGLEFTRIGHTARROW, ["<-->", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLELONGLEFTARROW, ["<==", 
].as_slice()), (CODEPOINT_LONGNAME_DOUBLELONGRIGHTARROW, ["==>", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLELONGLEFTRIGHTARROW, ["<==>", ].as_slice()), (CODEPOINT_LONGNAME_LEFTTRIANGLEBAR, ["<||", 
].as_slice()), (CODEPOINT_LONGNAME_RIGHTTRIANGLEBAR, ["||>", ].as_slice()), (CODEPOINT_LONGNAME_EQUIVALENT, ["<=>", 
].as_slice()), (CODEPOINT_LONGNAME_LESSSLANTEQUAL, ["<=", ].as_slice()), (CODEPOINT_LONGNAME_GREATERSLANTEQUAL, [">=", 
].as_slice()), (CODEPOINT_LONGNAME_NESTEDLESSLESS, ["<<", ].as_slice()), (CODEPOINT_LONGNAME_NESTEDGREATERGREATER, [">>"
, ].as_slice()), (CODEPOINT_LONGNAME_DOUBLELEFTTEE, ["=|", ].as_slice()), (CODEPOINT_LONGNAME_LEFTDOUBLEBRACKET, ["[[", 
].as_slice()), (CODEPOINT_LONGNAME_RIGHTDOUBLEBRACKET, ["]]", ].as_slice()), (CODEPOINT_LONGNAME_LEFTASSOCIATION, ["<|"
, ].as_slice()), (CODEPOINT_LONGNAME_RIGHTASSOCIATION, ["|>", ].as_slice()), (CODEPOINT_LONGNAME_TWOWAYRULE, ["<->", 
].as_slice()), (CODEPOINT_LONGNAME_INVISIBLESPACE, [" ", "", ].as_slice()), (CODEPOINT_LONGNAME_NEGATIVEVERYTHINSPACE, [
" ", ].as_slice()), (CODEPOINT_LONGNAME_NEGATIVETHINSPACE, [" ", ].as_slice()), (CODEPOINT_LONGNAME_NEGATIVEMEDIUMSPACE
, [" ", ].as_slice()), (CODEPOINT_LONGNAME_NEGATIVETHICKSPACE, [" ", ].as_slice()), (CODEPOINT_LONGNAME_IMPLICITPLUS, [
"+", ].as_slice()), (CODEPOINT_LONGNAME_NULL, ["", ].as_slice()), (CODEPOINT_LONGNAME_INDENTINGNEWLINE, ["\n", 
].as_slice()), (CODEPOINT_LONGNAME_CONTINUATION, ["\\", ].as_slice()), (CODEPOINT_LONGNAME_ROUNDSPACEINDICATOR, ["_", 
].as_slice()), (CODEPOINT_LONGNAME_INVISIBLEPREFIXSCRIPTBASE, ["", ].as_slice()), (
CODEPOINT_LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE, ["", ].as_slice()), (CODEPOINT_LONGNAME_PAGEBREAKABOVE, ["\n", 
].as_slice()), (CODEPOINT_LONGNAME_PAGEBREAKBELOW, ["\n", ].as_slice()), (CODEPOINT_LONGNAME_DISCRETIONARYPAGEBREAKABOVE
, ["\n", ].as_slice()), (CODEPOINT_LONGNAME_DISCRETIONARYPAGEBREAKBELOW, ["\n", ].as_slice()), (
CODEPOINT_LONGNAME_TRANSPOSE, ["T", ].as_slice()), (CODEPOINT_LONGNAME_CONJUGATE, ["*", ].as_slice()), (
CODEPOINT_LONGNAME_CONJUGATETRANSPOSE, ["t", ].as_slice()), (CODEPOINT_LONGNAME_HERMITIANCONJUGATE, ["H", ].as_slice())
, (CODEPOINT_LONGNAME_VERTICALBAR, ["|", ].as_slice()), (CODEPOINT_LONGNAME_NOTVERTICALBAR, ["!|", ].as_slice()), (
CODEPOINT_LONGNAME_DIRECTEDEDGE, ["->", ].as_slice()), (CODEPOINT_LONGNAME_CONTINUEDFRACTIONK, ["K", ].as_slice()), (
CODEPOINT_LONGNAME_NOTEQUALTILDE, ["!=~", ].as_slice()), (CODEPOINT_LONGNAME_NOTLEFTTRIANGLEBAR, ["!<||", ].as_slice())
, (CODEPOINT_LONGNAME_NOTRIGHTTRIANGLEBAR, ["!||>", ].as_slice()), (CODEPOINT_LONGNAME_NOTLESSLESS, ["!<<", ].as_slice()
), (CODEPOINT_LONGNAME_NOTNESTEDLESSLESS, ["!<<", ].as_slice()), (CODEPOINT_LONGNAME_NOTLESSSLANTEQUAL, ["!<=", 
].as_slice()), (CODEPOINT_LONGNAME_NOTGREATERGREATER, ["!>>", ].as_slice()), (CODEPOINT_LONGNAME_NOTNESTEDGREATERGREATER
, ["!>>", ].as_slice()), (CODEPOINT_LONGNAME_NOTGREATERSLANTEQUAL, ["!>=", ].as_slice()), (CODEPOINT_LONGNAME_EQUAL, [
"==", ].as_slice()), (CODEPOINT_LONGNAME_VERTICALSEPARATOR, ["|", ].as_slice()), (CODEPOINT_LONGNAME_CROSS, ["x", 
].as_slice()), (CODEPOINT_LONGNAME_FUNCTION, ["|->", ].as_slice()), (CODEPOINT_LONGNAME_RULEDELAYED, [":>", ].as_slice()
), (CODEPOINT_LONGNAME_SQUARE, ["[]", ].as_slice()), (CODEPOINT_LONGNAME_RULE, ["->", ].as_slice()), (
CODEPOINT_LONGNAME_IMPLIES, ["=>", ].as_slice()), (CODEPOINT_LONGNAME_SHORTRIGHTARROW, ["->", ].as_slice()), (
CODEPOINT_LONGNAME_SHORTLEFTARROW, ["<-", ].as_slice()), (CODEPOINT_LONGNAME_SELECTIONPLACEHOLDER, ["[*]", ].as_slice())
, (CODEPOINT_LONGNAME_PLACEHOLDER, ["[ ]", ].as_slice()), (CODEPOINT_LONGNAME_LEFTBRACKETINGBAR, ["|", ].as_slice()), (
CODEPOINT_LONGNAME_RIGHTBRACKETINGBAR, ["|", ].as_slice()), (CODEPOINT_LONGNAME_LEFTDOUBLEBRACKETINGBAR, ["||", 
].as_slice()), (CODEPOINT_LONGNAME_RIGHTDOUBLEBRACKETINGBAR, ["||", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTA, ["a", 
].as_slice()), (CODEPOINT_LONGNAME_SCRIPTB, ["b", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTC, ["c", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTD, ["d", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTF, ["f", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTH, ["h", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTI, ["i", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTJ, ["j", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTK, ["k", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTM, ["m", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTN, ["n", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTP, ["p", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTQ, ["q", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTR, ["r", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTS, ["s", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTT, ["t", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTU, ["u", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTV, ["v", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTW, ["w", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTX, ["x", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTY, ["y", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTZ, ["z", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICA, ["a", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICB, ["b", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICC, ["c", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICD, ["d", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICE, ["e", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICF, ["f", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICG, ["g", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICH, ["h", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICI, ["i", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICJ, ["j", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICK, ["k", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICL, ["l", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICM, ["m", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICN, ["n", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICO, ["o", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICP, ["p", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICQ, ["q", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICR, ["R", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICS, ["s", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICT, ["t", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICU, ["u", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICV, ["v", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICW, ["w", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICX, ["x", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICY, ["y", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICZ, ["z", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKA, ["a", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKB, ["b", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKC, ["c", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKD, ["d", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKE, ["e", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKF, ["f", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKG, ["g", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKH, ["h", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKI, ["i", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKJ, ["j", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKK, ["k", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKL, ["l", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKM, ["m", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKN, ["n", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKO, ["o", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKP, ["p", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKQ, ["q", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKR, ["r", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKS, ["s", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKT, ["t", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKU, ["u", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKV, ["v", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKW, ["w", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKX, ["x", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKY, ["y", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKZ, ["z", ].as_slice()), (CODEPOINT_LONGNAME_DOTLESSJ, ["j", ].as_slice()), (
CODEPOINT_LONGNAME_FREAKEDSMILEY, [":-@", ].as_slice()), (CODEPOINT_LONGNAME_NEUTRALSMILEY, [":-|", ].as_slice()), (
CODEPOINT_LONGNAME_NUMBERSIGN, ["#", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTDOTLESSI, ["i", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTDOTLESSJ, ["j", ].as_slice()), (CODEPOINT_LONGNAME_CAPITALDIFFERENTIALD, ["D", ].as_slice()), (
CODEPOINT_LONGNAME_DIFFERENTIALD, ["d", ].as_slice()), (CODEPOINT_LONGNAME_EXPONENTIALE, ["e", ].as_slice()), (
CODEPOINT_LONGNAME_IMAGINARYI, ["i", ].as_slice()), (CODEPOINT_LONGNAME_IMAGINARYJ, ["j", ].as_slice()), (
CODEPOINT_LONGNAME_LETTERSPACE, ["_", ].as_slice()), (CODEPOINT_LONGNAME_TRIPLEDOT, ["...", ].as_slice()), (
CODEPOINT_LONGNAME_LEFTSKELETON, ["<<", ].as_slice()), (CODEPOINT_LONGNAME_RIGHTSKELETON, [">>", ].as_slice()), (
CODEPOINT_LONGNAME_CONTROLKEY, ["CTRL", ].as_slice()), (CODEPOINT_LONGNAME_INVISIBLECOMMA, [",", "", ].as_slice()), (
CODEPOINT_LONGNAME_RETURNKEY, ["RET", ].as_slice()), (CODEPOINT_LONGNAME_ERRORINDICATOR, ["^^^", ].as_slice()), (
CODEPOINT_LONGNAME_ESCAPEKEY, ["ESC", ].as_slice()), (CODEPOINT_LONGNAME_COMMANDKEY, ["CMD", ].as_slice()), (
CODEPOINT_LONGNAME_LEFTMODIFIED, ["-[", ].as_slice()), (CODEPOINT_LONGNAME_RIGHTMODIFIED, ["]", ].as_slice()), (
CODEPOINT_LONGNAME_INVISIBLEAPPLICATION, ["@", "", ].as_slice()), (CODEPOINT_LONGNAME_DISCRETIONARYLINESEPARATOR, ["\n"
, ].as_slice()), (CODEPOINT_LONGNAME_DISCRETIONARYPARAGRAPHSEPARATOR, ["\n\n", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTCAPITALA, ["A", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTCAPITALC, ["C", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTCAPITALD, ["D", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTCAPITALG, ["G", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTCAPITALJ, ["J", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTCAPITALK, ["K", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTCAPITALN, ["N", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTCAPITALO, ["O", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTCAPITALP, ["P", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTCAPITALQ, ["Q", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTCAPITALS, ["S", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTCAPITALT, ["T", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTCAPITALU, ["U", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTCAPITALV, ["V", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTCAPITALW, ["W", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTCAPITALX, ["X", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTCAPITALY, ["Y", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTCAPITALZ, ["Z", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICCAPITALA, ["A", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICCAPITALB, ["B", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICCAPITALD, ["D", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICCAPITALE, ["E", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICCAPITALF, ["F", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICCAPITALG, ["G", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICCAPITALJ, ["J", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICCAPITALK, ["K", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICCAPITALL, ["L", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICCAPITALM, ["M", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICCAPITALN, ["N", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICCAPITALO, ["O", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICCAPITALP, ["P", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICCAPITALQ, ["Q", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICCAPITALS, ["S", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICCAPITALT, ["T", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICCAPITALU, ["U", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICCAPITALV, ["V", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICCAPITALW, ["W", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICCAPITALX, ["X", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICCAPITALY, ["Y", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALA, ["A", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALB, ["B", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALC, ["C", 
].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALD, ["D", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALE, 
["E", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALF, ["F", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALG, ["G", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALH, ["H", 
].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALI, ["I", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALJ, 
["J", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALK, ["K", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALL, ["L", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALM, ["M", 
].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALN, ["N", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALO, 
["O", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALP, ["P", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALQ, ["Q", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALR, ["R", 
].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALS, ["S", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALT, 
["T", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALU, ["U", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALV, ["V", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALW, ["W", 
].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALX, ["X", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALY, 
["Y", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKCAPITALZ, ["Z", ].as_slice()), (CODEPOINT_LONGNAME_TABKEY, ["TAB", 
].as_slice()), (CODEPOINT_LONGNAME_SPACEKEY, ["SPC", ].as_slice()), (CODEPOINT_LONGNAME_DELETEKEY, ["DEL", ].as_slice())
, (CODEPOINT_LONGNAME_ALTKEY, ["ALT", ].as_slice()), (CODEPOINT_LONGNAME_OPTIONKEY, ["OPT", ].as_slice()), (
CODEPOINT_LONGNAME_LONGEQUAL, ["==", ].as_slice()), (CODEPOINT_LONGNAME_CONSTANTC, ["c", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKZERO, ["0", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKONE, ["1", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKTWO, ["2", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKTHREE, ["3", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKFOUR, ["4", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKFIVE, ["5", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKSIX, ["6", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKSEVEN, ["7", ].as_slice()), (
CODEPOINT_LONGNAME_DOUBLESTRUCKEIGHT, ["8", ].as_slice()), (CODEPOINT_LONGNAME_DOUBLESTRUCKNINE, ["9", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICZERO, ["0", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICONE, ["1", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICTWO, ["2", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICTHREE, ["3", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICFOUR, ["4", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICFIVE, ["5", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICSIX, ["6", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICSEVEN, ["7", ].as_slice()), (
CODEPOINT_LONGNAME_GOTHICEIGHT, ["8", ].as_slice()), (CODEPOINT_LONGNAME_GOTHICNINE, ["9", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTZERO, ["0", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTONE, ["1", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTTWO, ["2", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTTHREE, ["3", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTFOUR, ["4", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTFIVE, ["5", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTSIX, ["6", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTSEVEN, ["7", ].as_slice()), (
CODEPOINT_LONGNAME_SCRIPTEIGHT, ["8", ].as_slice()), (CODEPOINT_LONGNAME_SCRIPTNINE, ["9", ].as_slice()), (
CODEPOINT_LONGNAME_NUMBERCOMMA, [",", ].as_slice()), (CODEPOINT_LONGNAME_FORMALA, ["a", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALB, ["b", ].as_slice()), (CODEPOINT_LONGNAME_FORMALC, ["c", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALD, ["d", ].as_slice()), (CODEPOINT_LONGNAME_FORMALE, ["e", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALF, ["f", ].as_slice()), (CODEPOINT_LONGNAME_FORMALG, ["g", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALH, ["h", ].as_slice()), (CODEPOINT_LONGNAME_FORMALI, ["i", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALJ, ["j", ].as_slice()), (CODEPOINT_LONGNAME_FORMALK, ["k", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALL, ["l", ].as_slice()), (CODEPOINT_LONGNAME_FORMALM, ["m", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALN, ["n", ].as_slice()), (CODEPOINT_LONGNAME_FORMALO, ["o", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALP, ["p", ].as_slice()), (CODEPOINT_LONGNAME_FORMALQ, ["q", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALR, ["r", ].as_slice()), (CODEPOINT_LONGNAME_FORMALS, ["s", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALT, ["t", ].as_slice()), (CODEPOINT_LONGNAME_FORMALU, ["u", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALV, ["v", ].as_slice()), (CODEPOINT_LONGNAME_FORMALW, ["w", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALX, ["x", ].as_slice()), (CODEPOINT_LONGNAME_FORMALY, ["y", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALZ, ["z", ].as_slice()), (CODEPOINT_LONGNAME_FORMALCAPITALA, ["A", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALCAPITALB, ["B", ].as_slice()), (CODEPOINT_LONGNAME_FORMALCAPITALC, ["C", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALCAPITALD, ["D", ].as_slice()), (CODEPOINT_LONGNAME_FORMALCAPITALE, ["E", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALCAPITALF, ["F", ].as_slice()), (CODEPOINT_LONGNAME_FORMALCAPITALG, ["G", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALCAPITALH, ["H", ].as_slice()), (CODEPOINT_LONGNAME_FORMALCAPITALI, ["I", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALCAPITALJ, ["J", ].as_slice()), (CODEPOINT_LONGNAME_FORMALCAPITALK, ["K", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALCAPITALL, ["L", ].as_slice()), (CODEPOINT_LONGNAME_FORMALCAPITALM, ["M", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALCAPITALN, ["N", ].as_slice()), (CODEPOINT_LONGNAME_FORMALCAPITALO, ["O", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALCAPITALP, ["P", ].as_slice()), (CODEPOINT_LONGNAME_FORMALCAPITALQ, ["Q", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALCAPITALR, ["R", ].as_slice()), (CODEPOINT_LONGNAME_FORMALCAPITALS, ["S", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALCAPITALT, ["T", ].as_slice()), (CODEPOINT_LONGNAME_FORMALCAPITALU, ["U", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALCAPITALV, ["V", ].as_slice()), (CODEPOINT_LONGNAME_FORMALCAPITALW, ["W", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALCAPITALX, ["X", ].as_slice()), (CODEPOINT_LONGNAME_FORMALCAPITALY, ["Y", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALCAPITALZ, ["Z", ].as_slice()), (CODEPOINT_LONGNAME_FILIGATURE, ["fi", ].as_slice()), (
CODEPOINT_LONGNAME_FLLIGATURE, ["fl", ].as_slice()), (CODEPOINT_LONGNAME_UNKNOWNGLYPH, ["?", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTA, ["a", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTB, ["b", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTC, ["c", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTD, ["d", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTE, ["e", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTF, ["f", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTG, ["g", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTH, ["h", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTI, ["i", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTJ, ["j", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTK, ["k", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTL, ["l", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTM, ["m", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTN, ["n", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTO, ["o", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTP, ["p", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTQ, ["q", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTR, ["r", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTS, ["s", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTT, ["t", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTU, ["u", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTV, ["v", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTW, ["w", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTX, ["x", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTY, ["y", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTZ, ["z", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALA, ["A", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALB, ["B", 
].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALC, ["C", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALD, 
["D", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALE, ["E", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALF, ["F", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALG, ["G", 
].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALH, ["H", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALI, 
["I", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALJ, ["J", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALK, ["K", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALL, ["L", 
].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALM, ["M", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALN, 
["N", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALO, ["O", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALP, ["P", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALQ, ["Q", 
].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALR, ["R", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALS, 
["S", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALT, ["T", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALU, ["U", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALV, ["V", 
].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALW, ["W", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALX, 
["X", ].as_slice()), (CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALY, ["Y", ].as_slice()), (
CODEPOINT_LONGNAME_FORMALSCRIPTCAPITALZ, ["Z", ].as_slice()), 
]));

//
//
//
pub const MB_PUNCTUATION_CODE_POINTS: [CodePoint; MBPUNCTUATIONCODEPOINTS_COUNT] = [
CODEPOINT_LONGNAME_NOT, CODEPOINT_LONGNAME_PLUSMINUS, CODEPOINT_LONGNAME_CENTERDOT, CODEPOINT_LONGNAME_TIMES, 
CODEPOINT_LONGNAME_DIVIDE, CODEPOINT_LONGNAME_OPENCURLYQUOTE, CODEPOINT_LONGNAME_CLOSECURLYQUOTE, 
CODEPOINT_LONGNAME_OPENCURLYDOUBLEQUOTE, CODEPOINT_LONGNAME_CLOSECURLYDOUBLEQUOTE, CODEPOINT_LONGNAME_INVISIBLETIMES, 
CODEPOINT_LONGNAME_LEFTARROW, CODEPOINT_LONGNAME_UPARROW, CODEPOINT_LONGNAME_RIGHTARROW, CODEPOINT_LONGNAME_DOWNARROW, 
CODEPOINT_LONGNAME_LEFTRIGHTARROW, CODEPOINT_LONGNAME_UPDOWNARROW, CODEPOINT_LONGNAME_UPPERLEFTARROW, 
CODEPOINT_LONGNAME_UPPERRIGHTARROW, CODEPOINT_LONGNAME_LOWERRIGHTARROW, CODEPOINT_LONGNAME_LOWERLEFTARROW, 
CODEPOINT_LONGNAME_LEFTTEEARROW, CODEPOINT_LONGNAME_UPTEEARROW, CODEPOINT_LONGNAME_RIGHTTEEARROW, 
CODEPOINT_LONGNAME_DOWNTEEARROW, CODEPOINT_LONGNAME_LEFTVECTOR, CODEPOINT_LONGNAME_DOWNLEFTVECTOR, 
CODEPOINT_LONGNAME_RIGHTUPVECTOR, CODEPOINT_LONGNAME_LEFTUPVECTOR, CODEPOINT_LONGNAME_RIGHTVECTOR, 
CODEPOINT_LONGNAME_DOWNRIGHTVECTOR, CODEPOINT_LONGNAME_RIGHTDOWNVECTOR, CODEPOINT_LONGNAME_LEFTDOWNVECTOR, 
CODEPOINT_LONGNAME_RIGHTARROWLEFTARROW, CODEPOINT_LONGNAME_UPARROWDOWNARROW, CODEPOINT_LONGNAME_LEFTARROWRIGHTARROW, 
CODEPOINT_LONGNAME_REVERSEEQUILIBRIUM, CODEPOINT_LONGNAME_EQUILIBRIUM, CODEPOINT_LONGNAME_DOUBLELEFTARROW, 
CODEPOINT_LONGNAME_DOUBLEUPARROW, CODEPOINT_LONGNAME_DOUBLERIGHTARROW, CODEPOINT_LONGNAME_DOUBLEDOWNARROW, 
CODEPOINT_LONGNAME_DOUBLELEFTRIGHTARROW, CODEPOINT_LONGNAME_DOUBLEUPDOWNARROW, CODEPOINT_LONGNAME_LEFTARROWBAR, 
CODEPOINT_LONGNAME_RIGHTARROWBAR, CODEPOINT_LONGNAME_DOWNARROWUPARROW, CODEPOINT_LONGNAME_FORALL, 
CODEPOINT_LONGNAME_PARTIALD, CODEPOINT_LONGNAME_EXISTS, CODEPOINT_LONGNAME_NOTEXISTS, CODEPOINT_LONGNAME_DEL, 
CODEPOINT_LONGNAME_ELEMENT, CODEPOINT_LONGNAME_NOTELEMENT, CODEPOINT_LONGNAME_REVERSEELEMENT, 
CODEPOINT_LONGNAME_NOTREVERSEELEMENT, CODEPOINT_LONGNAME_SUCHTHAT, CODEPOINT_LONGNAME_PRODUCT, 
CODEPOINT_LONGNAME_COPRODUCT, CODEPOINT_LONGNAME_SUM, CODEPOINT_LONGNAME_MINUS, CODEPOINT_LONGNAME_MINUSPLUS, 
CODEPOINT_LONGNAME_DIVISIONSLASH, CODEPOINT_LONGNAME_BACKSLASH, CODEPOINT_LONGNAME_SMALLCIRCLE, CODEPOINT_LONGNAME_SQRT,
 CODEPOINT_LONGNAME_CUBEROOT, CODEPOINT_LONGNAME_PROPORTIONAL, CODEPOINT_LONGNAME_DIVIDES, 
CODEPOINT_LONGNAME_DOUBLEVERTICALBAR, CODEPOINT_LONGNAME_NOTDOUBLEVERTICALBAR, CODEPOINT_LONGNAME_AND, 
CODEPOINT_LONGNAME_OR, CODEPOINT_LONGNAME_INTEGRAL, CODEPOINT_LONGNAME_CONTOURINTEGRAL, 
CODEPOINT_LONGNAME_DOUBLECONTOURINTEGRAL, CODEPOINT_LONGNAME_CLOCKWISECONTOURINTEGRAL, 
CODEPOINT_LONGNAME_COUNTERCLOCKWISECONTOURINTEGRAL, CODEPOINT_LONGNAME_THEREFORE, CODEPOINT_LONGNAME_BECAUSE, 
CODEPOINT_LONGNAME_COLON, CODEPOINT_LONGNAME_PROPORTION, CODEPOINT_LONGNAME_TILDE, CODEPOINT_LONGNAME_VERTICALTILDE, 
CODEPOINT_LONGNAME_NOTTILDE, CODEPOINT_LONGNAME_EQUALTILDE, CODEPOINT_LONGNAME_TILDEEQUAL, 
CODEPOINT_LONGNAME_NOTTILDEEQUAL, CODEPOINT_LONGNAME_TILDEFULLEQUAL, CODEPOINT_LONGNAME_NOTTILDEFULLEQUAL, 
CODEPOINT_LONGNAME_TILDETILDE, CODEPOINT_LONGNAME_NOTTILDETILDE, CODEPOINT_LONGNAME_CUPCAP, 
CODEPOINT_LONGNAME_HUMPDOWNHUMP, CODEPOINT_LONGNAME_HUMPEQUAL, CODEPOINT_LONGNAME_DOTEQUAL, CODEPOINT_LONGNAME_NOTEQUAL,
 CODEPOINT_LONGNAME_CONGRUENT, CODEPOINT_LONGNAME_NOTCONGRUENT, CODEPOINT_LONGNAME_LESSEQUAL, 
CODEPOINT_LONGNAME_GREATEREQUAL, CODEPOINT_LONGNAME_LESSFULLEQUAL, CODEPOINT_LONGNAME_GREATERFULLEQUAL, 
CODEPOINT_LONGNAME_NOTLESSFULLEQUAL, CODEPOINT_LONGNAME_NOTGREATERFULLEQUAL, CODEPOINT_LONGNAME_LESSLESS, 
CODEPOINT_LONGNAME_GREATERGREATER, CODEPOINT_LONGNAME_NOTCUPCAP, CODEPOINT_LONGNAME_NOTLESS, 
CODEPOINT_LONGNAME_NOTGREATER, CODEPOINT_LONGNAME_NOTLESSEQUAL, CODEPOINT_LONGNAME_NOTGREATEREQUAL, 
CODEPOINT_LONGNAME_LESSTILDE, CODEPOINT_LONGNAME_GREATERTILDE, CODEPOINT_LONGNAME_NOTLESSTILDE, 
CODEPOINT_LONGNAME_NOTGREATERTILDE, CODEPOINT_LONGNAME_LESSGREATER, CODEPOINT_LONGNAME_GREATERLESS, 
CODEPOINT_LONGNAME_NOTLESSGREATER, CODEPOINT_LONGNAME_NOTGREATERLESS, CODEPOINT_LONGNAME_PRECEDES, 
CODEPOINT_LONGNAME_SUCCEEDS, CODEPOINT_LONGNAME_PRECEDESSLANTEQUAL, CODEPOINT_LONGNAME_SUCCEEDSSLANTEQUAL, 
CODEPOINT_LONGNAME_PRECEDESTILDE, CODEPOINT_LONGNAME_SUCCEEDSTILDE, CODEPOINT_LONGNAME_NOTPRECEDES, 
CODEPOINT_LONGNAME_NOTSUCCEEDS, CODEPOINT_LONGNAME_SUBSET, CODEPOINT_LONGNAME_SUPERSET, CODEPOINT_LONGNAME_NOTSUBSET, 
CODEPOINT_LONGNAME_NOTSUPERSET, CODEPOINT_LONGNAME_SUBSETEQUAL, CODEPOINT_LONGNAME_SUPERSETEQUAL, 
CODEPOINT_LONGNAME_NOTSUBSETEQUAL, CODEPOINT_LONGNAME_NOTSUPERSETEQUAL, CODEPOINT_LONGNAME_UNIONPLUS, 
CODEPOINT_LONGNAME_SQUARESUBSET, CODEPOINT_LONGNAME_SQUARESUPERSET, CODEPOINT_LONGNAME_SQUARESUBSETEQUAL, 
CODEPOINT_LONGNAME_SQUARESUPERSETEQUAL, CODEPOINT_LONGNAME_SQUAREINTERSECTION, CODEPOINT_LONGNAME_SQUAREUNION, 
CODEPOINT_LONGNAME_CIRCLEPLUS, CODEPOINT_LONGNAME_CIRCLEMINUS, CODEPOINT_LONGNAME_CIRCLETIMES, 
CODEPOINT_LONGNAME_CIRCLEDOT, CODEPOINT_LONGNAME_RIGHTTEE, CODEPOINT_LONGNAME_LEFTTEE, CODEPOINT_LONGNAME_DOWNTEE, 
CODEPOINT_LONGNAME_UPTEE, CODEPOINT_LONGNAME_DOUBLERIGHTTEE, CODEPOINT_LONGNAME_LEFTTRIANGLE, 
CODEPOINT_LONGNAME_RIGHTTRIANGLE, CODEPOINT_LONGNAME_LEFTTRIANGLEEQUAL, CODEPOINT_LONGNAME_RIGHTTRIANGLEEQUAL, 
CODEPOINT_LONGNAME_XOR, CODEPOINT_LONGNAME_NAND, CODEPOINT_LONGNAME_NOR, CODEPOINT_LONGNAME_WEDGE, 
CODEPOINT_LONGNAME_VEE, CODEPOINT_LONGNAME_INTERSECTION, CODEPOINT_LONGNAME_UNION, CODEPOINT_LONGNAME_DIAMOND, 
CODEPOINT_LONGNAME_STAR, CODEPOINT_LONGNAME_LESSEQUALGREATER, CODEPOINT_LONGNAME_GREATEREQUALLESS, 
CODEPOINT_LONGNAME_NOTPRECEDESSLANTEQUAL, CODEPOINT_LONGNAME_NOTSUCCEEDSSLANTEQUAL, 
CODEPOINT_LONGNAME_NOTSQUARESUBSETEQUAL, CODEPOINT_LONGNAME_NOTSQUARESUPERSETEQUAL, CODEPOINT_LONGNAME_NOTPRECEDESTILDE,
 CODEPOINT_LONGNAME_NOTSUCCEEDSTILDE, CODEPOINT_LONGNAME_NOTLEFTTRIANGLE, CODEPOINT_LONGNAME_NOTRIGHTTRIANGLE, 
CODEPOINT_LONGNAME_NOTLEFTTRIANGLEEQUAL, CODEPOINT_LONGNAME_NOTRIGHTTRIANGLEEQUAL, CODEPOINT_LONGNAME_LEFTCEILING, 
CODEPOINT_LONGNAME_RIGHTCEILING, CODEPOINT_LONGNAME_LEFTFLOOR, CODEPOINT_LONGNAME_RIGHTFLOOR, CODEPOINT_LONGNAME_CAP, 
CODEPOINT_LONGNAME_CUP, CODEPOINT_LONGNAME_LEFTANGLEBRACKET, CODEPOINT_LONGNAME_RIGHTANGLEBRACKET, 
CODEPOINT_LONGNAME_PERPENDICULAR, CODEPOINT_LONGNAME_LONGLEFTARROW, CODEPOINT_LONGNAME_LONGRIGHTARROW, 
CODEPOINT_LONGNAME_LONGLEFTRIGHTARROW, CODEPOINT_LONGNAME_DOUBLELONGLEFTARROW, CODEPOINT_LONGNAME_DOUBLELONGRIGHTARROW, 
CODEPOINT_LONGNAME_DOUBLELONGLEFTRIGHTARROW, CODEPOINT_LONGNAME_UPARROWBAR, CODEPOINT_LONGNAME_DOWNARROWBAR, 
CODEPOINT_LONGNAME_LEFTRIGHTVECTOR, CODEPOINT_LONGNAME_RIGHTUPDOWNVECTOR, CODEPOINT_LONGNAME_DOWNLEFTRIGHTVECTOR, 
CODEPOINT_LONGNAME_LEFTUPDOWNVECTOR, CODEPOINT_LONGNAME_LEFTVECTORBAR, CODEPOINT_LONGNAME_RIGHTVECTORBAR, 
CODEPOINT_LONGNAME_RIGHTUPVECTORBAR, CODEPOINT_LONGNAME_RIGHTDOWNVECTORBAR, CODEPOINT_LONGNAME_DOWNLEFTVECTORBAR, 
CODEPOINT_LONGNAME_DOWNRIGHTVECTORBAR, CODEPOINT_LONGNAME_LEFTUPVECTORBAR, CODEPOINT_LONGNAME_LEFTDOWNVECTORBAR, 
CODEPOINT_LONGNAME_LEFTTEEVECTOR, CODEPOINT_LONGNAME_RIGHTTEEVECTOR, CODEPOINT_LONGNAME_RIGHTUPTEEVECTOR, 
CODEPOINT_LONGNAME_RIGHTDOWNTEEVECTOR, CODEPOINT_LONGNAME_DOWNLEFTTEEVECTOR, CODEPOINT_LONGNAME_DOWNRIGHTTEEVECTOR, 
CODEPOINT_LONGNAME_LEFTUPTEEVECTOR, CODEPOINT_LONGNAME_LEFTDOWNTEEVECTOR, CODEPOINT_LONGNAME_UPEQUILIBRIUM, 
CODEPOINT_LONGNAME_REVERSEUPEQUILIBRIUM, CODEPOINT_LONGNAME_ROUNDIMPLIES, CODEPOINT_LONGNAME_LEFTTRIANGLEBAR, 
CODEPOINT_LONGNAME_RIGHTTRIANGLEBAR, CODEPOINT_LONGNAME_EQUIVALENT, CODEPOINT_LONGNAME_LESSSLANTEQUAL, 
CODEPOINT_LONGNAME_GREATERSLANTEQUAL, CODEPOINT_LONGNAME_NESTEDLESSLESS, CODEPOINT_LONGNAME_NESTEDGREATERGREATER, 
CODEPOINT_LONGNAME_PRECEDESEQUAL, CODEPOINT_LONGNAME_SUCCEEDSEQUAL, CODEPOINT_LONGNAME_DOUBLELEFTTEE, 
CODEPOINT_LONGNAME_LEFTDOUBLEBRACKET, CODEPOINT_LONGNAME_RIGHTDOUBLEBRACKET, CODEPOINT_LONGNAME_LEFTASSOCIATION, 
CODEPOINT_LONGNAME_RIGHTASSOCIATION, CODEPOINT_LONGNAME_TWOWAYRULE, CODEPOINT_LONGNAME_PIECEWISE, 
CODEPOINT_LONGNAME_IMPLICITPLUS, CODEPOINT_LONGNAME_AUTOLEFTMATCH, CODEPOINT_LONGNAME_AUTORIGHTMATCH, 
CODEPOINT_LONGNAME_INVISIBLEPREFIXSCRIPTBASE, CODEPOINT_LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE, 
CODEPOINT_LONGNAME_TRANSPOSE, CODEPOINT_LONGNAME_CONJUGATE, CODEPOINT_LONGNAME_CONJUGATETRANSPOSE, 
CODEPOINT_LONGNAME_HERMITIANCONJUGATE, CODEPOINT_LONGNAME_VERTICALBAR, CODEPOINT_LONGNAME_NOTVERTICALBAR, 
CODEPOINT_LONGNAME_DISTRIBUTED, CODEPOINT_LONGNAME_CONDITIONED, CODEPOINT_LONGNAME_UNDIRECTEDEDGE, 
CODEPOINT_LONGNAME_DIRECTEDEDGE, CODEPOINT_LONGNAME_CONTINUEDFRACTIONK, CODEPOINT_LONGNAME_TENSORPRODUCT, 
CODEPOINT_LONGNAME_TENSORWEDGE, CODEPOINT_LONGNAME_PROBABILITYPR, CODEPOINT_LONGNAME_EXPECTATIONE, 
CODEPOINT_LONGNAME_PERMUTATIONPRODUCT, CODEPOINT_LONGNAME_NOTEQUALTILDE, CODEPOINT_LONGNAME_NOTHUMPEQUAL, 
CODEPOINT_LONGNAME_NOTHUMPDOWNHUMP, CODEPOINT_LONGNAME_NOTLEFTTRIANGLEBAR, CODEPOINT_LONGNAME_NOTRIGHTTRIANGLEBAR, 
CODEPOINT_LONGNAME_NOTLESSLESS, CODEPOINT_LONGNAME_NOTNESTEDLESSLESS, CODEPOINT_LONGNAME_NOTLESSSLANTEQUAL, 
CODEPOINT_LONGNAME_NOTGREATERGREATER, CODEPOINT_LONGNAME_NOTNESTEDGREATERGREATER, 
CODEPOINT_LONGNAME_NOTGREATERSLANTEQUAL, CODEPOINT_LONGNAME_NOTPRECEDESEQUAL, CODEPOINT_LONGNAME_NOTSUCCEEDSEQUAL, 
CODEPOINT_LONGNAME_NOTSQUARESUBSET, CODEPOINT_LONGNAME_NOTSQUARESUPERSET, CODEPOINT_LONGNAME_EQUAL, 
CODEPOINT_LONGNAME_VERTICALSEPARATOR, CODEPOINT_LONGNAME_VECTORGREATER, CODEPOINT_LONGNAME_VECTORGREATEREQUAL, 
CODEPOINT_LONGNAME_VECTORLESS, CODEPOINT_LONGNAME_VECTORLESSEQUAL, CODEPOINT_LONGNAME_LIMIT, CODEPOINT_LONGNAME_MAXLIMIT
, CODEPOINT_LONGNAME_MINLIMIT, CODEPOINT_LONGNAME_CROSS, CODEPOINT_LONGNAME_FUNCTION, CODEPOINT_LONGNAME_XNOR, 
CODEPOINT_LONGNAME_DISCRETESHIFT, CODEPOINT_LONGNAME_DIFFERENCEDELTA, CODEPOINT_LONGNAME_DISCRETERATIO, 
CODEPOINT_LONGNAME_RULEDELAYED, CODEPOINT_LONGNAME_SQUARE, CODEPOINT_LONGNAME_RULE, CODEPOINT_LONGNAME_IMPLIES, 
CODEPOINT_LONGNAME_SHORTRIGHTARROW, CODEPOINT_LONGNAME_SHORTLEFTARROW, CODEPOINT_LONGNAME_SHORTUPARROW, 
CODEPOINT_LONGNAME_SHORTDOWNARROW, CODEPOINT_LONGNAME_APPLICATION, CODEPOINT_LONGNAME_LEFTBRACKETINGBAR, 
CODEPOINT_LONGNAME_RIGHTBRACKETINGBAR, CODEPOINT_LONGNAME_LEFTDOUBLEBRACKETINGBAR, 
CODEPOINT_LONGNAME_RIGHTDOUBLEBRACKETINGBAR, CODEPOINT_LONGNAME_CAPITALDIFFERENTIALD, CODEPOINT_LONGNAME_DIFFERENTIALD, 
CODEPOINT_LONGNAME_INVISIBLECOMMA, CODEPOINT_LONGNAME_INVISIBLEAPPLICATION, CODEPOINT_LONGNAME_LONGEQUAL, 
];

//
//
//
pub const MB_WHITESPACE_CODE_POINTS: [CodePoint; MBWHITESPACECODEPOINTS_COUNT] = [
CODEPOINT_LONGNAME_NONBREAKINGSPACE, CODEPOINT_LONGNAME_THICKSPACE, CODEPOINT_LONGNAME_THINSPACE, 
CODEPOINT_LONGNAME_VERYTHINSPACE, CODEPOINT_LONGNAME_MEDIUMSPACE, CODEPOINT_LONGNAME_NOBREAK, 
CODEPOINT_LONGNAME_SPACEINDICATOR, CODEPOINT_LONGNAME_INVISIBLESPACE, CODEPOINT_LONGNAME_NEGATIVEVERYTHINSPACE, 
CODEPOINT_LONGNAME_NEGATIVETHINSPACE, CODEPOINT_LONGNAME_NEGATIVEMEDIUMSPACE, CODEPOINT_LONGNAME_NEGATIVETHICKSPACE, 
CODEPOINT_LONGNAME_AUTOSPACE, CODEPOINT_LONGNAME_CONTINUATION, CODEPOINT_LONGNAME_ROUNDSPACEINDICATOR, 
CODEPOINT_LONGNAME_PAGEBREAKABOVE, CODEPOINT_LONGNAME_PAGEBREAKBELOW, CODEPOINT_LONGNAME_DISCRETIONARYPAGEBREAKABOVE, 
CODEPOINT_LONGNAME_DISCRETIONARYPAGEBREAKBELOW, CODEPOINT_LONGNAME_ALIGNMENTMARKER, 
];

//
//
//
pub const MB_NEWLINE_CODE_POINTS: [CodePoint; MBNEWLINECODEPOINTS_COUNT] = [
CODEPOINT_CRLF, CODEPOINT_LONGNAME_LINESEPARATOR, CODEPOINT_LONGNAME_PARAGRAPHSEPARATOR, 
CODEPOINT_LONGNAME_INDENTINGNEWLINE, CODEPOINT_LONGNAME_DISCRETIONARYLINESEPARATOR, 
CODEPOINT_LONGNAME_DISCRETIONARYPARAGRAPHSEPARATOR, 
];

//
//
//
pub const MB_UNINTERPRETABLE_CODE_POINTS: [CodePoint; MBUNINTERPRETABLECODEPOINTS_COUNT] = [
CODEPOINT_LONGNAME_SKELETONINDICATOR, CODEPOINT_LONGNAME_LEFTSKELETON, CODEPOINT_LONGNAME_RIGHTSKELETON, 
CODEPOINT_LONGNAME_ERRORINDICATOR, 
];

//
//
//
pub(crate) fn LongNameCodePointToOperator(c: CodePoint) -> TokenKind {
    match c {
        CODEPOINT_LONGNAME_NOT => return LongName_Not,
        CODEPOINT_LONGNAME_PLUSMINUS => return LongName_PlusMinus,
        CODEPOINT_LONGNAME_CENTERDOT => return LongName_CenterDot,
        CODEPOINT_LONGNAME_TIMES => return LongName_Times,
        CODEPOINT_LONGNAME_DIVIDE => return LongName_Divide,
        CODEPOINT_LONGNAME_OPENCURLYQUOTE => return LongName_OpenCurlyQuote,
        CODEPOINT_LONGNAME_CLOSECURLYQUOTE => return LongName_CloseCurlyQuote,
        CODEPOINT_LONGNAME_OPENCURLYDOUBLEQUOTE => return LongName_OpenCurlyDoubleQuote,
        CODEPOINT_LONGNAME_CLOSECURLYDOUBLEQUOTE => return LongName_CloseCurlyDoubleQuote,
        CODEPOINT_LONGNAME_INVISIBLETIMES => return LongName_InvisibleTimes,
        CODEPOINT_LONGNAME_LEFTARROW => return LongName_LeftArrow,
        CODEPOINT_LONGNAME_UPARROW => return LongName_UpArrow,
        CODEPOINT_LONGNAME_RIGHTARROW => return LongName_RightArrow,
        CODEPOINT_LONGNAME_DOWNARROW => return LongName_DownArrow,
        CODEPOINT_LONGNAME_LEFTRIGHTARROW => return LongName_LeftRightArrow,
        CODEPOINT_LONGNAME_UPDOWNARROW => return LongName_UpDownArrow,
        CODEPOINT_LONGNAME_UPPERLEFTARROW => return LongName_UpperLeftArrow,
        CODEPOINT_LONGNAME_UPPERRIGHTARROW => return LongName_UpperRightArrow,
        CODEPOINT_LONGNAME_LOWERRIGHTARROW => return LongName_LowerRightArrow,
        CODEPOINT_LONGNAME_LOWERLEFTARROW => return LongName_LowerLeftArrow,
        CODEPOINT_LONGNAME_LEFTTEEARROW => return LongName_LeftTeeArrow,
        CODEPOINT_LONGNAME_UPTEEARROW => return LongName_UpTeeArrow,
        CODEPOINT_LONGNAME_RIGHTTEEARROW => return LongName_RightTeeArrow,
        CODEPOINT_LONGNAME_DOWNTEEARROW => return LongName_DownTeeArrow,
        CODEPOINT_LONGNAME_LEFTVECTOR => return LongName_LeftVector,
        CODEPOINT_LONGNAME_DOWNLEFTVECTOR => return LongName_DownLeftVector,
        CODEPOINT_LONGNAME_RIGHTUPVECTOR => return LongName_RightUpVector,
        CODEPOINT_LONGNAME_LEFTUPVECTOR => return LongName_LeftUpVector,
        CODEPOINT_LONGNAME_RIGHTVECTOR => return LongName_RightVector,
        CODEPOINT_LONGNAME_DOWNRIGHTVECTOR => return LongName_DownRightVector,
        CODEPOINT_LONGNAME_RIGHTDOWNVECTOR => return LongName_RightDownVector,
        CODEPOINT_LONGNAME_LEFTDOWNVECTOR => return LongName_LeftDownVector,
        CODEPOINT_LONGNAME_RIGHTARROWLEFTARROW => return LongName_RightArrowLeftArrow,
        CODEPOINT_LONGNAME_UPARROWDOWNARROW => return LongName_UpArrowDownArrow,
        CODEPOINT_LONGNAME_LEFTARROWRIGHTARROW => return LongName_LeftArrowRightArrow,
        CODEPOINT_LONGNAME_REVERSEEQUILIBRIUM => return LongName_ReverseEquilibrium,
        CODEPOINT_LONGNAME_EQUILIBRIUM => return LongName_Equilibrium,
        CODEPOINT_LONGNAME_DOUBLELEFTARROW => return LongName_DoubleLeftArrow,
        CODEPOINT_LONGNAME_DOUBLEUPARROW => return LongName_DoubleUpArrow,
        CODEPOINT_LONGNAME_DOUBLERIGHTARROW => return LongName_DoubleRightArrow,
        CODEPOINT_LONGNAME_DOUBLEDOWNARROW => return LongName_DoubleDownArrow,
        CODEPOINT_LONGNAME_DOUBLELEFTRIGHTARROW => return LongName_DoubleLeftRightArrow,
        CODEPOINT_LONGNAME_DOUBLEUPDOWNARROW => return LongName_DoubleUpDownArrow,
        CODEPOINT_LONGNAME_LEFTARROWBAR => return LongName_LeftArrowBar,
        CODEPOINT_LONGNAME_RIGHTARROWBAR => return LongName_RightArrowBar,
        CODEPOINT_LONGNAME_DOWNARROWUPARROW => return LongName_DownArrowUpArrow,
        CODEPOINT_LONGNAME_FORALL => return LongName_ForAll,
        CODEPOINT_LONGNAME_PARTIALD => return LongName_PartialD,
        CODEPOINT_LONGNAME_EXISTS => return LongName_Exists,
        CODEPOINT_LONGNAME_NOTEXISTS => return LongName_NotExists,
        CODEPOINT_LONGNAME_DEL => return LongName_Del,
        CODEPOINT_LONGNAME_ELEMENT => return LongName_Element,
        CODEPOINT_LONGNAME_NOTELEMENT => return LongName_NotElement,
        CODEPOINT_LONGNAME_REVERSEELEMENT => return LongName_ReverseElement,
        CODEPOINT_LONGNAME_NOTREVERSEELEMENT => return LongName_NotReverseElement,
        CODEPOINT_LONGNAME_SUCHTHAT => return LongName_SuchThat,
        CODEPOINT_LONGNAME_PRODUCT => return LongName_Product,
        CODEPOINT_LONGNAME_COPRODUCT => return LongName_Coproduct,
        CODEPOINT_LONGNAME_SUM => return LongName_Sum,
        CODEPOINT_LONGNAME_MINUS => return LongName_Minus,
        CODEPOINT_LONGNAME_MINUSPLUS => return LongName_MinusPlus,
        CODEPOINT_LONGNAME_DIVISIONSLASH => return LongName_DivisionSlash,
        CODEPOINT_LONGNAME_BACKSLASH => return LongName_Backslash,
        CODEPOINT_LONGNAME_SMALLCIRCLE => return LongName_SmallCircle,
        CODEPOINT_LONGNAME_SQRT => return LongName_Sqrt,
        CODEPOINT_LONGNAME_CUBEROOT => return LongName_CubeRoot,
        CODEPOINT_LONGNAME_PROPORTIONAL => return LongName_Proportional,
        CODEPOINT_LONGNAME_DIVIDES => return LongName_Divides,
        CODEPOINT_LONGNAME_DOUBLEVERTICALBAR => return LongName_DoubleVerticalBar,
        CODEPOINT_LONGNAME_NOTDOUBLEVERTICALBAR => return LongName_NotDoubleVerticalBar,
        CODEPOINT_LONGNAME_AND => return LongName_And,
        CODEPOINT_LONGNAME_OR => return LongName_Or,
        CODEPOINT_LONGNAME_INTEGRAL => return LongName_Integral,
        CODEPOINT_LONGNAME_CONTOURINTEGRAL => return LongName_ContourIntegral,
        CODEPOINT_LONGNAME_DOUBLECONTOURINTEGRAL => return LongName_DoubleContourIntegral,
        CODEPOINT_LONGNAME_CLOCKWISECONTOURINTEGRAL => return LongName_ClockwiseContourIntegral,
        CODEPOINT_LONGNAME_COUNTERCLOCKWISECONTOURINTEGRAL => return LongName_CounterClockwiseContourIntegral,
        CODEPOINT_LONGNAME_THEREFORE => return LongName_Therefore,
        CODEPOINT_LONGNAME_BECAUSE => return LongName_Because,
        CODEPOINT_LONGNAME_COLON => return LongName_Colon,
        CODEPOINT_LONGNAME_PROPORTION => return LongName_Proportion,
        CODEPOINT_LONGNAME_TILDE => return LongName_Tilde,
        CODEPOINT_LONGNAME_VERTICALTILDE => return LongName_VerticalTilde,
        CODEPOINT_LONGNAME_NOTTILDE => return LongName_NotTilde,
        CODEPOINT_LONGNAME_EQUALTILDE => return LongName_EqualTilde,
        CODEPOINT_LONGNAME_TILDEEQUAL => return LongName_TildeEqual,
        CODEPOINT_LONGNAME_NOTTILDEEQUAL => return LongName_NotTildeEqual,
        CODEPOINT_LONGNAME_TILDEFULLEQUAL => return LongName_TildeFullEqual,
        CODEPOINT_LONGNAME_NOTTILDEFULLEQUAL => return LongName_NotTildeFullEqual,
        CODEPOINT_LONGNAME_TILDETILDE => return LongName_TildeTilde,
        CODEPOINT_LONGNAME_NOTTILDETILDE => return LongName_NotTildeTilde,
        CODEPOINT_LONGNAME_CUPCAP => return LongName_CupCap,
        CODEPOINT_LONGNAME_HUMPDOWNHUMP => return LongName_HumpDownHump,
        CODEPOINT_LONGNAME_HUMPEQUAL => return LongName_HumpEqual,
        CODEPOINT_LONGNAME_DOTEQUAL => return LongName_DotEqual,
        CODEPOINT_LONGNAME_NOTEQUAL => return LongName_NotEqual,
        CODEPOINT_LONGNAME_CONGRUENT => return LongName_Congruent,
        CODEPOINT_LONGNAME_NOTCONGRUENT => return LongName_NotCongruent,
        CODEPOINT_LONGNAME_LESSEQUAL => return LongName_LessEqual,
        CODEPOINT_LONGNAME_GREATEREQUAL => return LongName_GreaterEqual,
        CODEPOINT_LONGNAME_LESSFULLEQUAL => return LongName_LessFullEqual,
        CODEPOINT_LONGNAME_GREATERFULLEQUAL => return LongName_GreaterFullEqual,
        CODEPOINT_LONGNAME_NOTLESSFULLEQUAL => return LongName_NotLessFullEqual,
        CODEPOINT_LONGNAME_NOTGREATERFULLEQUAL => return LongName_NotGreaterFullEqual,
        CODEPOINT_LONGNAME_LESSLESS => return LongName_LessLess,
        CODEPOINT_LONGNAME_GREATERGREATER => return LongName_GreaterGreater,
        CODEPOINT_LONGNAME_NOTCUPCAP => return LongName_NotCupCap,
        CODEPOINT_LONGNAME_NOTLESS => return LongName_NotLess,
        CODEPOINT_LONGNAME_NOTGREATER => return LongName_NotGreater,
        CODEPOINT_LONGNAME_NOTLESSEQUAL => return LongName_NotLessEqual,
        CODEPOINT_LONGNAME_NOTGREATEREQUAL => return LongName_NotGreaterEqual,
        CODEPOINT_LONGNAME_LESSTILDE => return LongName_LessTilde,
        CODEPOINT_LONGNAME_GREATERTILDE => return LongName_GreaterTilde,
        CODEPOINT_LONGNAME_NOTLESSTILDE => return LongName_NotLessTilde,
        CODEPOINT_LONGNAME_NOTGREATERTILDE => return LongName_NotGreaterTilde,
        CODEPOINT_LONGNAME_LESSGREATER => return LongName_LessGreater,
        CODEPOINT_LONGNAME_GREATERLESS => return LongName_GreaterLess,
        CODEPOINT_LONGNAME_NOTLESSGREATER => return LongName_NotLessGreater,
        CODEPOINT_LONGNAME_NOTGREATERLESS => return LongName_NotGreaterLess,
        CODEPOINT_LONGNAME_PRECEDES => return LongName_Precedes,
        CODEPOINT_LONGNAME_SUCCEEDS => return LongName_Succeeds,
        CODEPOINT_LONGNAME_PRECEDESSLANTEQUAL => return LongName_PrecedesSlantEqual,
        CODEPOINT_LONGNAME_SUCCEEDSSLANTEQUAL => return LongName_SucceedsSlantEqual,
        CODEPOINT_LONGNAME_PRECEDESTILDE => return LongName_PrecedesTilde,
        CODEPOINT_LONGNAME_SUCCEEDSTILDE => return LongName_SucceedsTilde,
        CODEPOINT_LONGNAME_NOTPRECEDES => return LongName_NotPrecedes,
        CODEPOINT_LONGNAME_NOTSUCCEEDS => return LongName_NotSucceeds,
        CODEPOINT_LONGNAME_SUBSET => return LongName_Subset,
        CODEPOINT_LONGNAME_SUPERSET => return LongName_Superset,
        CODEPOINT_LONGNAME_NOTSUBSET => return LongName_NotSubset,
        CODEPOINT_LONGNAME_NOTSUPERSET => return LongName_NotSuperset,
        CODEPOINT_LONGNAME_SUBSETEQUAL => return LongName_SubsetEqual,
        CODEPOINT_LONGNAME_SUPERSETEQUAL => return LongName_SupersetEqual,
        CODEPOINT_LONGNAME_NOTSUBSETEQUAL => return LongName_NotSubsetEqual,
        CODEPOINT_LONGNAME_NOTSUPERSETEQUAL => return LongName_NotSupersetEqual,
        CODEPOINT_LONGNAME_UNIONPLUS => return LongName_UnionPlus,
        CODEPOINT_LONGNAME_SQUARESUBSET => return LongName_SquareSubset,
        CODEPOINT_LONGNAME_SQUARESUPERSET => return LongName_SquareSuperset,
        CODEPOINT_LONGNAME_SQUARESUBSETEQUAL => return LongName_SquareSubsetEqual,
        CODEPOINT_LONGNAME_SQUARESUPERSETEQUAL => return LongName_SquareSupersetEqual,
        CODEPOINT_LONGNAME_SQUAREINTERSECTION => return LongName_SquareIntersection,
        CODEPOINT_LONGNAME_SQUAREUNION => return LongName_SquareUnion,
        CODEPOINT_LONGNAME_CIRCLEPLUS => return LongName_CirclePlus,
        CODEPOINT_LONGNAME_CIRCLEMINUS => return LongName_CircleMinus,
        CODEPOINT_LONGNAME_CIRCLETIMES => return LongName_CircleTimes,
        CODEPOINT_LONGNAME_CIRCLEDOT => return LongName_CircleDot,
        CODEPOINT_LONGNAME_RIGHTTEE => return LongName_RightTee,
        CODEPOINT_LONGNAME_LEFTTEE => return LongName_LeftTee,
        CODEPOINT_LONGNAME_DOWNTEE => return LongName_DownTee,
        CODEPOINT_LONGNAME_UPTEE => return LongName_UpTee,
        CODEPOINT_LONGNAME_DOUBLERIGHTTEE => return LongName_DoubleRightTee,
        CODEPOINT_LONGNAME_LEFTTRIANGLE => return LongName_LeftTriangle,
        CODEPOINT_LONGNAME_RIGHTTRIANGLE => return LongName_RightTriangle,
        CODEPOINT_LONGNAME_LEFTTRIANGLEEQUAL => return LongName_LeftTriangleEqual,
        CODEPOINT_LONGNAME_RIGHTTRIANGLEEQUAL => return LongName_RightTriangleEqual,
        CODEPOINT_LONGNAME_XOR => return LongName_Xor,
        CODEPOINT_LONGNAME_NAND => return LongName_Nand,
        CODEPOINT_LONGNAME_NOR => return LongName_Nor,
        CODEPOINT_LONGNAME_WEDGE => return LongName_Wedge,
        CODEPOINT_LONGNAME_VEE => return LongName_Vee,
        CODEPOINT_LONGNAME_INTERSECTION => return LongName_Intersection,
        CODEPOINT_LONGNAME_UNION => return LongName_Union,
        CODEPOINT_LONGNAME_DIAMOND => return LongName_Diamond,
        CODEPOINT_LONGNAME_STAR => return LongName_Star,
        CODEPOINT_LONGNAME_LESSEQUALGREATER => return LongName_LessEqualGreater,
        CODEPOINT_LONGNAME_GREATEREQUALLESS => return LongName_GreaterEqualLess,
        CODEPOINT_LONGNAME_NOTPRECEDESSLANTEQUAL => return LongName_NotPrecedesSlantEqual,
        CODEPOINT_LONGNAME_NOTSUCCEEDSSLANTEQUAL => return LongName_NotSucceedsSlantEqual,
        CODEPOINT_LONGNAME_NOTSQUARESUBSETEQUAL => return LongName_NotSquareSubsetEqual,
        CODEPOINT_LONGNAME_NOTSQUARESUPERSETEQUAL => return LongName_NotSquareSupersetEqual,
        CODEPOINT_LONGNAME_NOTPRECEDESTILDE => return LongName_NotPrecedesTilde,
        CODEPOINT_LONGNAME_NOTSUCCEEDSTILDE => return LongName_NotSucceedsTilde,
        CODEPOINT_LONGNAME_NOTLEFTTRIANGLE => return LongName_NotLeftTriangle,
        CODEPOINT_LONGNAME_NOTRIGHTTRIANGLE => return LongName_NotRightTriangle,
        CODEPOINT_LONGNAME_NOTLEFTTRIANGLEEQUAL => return LongName_NotLeftTriangleEqual,
        CODEPOINT_LONGNAME_NOTRIGHTTRIANGLEEQUAL => return LongName_NotRightTriangleEqual,
        CODEPOINT_LONGNAME_LEFTCEILING => return LongName_LeftCeiling,
        CODEPOINT_LONGNAME_RIGHTCEILING => return LongName_RightCeiling,
        CODEPOINT_LONGNAME_LEFTFLOOR => return LongName_LeftFloor,
        CODEPOINT_LONGNAME_RIGHTFLOOR => return LongName_RightFloor,
        CODEPOINT_LONGNAME_CAP => return LongName_Cap,
        CODEPOINT_LONGNAME_CUP => return LongName_Cup,
        CODEPOINT_LONGNAME_LEFTANGLEBRACKET => return LongName_LeftAngleBracket,
        CODEPOINT_LONGNAME_RIGHTANGLEBRACKET => return LongName_RightAngleBracket,
        CODEPOINT_LONGNAME_PERPENDICULAR => return LongName_Perpendicular,
        CODEPOINT_LONGNAME_LONGLEFTARROW => return LongName_LongLeftArrow,
        CODEPOINT_LONGNAME_LONGRIGHTARROW => return LongName_LongRightArrow,
        CODEPOINT_LONGNAME_LONGLEFTRIGHTARROW => return LongName_LongLeftRightArrow,
        CODEPOINT_LONGNAME_DOUBLELONGLEFTARROW => return LongName_DoubleLongLeftArrow,
        CODEPOINT_LONGNAME_DOUBLELONGRIGHTARROW => return LongName_DoubleLongRightArrow,
        CODEPOINT_LONGNAME_DOUBLELONGLEFTRIGHTARROW => return LongName_DoubleLongLeftRightArrow,
        CODEPOINT_LONGNAME_UPARROWBAR => return LongName_UpArrowBar,
        CODEPOINT_LONGNAME_DOWNARROWBAR => return LongName_DownArrowBar,
        CODEPOINT_LONGNAME_LEFTRIGHTVECTOR => return LongName_LeftRightVector,
        CODEPOINT_LONGNAME_RIGHTUPDOWNVECTOR => return LongName_RightUpDownVector,
        CODEPOINT_LONGNAME_DOWNLEFTRIGHTVECTOR => return LongName_DownLeftRightVector,
        CODEPOINT_LONGNAME_LEFTUPDOWNVECTOR => return LongName_LeftUpDownVector,
        CODEPOINT_LONGNAME_LEFTVECTORBAR => return LongName_LeftVectorBar,
        CODEPOINT_LONGNAME_RIGHTVECTORBAR => return LongName_RightVectorBar,
        CODEPOINT_LONGNAME_RIGHTUPVECTORBAR => return LongName_RightUpVectorBar,
        CODEPOINT_LONGNAME_RIGHTDOWNVECTORBAR => return LongName_RightDownVectorBar,
        CODEPOINT_LONGNAME_DOWNLEFTVECTORBAR => return LongName_DownLeftVectorBar,
        CODEPOINT_LONGNAME_DOWNRIGHTVECTORBAR => return LongName_DownRightVectorBar,
        CODEPOINT_LONGNAME_LEFTUPVECTORBAR => return LongName_LeftUpVectorBar,
        CODEPOINT_LONGNAME_LEFTDOWNVECTORBAR => return LongName_LeftDownVectorBar,
        CODEPOINT_LONGNAME_LEFTTEEVECTOR => return LongName_LeftTeeVector,
        CODEPOINT_LONGNAME_RIGHTTEEVECTOR => return LongName_RightTeeVector,
        CODEPOINT_LONGNAME_RIGHTUPTEEVECTOR => return LongName_RightUpTeeVector,
        CODEPOINT_LONGNAME_RIGHTDOWNTEEVECTOR => return LongName_RightDownTeeVector,
        CODEPOINT_LONGNAME_DOWNLEFTTEEVECTOR => return LongName_DownLeftTeeVector,
        CODEPOINT_LONGNAME_DOWNRIGHTTEEVECTOR => return LongName_DownRightTeeVector,
        CODEPOINT_LONGNAME_LEFTUPTEEVECTOR => return LongName_LeftUpTeeVector,
        CODEPOINT_LONGNAME_LEFTDOWNTEEVECTOR => return LongName_LeftDownTeeVector,
        CODEPOINT_LONGNAME_UPEQUILIBRIUM => return LongName_UpEquilibrium,
        CODEPOINT_LONGNAME_REVERSEUPEQUILIBRIUM => return LongName_ReverseUpEquilibrium,
        CODEPOINT_LONGNAME_ROUNDIMPLIES => return LongName_RoundImplies,
        CODEPOINT_LONGNAME_LEFTTRIANGLEBAR => return LongName_LeftTriangleBar,
        CODEPOINT_LONGNAME_RIGHTTRIANGLEBAR => return LongName_RightTriangleBar,
        CODEPOINT_LONGNAME_EQUIVALENT => return LongName_Equivalent,
        CODEPOINT_LONGNAME_LESSSLANTEQUAL => return LongName_LessSlantEqual,
        CODEPOINT_LONGNAME_GREATERSLANTEQUAL => return LongName_GreaterSlantEqual,
        CODEPOINT_LONGNAME_NESTEDLESSLESS => return LongName_NestedLessLess,
        CODEPOINT_LONGNAME_NESTEDGREATERGREATER => return LongName_NestedGreaterGreater,
        CODEPOINT_LONGNAME_PRECEDESEQUAL => return LongName_PrecedesEqual,
        CODEPOINT_LONGNAME_SUCCEEDSEQUAL => return LongName_SucceedsEqual,
        CODEPOINT_LONGNAME_DOUBLELEFTTEE => return LongName_DoubleLeftTee,
        CODEPOINT_LONGNAME_LEFTDOUBLEBRACKET => return LongName_LeftDoubleBracket,
        CODEPOINT_LONGNAME_RIGHTDOUBLEBRACKET => return LongName_RightDoubleBracket,
        CODEPOINT_LONGNAME_LEFTASSOCIATION => return LongName_LeftAssociation,
        CODEPOINT_LONGNAME_RIGHTASSOCIATION => return LongName_RightAssociation,
        CODEPOINT_LONGNAME_TWOWAYRULE => return LongName_TwoWayRule,
        CODEPOINT_LONGNAME_PIECEWISE => return LongName_Piecewise,
        CODEPOINT_LONGNAME_IMPLICITPLUS => return LongName_ImplicitPlus,
        CODEPOINT_LONGNAME_AUTOLEFTMATCH => return LongName_AutoLeftMatch,
        CODEPOINT_LONGNAME_AUTORIGHTMATCH => return LongName_AutoRightMatch,
        CODEPOINT_LONGNAME_INVISIBLEPREFIXSCRIPTBASE => return LongName_InvisiblePrefixScriptBase,
        CODEPOINT_LONGNAME_INVISIBLEPOSTFIXSCRIPTBASE => return LongName_InvisiblePostfixScriptBase,
        CODEPOINT_LONGNAME_TRANSPOSE => return LongName_Transpose,
        CODEPOINT_LONGNAME_CONJUGATE => return LongName_Conjugate,
        CODEPOINT_LONGNAME_CONJUGATETRANSPOSE => return LongName_ConjugateTranspose,
        CODEPOINT_LONGNAME_HERMITIANCONJUGATE => return LongName_HermitianConjugate,
        CODEPOINT_LONGNAME_VERTICALBAR => return LongName_VerticalBar,
        CODEPOINT_LONGNAME_NOTVERTICALBAR => return LongName_NotVerticalBar,
        CODEPOINT_LONGNAME_DISTRIBUTED => return LongName_Distributed,
        CODEPOINT_LONGNAME_CONDITIONED => return LongName_Conditioned,
        CODEPOINT_LONGNAME_UNDIRECTEDEDGE => return LongName_UndirectedEdge,
        CODEPOINT_LONGNAME_DIRECTEDEDGE => return LongName_DirectedEdge,
        CODEPOINT_LONGNAME_CONTINUEDFRACTIONK => return LongName_ContinuedFractionK,
        CODEPOINT_LONGNAME_TENSORPRODUCT => return LongName_TensorProduct,
        CODEPOINT_LONGNAME_TENSORWEDGE => return LongName_TensorWedge,
        CODEPOINT_LONGNAME_PROBABILITYPR => return LongName_ProbabilityPr,
        CODEPOINT_LONGNAME_EXPECTATIONE => return LongName_ExpectationE,
        CODEPOINT_LONGNAME_PERMUTATIONPRODUCT => return LongName_PermutationProduct,
        CODEPOINT_LONGNAME_NOTEQUALTILDE => return LongName_NotEqualTilde,
        CODEPOINT_LONGNAME_NOTHUMPEQUAL => return LongName_NotHumpEqual,
        CODEPOINT_LONGNAME_NOTHUMPDOWNHUMP => return LongName_NotHumpDownHump,
        CODEPOINT_LONGNAME_NOTLEFTTRIANGLEBAR => return LongName_NotLeftTriangleBar,
        CODEPOINT_LONGNAME_NOTRIGHTTRIANGLEBAR => return LongName_NotRightTriangleBar,
        CODEPOINT_LONGNAME_NOTLESSLESS => return LongName_NotLessLess,
        CODEPOINT_LONGNAME_NOTNESTEDLESSLESS => return LongName_NotNestedLessLess,
        CODEPOINT_LONGNAME_NOTLESSSLANTEQUAL => return LongName_NotLessSlantEqual,
        CODEPOINT_LONGNAME_NOTGREATERGREATER => return LongName_NotGreaterGreater,
        CODEPOINT_LONGNAME_NOTNESTEDGREATERGREATER => return LongName_NotNestedGreaterGreater,
        CODEPOINT_LONGNAME_NOTGREATERSLANTEQUAL => return LongName_NotGreaterSlantEqual,
        CODEPOINT_LONGNAME_NOTPRECEDESEQUAL => return LongName_NotPrecedesEqual,
        CODEPOINT_LONGNAME_NOTSUCCEEDSEQUAL => return LongName_NotSucceedsEqual,
        CODEPOINT_LONGNAME_NOTSQUARESUBSET => return LongName_NotSquareSubset,
        CODEPOINT_LONGNAME_NOTSQUARESUPERSET => return LongName_NotSquareSuperset,
        CODEPOINT_LONGNAME_EQUAL => return LongName_Equal,
        CODEPOINT_LONGNAME_VERTICALSEPARATOR => return LongName_VerticalSeparator,
        CODEPOINT_LONGNAME_VECTORGREATER => return LongName_VectorGreater,
        CODEPOINT_LONGNAME_VECTORGREATEREQUAL => return LongName_VectorGreaterEqual,
        CODEPOINT_LONGNAME_VECTORLESS => return LongName_VectorLess,
        CODEPOINT_LONGNAME_VECTORLESSEQUAL => return LongName_VectorLessEqual,
        CODEPOINT_LONGNAME_LIMIT => return LongName_Limit,
        CODEPOINT_LONGNAME_MAXLIMIT => return LongName_MaxLimit,
        CODEPOINT_LONGNAME_MINLIMIT => return LongName_MinLimit,
        CODEPOINT_LONGNAME_CROSS => return LongName_Cross,
        CODEPOINT_LONGNAME_FUNCTION => return LongName_Function,
        CODEPOINT_LONGNAME_XNOR => return LongName_Xnor,
        CODEPOINT_LONGNAME_DISCRETESHIFT => return LongName_DiscreteShift,
        CODEPOINT_LONGNAME_DIFFERENCEDELTA => return LongName_DifferenceDelta,
        CODEPOINT_LONGNAME_DISCRETERATIO => return LongName_DiscreteRatio,
        CODEPOINT_LONGNAME_RULEDELAYED => return LongName_RuleDelayed,
        CODEPOINT_LONGNAME_SQUARE => return LongName_Square,
        CODEPOINT_LONGNAME_RULE => return LongName_Rule,
        CODEPOINT_LONGNAME_IMPLIES => return LongName_Implies,
        CODEPOINT_LONGNAME_SHORTRIGHTARROW => return LongName_ShortRightArrow,
        CODEPOINT_LONGNAME_SHORTLEFTARROW => return LongName_ShortLeftArrow,
        CODEPOINT_LONGNAME_SHORTUPARROW => return LongName_ShortUpArrow,
        CODEPOINT_LONGNAME_SHORTDOWNARROW => return LongName_ShortDownArrow,
        CODEPOINT_LONGNAME_APPLICATION => return LongName_Application,
        CODEPOINT_LONGNAME_LEFTBRACKETINGBAR => return LongName_LeftBracketingBar,
        CODEPOINT_LONGNAME_RIGHTBRACKETINGBAR => return LongName_RightBracketingBar,
        CODEPOINT_LONGNAME_LEFTDOUBLEBRACKETINGBAR => return LongName_LeftDoubleBracketingBar,
        CODEPOINT_LONGNAME_RIGHTDOUBLEBRACKETINGBAR => return LongName_RightDoubleBracketingBar,
        CODEPOINT_LONGNAME_CAPITALDIFFERENTIALD => return LongName_CapitalDifferentialD,
        CODEPOINT_LONGNAME_DIFFERENTIALD => return LongName_DifferentialD,
        CODEPOINT_LONGNAME_INVISIBLECOMMA => return LongName_InvisibleComma,
        CODEPOINT_LONGNAME_INVISIBLEAPPLICATION => return LongName_InvisibleApplication,
        CODEPOINT_LONGNAME_LONGEQUAL => return LongName_LongEqual,
        _ => panic!("Need to add operator"),
    }
}
