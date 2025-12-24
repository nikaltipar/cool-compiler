/*
 *  The scanner definition for COOL.
 */

/* Flex options for Windows compatibility */
%option nounistd
%option never-interactive
%option noyywrap
%option debug

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include "cool-parse.hpp"
#include "stringtab.hpp"
#include "utilities.hpp"

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
constexpr size_t MAX_STR_LEN {1024};
int comment_level {};
std::string cc_text {};
bool string_error_reported {};

void cool_yylex_reset();

%}

/*
 * Define names for regular expressions here.
 */
DIGIT           [0-9]
ALPHA           [a-zA-Z0-9_]
CLETTER         [A-Z]
LLETTER         [a-z]
WS_NONEWLINE    [\f\r\t\v ]+
NOT_ALPHA       ([^0-9a-zA-Z_]|$)
NEWLINE         \n
QUOTE           \"
BACKSLASH       \\

DARROW          =>
ASSIGN          <-
LE              <=

CLASS           (?i:class)
ELSE            (?i:else)
IF              (?i:if)
FI              (?i:fi)
IN              (?i:in)
INHERITS        (?i:inherits)
ISVOID          (?i:isvoid)
LET             (?i:let)
LOOP            (?i:loop)
POOL            (?i:pool)
THEN            (?i:then)
WHILE           (?i:while)
CASE            (?i:case)
ESAC            (?i:esac)
NEW             (?i:new)
OF              (?i:of)
NOT             (?i:not)

TRUE            t(?i:rue)
FALSE           f(?i:alse)

COMMENT_START   \(\*
COMMENT_END     \*\)
COMMENT_CHAR    .

INLINE_COMMENT  --[^\n]*

TYPEID          {CLETTER}{ALPHA}*
OBJECTID        {LLETTER}{ALPHA}*
INTEGER         {DIGIT}+

%x STRING
%x NESTED_COMMENT


%%


<*>{NEWLINE} {
    curr_lineno++;
    if (YY_START == STRING)
    {
        BEGIN(INITIAL);
        if (string_error_reported)
        {
            string_error_reported = false;
        }
        else
        {
            cool_yylval.error_msg = "Unterminated string constant";
            return ERROR;
        }
    }
}
        
<INITIAL,NESTED_COMMENT>{WS_NONEWLINE} {}


<INITIAL,NESTED_COMMENT>{COMMENT_START} {
    BEGIN(NESTED_COMMENT);
    comment_level++;
}

<NESTED_COMMENT>{COMMENT_END} {
    comment_level--;
    if (comment_level == 0)
    {
        BEGIN(INITIAL);
    }
}

{COMMENT_END} {
    cool_yylval.error_msg = "Unmatched *)";
    return ERROR;
}

<NESTED_COMMENT><<EOF>> {
    BEGIN(INITIAL);
    cool_yylval.error_msg = "EOF in comment";
    return ERROR;
}

<NESTED_COMMENT>{COMMENT_CHAR} {
}

{INLINE_COMMENT} {
}


{DARROW} {
    return DARROW;
}

{ASSIGN} {
    return ASSIGN;
}

{LE} {
    return LE;
}


{CLASS} {
    return CLASS;
}

{ELSE} {
    return ELSE;
}

{IF} {
    return IF;
}

{FI} {
    return FI;
}

{IN} {
    return IN;
}

{INHERITS} {
    return INHERITS;
}

{ISVOID} {
    return ISVOID;
}

{LET} {
    return LET;
}

{LOOP} {
    return LOOP;
}

{POOL} {
    return POOL;
}

{THEN} {
    return THEN;
}

{WHILE} {
    return WHILE;
}

{CASE} {
    return CASE;
}

{ESAC} {
    return ESAC;
}

{NEW} {
    return NEW;
}

{OF} {
    return OF;
}

{NOT} {
    return NOT;
}

{TRUE} {
    cool_yylval.boolean = true;
    return BOOL_CONST;
}

{FALSE} {
    cool_yylval.boolean = false;
    return BOOL_CONST;
}

{TYPEID} {
    cool_yylval.symbol = idtable.add_string(yytext); 
    return TYPEID;
}

{OBJECTID} {
    cool_yylval.symbol = idtable.add_string(yytext);
    return OBJECTID;
}

{INTEGER} {
    cool_yylval.symbol = inttable.add_string(yytext);
    return INT_CONST;
}

{QUOTE} {
    cc_text = std::string {};
    string_error_reported = false;
    BEGIN(STRING);
}

<STRING><<EOF>> {
    BEGIN(INITIAL);
    if (!string_error_reported)
    {
        cool_yylval.error_msg = "EOF in string constant";
        return ERROR;
    }
    string_error_reported = false;
}

<STRING>\\(.|\n) {
    switch (yytext[1])
    {
        case '\n':
            curr_lineno++;
            cc_text += '\n';
            break;
        case '\0':
            string_error_reported = true;
            cool_yylval.error_msg = "String contains escaped null character.";
            return ERROR;
        case 'b':
            cc_text += '\b';
            break;
        case 'n':
            cc_text += '\n';
            break;
        case 't':
            cc_text += '\t';
            break;
        case 'f':
            cc_text += '\f';
            break;
        default:
            cc_text += yytext[1];
            break;
    }
}

<STRING>\0 {
    string_error_reported = true;
    cool_yylval.error_msg = "String contains null character.";
    return ERROR;
}

<STRING>[^\"\n\\\0]+ {
    cc_text += std::string(yytext, yyleng);
}

<STRING>{QUOTE} {
    BEGIN(INITIAL);
    if (string_error_reported)
    {
        string_error_reported = false;
    }
    else if (cc_text.size() > MAX_STR_LEN)
    {
        cool_yylval.error_msg = "String constant too long";
        return ERROR;
    }
    else
    {
        cool_yylval.symbol = stringtable.add_string(const_cast<char*>(cc_text.c_str()));
        return STR_CONST;
    }
}


. {
    std::string allowed_chars {"+/-*=<.~,;:()@{}"};
    if (allowed_chars.find(yytext[0]) == std::string::npos)
    {
        cool_yylval.error_msg = yytext;
        return ERROR;
    }
    return yytext[0];
}

%%

void cool_yylex_reset() {
    comment_level = 0;
    cc_text.clear();
    string_error_reported = false;
    BEGIN(INITIAL);
}

