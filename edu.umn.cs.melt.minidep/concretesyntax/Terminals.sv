grammar edu:umn:cs:melt:minidep:concretesyntax;

lexer class COMMENT;
lexer class IDENTIFIER;
lexer class KEYWORD dominates IDENTIFIER;
lexer class LITERAL;
lexer class OPERATOR;

terminal Arrow_t   '->' lexer classes {OPERATOR};
terminal Colon_t   ':'  lexer classes {OPERATOR};
terminal Cons_t    '::' lexer classes {OPERATOR};
terminal Equals_t  '='  lexer classes {OPERATOR};
terminal Equals2_t '==' lexer classes {OPERATOR};
terminal Plus_t    '+'  lexer classes {OPERATOR};
terminal Times_t   '*'  lexer classes {OPERATOR};

terminal Backslash_t '\';
terminal Comma_t     ',';
terminal Semicolon_t ';';

terminal LParen_t '(';
terminal RParen_t ')';
terminal LBrace_t '{';
terminal RBrace_t '}';

terminal Hash_t   '#';
terminal LBrack_t '[';
terminal RBrack_t ']';

terminal Anon_t '_'    lexer classes {KEYWORD};
terminal Else_t 'else' lexer classes {KEYWORD};
terminal If_t   'if'   lexer classes {KEYWORD};
terminal Pi_t   'Pi'   lexer classes {KEYWORD};
terminal Then_t 'then' lexer classes {KEYWORD};
terminal Type_t 'TYPE' lexer classes {KEYWORD};

terminal Nat_t  /[0-9]+/ named "a natural number";
terminal Name_t /[a-zA-Z_][0-9a-zA-Z_]*|\([!$%&\*\+\.\/<=>\?@\^\|\-~]+\)/ lexer classes {IDENTIFIER}, named "a name";

ignore terminal Whitespace_t  /[\n\r\t\ ]+/ named "whitespace";
ignore terminal LineComment_t  /[\-][\-][^\n\r]*/ lexer classes {COMMENT}, named "a comment";
