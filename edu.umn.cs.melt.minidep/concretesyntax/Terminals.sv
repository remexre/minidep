grammar edu:umn:cs:melt:minidep:concretesyntax;

-- lexer class COMMENT dominates OPERATOR;
-- lexer class IDENTIFIER;
-- lexer class KEYWORD dominates IDENTIFIER;
-- lexer class LITERAL;
-- lexer class OPERATOR;

terminal Semicolon_t  ';';
terminal Semicolons_t ';;';
terminal Equals_t     '=';
terminal If_t         'if';
terminal Then_t       'then';
terminal Else_t       'else';

terminal Colon_t ':';
terminal Cons_t  '::';

terminal Plus_t  '+';
terminal Minus_t '-';
terminal Times_t '*';

terminal LParen_t '(';
terminal RParen_t ')';
terminal LBrack_t '[';
terminal RBrack_t ']';

terminal False_t    'false';
terminal IntTy_t    'Int';
terminal True_t     'true';
terminal TypeKind_t 'TYPE';

terminal Int_t  /[0-9]+/ named "an integer";
terminal Name_t /[a-zA-Z_][0-9a-zA-Z_]*/ named "a name";

ignore terminal Whitespace_t  /[\n\r\t\ ]+/ named "whitespace";
ignore terminal LineComment_t /[\-][\-].*/ named "a comment";
-- ignore terminal LineComment_t  /[\-][\-].*/ lexer classes {COMMENT}, named "a comment";
