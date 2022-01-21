CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "AST.sml";
use "parser.yacc.sig";
use "parser.yacc.sml";
use "lexer.lex.sml";
use "interface.sml";
Control.Print.printLength := 1000;
Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;
open Intexp;