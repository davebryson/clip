%====================================
% Simple tokenizer for the CLI app.
% Handles stringS, integers, and floats
%=====================================
Definitions.

CharValues = [A-Za-z]
NUMBER = [0-9]
Floats = (\+|-)?[0-9]+\.[0-9]+((E|e)(\+|-)?[0-9]+)?
WS = [\s\t]
LB = \n|\r\n|\r
ControlChars = ([\000-\037])

Rules.

{CharValues}+   :{token, {string, TokenChars}}.
{NUMBER}+   :{token, {number, list_to_integer(TokenChars)}}.
{Floats}+ :{token, {float, list_to_float(TokenChars)}}.
{WS}+ :skip_token.
{LB}+ :skip_token.
{ControlChars}+ :skip_token.

Erlang code.
