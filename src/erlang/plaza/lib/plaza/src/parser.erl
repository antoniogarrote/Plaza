%---------------------------------------------------------------------
% FILE:              parser.erl
% DESCRIPTION:       Parser functions
% DATE:              08/21/2001
% LANGUAGE PLATFORM: Erlang 5.0.1.1
% OS PLATFORM:       RedHat Linux 7.0
% AUTHOR:            Jeffrey A. Meunier
% EMAIL:             jeffm@cse.uconn.edu
%---------------------------------------------------------------------

% This module is based on the Haskell Parsec parser library written
% by Erik Meijer.

-module( parser ).

% important functions
-export( [ parse/2
         ] ).

% parsers
-export( [ pAlphaNum/1
         , pAnd/1
         , pAWord/1
         , pBetween/2
         , pCapWord/1
         , pChar/1
         , pDebug/1
         , pDigit/1
         , pEoi/1
         , pEol/1
         , pList/4            % begin, element, separator, end
         , pLower/1
         , pMany/1
         , pMany1/1
         , pMaybe/1
         , pNewline/1
         , pNonCapWord/1
         , pNot/1
         , pNotFollowedBy/2
         , pOr/1
         , pSat/1
         , pSkipMany/1
         , pSpace/1
         , pSpaces/1
         , pSpaces1/1
         , pString/1
         , pTheWord/1
         , pThen/2
         , pUntil/1
         , pUpper/1
         , pWhile/1
         , pWordSep/1
         ] ).

% predicates
-export( [ isAlpha/1
         , isAlphaNum/1
         , isDigit/1
         , isHexDigit/1
         , isLower/1
         , isNl/1
%         , isPunct/1
         , isSpace/1
         , isUpper/1
         , isWordSep/1
         ] ).

% test exports
-export( [ scan/3
         ] ).



%---------------------------------------------------------------------
% Top-level parsing function.
%---------------------------------------------------------------------
parse( Parser, Inp )
  -> Parser( Inp )
   .



%---------------------------------------------------------------------
% Display a debug message.
%---------------------------------------------------------------------
pDebug( String )
  -> fun( _Inp )
       -> io:format( String )
        , fail
     end
   .



%=====================================================================
% Higher-level data structure parsers.
%=====================================================================

%---------------------------------------------------------------------
% Parse a list of elements.
%---------------------------------------------------------------------
pList( PBegin, PElem, PSep, PEnd )
  -> fun( Inp )
       -> Result = parse( pAnd( [ fun pSpaces/1
                                , PBegin
                                , pMaybe( pAnd( [ fun pSpaces/1
                                                , PElem
                                                , pMany( pAnd( [ fun pSpaces/1
                                                               , PSep
                                                               , fun pSpaces/1
                                                               , PElem
                                                               ] ) )
                                                ] ) )
                                , fun pSpaces/1
                                , PEnd
                                ] ), Inp )
        , case Result of
            fail
              -> fail
               ;
            {[_, _, List, _, _], Rest}
              -> StripElem
                   = fun( [_, _, _, Elem] ) -> Elem end
               , case List of
                   []
                     -> {[], Rest}
                      ;
                   [[_, Elem, Elems] | _]
                     -> {[Elem | lists:map( StripElem, Elems )], Rest}
                 end
          end
     end
   .

% test expressions for pList
% O=parser:pChar($[),C=parser:pChar($]),S=parser:pChar($,),A=parser:pChar($A).
% (parser:pList(O,A,S,C))(" [ A, A, A] ").


%=====================================================================
% Special parsers.
%=====================================================================

%---------------------------------------------------------------------
% Check for end of input.
%---------------------------------------------------------------------
pEoi( [] )
  -> {eoi, []}
   ;
pEoi( _ )
  -> fail
   .



%---------------------------------------------------------------------
% Check for end of line.  EOL is either a newline or EOI.
%---------------------------------------------------------------------
pEol( [] )
  -> {eol, []}
   ;
pEol( [C | Cs] )
  -> case isNl( C ) of
       true
         -> {C, Cs}
          ;
       false
         -> fail
     end
   ;
pEol( _ )
  -> fail
   .



%=====================================================================
% Parser combinators.
%=====================================================================

%---------------------------------------------------------------------
% Return input between elements parsed by P1 and P2.
%---------------------------------------------------------------------
pBetween( P1, P2 )
  -> fun( Inp )
       -> Result = parse( pThen( P1, pUntil( P2 ) ), Inp )
        , case Result of
            {[_, {Betw, _}], Rest}
              -> {Betw, Rest}
               ;
            _ -> fail
          end
     end
   .



%---------------------------------------------------------------------
% Parse 0 or 1 element.
%---------------------------------------------------------------------
pMaybe( P )
  -> fun( Inp )
       -> case P( Inp ) of
            fail
              -> {[], Inp}
               ;
            {Result, InpRem}
              -> {[Result], InpRem}
          end
     end
   .



%---------------------------------------------------------------------
% Parser success inverter.
%---------------------------------------------------------------------
pNot( P )
  -> fun( Inp )
       -> case P( Inp ) of
            fail
              -> {ok, Inp}
               ;
            _ -> fail
          end
     end
   .



%---------------------------------------------------------------------
% Succeed if P2 does not follow P1.
%---------------------------------------------------------------------
pNotFollowedBy( P1, P2 )
  -> fun( Inp )
       -> case P1( Inp ) of
            fail
              -> fail
               ;
            {Result, InpRem}
              -> case P2( InpRem ) of
                   fail
                     -> {Result, InpRem}
                      ;
                   _ -> fail
                 end
          end
     end
   .


%---------------------------------------------------------------------
% Succeed if all parsers succeed.  This can be used as a
% sequencing parser.
%---------------------------------------------------------------------
pAnd( Parsers )
  -> fun( Inp )
       -> all( Parsers, Inp, [] )
     end
   .

all( [], Inp, Accum )
  -> {lists:reverse( Accum ), Inp}
   ;
all( [P | Parsers], Inp, Accum )
  -> case P( Inp ) of
       fail
         -> fail
          ;
       {Result, InpRem}
         -> all( Parsers, InpRem, [Result | Accum] )
     end
   .



%---------------------------------------------------------------------
% Succeed if P1 and P2 succeed.
%---------------------------------------------------------------------
pThen( P1, P2 )
  -> fun( Inp )
       -> case P1( Inp ) of
            {Result1, InpRem1}
              -> case P2( InpRem1 ) of
                   {Result2, InpRem2}
                     -> {[Result1, Result2], InpRem2}
                      ;
                   fail
                     -> fail
                 end
               ;
            fail
              -> fail
          end
     end
   .



%---------------------------------------------------------------------
% Succeed when one of a list of parsers succeeds.
%---------------------------------------------------------------------
pOr( Parsers ) -> fun( Inp ) -> ptry( Parsers, Inp ) end .

ptry( [], _Inp )
  -> fail
   ;
ptry( [P | Parsers], Inp )
  -> case P( Inp ) of
       fail
         -> ptry( Parsers, Inp )
          ;
       Result
         -> Result
     end
   .



%---------------------------------------------------------------------
% Parse 0 or more elements.
%---------------------------------------------------------------------
pMany( P )
  -> fun( Inp )
       -> scan( P, Inp, [] )
     end
   .



%---------------------------------------------------------------------
% Parse 1 or more elements.
%---------------------------------------------------------------------
pMany1( P )
  -> fun( Inp )
       -> Result = scan( P, Inp, [] )
        , case Result of
            {[_ | _], _}
              -> Result
               ;
            _ -> fail
          end
     end
   .



%---------------------------------------------------------------------
% Skip over 0 or more elements
%---------------------------------------------------------------------
pSkipMany( P )
  -> fun( Inp )
       -> {_, InpRem} = scan( P, Inp, [] )
        , {ok, InpRem}
     end
   .

scan( _, [], Accum )
  -> {lists:reverse( Accum ), []}
   ;
scan( P, Inp, Accum )
  -> case P( Inp ) of
       fail
         -> {lists:reverse( Accum ), Inp}
          ;
       {Result, InpRem}
         -> scan( P, InpRem, [Result | Accum] )
     end
   .



%---------------------------------------------------------------------
% Parse input until parser succeeds.  Do not remove successful
% element from input stream.
%---------------------------------------------------------------------
pUntil( P )
  -> fun( Inp )
       -> until( P, Inp, [] )
     end
   .

until( P, Inp, Accum )
  -> case P( Inp ) of
       fail
         -> case Inp of
              [C | Cs]
                -> until( P, Cs, [C | Accum] )
                 ;
              % delaying test for empty list until here allows a parser
              % to check for empty input (pEof)
              []
                -> fail
            end
          ;
       {Result, InpRem}
         -> {{lists:reverse( Accum ), Result}, InpRem}
     end
   .



%---------------------------------------------------------------------
% Parse input while parser succeeds.
%---------------------------------------------------------------------
pWhile( P )
  -> fun( Inp )
       -> while( P, Inp, [] )
     end
   .

while( _, [], _ )
  -> fail
   ;
while( P, Inp = [_C | Cs], Accum )
  -> case P( Inp ) of
       {Result, _InpRem}
         -> while( P, Cs, [Result | Accum] )
          ;
       fail
         -> {lists:reverse( Accum ), Inp}
     end
   .



%=====================================================================
% String parsers.
%=====================================================================

%---------------------------------------------------------------------
% Consume any number of spaces.
%---------------------------------------------------------------------
pSpaces( Inp )
  -> (pMany( fun pSpace/1 ))( Inp )
   .



%---------------------------------------------------------------------
% Consume at least 1 space.
%---------------------------------------------------------------------
pSpaces1( Inp )
  -> (pMany1( fun pSpace/1 ))( Inp )
   .



%---------------------------------------------------------------------
% Match a specific string.
%---------------------------------------------------------------------
pString( S )
  -> fun( Inp )
	-> match( S, Inp, [] )
     end
   .

match( [], String, Accum )
  -> {lists:reverse( Accum ), String}
   ;
match( [C1 | C1s], [C2 | C2s], Accum ) when C1 == C2
  -> match( C1s, C2s, [C1 | Accum] )
   ;
match( _, _, _ )
  -> fail
   .



%---------------------------------------------------------------------
% Parse a capitalized (first letter) word.
%---------------------------------------------------------------------
pCapWord( Inp )
  -> Result = (pAnd( [ fun pUpper/1
                     , fun pAWord/1
                     ] ))( Inp )
   , case Result of
       {[Cap, Word], Rest}
         -> {[Cap | Word], Rest}
          ;
       fail
         -> fail
     end
   .



%---------------------------------------------------------------------
% Parse a non-capitalized (first letter) word.
%---------------------------------------------------------------------
pNonCapWord( Inp )
  -> Result = (pAnd( [ fun pLower/1
                     , fun pAWord/1
                     ] ))( Inp )
   , case Result of
       {[Cap, Word], Rest}
         -> {[Cap | Word], Rest}
          ;
       fail
         -> fail
     end
   .




%---------------------------------------------------------------------
% Parse an upper-case-only word.
%---------------------------------------------------------------------
%pUCWord( Inp )

%---------------------------------------------------------------------
% Parse a lower-case-only word.
%---------------------------------------------------------------------
%pLCWord( Inp )



%---------------------------------------------------------------------
% Get the next word from the input.
%---------------------------------------------------------------------
pAWord( Inp )
  -> Result = parse( pUntil( fun pWordSep/1 ), Inp )
   , case Result of
       fail
         -> fail
          ;
       {{Word, Sep}, Rest}
         -> {Word, [Sep | Rest]}
     end
   .



%---------------------------------------------------------------------
% Get a specific word from the input.
%---------------------------------------------------------------------
pTheWord( W )
  -> fun( Inp )
       -> Result = pAWord( Inp )
        , case Result of
            {W, _Rest}
              -> Result
               ;
            _ -> fail
          end
     end
   .



%=====================================================================
% Character parsers.
%=====================================================================

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
pChar( C )
  -> pSat( fun( C1 ) -> C1 == C end )
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
pDigit( Inp )
  -> (pSat( fun isDigit/1 ))( Inp )
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
pLower( Inp )
  -> (pSat( fun isLower/1 ))( Inp )
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
pNewline( Inp )
  -> (pChar( $\n ))( Inp )
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
pSpace( Inp )
  -> (pSat( fun isSpace/1 ))( Inp )
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
pUpper( Inp )
  -> (pSat( fun isUpper/1 ))( Inp )
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
pWordSep( Inp )
  -> (pOr( [ pSat( fun isWordSep/1 )
           , fun pEoi/1
           ] ))( Inp )
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
pAlphaNum( Inp )
  -> (pSat( fun isAlphaNum/1))( Inp )
   .



%---------------------------------------------------------------------
% Primitive character predicate satisfier.
%---------------------------------------------------------------------
pSat( Pred )
  -> fun( [] )
          -> fail
           ;
        ( [C | Cs] )
          -> case Pred( C ) of
               true
                 -> {C, Cs}
                  ;
               false
                 -> fail
             end
     end
   .



%=====================================================================
% Various predicates.
%=====================================================================

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
isAlpha( C )
  -> case isLower( C ) of
       true
         -> true
          ;
       false
         -> isUpper( C )
     end
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
isAlphaNum( C )
  -> case isAlpha( C ) of
       true
         -> true
          ;
       false
         -> isDigit( C )
     end
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
isDigit( C ) when $0 =< C, C =< $9
  -> true
   ;
isDigit( _C )
  -> false
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
isHexDigit( C ) when $0 =< C, C =< $9; $A =< C, C =< $F; $a =< C, C =< $f
  -> true
   ;
isHexDigit( _C )
  -> false
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
isLower( C ) when $a =< C, C =< $z
  -> true
   ;
isLower( _C )
  -> false
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
isNl( C )
  -> C == $\n
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
isWordSep( C )
  -> case isSpace( C ) of
       true
         -> true
          ;
       false
         -> lists:member( C, ",.:;-+*|=()[]{}" )
     end
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
isSpace( C )
  -> lists:member( C, " \t\n\r" )
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
isUpper( C ) when $A =< C, C =< $Z
  -> true
   ;
isUpper( _C )
  -> false
   .



%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
% isWordSep( C )
%   -> case isSpace( C ) of
%        true
%          -> true
%           ;
%        false
%          -> isPunct( C )
%      end
%    .



%---------------------------------------------------------------------
% eof
%---------------------------------------------------------------------
