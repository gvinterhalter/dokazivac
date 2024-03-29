{-# LANGUAGE OverloadedStrings #-}
{-|
  Module:    HaskHOL.Core.Parser.TypeParser
  Copyright: (c) The University of Kansas 2013
  LICENSE:   BSD3

  Maintainer:  ecaustin@ittc.ku.edu
  Stability:   unstable
  Portability: unknown

  This module defines the parser for 'HOLType's that satisfies the following BNF
  grammar:

@
  TYPE        :: % small-type-variables . TYPE                              
               | SUMTYPE -> TYPE                                            
               | SUMTYPE                                                    
                                                                           
  SUMTYPE     :: PRODTYPE + SUMTYPE                                         
               | PRODTYPE                                                   
                                                                           
  PRODTYPE    :: POWTYPE # PRODTYPE                                         
               | POWTYPE     

  POWTYPE     :: APPTYPE ^ POWTYPE
               | POWTYPE                                         
                                                                           
  APPTYPE     :: ( TYPELIST ) type-constructor [Provided arity matches]     
               | ( TYPELIST ) tyop-var [Provided arity matches or fresh]    
               | small-type-variables+ tyop-var [Special case of above]
               | ( TYPE )                                                   
               | ATOMICTYPE                                                 
                                                                           
  ATOMICTYPE  :: type-constructor      [Provided arity zero]                
               | tyop-var              [Provided arity zero or fresh]       
               | type-variable         [Large or Small]                     
                                                                           
  TYPELIST    :: TYPE , TYPELIST                                            
               | TYPE   
@

  Note that this module also exposes a parser for small type variables to be
  used by the term parser. 

  As a heads up, the error messages thrown by this parser leave much to be
  desired.
-}
module HaskHOL.Core.Parser.TypeParser 
    ( ptype
    , psmall
    ) where

import HaskHOL.Core.Lib

import HaskHOL.Core.Parser.Lib

-- | Parser for HOL types.
ptype :: MyParser thry PreType
ptype = 
    mywhiteSpace >> (putype <|> pbinty "->" "fun" psumty ptype)

-- | Parser for small type variables.
psmall :: MyParser thry PreType
psmall =
    do myreservedOp "'"
       x <- myidentifier
       return $! UTyVar True x 0

popvar :: MyParser thry (Either PreType PreType)
popvar =
    do myreservedOp "_"
       x <- myidentifier
       {-
         Tracks introduction of type operator variables to make sure that all
         tyopvars of the same name in a term are of the same arity.
         Left is fresh.
         Right is existing.
       -}
       let x' = '_' `cons` x
       (_, opvars, _) <- getState
       case lookup x' opvars of
         Nothing -> return . Left $ UTyVar False x' 0
         Just n -> return . Right $ UTyVar False x' n

pbinty :: String -> Text -> MyParser thry PreType -> MyParser thry PreType -> 
          MyParser thry PreType
pbinty op name pty1 pty2 =
    do ty1 <- pty1
       (do myreservedOp op
           ty2 <- pty2
           return $! PTyComb (PTyCon name) [ty1, ty2]) 
        <|> return ty1

putype :: MyParser thry PreType
putype = 
    do myreservedOp "%"
       tvs <- mymany1 psmall
       myreservedOp "."
       ty <- ptype
       return $! foldr PUTy ty tvs

psumty :: MyParser thry PreType
psumty = pbinty "+" "sum" pprodty psumty

pprodty :: MyParser thry PreType
pprodty = pbinty "#" "prod" ppowty pprodty

ppowty :: MyParser thry PreType
ppowty = pbinty "^" "cart" pappty ppowty

pappty :: MyParser thry PreType
pappty =
    do tys <- myparens $ mycommaSep1 ptype
       (do c <- popvar
           case c of
             Left (UTyVar _ s _) ->
               -- fresh ty op var so add it to state
               let n = length tys in
                 do updateState (\ (x, ops, y) -> (x, (s, n):ops, y))
                    let c' = UTyVar False s n
                    return $! PTyComb c' tys
             Right c'@(UTyVar _ _ n) ->
               -- existing ty op var so check arity
               if n == length tys
               then return $! PTyComb c' tys
               else fail "type parser: bad arity for type application"
             _ -> fail $ "type parser: unrecognized case for type operator " ++
                         "variable")
        <|> ((do x <- myidentifier
                 (ctxt, _, _) <- getState
                 case getTypeArityCtxt ctxt x of
                   Nothing -> fail $ "type parser: unsupported type " ++ 
                                     "variable application"
                   Just n ->
                     if n == length tys
                     then return $! PTyComb (PTyCon x) tys
                     else fail "type parser: bad arity for type application")
        <|> (case tys of
               [ty] -> return ty
               _ -> fail "type parser: unexpected list of types"))
   <|> mytry (do tys <- mymany1 psmall
                 c <- popvar
                 case c of
                  Left (UTyVar _ s _) ->
                    let n = length tys in
                      do updateState (\ (x, ops, y) -> (x, (s, n):ops, y))
                         return $! PTyComb (UTyVar False s n) tys
                  Right c'@(UTyVar _ _ n) ->
                    if n == length tys
                    then return $! PTyComb c' tys
                    else fail "type parser: bad type operator application."
                  _ -> fail "type parser: unrecognized case for type operator.")
   <|> (do ty <- patomty
           mytry (do x <- myidentifier
                     (ctxt, _, _) <- getState
                     case getTypeArityCtxt ctxt x of
                       Nothing -> fail $ "type parser: unrecognized constant" 
                                         ++ " in unary application."
                       Just n ->
                           if n == 1
                           then return $! PTyComb (PTyCon x) [ty]
                           else fail $ "type parser: bad arity for unary type" 
                                       ++ " application")
            <|> return ty)

patomty :: MyParser thry PreType
patomty = 
    psmall
    <|> (do c <- popvar
            case c of
              Left c'@(UTyVar _ s 0) ->
                -- fresh ty-op of zero arity
                do updateState (\ (x, ops, y) -> (x, (s, 0):ops, y))
                   return $! PTyComb c' []
              Right c'@(UTyVar _ _ 0) ->
                   return $! PTyComb c' []
              _ -> fail $ "type parser: type operator variable of non-zero " ++
                          "arity outside of application")
    <|> (do x <- myidentifier
            (ctxt, _, _) <- getState
            case x `mapLookup` typeAbbrevsCtxt ctxt of
              Just ty -> return $! pretypeOfType ty
              Nothing -> case getTypeArityCtxt ctxt x of
                           Nothing -> return $! UTyVar False x 0
                           Just 0 -> return $! PTyComb (PTyCon x) []
                           _ -> fail "type parser: bad type construction")

