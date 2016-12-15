------------------------------------------------------------------------------
-- Copyright 2012 Microsoft Corporation.
--
-- This is free software; you can redistribute it and/or modify it under the
-- terms of the Apache License, Version 2.0. A copy of the License can be
-- found in the file "license.txt" at the root of this distribution.
-----------------------------------------------------------------------------
module Lib.JSON( readJSON, readJSONFromFile,
                 parseJSON, parseJSONFromFile,
                 JsValue(..), JsObject,
                 toString, jsLookup, jsFind
               ) where

import Data.Char
import Data.List
import Text.Parsec       as Parsec hiding ( token )
import Text.Parsec.Pos   (newPos)
import Text.Parsec.String
import Platform.Runtime (exCatch)


-----------------------------------------------------------
-- JSON data
-----------------------------------------------------------
-- | JSON values
data JsValue = JsNull
             | JsBool Bool
             | JsInt  Integer
             | JsDouble Double
             | JsString String
             | JsArray  [JsValue]
             | JsObject (JsObject JsValue)

-- | A JSON object is a map from fields to JSON values
type JsObject a = [(String,a)]

-- | Convert a JSON value to a string (leaving string values unquoted)
toString :: JsValue -> String
toString v
  = case v of
      JsString s -> s
      _          -> show v

instance Show JsValue where
  show v 
    = case v of
        JsNull     -> "null"
        JsBool b   -> if b then "true" else "false"
        JsInt i    -> show i
        JsDouble d -> show d
        JsString s -> show s
        JsArray vs -> show vs
        JsObject ms-> "{ " ++ concat (intersperse ",\n  " (map showMember ms)) ++ "}"
    where
      showMember (s,v)
        = show s ++ ": " ++ show v

-- | Lookup a JSON value by giving a path of name strings.
-- The name strings can either refer to attributes in an object, or be indexes in an array.
jsLookup :: JsValue -> [String] -> JsValue
jsLookup v [] = v
jsLookup v (p:ps)
  = case v of
      JsObject ms -> case lookup p ms of 
                       Nothing -> JsNull
                       Just v  -> jsLookup v ps
      JsArray vs  | all isDigit p
                  -> let idx = read p in
                     if (idx < length vs)
                      then jsLookup (vs !! idx) ps
                      else JsNull
      _ -> JsNull            

-- | Lookup a JSON value using a default value if it cannot be found.
jsFind :: JsValue -> JsValue -> [String] -> JsValue
jsFind v def path
  = case jsLookup v path of
      JsNull -> def
      val    -> val


-----------------------------------------------------------
-- Parse
-----------------------------------------------------------

-- | Parse JSON from a given string, using FilePath for parse error messages. 
-- | On Error, return a 'JsNull' value.
readJSON :: FilePath -> String -> JsValue
readJSON fname xs
  = case parseJSON fname xs of
      Left err -> JsNull
      Right v  -> v

-- | Parse JSON given a file path and input. The FilePath is used for error messages.
-- Returns either an error message or a 'JsValue'
parseJSON :: FilePath -> String -> Either String JsValue 
parseJSON source xs
  = case parse (do{ skipWhite; pValue}) source xs of
      Left err -> Left (show err)
      Right v  -> Right v


-- | Read a JSON value from a file, return 'JsNull' on errors. 
readJSONFromFile :: FilePath -> IO JsValue
readJSONFromFile fname
  = do res <- parseJSONFromFile fname `exCatch` \e -> return (Left (show e))
       case res of
         Left err -> return JsNull
         Right v  -> return v

-- | Parse a JSON value from a file.
parseJSONFromFile :: FilePath -> IO (Either String JsValue)
parseJSONFromFile fname
  = do txt <- readFile fname
       return (parseJSON fname txt)


-----------------------------------------------------------
-- Values
-----------------------------------------------------------
pValue :: Parser JsValue
pValue
  = token $ choice [stringLit,decimalFloat,pLit]
  <|> pObject <|> pArray


pObject :: Parser JsValue
pObject
  = do token $ string "{"
       xs <- pMembers
       token $ string "}"
       return (JsObject xs)

pMembers :: Parser [(String,JsValue)]
pMembers
  = pMember `sepBy` (token (string ","))

pMember :: Parser (String,JsValue)
pMember 
  = do s <- token stringLit
       token (string ":")
       v <- pValue
       return (toString s,v) 

pArray :: Parser JsValue
pArray 
  = do token $ string "["
       xs <- pValue `sepBy` (token (string ","))
       token $ string "]"
       return (JsArray xs)


-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------
pLit :: Parser JsValue
pLit 
  =   do string "true"; return (JsBool True)
  <|> do string "false"; return (JsBool False)
  <|> do string "null"; return (JsNull)


decimalFloat :: Parser JsValue
decimalFloat    = do{ s <- sign
                    ; n <- decimal
                    ; option (JsInt (s n)) (fractFloat (s n))
                    }

fractFloat n    = do{ f <- fractExponent n
                    ; return (JsDouble f)
                    }

fractExponent n = do{ fract <- fraction 
                    ; expo  <- option 1.0 exponent'
                    ; return ((fromInteger n + fract)*expo)
                    }
                <|>
                  do{ expo <- exponent'
                    ; return ((fromInteger n)*expo)
                    }

fraction        = do{ char '.'
                    ; digits <- many1 digit <?> "fraction"
                    ; return (foldr op 0.0 digits)
                    }
                  <?> "fraction"
                where
                  op d f    = (f + fromIntegral (digitToInt d))/10.0

exponent'       = do{ oneOf "eE"
                    ; f <- sign
                    ; e <- decimal <?> "exponent"
                    ; return (power (f e))
                    }
                  <?> "exponent"
                where
                   power e  | e < 0      = 1.0/power(-e)
                            | otherwise  = fromInteger (10^e)

sign            =   (char '-' >> return negate)
                <|> (char '+' >> return id)
                <|> return id


-- integers
decimal
  = do ds <- many1 digit
       return (digitsToNum 10 ds)

digitsToNum :: Integer -> [Char] -> Integer
digitsToNum base digits
  = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits

-----------------------------------------------------------
-- Strings
-----------------------------------------------------------

stringLit :: Parser JsValue
stringLit   
  = do char '"'
       s <- many (satisfy (\c -> c /= '"' && c /= '\\') <|> escape) <?> ""
       char '"' <?> "end of a string literal"
       return (JsString s)                    
    <?> "string"

escape          
  = do char '\\'
       (charesc <|> hexesc <?> "escape code")

hexesc :: Parser Char
hexesc
  = do digits <- do{ char 'u'; count 4 hexDigit }
                 <|>
                 do{ char 'U'; count 4 hexDigit }
       return (toEnum (fromInteger (digitsToNum 16 digits)))  -- TODO: check overflow

charesc :: Parser Char
charesc         
  = choice (map parseEsc escMap)
 where
    parseEsc :: (Char,Char) -> Parser Char
    parseEsc (c,code) = do{ char c; return code }
    escMap            = zip ("bfnrt\\/\"")
                            ("\b\f\n\r\t\\/\"")



-----------------------------------------------------------
-- Whitespace
-----------------------------------------------------------
token p
  = do x <- p
       skipWhite
       return x

skipWhite 
  = many (whitespace <|> comment)

whitespace :: Parser ()
whitespace
  = do xs <- many1 whitechar
       return ()
  <?> ""

comment :: Parser ()
comment
  = do content <- linecomment <|> blockcomment
       return ()
  <?> ""

linecomment
  = do{ try (string "//")
      ; many (satisfy (/='\n'))
      ; return ()
      }

blockcomment
  = do{ start <- try (string "/*")
      ; incomment 
      }

incomment :: Parser ()
incomment 
    =   do{ try (string "*/");  return () }
    <|> do{ blockcomment;       incomment  }
    <|> do{ many1 contentchar;  incomment  }
    <|> do{ oneOf commentchar;  incomment }
    where
      commentchar     = "*/"
      contentchar     = satisfy (\c -> not (c `elem` commentchar))


-----------------------------------------------------------
-- Character classes
-----------------------------------------------------------

whitechar :: Parser Char
whitechar = satisfy isSpace


-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------
skip :: Parser a -> Parser ()
skip p
  = do{ p; return () }
