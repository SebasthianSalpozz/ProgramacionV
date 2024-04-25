module Parser where
 
import UU.Parsing
import Scanner
    ( Token(..), Type(EndSlide, String, Keyword, OpenBlock, EndBlock, OpenCode, CloseCode) )
import AbstractGrammar
 
pSlides :: AnaParser [Token] Pair Token (Maybe Token) Slides
pSlides = Slides <$> pList pSlide
 
-- pSlide = Slide <$> pTitleSlide <*> pBodySlide <* pEndSlide "---"
pSlide = Slide <$> pBackground <*> pTitleSlide <*> pBodySlide <* pEndSlide "---"

pTitleSlide = TitleSlide <$ pKeyword "!" <*> pStrings

pBackground = Background <$ pKeyword "·" <*> pStrings
 
pBodySlide = BodySlide <$ pOpenBlock "{" <*> pList pMarckdownBlock <* pEndBlock "}"  
 
pMarckdownBlock = MdParagraph <$> pStrings
               <|> MdHeaderH1 <$ pKeyword "#" <*> pStrings
               <|> MdHeaderH2 <$ pKeyword "##" <*> pStrings
               <|> MdHeaderH3 <$ pKeyword "###" <*> pStrings
               <|> MdHeaderH4 <$ pKeyword "####" <*> pStrings
               <|> MdHeaderH5 <$ pKeyword "#####" <*> pStrings
               <|> MdHeaderH6 <$ pKeyword "######" <*> pStrings
               <|> Bold <$ pKeyword "$" <*> pStrings
               <|> Italic <$ pKeyword "$$" <*> pStrings
               <|> BoldItalic <$ pKeyword "$$$" <*> pStrings
               <|> Image <$ pKeyword "**" <*> pStrings
               <|> List <$ pKeyword "+" <*> pStrings
               <|> Code <$ pOpenCode "<" <*> pList pStrings <* pCloseCode ">"
 
--- Join Scanner with Parser
instance Symbol Token
 
getValue:: Token -> String
getValue (Token _ v _ _) = v
 
tSym :: Type -> String -> Parser Token String
tSym typ value = getValue <$> pSym (Token typ value 0 0)
 
tStr = getValue <$> pSym (Token String "" 0 0)
 
pKeyword :: String -> Parser Token String
pKeyword = tSym Keyword
 
pOpenBlock :: String -> Parser Token String
pOpenBlock = tSym OpenBlock
 
pEndBlock :: String -> Parser Token String
pEndBlock = tSym EndBlock
 
pEndSlide :: String -> Parser Token String
pEndSlide = tSym EndSlide
 
pStrings :: Parser Token String
pStrings = tStr

pOpenCode :: String -> Parser Token String
pOpenCode = tSym OpenCode
 
pCloseCode :: String -> Parser Token String
pCloseCode = tSym CloseCode
