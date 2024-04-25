module AbstractGrammar where

type Strings = String

data Slides = Slides [Slide]
    deriving Show 

data Slide = Slide Background TitleSlide BodySlide
    deriving Show 

data TitleSlide = TitleSlide Strings
    deriving Show

data BodySlide = BodySlide [MarckdownBlock]
    deriving Show

data Background = Background Strings
    deriving Show

data MarckdownBlock = MdParagraph Strings 
                | MdHeaderH1 Strings
                | MdHeaderH2 Strings
                | MdHeaderH3 Strings
                | MdHeaderH4 Strings
                | MdHeaderH5 Strings
                | MdHeaderH6 Strings
                | Image Strings
                | Bold Strings
                | Italic Strings
                | BoldItalic Strings
                | List Strings
                | Code [Strings]
        deriving Show

